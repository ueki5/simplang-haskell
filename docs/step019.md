# 配列型（`[T; N]`）と usize 型の導入における設計上の考慮点

これまでの型は整数（`i32`/`i64`/`u32`/`u64`）・`bool`・ポインタのみで、複数の値をまとめて扱う手段がなかった。今回は固定長配列 **`[T; N]`** と、その添字に使う **`usize`** を導入する。

```
let a: [i32; 5] = [3, 1, 4, 1, 5];
let sum: i32 = 0;
let i: usize = 0;
while i < 5 {
    sum = sum + a[i];
    i = i + 1;
}
sum
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 要素型 | 全スカラー型（`i64`/`i32`/`u64`/`u32`/`bool`）と、そのポインタ型（`&i32`、`&&bool` など）。配列の配列（ネスト）は不可 |
| 配列型の構文 | `[T; N]`（Rust準拠）。`N` は1以上の無型整数リテラル |
| 初期化構文 | `[e1, e2, ...]`（列挙）と `[e; N]`（繰り返し）の両方 |
| 添字の型 | **`usize`**。表現・命令列は `u64` と同一だが、**`u64` とは別の型**（Rust準拠。混在はエラー） |
| `as usize` | `as u64` と同じ処理（同幅の再解釈、32bitからは元の符号で拡張） |
| 範囲外の添字 | **実行時に検査**し、`index out of bounds` を表示して `exit(1)`（ゼロ除算と同方式）。添字が整数リテラルならコンパイル時にも検査する |
| 利用範囲 | **ローカル変数のみ**。`let` による宣言、`a[i]` の読み出し、`a[i] = v;`、`&a[i]` |
| 対象外（エラー） | 配列全体のコピー・代入・比較、値としての使用、fnの引数・戻り値、`&a`（配列へのポインタ）、ネストした配列、空の配列 |

### usize を u64 と別の型にした理由

| 選択肢 | 評価 |
|---|---|
| 別の型（採用） | Rustと同じ。「添字として使う値」が型で区別され、`u64` の値をうっかり添字に使うと `as usize` が必要になる。既存の「整数型の混在はエラー」という規則とも一貫する |
| `u64` の別名 | `u64` の変数をそのまま添字にできて楽だが、型名が2つあるのに区別されないため、エラーメッセージの型名が書いた型名と食い違う |

## 1. 型の表現

```haskell
data Type = TyInt Signedness Width | TUsize | TBool | TPtr Type | TArray Type Int

intRepr :: Type -> Maybe (Signedness, Width)
intRepr (TyInt sg w) = Just (sg, w)
intRepr TUsize = Just (Unsigned, W64)
intRepr _ = Nothing
```

- **`TUsize`** は独立した構築子とし、`intRepr` で `(Unsigned, W64)` に分解する。算術・比較・キャストのコード生成は `TyInt` に直接マッチする代わりに `intRepr` を経由するため、usize は u64 と完全に同じ命令列になる。一方、型の等価判定（`==`）では `TyInt Unsigned W64` と区別されるため、混在は自動的に `type mismatch` になる
- usize もキャスト先やリテラルのサフィックスに使えるよう、`Cast Expr Type`・`LitTyped Int Type`・`TIntSuffixed Int Type` は（符号・幅の組ではなく）`Type` を持つように変更した。`5usize` サフィックスを追加した
- **`TArray 要素型 要素数`**: `typeName` では `[i32; 3]` と表示する。`storageWidth` は要素1個の格納幅を返し、配列全体の占有バイト数は新しいヘルパー `stackBytes`（＝要素数 × 要素の格納幅）が返す

## 2. 構文

```
type       ::= '&' type | '[' type ';' INT ']' | 'i32' | 'i64' | 'u32' | 'u64' | 'usize' | 'bool'
index-assign-stmt ::= IDENT '[' expr ']' '=' expr ';'
factor     ::= '-' factor | '!' factor | '&' factor | '*' factor | postfix
postfix    ::= primary ('[' expr ']')*
primary    ::= ...（従来のfactorの非単項部分） | '[' expr (',' expr)* ']' | '[' expr ';' INT ']'
```

- 添字 `[]` は後置演算子で、**単項演算子より強く**結合する（Rustと同じ）。`-a[0]` は `-(a[0])`、`&a[i]` は `&(a[i])`、`*ps[0]` は `*(ps[0])` になる。そのため、従来の `parseFactor` を「単項演算子 → 後置添字 → primary」の3段に分割した
- **要素への代入文と末尾式の判別**: 文の先頭で `IDENT '['` を見たら `IDENT '[' expr ']'` まで試しに読み、次が `=` なら代入文とする。そうでなければ文はないものとして元のトークン列に戻し、末尾式（例: `a[0]`）として読ませる
- 新しいAST: `ArrayLit [Expr]`・`ArrayRepeat Expr Int`・`Index Expr Expr`（式）、`SAssignIndex String Expr Expr`（文）

## 3. メモリレイアウト

配列は、スタックフレーム上で要素を連続して並べたものとする。他の変数と同じく、`cursor` から配列全体のサイズだけ下に詰めて割り付ける。

```
off = cursor - N * elemSize   -- 配列の先頭（最下位アドレス）
要素 i のアドレス = %rbp + off + i * elemSize
```

| 要素型 | elemSize |
|---|---|
| i32 / u32 / bool | 4 |
| i64 / u64 / ポインタ | 8 |

`frameSize` は、既存の `Load`/`Store`/`LoadAddr` のオフセット走査で配列の先頭（最も深いオフセット）を拾うため、変更は不要だった。

## 4. 命令

要素のアドレスは、既存の命令を組み合わせて計算する（`indexAddr`。読み出し・代入・`&a[i]` で共用）。

```
LoadAddr off          -- 配列先頭のアドレス
<添字の式>            -- 期待型 usize でコンパイル
IBoundsCheck N        -- 範囲検査（新命令）
Push elemSize
IMul W64
IAdd W64              -- 先頭 + 添字 × 要素サイズ
```

| 構文 | 命令列 |
|---|---|
| `a[i]` | `indexAddr ++ [LoadInd w] ++ loadFixup 要素型`（u32 は `IZext32` を付ける） |
| `a[i] = v;` | `indexAddr ++ <v> ++ [StoreInd w]` |
| `&a[i]` | `indexAddr` のみ（型は `&要素型`） |

追加した命令:

| 命令 | x86-64 | 説明 |
|---|---|---|
| `IBoundsCheck n` | `movq (%rsp),%rax; movabsq $n,%rcx; cmpq %rcx,%rax; jae .Lindex_error` | スタック先頭の添字をpopせずに覗き見る。**符号なし比較**（`jae`）なので、`0 - 1` のようにラップアラウンドした巨大な添字も範囲外として検出される。スタックの増減は0 |
| `StoreInd w` | `popq %rax; popq %rcx; movq %rax,(%rcx)`（W32は`movl %eax,(%rcx)`） | 値、アドレスの順にpopして間接ストアする。スタックの増減は−2 |

`.Lindex_error` は `.Ldiv_zero_error` と同じく、ファイル末尾の共通部分に一度だけ出力する（`puts("index out of bounds")` → `exit(1)`）。

### 初期化

`let` の型が配列型のときは、専用の `compileArrayInit` が担当する。要素ごとに静的なオフセットへ直接 `Store` する。

- `[e1, ..., eN]`: 各 `ei` を要素型でコンパイルし、`Store w (off + i*size)`
- `[e; N]`: `e` を**1回だけ**評価して要素0へストアし、残りの要素へは `Load w off0; Store w off_i` でコピーする（`e` が関数呼び出しでも副作用は1回のみ）
- 要素数が型注釈と食い違えばエラー。配列リテラル以外（配列変数など）による初期化もエラー（配列のコピーは対象外）

### 型推論

- `let a = [1u32, 2, 3];` のように型注釈を省略した場合は、要素の型を単一化して要素型を決める（全要素が無型リテラルなら `i64`）。`inferMaybeType`・`resolveExprType`（fnシグネチャ推論）の両方に `ArrayLit`/`ArrayRepeat`/`Index` を追加した
- 添字は常に期待型 `usize` でコンパイルする。無型リテラル `a[0]` はそのまま usize になるが、`let i = 0;` は従来どおり `i64` に推論されるため、ループ変数は `let i: usize = 0;` と書く必要がある（使われ方からのボトムアップ推論は行わない、という既存の方針のまま）

## 5. エラーメッセージ

| ケース | メッセージ |
|---|---|
| 要素数の不一致 | `array length mismatch: expected 3, found 2` |
| リテラル添字の範囲外（コンパイル時） | `index out of bounds: the length is 3 but the index is 3` |
| 添字の範囲外（実行時） | `index out of bounds`（exit 1） |
| u64 などを添字に使った | `type mismatch: expected usize, found u64` |
| 配列を値として使った | `array cannot be used as a value: a` |
| 配列全体への代入 | `cannot assign to array a; assign to elements instead` |
| 配列のアドレス取得 | `cannot take address of array a` |
| 配列リテラル以外で初期化 | `array must be initialized with an array literal` |
| 配列でない値への添字 | `cannot index into a value of type i64` |
| fnシグネチャ中の配列（注釈・推論とも） | `array type is not allowed in function signature: f` |
| ネストした配列 / 配列へのポインタ | `nested arrays are not supported` / `pointer to array is not supported` |
| 空の配列 / 長さ0 | `empty array literal is not supported` / `array length must be at least 1` |
| usize への単項マイナス | `cannot apply unary minus to unsigned type usize` |

## 6. その他の考慮点・スコープ外

- **配列へのポインタ・関数への受け渡し**: 今回はローカル変数に限定した。`&[T; N]` や、スライス（長さ情報付きのポインタ）による関数への受け渡しは将来の拡張とする
- **配列のコピー・比較**: 要素単位のコピーが必要になるため対象外とした
- **ポインタ経由の書き込み**: `*p = v;` は引き続き未対応。ただし、配列要素への書き込み用に `StoreInd` を導入したため、将来はこれを流用して実現できる
- **アライメント**: 既存の変数と同じタイトパッキングのため、i64 の配列が4バイト境界に置かれることがある。x86-64 では非アライメントアクセスも正しく動作するため、許容する

## 7. テストへの影響

- 既存テストは、`Cast e Signed W32` → `Cast e (TyInt Signed W32)` のように機械的に置き換えた（`LitTyped`/`TIntSuffixed` も同様）。キャスト先エラーのメッセージには `usize` を追記した
- 追加したテスト:
  - 字句解析・構文解析: `[`/`]`、`usize` サフィックス、配列型、2種類の配列リテラル、添字と単項演算子の優先順位、代入文と末尾式の判別、`as usize`、構文エラー
  - 命令列: 初期化の `Store` 列、`[e; N]` のコピー、読み出し・代入、usize の演算とキャスト、配列の後ろの変数の割り付け
  - 意味論エラー: 5章の各ケース
  - VM / CodeGen: `IBoundsCheck`・`StoreInd`・usize の `%lu` 出力
  - Integration: 全要素型（i32/i64/u32/u64/bool/ポインタ）の配列、while ループ、`&a[i]`、`[e; N]` の1回評価、関数内のローカル配列、隣接変数を壊さないこと、usize の出力、実行時の範囲外（読み出し・代入）
- サンプル: `sample/samp019-0〜3.sl`

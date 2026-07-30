# to_i64/to_i32 単項演算子の導入における設計上の考慮点

`docs/step003.md` で `bool` 型と比較演算子を追加した際、`compileExprTyped :: Env -> Type -> Expr -> Either String [Instr]` の「期待する型（`expected`）を式木全体に一様伝播させる」という前提が `Eq`/`Neq`（被演算子の型が `expected` とは独立に決まる）によって初めて崩れた。今回追加する `to_i64`/`to_i32` は、この非対称性がさらに単純な形で再び現れるケースである。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 追加する演算子 | `to_i64`（i32→i64の拡大変換）、`to_i32`（i64→i32の縮小変換）。いずれも単項演算子 |
| 対象型 | `to_i64` は被演算子が **i32のみ**（i64・boolは不可）、`to_i32` は被演算子が **i64のみ**（i32・boolは不可）。整数とboolの暗黙変換は行わない（既存方針を踏襲） |
| 構文 | `-`/`!` と同じ `factor` レベルの前置単項演算子。`to_i64 x` と `to_i64(x)` はどちらも同じASTになる（括弧は`factor`の別ルールに過ぎず、キャスト構文専用のものではない） |
| 拡大変換の値保存 | `to_i64` はi32の値をそのままi64として保存する（値の変化なし） |
| 縮小変換の丸め方 | `to_i32` は下位32bitを符号拡張した値になる。既存のi32演算のラップアラウンド規則（`docs/step002.md`）と同じ挙動 |
| 実行時エラー | キャストは実行時に失敗しない（ゼロ除算のような専用エラーパスは不要） |

## 1. `docs/step003.md` からの変更点（要約）

| ファイル / 項目 | step003まで | step004での変更 |
|---|---|---|
| `Token` | — | `TToI64`、`TToI32` を追加 |
| `tokenize` | `let`/`true`/`false` の予約語判定 | `to_i64`/`to_i32` も同じ仕組みで予約語判定を追加（識別子スキャン後の完全一致で判定するため、`to_i64x` のような接頭辞一致は引き続き `TIdent` のまま） |
| 文法 | `factor ::= INT \| 'true' \| 'false' \| IDENT \| '(' expr ')' \| '-' factor \| '!' factor` | `\| 'to_i64' factor \| 'to_i32' factor` を追加。`-`/`!` と同じ位置に置くことで、単項演算子同士の入れ子（`to_i32(to_i64(x))`、`to_i32 -x` 等）が既存の再帰構造でそのまま扱える |
| `Expr` | — | `ToI64 Expr`、`ToI32 Expr` を追加 |
| `Instr` | — | `ISext32` を追加（後述、`to_i64`/`to_i32` で共通） |
| `inferMaybeType` | — | `ToI64`/`ToI32` のケースを追加。`BoolLit` と同様、被演算子を辿らずに常に確定型を返す（`ToI64 _ -> Just (TyInt W64)`、`ToI32 _ -> Just (TyInt W32)`） |
| `compileExprTyped` | — | `ToI64`/`ToI32` 用のケースを追加（後述） |
| `codegen`（`genInstr`） | — | `ISext32` → `popq %rax; cltq; pushq %rax` を追加 |
| `run`（VM） | — | `ISext32` の評価ケースを追加（`trunc W32` を再利用） |
| `app/Main.hs` | — | 変更なし（`compile`/`codegen`のシグネチャに変化がないため） |

## 2. 型チェック：`expected` 一様伝播の非対称性

`Eq`/`Neq` は「ノード自体の型は常に `bool`」「被演算子の型は `expected` とは独立に推論される」という2点で非対称だったが、`to_i64`/`to_i32` は被演算子の型が**推論すら不要**なほど単純な非対称性を持つ：

- `ToI64 e` のノード型は常に `i64` 固定（`expected` が `i64` でなければ即エラー）。子 `e` へ渡す期待型は `expected` を伝播させるのではなく、**常に `i32` 固定**
- `ToI32 e` はこの逆（ノード型は常に `i32` 固定、子への期待型は常に `i64` 固定）

`Eq`/`Neq` のように被演算子同士を単一化する `operandType` は不要で、`Not`（被演算子・結果ともに `bool` 固定）と同じくらい単純な固定型ディスパッチで済む：

```haskell
compileExprTyped _ TBool (ToI64 _) = Left "type mismatch: expected bool, found i64"
compileExprTyped _ expected@(TyInt W32) (ToI64 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i64")
compileExprTyped env (TyInt W64) (ToI64 e) = do
  ei <- compileExprTyped env (TyInt W32) e
  Right (ei ++ [ISext32])

compileExprTyped _ TBool (ToI32 _) = Left "type mismatch: expected bool, found i32"
compileExprTyped _ expected@(TyInt W64) (ToI32 _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found i32")
compileExprTyped env (TyInt W32) (ToI32 e) = do
  ei <- compileExprTyped env (TyInt W64) e
  Right (ei ++ [ISext32])
```

`inferMaybeType` にも `BoolLit` と同様の「常に確定型」ケースを追加する。`Lit`（`Nothing` = 未確定）とは異なり、キャスト結果の型は被演算子によらず演算子自体で確定するため、子の部分木を辿る必要はない。

### 副次的に生じる挙動

- `to_i64(to_i64(x))` のような同方向の二重キャストは、内側ノードの型（`i64` 固定）と外側が要求する子の型（`i32` 固定）が食い違うため自然に型エラーになる
- `to_i32(to_i64(x))` のようなラウンドトリップ（i32→i64→i32）は文法・型チェックともに問題なく通る（値は保存される）

いずれも `docs/step003.md` §8 の「比較演算子のチェイン」と同種の、文法から自然に導かれる決定済みの挙動として扱う（積極的に禁止する仕組みは追加しない）。

## 3. コード生成：`to_i64`と`to_i32`が同一命令を共有する理由

### 検討した案：`to_i64`をノーコストにする

`docs/step002.md`/CLAUDE.mdに明記された不変条件「i32演算は32bit命令の後に符号拡張して64bitスタックへ戻し、演算のたびにラップアラウンドする」により、**正しくi32型として扱われた値は常にスタック上で符号拡張済みの64bit表現になっている**。この不変条件を信頼すれば、i32→i64の拡大変換は理論上、追加命令なしの単なる型タグの付け替えで済む。

### 採用しなかった理由：リテラル直渡しでの不変条件の破れ

`Push`命令（`movabsq`でリテラルをそのまま積む）は単独では32bit幅への切り詰めを一切行わない。既存コードでi32文脈のリテラルが実際に切り詰められるのは、`Store W32`（メモリ書き込み時に`movl`で上位32bitが破棄される）や算術命令（`cltq`/`movslq`を伴う）を経由した場合のみである。

`to_i64`の被演算子は「常にi32」という固定の期待型で子を再帰コンパイルするため、`to_i64(5000000000)`のような範囲外リテラルが `Store`/算術演算を経由せず直接キャストの被演算子になるケースが**新たに**生まれる。ここで`to_i64`をノーコスト実装にすると、未切り詰めのビットパターンがそのままi64値として漏れてしまう。

### 採用した設計

`to_i64`も`to_i32`と同じ「下位32bitを符号拡張して64bitへ戻す」命令 `ISext32`（`popq %rax; cltq; pushq %rax`）を使い、**常に明示的に正規化する**。ビット演算としては両方向とも同一操作（下位32bitを符号拡張で64bitに戻す）なので、新規`Instr`は1種類で足りる。多少の実行コストと引き換えに、リテラル起因の未切り詰めバグを構造的に防ぐ。

`to_i32(5000000000)`のような縮小変換側の範囲外リテラルは、そもそも被演算子がi64固定でリテラルがそのまま`Push`されるため問題は起きない（`ISext32`が実際に縮小処理を行う）。

## 4. VM（`run`、テスト用スタックマシン）

`ISext32`を追加。既存の`trunc :: Width -> Int -> Int`ヘルパー（`trunc W32`）をそのまま再利用でき、専用の評価ロジックは不要（`docs/step002.md`で定義済みの符号拡張シミュレーションが両方向のキャストに転用できる）。

## 5. テストへの影響

- Tokenizer: `to_i64`/`to_i32`予約語の変換、接頭辞一致する識別子（`to_i64x`等）が引き続き`TIdent`になる回帰確認
- Parser: 括弧あり/なしが同一ASTになること、単項演算子としての結合順位（`-`/`!`より弱い優先順位で単項演算子同士が入れ子にできること）
- CodeGen: `ISext32`の命令出力確認
- 意味論エラー（compile）: `to_i64`にi64/bool値を渡す、`to_i32`にi32/bool値を渡す、キャスト結果を誤った型コンテキストで使う
- VM: 拡大変換での値保存、縮小変換でのラップアラウンド（符号あり・なし両方の境界値）
- Integration: 実際にGCCでコンパイルしたバイナリでの拡大変換・縮小変換（範囲外値のラップアラウンド含む）、括弧なし構文、ラウンドトリップキャスト

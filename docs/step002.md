# i32（32bit符号付き整数）型の導入における設計上の考慮点

`docs/step001.md` で導入した変数機能は型が `i64` の1種類のみで、型情報はパース時に検証した後は破棄され、AST・IR・コード生成のどこにも保持されていなかった。今回 `i32` を追加するにあたり、「型は1種類なので型チェック不要」という前提が崩れるため、パイプラインの各段階に型の概念を通す必要がある。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| サポートする型 | `i32`（32bit符号付き整数）、`i64`（既存の64bit符号付き整数） |
| 型混在 | 同一式内で `i32` 変数と `i64` 変数を混在させることは**コンパイルエラー**とする |
| オーバーフロー | i32が関わる演算（`+` `-` `*` `/` 単項`-`）は、演算のたびに32bit幅で切り詰める（Cの`int`と同様、毎回ラップアラウンドする） |
| スタックレイアウト | 変数のスタックスロットは型の実サイズで確保する（i32=4バイト、i64=8バイト）。タイトパッキングとし、8バイト固定では確保しない |
| リテラルの型 | リテラル自体は無型（ポリモーフィック）。宣言・代入の期待型、または式中の変数参照から型が決まる。式全体を通して型の手がかりが1つも無ければ `i64` をデフォルトとする |

## 1. 型の表現（`Width`）

ソースレベルの型名（`"i32"`/`"i64"`）とコード生成レベルのレジスタ幅を別々の型として持たず、1つの型 `Width` で兼用する（既存の `Instr` がすでに Compiler/CodeGen 両方から参照される共有型であるのと同じ位置づけ）。

```haskell
data Width = W32 | W64 deriving (Show, Eq)
```

- `parseType :: String -> Either String Width` — `"i32" -> W32`、`"i64" -> W64`、それ以外は `Left "unsupported type: <name>"`
- `widthBytes :: Width -> Int` — スタック上の占有バイト数（`W32 -> 4`、`W64 -> 8`）
- `typeName :: Width -> String` — エラーメッセージ表示用（`W32 -> "i32"`、`W64 -> "i64"`）

型名 `i32`/`i64` はこれまでどおり専用トークンを増やさず `TIdent` として字句解析し、`parseLetStmt` 側で `parseType` により検証する。

## 2. Lexer / Parser

- 文法上の変更は型名のバリエーションが増えるのみ: `let-stmt ::= 'let' IDENT ':' ('i32' | 'i64') '=' expr ';'`
- `Stmt` に型情報を追加: `data Stmt = SLet String Width Expr | SAssign String Expr`（`SAssign` は代入先の型を変数宣言時の環境から引けるため型注釈は不要）
- `Instr` に `Width` フィールドを追加: `IAdd Width | ISub Width | IMul Width | IDiv Width | INeg Width | Load Width Int | Store Width Int`

## 3. 型チェックの実装方針（式ノードへの型タグ付けは不要）

`let`/代入は構文上つねに「期待される幅」が確定している（`let` は型注釈から、代入は環境に登録済みの変数の型から）。そのため式コンパイルは「期待する `Width` を受け取り、その幅で一様に命令を出す」1本の関数 `compileExprTyped :: Env -> Width -> Expr -> Either String [Instr]` で完結する。

- `Lit n` はリテラルなので期待幅をそのまま採用し `Push n` を出す（無型）
- `Var name` は環境から実際の型を引き、期待幅と食い違えば `type mismatch: expected <expected>, found <actual>` でエラー
- 二項演算・単項マイナスは左右の部分式を同じ期待幅で再帰的にコンパイルし、最後に期待幅を積んだ演算命令（`IAdd expected` 等）を1つ追加する

同一式内で異なる型の変数が混在すれば、木のどこかで必ず `Var` の型チェックに引っかかるため、式ノードごとに型タグを持たせたり、ボトムアップの型推論・単一化を行う必要はない。

末尾式だけは宣言のような「期待幅」の手がかりがない。ここだけ別途 `inferType :: Env -> Expr -> Either String Width` を用意し、式中の `Var` 参照から型を集めて単一化する（`Maybe Width` で「まだリテラルしか見ていない」を表現し、`Nothing` 同士の単一化は `Nothing`、型が食い違えば `Left "type mismatch: <w1> and <w2>"`、最後まで `Nothing` なら `W64` にデフォルトする）。得られた `Width` を使って同じ `compileExprTyped` を呼べばよい。この末尾式の解決幅が、そのままプログラム全体の出力幅（`printf` のフォーマット選択）になる。

## 4. コンパイラ: スタックスロットのタイトパッキング

`compileStmts` の状態に「次に使えるオフセット」を表すカーソル（`Int`、0始まりで負方向に伸びる）を追加し、変数ごとに実サイズ分だけ切り出す。

```haskell
step (env, cursor, acc) (SLet name width expr) = do
  ...
  let sz  = widthBytes width
      off = cursor - sz
  Right (Map.insert name (off, width) env, off, acc ++ instrs ++ [Store width off])
```

`Env` は `Map String (Int, Width)`（オフセットと型のペア）に変更する。`codegen` 側の `frameSize`（`Load`/`Store` の最大絶対オフセットをスキャンする方式）はロジック変更不要。x86-64は `movq`/`movl`/`movslq` のアンアラインアクセスを許容するため、4バイト単位のタイトパッキングでも正当性上の問題はない。既存設計どおり `call` 直前の `andq $-16, %rsp` が最終的なアライメントを保証する。

## 5. コード生成（32bit命令）

64bit側は既存のまま。32bit側は「32bit幅で演算 → 上位32bitを符号拡張して64bitスタックへ戻す」というパターンで統一する。スタックマシン自体のpush/popは常に `q` 接尾辞のままとし、値スタック上は常に「正しく符号拡張された64bit表現」という不変条件を保つ。

| Instr | 命令列 |
|---|---|
| `IAdd W32` | `popq %rax; popq %rbx; addl %ebx,%eax; cltq; pushq %rax` |
| `ISub W32` | `popq %rax; popq %rbx; subl %eax,%ebx; movslq %ebx,%rbx; pushq %rbx` |
| `IMul W32` | `popq %rax; popq %rbx; imull %ebx,%eax; cltq; pushq %rax` |
| `IDiv W32` | `popq %rcx; cmpq $0,%rcx; je .Ldiv_zero_error; popq %rax; cltd; idivl %ecx; cltq; pushq %rax` |
| `INeg W32` | `popq %rax; negl %eax; cltq; pushq %rax` |
| `Load W32 off` | `movslq off(%rbp), %rax; pushq %rax`（符号拡張ロード） |
| `Store W32 off` | `popq %rax; movl %eax, off(%rbp)`（自然にラップアラウンドする切り詰めストア） |

ゼロ除算チェックは幅に関わらず `cmpq $0, %rcx` のままでよい（スタック上の値は常に符号拡張済みなので、64bit比較でゼロ判定しても32bit版と結果は一致する）。

`printf` のフォーマット文字列は末尾式の解決幅に応じて `fmt32`（`"%d\n"`）/ `fmt64`（`"%ld\n"`）を出し分ける。`codegen :: Width -> [Instr] -> String` に変更し、`compile :: Program -> Either String (Width, [Instr])` の返り値から得た最終幅をそのまま渡す。これは同時に「64bit値なのに常に `%d` を使っていた」既存の潜在バグの修正にもなる。

### 付随する修正: `Push` の即値を `movabsq` 経由にする

`i32` の境界値（特に `INT32_MIN = -2147483648`、`Neg (Lit 2147483648)` という形でパースされる）で `Push 2147483648` が生成されるが、x86-64 の `pushq` は符号拡張32bit即値しか受け付けないため、2147483648（符号付き32bit範囲外）を直接 `pushq $2147483648` すると正しく動作しない。`genInstr (Push n) = ["movabsq $" ++ show n ++ ", %rax", "pushq %rax"]` に変更し、任意の64bit即値を安全に扱えるようにした。

## 6. VM（`run`、テスト用スタックマシン）

`Instr` へのWidth追加にあわせてパターンマッチを更新し、演算結果に `trunc :: Width -> Int -> Int` を適用して実アセンブリと同じラップアラウンドを再現する（`Int32` への往復キャストで実装）。

```haskell
trunc W64 n = n
trunc W32 n = fromIntegral (fromIntegral n :: Int32)
```

あわせて、`IDiv` の除算に使っていたHaskellの `div`（床関数）を `quot`（0方向丸め）に修正した。実アセンブリの `idivq`/`idivl` は0方向丸めであり、`div` のままでは負数オペランドの除算結果が実行結果と食い違う既存の潜在的な不整合があったため、Widthフィールド追加のタイミングで合わせて修正した。

## 7. テストへの影響

- Parser: `i32` 型注釈のパース（正常系）、`i32`/`i64` 以外の型名のエラー
- 意味論エラー（compile）: `let`初期化・代入・末尾式それぞれでのi32/i64混在エラー、宣言型と異なる型の代入エラー
- CodeGen: i32用の32bit命令（`addl`/`subl`/`imull`/`idivl`/`negl`/`movslq`/`movl`）の出力確認、`fmt32`/`fmt64` の出し分け
- VM: `IAdd W32` 等のラップアラウンド、`Store W32` の切り詰め
- Integration: i32変数の宣言・演算・出力、オーバーフローのラップアラウンド、`INT32_MIN` 境界値（`movabsq`修正の確認）、i32/i64が交互に宣言された場合のタイトパッキング、i32変数のゼロ除算

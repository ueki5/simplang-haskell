# bool型と等価/非等価/否定演算子の導入における設計上の考慮点

`docs/step002.md` で `i32`/`i64` を導入した際、型は `Width`（`W32`/`W64`）という1つの値で「ソースの型名」と「codegenのレジスタ幅」を兼用していた。今回 `bool` 型と `==`/`!=`/`!` を追加するにあたり、この前提が崩れる: `bool` はレジスタ幅を持たない論理型であり、また `a == b` は「幅の揃った2つの被演算子」を取りつつ結果は**別の型（bool）**を返すため、算術演算の「入力と出力が同じ幅で閉じている」性質が成り立たない。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| サポートする型 | `i32`/`i64`（既存）に加えて `bool`（`true`/`false` の2値） |
| 追加する演算子 | `==`（等価）、`!=`（非等価）、`!`（否定、単項） |
| 型混在 | 比較演算子の被演算子は同じ型（`i32`同士、`i64`同士、または`bool`同士）でなければコンパイルエラー。整数とboolの暗黙変換は行わない |
| 否定演算子 | 被演算子は `bool` のみ。整数への `!` 適用はコンパイルエラー（Cのような「非ゼロは真」という暗黙変換はしない） |
| 比較結果の型 | `==`/`!=` の結果は常に `bool`。被演算子の型（`i32`/`i64`/`bool`）とは独立している |
| 末尾式がboolの場合の出力 | 標準出力に `"true"` / `"false"` を出力する（0/1の整数ではない） |
| `true`/`false` | `let` と同様の予約語としてトークナイザで判定する（`bool` という型名自体は `i32`/`i64` と同様、識別子として `parseType` でバリデーションする） |

## 1. `docs/step002.md` からの変更点（要約）

| ファイル / 項目 | step002まで | step003での変更 |
|---|---|---|
| 型の表現 | `Width`（`W32`/`W64`）1種類のみ。ソース型名とレジスタ幅を兼用 | `Width` はレジスタ幅として維持しつつ、`bool`を含むソースレベルの型として新たに `Type = TyInt Width \| TBool` を追加 |
| `Stmt` | `SLet String Width Expr` | `SLet String Type Expr` |
| `Env`（`Compiler.hs`内部） | `Map String (Int, Width)` | `Map String (Int, Type)` |
| `parseType` | `String -> Either String Width`（`i32`/`i64`のみ） | `String -> Either String Type`（`bool`ケースを追加） |
| `Token` | — | `TEq`（`==`）、`TNeq`（`!=`）、`TBang`（`!`）、`TTrue`、`TFalse` を追加 |
| `tokenize` | 1文字先読みのみ | `=`/`==`、`!`/`!=` を判別する2文字先読みを追加 |
| 文法 | `expr ::= additive`（加減算が最上位） | `equality`（`==`/`!=`）を`additive`の上位に追加。`parseEquality`を新設し、式が現れる全箇所（末尾式・`let`/代入の右辺・括弧の中）をこれに差し替え |
| `Expr` | `Lit`/`Var`/`Add`/`Sub`/`Mul`/`Div`/`Neg` | `BoolLit Bool`、`Eq Expr Expr`、`Neq Expr Expr`、`Not Expr` を追加 |
| `Instr` | 算術系 + `Load`/`Store`（すべて`Width`引数あり） | `ICmpEq`、`ICmpNe`、`INot` を追加（`Width`引数なし、§4参照） |
| `inferType` | 戻り値 `Either String Width` | 戻り値 `Either String Type`。`BoolLit`/`Not`/`Eq`/`Neq`のケースを追加し、内部の単一化ロジックを`inferMaybeType`/`unifyMaybeType`として`operandType`と共有できる形に分離 |
| `compileExprTyped` | `Env -> Width -> Expr -> Either String [Instr]` | `Env -> Type -> Expr -> Either String [Instr]`。`Eq`/`Neq`用に被演算子の型を独立推論する`operandType`ヘルパーを新設（§4） |
| `compile` | `Either String (Width, [Instr])` | `Either String (Type, [Instr])` |
| `codegen` | `Width -> [Instr] -> String` | `Type -> [Instr] -> String`。`epilogue`が`Type`で分岐し、bool時は`testq`/`jne`で`"true\n"`/`"false\n"`を出し分ける新経路を追加（`fmt32`/`fmt64`側は無変更） |
| `run`（VM） | — | `ICmpEq`/`ICmpNe`/`INot`のパターンを追加（bool値はIntの0/1としてそのまま扱う） |
| `app/Main.hs` | — | `compile`/`codegen`の型変更に追従して変数名を`width`から`finalType`に変更（ロジック変更なし） |

## 2. 型の表現（`Type` の新設）

`Width` は「整数のレジスタ幅」を表す既存の型のまま残し、`bool` を含むソースレベルの論理型として新たに `Type` を導入する。

```haskell
data Width = W32 | W64 deriving (Show, Eq)
data Type = TyInt Width | TBool deriving (Show, Eq)
```

（Haskellの制約上、整数リテラルの字句トークン `TInt Int` と型コンストラクタ名が衝突するため、型側は `TyInt` という名前にした。）

- `parseType :: String -> Either String Type` — `"i32" -> TyInt W32`、`"i64" -> TyInt W64`、`"bool" -> TBool`
- `storageWidth :: Type -> Width` — 変数の物理格納幅。`bool` は専用の1バイトスロットを新設せず、既存の32bit `Load`/`Store` 経路（`i32`用のスロット）をそのまま流用する（`storageWidth TBool = W32`）
- `typeName :: Type -> String` — エラーメッセージ表示用（`TBool -> "bool"` を追加）

`Env`（変数名→オフセット・型のマップ）、`Stmt` の `SLet` が保持する型情報は、いずれも `Width` から `Type` に置き換える。`Instr`（`Load`/`Store`/算術命令）や `codegen` の命令レベルの表現は `Width` のまま変更しない。

## 3. Lexer / Parser

- 新規トークン: `TEq`（`==`）、`TNeq`（`!=`）、`TBang`（`!`）、`TTrue`、`TFalse`
- `tokenize` は `=`/`==` と `!`/`!=` を判別するため2文字先読みが必要。`tokenize ('=' : '=' : cs)` / `tokenize ('!' : '=' : cs)` という2文字パターンを、既存の1文字ガード（`c == '='` 等）より前に置くことで対応した（Haskellのパターンマッチは上から順に試されるため、より具体的なパターンを先に置く必要がある）
- `true`/`false` は `let` と同じ扱いで、識別子スキャン後の文字列を見て予約語判定する（`"true" -> TTrue`、`"false" -> TFalse`、それ以外は `TIdent`）
- 文法に等価演算子の優先順位レベルを追加した:

  ```
  expr     ::= equality
  equality ::= additive (('==' | '!=') additive)*
  additive ::= term (('+' | '-') term)*
  term     ::= factor (('*' | '/') factor)*
  factor   ::= INT | 'true' | 'false' | IDENT | '(' expr ')' | '-' factor | '!' factor
  ```

  `parseEquality`/`parseEqualityRest` を既存の `parseExpr`（加減算）の上位として追加し、式が現れる場所（プログラムの末尾式、`let`/代入の右辺、括弧の中）はすべて `parseEquality` を呼ぶように変更した。`!` は単項 `-` と同じ形で `parseFactor` に追加する（`Not <$> parseFactor rest`）ため、`-`/`!` の単項演算子は `==`/`!=` より強く結合する
- `Expr` に `BoolLit Bool`、`Eq Expr Expr`、`Neq Expr Expr`、`Not Expr` を追加
- `Instr` に `ICmpEq`、`ICmpNe`、`INot` を追加

## 4. 比較・否定の型チェック（`expected` の一様伝播が破綻する箇所）

既存の `compileExprTyped :: Env -> Width -> Expr -> Either String [Instr]` は「期待する型を式木全体に一様に伝播させる」設計だった。算術演算はこれで閉じるが、比較演算子は破綻する:

- `Not e` は被演算子・結果ともに `bool` なので問題なく伝播できる
- `Eq l r` / `Neq l r` は、外側から伝播してくる `expected`（`bool` のはず）を被演算子にそのまま渡せない。被演算子 `l`/`r` の型は `expected` とは独立に決まる（`i32`同士かもしれないし`i64`同士かもしれない）

この非対称性に対応するため、`compileExprTyped :: Env -> Type -> Expr -> Either String [Instr]` の `Eq`/`Neq` ケースだけは、被演算子の型を独立に推論する `operandType :: Env -> Expr -> Expr -> Either String Type` を呼び、その型で両辺を再帰コンパイルしてから `ICmpEq`/`ICmpNe` を1つ追加する。`operandType` は既存の「末尾式の型推論」（`inferType`）が内部で使っていた `Maybe Type` による単一化ロジック（リテラルは `Nothing` = 未確定として扱う）を `inferMaybeType`/`unifyMaybeType` として切り出し、`inferType` と `operandType` の両方から共有する。

それ以外の型不一致（`bool` コンテキストでの算術演算、整数コンテキストでの `BoolLit`/`Not`/`Eq`/`Neq`、整数リテラルへの `!` 適用など）は、`compileExprTyped` が `expected` と式の形（`Add`/`Not`/`BoolLit` 等）の組み合わせで即座にエラーを返すことで検出する。これは既存の「`Var` の実際の型が `expected` と食い違えばエラー」という設計をそのまま拡張したもので、式ノードへの型タグ付けやボトムアップの型推論は依然として不要。

`inferType`（末尾式のみに使う型推論）にも `BoolLit`/`Not`/`Eq`/`Neq` のケースを追加した。`BoolLit` は整数リテラルと異なり曖昧さがないため常に確定型 `Just TBool` を返す。`Eq`/`Neq` は被演算子同士を単一化した上で、**ノード自体の型は常に `TBool`**（算術演算と異なり、被演算子の型と結果の型が一致しない非対称ポイント）。

なお、この `operandType` による独立推論は、`let`/代入の右辺（`compileStmts` 経由）では実質的に唯一の型チェック経路である点に注意が必要（`inferType` はプログラム全体の末尾式に対してしか呼ばれないため、`let z: bool = x == y;` のような文レベルの比較式は `inferType` を経由せず、`operandType` だけが `x`/`y` の型不一致を検出する）。

## 5. コード生成

### 比較命令はwidth非依存で良い

既存の不変条件（スタック上の値は演算のたびに正しく符号拡張された64bit表現に戻される）により、`i32`同士・`i64`同士のどちらの比較でも、常に64bit全体を `cmpq` で比較すれば正しい結果が得られる。そのため `ICmpEq`/`ICmpNe` は `IAdd`/`ISub` 等と異なり `Width` パラメータを持たない。

| Instr | 命令列 |
|---|---|
| `ICmpEq` | `popq %rbx; popq %rax; cmpq %rbx,%rax; sete %al; movzbq %al,%rax; pushq %rax` |
| `ICmpNe` | `popq %rbx; popq %rax; cmpq %rbx,%rax; setne %al; movzbq %al,%rax; pushq %rax` |
| `INot` | `popq %rax; xorq $1,%rax; pushq %rax`（bool値は常に0/1という不変条件を利用した最小実装） |

`sete`/`setne`はフラグから0/1を生成するが書き込み先が`%al`（8bit）に限られるため、`movzbq`で`%rax`全体にゼロ拡張する（bool値に符号の概念はないため符号拡張ではなくゼロ拡張を使う）。内部表現は `true = 1` / `false = 0`。

### 末尾式がboolの場合の出力

`codegen :: Type -> [Instr] -> String` に変更。末尾式が `bool` の場合、既存の `fmt32`/`fmt64`（`%d`/`%ld` で数値を出力する）経路とは別に、popした0/1値を `testq`/`jne` で分岐し、`.rodata` に追加した `strTrue`（`"true\n"`）/`strFalse`（`"false\n"`）のどちらかのアドレスを `%rdi` に積んで `printf` を呼ぶ経路を追加した（フォーマット文字列自体に変換指定子を含まないため `%rsi` は不要）。ゼロ除算時のエラー処理とrodataセクションは末尾式の型によらず共通（`commonTail`）。

## 6. VM（`run`、テスト用スタックマシン）

`ICmpEq`/`ICmpNe`/`INot` を追加。bool値は専用の値型を導入せず、そのままIntの0/1として扱う。

## 7. テストへの影響

- Tokenizer: `==`/`!=`/`!`/`true`/`false` の変換、`=` 単体が引き続き `TAssign` になる回帰確認
- Parser: `BoolLit`/`Eq`/`Neq`/`Not` の生成、優先順位（等価演算子は加減算より低い、否定演算子は等価演算子より強く結合する）、括弧内の比較式
- CodeGen: `ICmpEq`/`ICmpNe`/`INot` の命令出力確認、末尾式がboolの場合の分岐・`"true\n"`/`"false\n"`文字列出力
- 意味論エラー（compile）: bool変数への整数リテラル代入、整数変数へのboolリテラル代入、boolへの算術演算、整数への`!`適用（変数・リテラルの両方）、比較結果をintコンテキストで使う場合、`let`右辺の比較でのi32/i64混在
- VM: `ICmpEq`/`ICmpNe`/`INot` の評価結果
- Integration: bool変数の宣言・代入・末尾式出力、`==`/`!=`/`!`の実際の実行結果、算術式の結果同士の比較

## 8. 実装上のトレードオフ・既知の挙動

前2ステップの「未決定事項」とは異なり、以下は保留中の課題ではなく、実装済みの上であえて選んだトレードオフ・および文法上自然に生じる決定済みの挙動。

- **bool変数のスタック格納幅**: 専用の1バイトスロットは新設せず、`i32`用の32bitスロットを流用している（`storageWidth TBool = W32`）。タイトパッキングの厳密さより実装の単純さを優先した、という確定した判断
- **比較演算子のチェイン**（`a == b == c` のような書き方）: 禁止する特別な仕組みは入れておらず、文法・型チェックの両方をそのまま素通りする（`a == b` の結果が`bool`型なので、`bool == c`として型チェック上も問題なく通る）。積極的にサポートを謳っている機能ではないが、現在の文法から自然に導かれる決定済みの挙動であり、今後禁止する予定もない

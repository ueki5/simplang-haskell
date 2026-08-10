# 比較演算子（`<` / `<=` / `>` / `>=`）の導入における設計上の考慮点

`docs/step007.md` §7 で「今回のスコープ外」として据え置かれていた比較演算子（`<`、`<=`、`>`、`>=`）を導入する。`docs/step003.md` で導入した `==`/`!=`（bool型の等価/非等価演算子）と同じ「二項演算子・結果は常に `TBool`」という形なので、字句解析・パーサの優先順位階層・型検査・命令セット・コード生成のいずれも `==`/`!=` の実装パターンをそのまま複製するだけで済み、`Instr` の命令追加以外に構造的な変更は不要だった。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 演算子の種別 | 二項演算子（式）。結果型は常に `bool`（`==`/`!=` と同型） |
| 構文 | `comparison ::= additive (('<' \| '<=' \| '>' \| '>=') additive)*` |
| 優先順位 | 等価演算子（`==`/`!=`）より**高く**、加減算より**低い**（C言語の慣習に合わせる）。単項演算子（`-`/`!`/`to_i64`/`to_i32`）より弱く結合する点は他の二項演算子と同じ |
| 被演算子の型 | **`i32`/`i64` のみ**に限定する。`bool` 同士の比較（`true < false` 等）はコンパイルエラーとする。`==`/`!=` が bool 同士の比較を許可しているのとは非対称だが、順序比較は数値にのみ意味があるため今回はこの制約を新規に設ける |
| 被演算子同士の型混在 | i32 と i64 の混在は不可（`==`/`!=` と同じ `operandType` による独立推論で検出） |
| 演算子のチェイン | 今回もチェイン（`a < b < c`）を禁止する仕組みは追加しない。`docs/step003.md` に記載済みの `a == b == c` と同種の既存の仕様上の制約であり、新規に生じる問題ではない |
| コード生成方式 | `==`/`!=` と同じ `cmp` + `setXX` + `movzbq` 方式（ジャンプ命令は使わない）。符号付き比較命令（`setl`/`setle`/`setg`/`setge`）を使う。符号無し整数型がこの言語に存在しないため符号付きで問題ない |

## 1. 文法・構文（Lexer / Parser）

- **トークン追加**: `TLt`（`<`）、`TLe`（`<=`）、`TGt`（`>`）、`TGe`（`>=`）。`tokenize` では `==`/`!=` と同じ「マルチ文字パターンを単体ガードより先に置く」方式で `<=`/`>=` を `<`/`>` 単体より先に判別する
- **文法**:
  ```
  expr       ::= equality
  equality   ::= comparison (('==' | '!=') comparison)*
  comparison ::= additive (('<' | '<=' | '>' | '>=') additive)*
  additive   ::= term (('+' | '-') term)*
  term       ::= factor (('*' | '/') factor)*
  ```
- **AST追加**: `Expr` に `Lt Expr Expr`、`Le Expr Expr`、`Gt Expr Expr`、`Ge Expr Expr` を追加。`Eq`/`Neq` と同じく専用コンストラクタ方式（共通の `BinOp` 型は導入しない）
- **`parseComparison`/`parseComparisonRest`**: `parseEquality`（等価演算子の抽出）と `parseExpr`（加減算の抽出）の間に新設。`parseEquality` は加減算の抽出（`parseExpr`）を直接呼んでいた箇所をすべて `parseComparison` 経由に差し替える。式が登場する全箇所（末尾式、`let`/代入右辺、if/while条件、括弧内）は引き続き `parseEquality` を呼ぶだけでよく、呼び出し元の変更は不要

## 2. 意味論：型検査

- 結果型は常に `TBool`。`compileExprTyped _ (TyInt _) (Lt _ _)` のように整数コンテキストで使われた場合は `==`/`!=` と同じ形でエラーにする
- 被演算子の型は `==`/`!=` と同じ `operandType`（外側の `expected` とは独立に `l`/`r` の型を単一化する関数）で決定するが、その結果が `TBool` だった場合は追加で `Left "type mismatch: expected i32 or i64, found bool"` を返す。これにより bool 同士の比較のみを新たに禁止する
- `inferMaybeType` には `Eq`/`Neq` と全く同じ形（`combine a b >> Right (Just TBool)`）で追加する。bool/数値の区別はここでは行わず、型検査の責務は `compileExprTyped` 側のみに置く（`Eq`/`Neq` の既存設計と同じ役割分担）

## 3. コード生成

`Instr` に `ICmpLt`、`ICmpLe`、`ICmpGt`、`ICmpGe` を追加。`ICmpEq`/`ICmpNe` と同じく **`Width` パラメータを持たない**（`docs/step003.md` で確立した不変条件——スタック上の値は演算のたびに正しく符号拡張された64bit表現に戻される——により、i32/i64どちらの比較でも `cmpq` で正しく比較できるため）。

`CodeGen.genInstr` では `popq %rbx` / `popq %rax` / `cmpq %rbx, %rax` の後、`setl`/`setle`/`setg`/`setge` のいずれかで `%al` に結果をセットし、`movzbq %al, %rax` でゼロ拡張して `pushq` する。`Lt a b` の場合、先に積まれた `a` が `%rax`、後に積まれた `b` が `%rbx` に入るため、`cmpq %rbx, %rax` は `a - b` としてフラグを立て、`setl` は「`a < b`」を正しく表す（AT&T構文の `cmpq src, dst` は `dst - src` の意味）。

## 4. 簡易VM（`run`）・`app/Main.hs` への影響

- `run`（Hspec専用の素朴なスタックマシン）: `ICmpEq`/`ICmpNe` と同じ形で `ICmpLt`/`ICmpLe`/`ICmpGt`/`ICmpGe` を追加（Haskellの `<`/`<=`/`>`/`>=` をそのまま利用）
- `app/Main.hs`: 変更不要

## 5. `docs/step007.md` からの変更点まとめ

| ファイル / 項目 | step007まで | step008での変更 |
|---|---|---|
| `Token` | `TEq`、`TNeq` 等 | `TLt`、`TLe`、`TGt`、`TGe` を追加 |
| `tokenize` | — | `<=`/`>=`（マルチ文字、`<`/`>`単体より先に判別）と `<`/`>`（単体）を追加 |
| 文法 | `equality ::= additive (('==' \| '!=') additive)*` | `equality` は `comparison` を経由するように変更し、新たに `comparison ::= additive (('<' \| '<=' \| '>' \| '>=') additive)*` を追加 |
| `Expr` | `Eq`、`Neq` 等 | `Lt`、`Le`、`Gt`、`Ge` を追加 |
| 新規パース関数 | — | `parseComparison`、`parseComparisonRest` |
| `parseEqualityRest` | `parseExpr` を呼ぶ | `parseComparison` を呼ぶように変更（優先順位挿入のための差し替えのみ） |
| `inferMaybeType` | `Eq`/`Neq` のケース | `Lt`/`Le`/`Gt`/`Ge` のケースを同型で追加 |
| `compileExprTyped` | `Eq`/`Neq` のケース | `Lt`/`Le`/`Gt`/`Ge` のケースを追加。`operandType` の結果が `TBool` ならエラーにする追加チェックあり（`Eq`/`Neq` には無い制約） |
| `Instr` | `ICmpEq`、`ICmpNe` | `ICmpLt`、`ICmpLe`、`ICmpGt`、`ICmpGe` を追加（いずれも `Width` 非依存） |
| `CodeGen.genInstr` | `ICmpEq`→`sete`、`ICmpNe`→`setne` | `ICmpLt`→`setl`、`ICmpLe`→`setle`、`ICmpGt`→`setg`、`ICmpGe`→`setge` を追加 |
| `run`（VM） | `ICmpEq`/`ICmpNe` に対応 | `ICmpLt`/`ICmpLe`/`ICmpGt`/`ICmpGe` にも対応 |
| `app/Main.hs` | — | 変更なし |
| 依存パッケージ | — | 変更なし |

## 6. テストへの影響

- **Tokenizer**: `<`/`<=`/`>`/`>=` の字句化、`<=`/`>=` が2文字トークンとして正しく認識され `<`+`=` 等に分割されないことの回帰確認
- **Parser**: 比較演算子のAST形状、優先順位（加減算が比較より強く結合すること、比較が等価より強く結合すること）、括弧内・単項演算子との組み合わせ
- **CodeGen**: `ICmpLt`/`ICmpLe`/`ICmpGt`/`ICmpGe` がそれぞれ `setl`/`setle`/`setg`/`setge` を含むアセンブリへ変換されること
- **意味論エラー（compile）**: 比較演算子の被演算子でのi32/i64混在エラー、bool同士の比較エラー（新規制約）、比較結果を整数コンテキストで使った場合のエラー
- **VM統合テスト**: `run` への直接命令列、`compileAndRun` によるAST直接構築での各比較演算子の真偽判定
- **結合テスト（gcc実行）**: `while`条件式での使用（`sample/src008.sl` 相当）、i32/i64変数同士の比較、`!(a < b)` との組み合わせ

## 7. 今回のスコープ外とした将来の検討課題

- 比較演算子のチェイン（`a < b < c`）を構文レベルで禁止する仕組み（`==`/`!=` から続く既存の課題）
- 符号無し整数の比較（この言語に符号無し整数型自体が存在しない）
- `for` 文、値を返す `while` 式（`docs/step007.md` から引き続き据え置き）

# モジュール分割（Lexer/Parser を Parser.hs へ）

`src/Compiler.hs` が1000行を超えて肥大化していたため、Lexer（字句解析）・Parser（構文解析）を
`src/Parser.hs` へ分割した。すでに分離済みの `src/CodeGen.hs`（x86-64アセンブリ生成）と合わせ、
`docs/step000.md` のパイプライン図どおり `tokenize/parse (Parser.hs) → compile (Compiler.hs)
→ codegen (CodeGen.hs)` という一方向の依存関係になる。挙動を変えない純粋なモジュール分割。

- `Token` 型 + `tokenize`（Lexer全体）を `Parser.hs` へ移動
- `parse` と全 `parse*` 補助関数（`parseTopLevel` 〜 `parseArgListRest`）を `Parser.hs` へ移動
- AST型（`Expr`/`Stmt`/`FnDecl`/`Program`）も `Parser.hs` へ移動。`Width`/`Type` は
  `SLet`/`FnDecl` のフィールドとしてAST側に直接埋め込まれているため、AST と一緒に移した
  （`Compiler.hs` 側はこれらを `Parser` からimportして使う）
- `Compiler.hs` は `Instr` 型・型検査（`compileExprTyped`等）・IR生成（`compile`）・
  テスト用スタックVM（`run`）専任になった
- `typeName`（エラーメッセージ用）は型検査側の関心事なので `Compiler.hs` に残した
- `simplang-haskell.cabal` の `exposed-modules` に `Parser` を追加
- `app/Main.hs` / `test/Spec.hs` のimportを新しいモジュール境界に合わせて分割

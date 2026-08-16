# simplang-haskell アプリケーション仕様

算術式を入力として x86-64 ネイティブバイナリを生成するコンパイラ。
Haskell で実装されており、GCC をリンカとして利用する。

## Architecture

コンパイルパイプライン:

```
Source text
  → tokenize  (src/Parser.hs)    -- 字句解析
  → parse     (src/Parser.hs)    -- 再帰下降パーサ（演算子優先順位あり）
  → compile   (src/Compiler.hs)  -- AST → スタックベース中間命令列
  → codegen   (src/CodeGen.hs)   -- 中間命令 → x86-64 AT&T 構文アセンブリ
  → gcc       (app/Main.hs)      -- .s → ネイティブバイナリ
```

### モジュール責務

| ファイル | 責務 |
|---|---|
| `src/Parser.hs` | Lexer / Parser（AST・`Type`/`Width`の定義を含む） |
| `src/Compiler.hs` | IR Compiler / Stack VM（テスト用） |
| `src/CodeGen.hs` | x86-64 アセンブリ生成 |
| `app/Main.hs` | CLI (optparse-applicative)、パイプライン統合 |
| `test/Spec.hs` | Hspec テストスイート |

### 主要型

```haskell
-- 字句トークン
data Token = TInt Int | TPlus | TMinus | TStar | TSlash | TLParen | TRParen

-- 抽象構文木
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Neg Expr

-- スタックベース中間命令
data Instr = Push Int | IAdd | ISub | IMul | IDiv | INeg
```

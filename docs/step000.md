# simplang-haskell アプリケーション仕様

算術式を入力として x86-64 ネイティブバイナリを生成するコンパイラ。
Haskell で実装されており、GCC をリンカとして利用する。

## Build & Run

```bash
cabal build                                          # ビルド
cabal test                                           # テスト (Hspec)
cabal run simplang-haskell -- FILE [-o OUTPUT] [-S ASM_FILE]
```

- `FILE`: 算術式を含むソースファイル（必須）
- `-o OUTPUT`: 出力バイナリ名（デフォルト: `out`）
- `-S ASM_FILE`: アセンブリファイルの保存先（省略可）

## Architecture

コンパイルパイプライン:

```
Source text
  → tokenize  (src/Compiler.hs)  -- 字句解析
  → parse     (src/Compiler.hs)  -- 再帰下降パーサ（演算子優先順位あり）
  → compile   (src/Compiler.hs)  -- AST → スタックベース中間命令列
  → codegen   (src/CodeGen.hs)   -- 中間命令 → x86-64 AT&T 構文アセンブリ
  → gcc       (app/Main.hs)      -- .s → ネイティブバイナリ
```

### モジュール責務

| ファイル | 責務 |
|---|---|
| `src/Compiler.hs` | Lexer / Parser / IR Compiler / Stack VM（テスト用） |
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

## Testing

```bash
cabal test
```

フレームワーク: **Hspec** (`test/Spec.hs`)

| テストカテゴリ | 内容 |
|---|---|
| Tokenizer | 空白処理、複数桁の整数、エラーケース |
| Parser | リテラル、二項演算子、演算子優先順位、括弧、単項マイナス |
| CodeGen | プロローグ/エピローグ構造、各命令の x86-64 変換 |
| Integration | GCC でコンパイルして実際に実行し結果を検証 |

Integration テストは `withSystemTempDirectory` で一時ディレクトリを管理する。

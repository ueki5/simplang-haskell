# simplang-haskell

Haskell で実装されており、GCC をリンカとして利用する。
アプリケーション仕様（Architecture）は `docs/step000.md`、変数・代入機能の設計は `docs/step001.md`、i32型の設計は `docs/step002.md` を参照。

## Build & Run

```bash
cabal build                                          # ビルド
cabal test                                           # テスト (Hspec)
cabal run simplang-haskell -- FILE [-o OUTPUT] [-S ASM_FILE]
```

- `FILE`: 算術式を含むソースファイル（必須）
- `-o OUTPUT`: 出力バイナリ名（デフォルト: `out`）
- `-S ASM_FILE`: アセンブリファイルの保存先（省略可）

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

## Key Design Notes

- **Parser**: 左再帰を除去した反復ループで左結合を実現（`parseExpr` → `parseTerm` → `parseFactor`）。`let`/代入文は `parseStmts` で先読み2トークンにより判別し、末尾の `Expr` は省略不可
- **型システム**: `Width`（`W32`/`W64`）がソース型名（`i32`/`i64`）とcodegenのレジスタ幅を兼用する唯一の型。`compileExprTyped` が期待幅を式木に一様伝播させ、`Var` の実際の型と食い違えば `type mismatch` エラー。同一式内でのi32/i64混在は必ずこのチェックで検出される（式ノードへの型タグ付けやボトムアップ型推論は不要）
- **変数の環境管理**: `compile` が `Map String (Int, Width)`（変数名→`%rbp`相対オフセットと型）を左から右への単一パスで引き回す。スタックスロットは型の実サイズ（i32=4バイト、i64=8バイト）でタイトパッキング。`let` の二重宣言・未宣言変数の参照はコンパイルエラー
- **コード生成**: スタック操作に `%rax` / `%rbx` / `%rcx` を使用。i32演算は32bit命令（`addl`/`subl`/`imull`/`idivl`/`negl`）の後に符号拡張（`cltq`/`movslq`）して64bitスタックへ戻し、演算のたびにラップアラウンドする。`Push` の即値は `movabsq` 経由（`pushq` の符号拡張32bit即値制限を回避）。`divq`/`idivl` 実行前に `cmpq $0` でゼロ除算チェック、エラー時は `.Ldiv_zero_error` ラベルへジャンプ
- **スタックフレーム**: `codegen` 側で命令列中の `Load`/`Store` オフセットの最大絶対値をスキャンしてフレームサイズを決定し、プロローグで一度だけ `subq`。エピローグは `leave`（`movq %rbp,%rsp; popq %rbp`）で確保量によらず正しく巻き戻す
- **アライメント**: `call printf` / `call exit` 直前に `andq $-16, %rsp` を挿入し16バイト境界を強制
- **リンク**: `callProcess "gcc" [...]` で外部プロセス呼び出し
- **出力**: コンパイルされたバイナリは `printf` で結果を標準出力へ表示（末尾式の型が `i64` なら `%ld`、`i32` なら `%d`）

## Dependencies

- `optparse-applicative` — CLI 引数パース
- `process` — `callProcess "gcc"`
- `directory` / `filepath` — ファイルパス操作
- `containers` — 変数環境の `Map String (Int, Width)` 管理
- `hspec` / `temporary` — テスト専用

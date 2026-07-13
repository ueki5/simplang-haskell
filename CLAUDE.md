# simplang-haskell

Haskell で実装されており、GCC をリンカとして利用する。
アプリケーション仕様（Build & Run、Architecture、Testing）は `docs/step000.md`、変数・代入機能の設計は `docs/step001.md` を参照。

## Key Design Notes

- **Parser**: 左再帰を除去した反復ループで左結合を実現（`parseExpr` → `parseTerm` → `parseFactor`）。`let`/代入文は `parseStmts` で先読み2トークンにより判別し、末尾の `Expr` は省略不可
- **変数の環境管理**: `compile` が `Map String Int`（変数名→`%rbp`相対オフセット）を左から右への単一パスで引き回す。`let` の二重宣言・未宣言変数の参照はコンパイルエラー
- **コード生成**: スタック操作に `%rax` / `%rbx` / `%rcx` を使用。`divq` 実行前に `cmpq $0` でゼロ除算チェック、エラー時は `.Ldiv_zero_error` ラベルへジャンプ
- **スタックフレーム**: `codegen` 側で命令列中の `Load`/`Store` オフセットの最大絶対値をスキャンしてフレームサイズを決定し、プロローグで一度だけ `subq`。エピローグは `leave`（`movq %rbp,%rsp; popq %rbp`）で確保量によらず正しく巻き戻す
- **アライメント**: `call printf` / `call exit` 直前に `andq $-16, %rsp` を挿入し16バイト境界を強制
- **リンク**: `callProcess "gcc" [...]` で外部プロセス呼び出し
- **出力**: コンパイルされたバイナリは `printf` で結果を標準出力へ表示

## Dependencies

- `optparse-applicative` — CLI 引数パース
- `process` — `callProcess "gcc"`
- `directory` / `filepath` — ファイルパス操作
- `containers` — 変数環境の `Map String Int` 管理
- `hspec` / `temporary` — テスト専用

# simplang-haskell

Haskell で実装されており、GCC をリンカとして利用する。
アプリケーション仕様（Build & Run、Architecture、Testing）は `docs/step000.md` を参照。

## Key Design Notes

- **Parser**: 左再帰を除去した反復ループで左結合を実現（`parseExpr` → `parseTerm` → `parseFactor`）
- **コード生成**: スタック操作に `%rax` / `%rbx` / `%rcx` を使用。`divq` 実行前に `cmpq $0` でゼロ除算チェック、エラー時は `.Ldiv_zero_error` ラベルへジャンプ
- **リンク**: `callProcess "gcc" [...]` で外部プロセス呼び出し
- **出力**: コンパイルされたバイナリは `printf` で結果を標準出力へ表示

## Dependencies

- `optparse-applicative` — CLI 引数パース
- `process` — `callProcess "gcc"`
- `directory` / `filepath` — ファイルパス操作
- `hspec` / `temporary` — テスト専用

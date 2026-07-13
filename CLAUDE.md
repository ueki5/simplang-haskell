# simplang-haskell

Haskell で実装されており、GCC をリンカとして利用する。
アプリケーション仕様（Architecture）は `docs/step000.md` を参照。

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

- **Parser**: 左再帰を除去した反復ループで左結合を実現（`parseExpr` → `parseTerm` → `parseFactor`）
- **コード生成**: スタック操作に `%rax` / `%rbx` / `%rcx` を使用。`divq` 実行前に `cmpq $0` でゼロ除算チェック、エラー時は `.Ldiv_zero_error` ラベルへジャンプ
- **リンク**: `callProcess "gcc" [...]` で外部プロセス呼び出し
- **出力**: コンパイルされたバイナリは `printf` で結果を標準出力へ表示

## Dependencies

- `optparse-applicative` — CLI 引数パース
- `process` — `callProcess "gcc"`
- `directory` / `filepath` — ファイルパス操作
- `hspec` / `temporary` — テスト専用

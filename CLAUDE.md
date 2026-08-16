# simplang-haskell

Haskell で実装されており、GCC をリンカとして利用する。
各種設計ドキュメントは以下を参照。

- アプリケーション仕様（Architecture）: `docs/step000.md`
- 変数・代入機能の設計: `docs/step001.md`
- i32型の設計: `docs/step002.md`
- bool型と等価/非等価/否定演算子の設計: `docs/step003.md`
- to_i64/to_i32単項演算子の設計: `docs/step004.md`
- コードブロック`{}`（変数スコープ）の設計: `docs/step005.md`
- if/else if/elseの設計: `docs/step006.md`
- while/break/continueの設計: `docs/step007.md`
- 比較演算子（`<`, `<=`, `>`, `>=`）の設計: `docs/step008.md`
- 関数定義（fn）の設計: `docs/step009.md`
- リファクタリング（関数名見直し）: `docs/step010.md`
- リファクタリング（モジュール構成見直し）: `docs/step011.md`

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

- **Parser**: 左再帰を除去した反復ループで左結合を実現（`parseExpr` → ... → `parseFactor`）。`let`/代入文は `parseStmts` で先読み2トークンにより判別し、末尾の `Expr` は省略不可
- **型システム**: `Width`（`W32`/`W64`）がソース型名（`i32`/`i64`）とcodegenのレジスタ幅を兼用する唯一の型。`compileExprTyped` が期待幅を式木に一様伝播させ、`Var` の実際の型と食い違えば `type mismatch` エラー。同一式内でのi32/i64混在は必ずこのチェックで検出される（式ノードへの型タグ付けやボトムアップ型推論は不要）
- **変数の環境管理**: `Env`（変数名→`%rbp`相対オフセットと型）はスコープのスタック `[Map String (Int, Type)]`（先頭が最内側）で、`compile` が `[Map.empty]` から開始し左から右への単一パスで引き回す。スタックスロットは型の実サイズ（i32=4バイト、i64=8バイト）でタイトパッキング。`{}` ブロックは文としてスコープを1段push/popし、ブロックを抜けると内部で宣言した変数は不可視になる（外側と同名の再宣言＝シャドーイングは許可、同一ブロック内の再宣言は引き続きコンパイルエラー）。未宣言変数の参照もコンパイルエラー。スタックオフセットもブロックの出入りで巻き戻され、時間的に重ならない兄弟ブロックは同じオフセット範囲を再利用する
- **コード生成**: スタック操作に `%rax` / `%rbx` / `%rcx` を使用。i32演算は32bit命令（`addl`/`subl`/`imull`/`idivl`/`negl`）の後に符号拡張（`cltq`/`movslq`）して64bitスタックへ戻し、演算のたびにラップアラウンドする。`Push` の即値は `movabsq` 経由（`pushq` の符号拡張32bit即値制限を回避）。`divq`/`idivl` 実行前に `cmpq $0` でゼロ除算チェック、エラー時は `.Ldiv_zero_error` ラベルへジャンプ
- **スタックフレーム**: `codegen` 側で命令列中の `Load`/`Store` オフセットの最大絶対値をスキャンしてフレームサイズを決定し、プロローグで一度だけ `subq`。エピローグは `leave`（`movq %rbp,%rsp; popq %rbp`）で確保量によらず正しく巻き戻す
- **アライメント**: `call printf` / `call exit` 直前に `andq $-16, %rsp` を挿入し16バイト境界を強制
- **リンク**: `callProcess "gcc" [...]` で外部プロセス呼び出し
- **出力**: コンパイルされたバイナリは `printf` で結果を標準出力へ表示（末尾式の型が `i64` なら `%ld`、`i32` なら `%d`）

## Dependencies

- `optparse-applicative` — CLI 引数パース
- `process` — `callProcess "gcc"`
- `directory` / `filepath` — ファイルパス操作
- `containers` — 変数環境の `[Map String (Int, Type)]`（スコープのスタック）管理
- `hspec` / `temporary` — テスト専用

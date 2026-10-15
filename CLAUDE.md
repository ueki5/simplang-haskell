# simplang-haskell

Rust に似た小さな言語（simplang）を x86-64 アセンブリへコンパイルする、Haskell 製のコンパイラ。アセンブルとリンクには GCC を使う。

仕様と設計はすべて `docs/` にある（目次は `docs/README.md`）。作業の前に、関係する次の文書を読むこと。

- `docs/language.md`：言語仕様
- `docs/architecture.md`：実装の全体像
- `docs/cli.md`：コマンドラインとビルドの仕様
- `docs/backlog.md`：将来の課題

## コマンド

```bash
cabal build                                                    # ビルド
cabal test                                                     # テスト（Hspec。結合テストは gcc を使う）
cabal run simplang-haskell -- FILES... -o OUTPUT [-S|-c] [-d OUTDIR]   # 実行
```

## 作業の進め方

機能の追加・変更は「1 step = 1 テーマ」で進める。step 番号は `docs/stepNNN.md` の連番の続き。

1. **ブランチ**：main から `stepNNN` ブランチを作る
2. **設計**：実装の前に `docs/stepNNN.md` に設計を書き、ユーザーの合意を得る。構成は既存の step 文書にならう
   - 冒頭：背景と、変更後のコード例
   - `0. 方針として確定した事項`：決定事項の表
   - `1. 考慮点`：検討した論点・代替案と、その結論の理由
   - 構文・AST / コンパイラ・コード生成 / 移行（既存のテスト・サンプルへの影響）など、変更範囲に応じた節
   - `スコープ外（将来の課題）`：見送った事項
3. **実装とテスト**：`test/Spec.hs` にテストを追加する。新しい構文・型には、parse・compile・意味論エラー・結合テスト（gcc で実行して出力を検証）を揃える。`cabal test` がすべて通ること。PR では GitHub Actions の CI（`.github/workflows/ci.yml`）が通ることも確認する
4. **ドキュメントの更新**（実装と同じコミットに含める）
   - `docs/language.md` / `docs/architecture.md` / `docs/cli.md` を変更後の状態に合わせる
   - `docs/backlog.md`：見送った課題を追記し、実現した課題は「実現済み」へ移す
   - `docs/README.md` の step 一覧に追加する
   - 影響を受ける `sample/*.sl` を更新する。新機能のサンプルは `sample/sampNNN-K.sl` として追加する
5. **コミット・PR**：日本語のコミットメッセージで、step の内容を1行で表す。コミットと PR の作成はユーザーの指示があってから行う

## ドキュメントの規約

- `docs/language.md` / `docs/architecture.md` / `docs/cli.md` は**常に main の最新の状態と一致させる**。現在の仕様を知りたいときはこれらを正とする
- `docs/stepNNN.md` は、その step で決めたこととその理由の記録。**マージ後は書き換えない**。後の step で撤回したときだけ、撤回された step 文書の冒頭と `docs/README.md` の一覧に「stepMMM で撤回」と注記する
- CLAUDE.md には作業の進め方と規約だけを書き、仕様・実装の説明は docs に書く
- ドキュメントは日本語で書く

## 設計の規約

- 言語仕様に迷ったら **Rust の仕様に合わせる**（暗黙の型変換をしない、`as` はビットの再解釈、`->` 省略は `()`、など）
- 誤りはできるだけコンパイル時に検出する。実行時にしか分からない誤り（ゼロ除算・添字の範囲外）は、メッセージを出力して終了コード 1 で終了させる
- 一度入れた仕様を取り消すときは、黙って消さずに新しい step として撤回の理由を記録する（例：step016 → step017、step015 → step023）

## コーディング・テストの規約

- 既存のコードに合わせる：コメントは日本語、エラーメッセージは英語の小文字始まり（`type mismatch: expected i64, found i32` の形式）
- `-Wall` の警告を出さない
- 整数型の判定・命令選択は `TyInt` へ直接マッチせず `intRepr` を経由する
- `test/Spec.hs` の `describe` は「機能名（段階）」の形で日本語で書く（例：`配列・usize（意味論エラー）`、`compile + codegen + gcc（配列の結合テスト）`）
- 一時ファイルを使うテストは `withSystemTempDirectory` で一時ディレクトリを管理する

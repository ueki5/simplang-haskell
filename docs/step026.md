# GitHub Actions による CI の導入

これまでテストはローカルの `cabal test` でしか実行しておらず、PR がテストを通っているかは作業者の確認に頼っていた。
GitHub Actions で、push と PR のたびにビルドとテストを自動で実行するようにする。

挙動を変えない、開発環境だけの変更。

```
PR を作る / push する
  → GitHub が使い捨ての仮想マシン（ubuntu-latest）を用意する
  → cabal.project と同じ GHC を入れる
  → cabal build → cabal test（結合テストはランナーの gcc で実行）
  → 結果が PR の画面に表示される
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 設定ファイル | `.github/workflows/ci.yml` |
| 実行するタイミング | `main` への push、すべての PR、手動実行（`workflow_dispatch`） |
| 実行環境 | `ubuntu-latest` |
| GHC のバージョン | `cabal.project` の `with-compiler: ghc-X.Y.Z` から読み取る（バージョンを CI 側に書かない） |
| cabal-install のバージョン | `latest`（`.cabal` の `cabal-version: 3.14` が下限を決める） |
| 実行する内容 | `cabal build all --enable-tests` と `cabal test all` |
| キャッシュ | 依存パッケージのビルド結果（cabal store）を、ビルドプラン（`plan.json`）のハッシュをキーに保存する |
| 古い実行の取り消し | 同じブランチ・PR に新しい push があれば、実行中の古い CI を取り消す |

## 1. 考慮点

### 1.1 GHC のバージョンをどこから取るか

`haskell-actions/setup` は `cabal.project` や `.cabal` を読まないため、何らかの形でバージョンを渡す必要がある。

| 案 | 長所 | 短所 |
|---|---|---|
| ci.yml に直接書く | 単純 | `cabal.project` と2か所で管理することになり、片方だけ更新する誤りが起きる |
| `.cabal` に `tested-with` を書き、`haskell-ci` で ci.yml を生成する | 複数の GHC での検証に強い | 生成ツールへの依存が増える。`cabal.project` とも二重管理になる |
| **`cabal.project` の `with-compiler` を CI の中で読み取る** | ローカルのビルドが実際に使う値が唯一の情報源になる | 読み取り用のステップが1つ増える |

3つ目を採用する。`with-compiler` が見つからなければ、CI をエラーで止める。

### 1.2 cabal-install のバージョン

`.cabal` の `cabal-version: 3.14` は `.cabal` ファイルの書式のバージョンで、cabal-install 3.14 以上を要求するという下限の意味を持つ。
GHC と違ってプロジェクトのビルド結果を左右しにくいため、`latest` として固定バージョンの管理を省く。

### 1.3 キャッシュ

GHC と依存パッケージのビルドには、キャッシュが無いと数分以上かかる。

- `cabal build --dry-run` でビルドプラン（`dist-newstyle/cache/plan.json`）を作り、そのハッシュを cabal store のキャッシュのキーにする。依存関係が変わらなければキャッシュが当たる
- キャッシュの復元（restore）と保存（save）を分け、依存パッケージのビルド直後に保存する。本体のビルドやテストが失敗しても、依存パッケージのキャッシュは残る
- キーには GHC のバージョンを含める（GHC を上げたら別のキャッシュになる）

### 1.4 結合テストと gcc

結合テスト・Driver のテストは gcc でアセンブル・リンクして実行する。`ubuntu-latest` のランナーには gcc が入っているため、追加のインストールは不要。
テストがリポジトリ外のファイルや固定のパスに依存していないことは確認済み（一時ファイルは `withSystemTempDirectory` で作る）。

## 2. ドキュメントの更新

| ファイル | 内容 |
|---|---|
| `CLAUDE.md` | 作業の進め方の「実装とテスト」に「PR の CI が通ること」を加える |
| `docs/architecture.md` | 「9. テスト」に CI の節を加える |
| `docs/README.md` | step026 を一覧に加える |
| `docs/backlog.md` | スコープ外とした事項を「1.7 テスト基盤」に加える |

## 3. スコープ外（将来の課題）

- `-Werror`（警告をエラーとして扱う）：現在は警告 0 件だが、CI で強制するかは別途決める
- HLint・フォーマッタ（fourmolu / ormolu）の導入と CI での検査
- `main` のブランチ保護（CI の成功をマージの条件にする）：GitHub の画面での設定のため、リポジトリの管理者が行う
- 複数の GHC バージョン・OS での検証

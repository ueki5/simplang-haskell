# ormolu によるコード整形の導入

エディタ（nvim・VS Code）は保存時に HLS 経由でコードを整形しているが、Claude Code が書いたコードは整形されないまま残る。
そのため、内容を変えずにエディタで開いて保存しただけで整形の差分が出る。

整形を ormolu に統一し、**エディタ・Claude Code・CI のすべてが、プロジェクトで版を固定した同じ `ormolu` コマンドを使う**ようにする。

挙動を変えない、開発環境だけの変更。

```
ormolu の版：flake.nix の devShell と flake.lock で固定する
  └ cd したとき、nix-direnv（.envrc の use flake）が PATH に載せる
       ├ nvim / VS Code：HLS が外部の ormolu を呼んで保存時に整形する（external = true）
       ├ Claude Code：.hs を Write/Edit した直後に、hook が ormolu で整形する
       └ CI：nix develop の中で ormolu --mode check を実行し、整形漏れを検出する
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| フォーマッタ | ormolu（設定項目を持たない。HLS の既定のフォーマッタ） |
| 版の固定 | `flake.nix` の devShell に `ormolu` だけを入れ、`flake.lock` で nixpkgs のリビジョンを固定する。導入時の版は 0.8.0.2 |
| PATH への反映 | `.envrc` に `use flake`（nix-direnv）。`.direnv/` は `.gitignore` に加える |
| GHC・cabal・HLS | これまでどおり ghcup で管理する（devShell には入れない） |
| エディタ | HLS の設定 `haskell.plugin.ormolu.config.external = true` で、内蔵の ormolu ではなく PATH の `ormolu` を使わせる。設定は各自のエディタ側で行う（リポジトリ外） |
| Claude Code | `.claude/settings.json` の PostToolUse hook（`Write`・`Edit`）から `.claude/hooks/ormolu.sh` を実行し、書いた `.hs` を `ormolu --mode inplace` で整形する |
| CI | `ci.yml` に整形を検査するジョブを加える。Nix を入れて `nix develop -c ormolu --mode check` を実行する |
| 既存コードの整形 | 全 `.hs` を一括で整形する。整形だけのコミットを分け、`.git-blame-ignore-revs` に登録する |

## 1. 考慮点

### 1.1 なぜ差分が出ていたか

nvim（conform）は Haskell 用の専用フォーマッタを持たないため、保存時の整形を HLS に任せている。HLS は ormolu を**ライブラリとして内蔵**しており、単独で実行できる `ormolu` コマンドは手元に無かった。
Claude Code の書いたコードはどこでも整形されないため、エディタで保存したときに初めて整形される。

### 1.2 ormolu と fourmolu

| 案 | 長所 | 短所 |
|---|---|---|
| **ormolu** | HLS の既定。設定が無いので迷いが無い。既存コードの大半がすでに ormolu の書式に近い（差分は主に `CodeGen.hs`・`Spec.hs`） | 書式を調整できない |
| fourmolu | インデント幅などを設定できる | 設定ファイルの管理が増える。既存コードとの差分が大きくなる |

ormolu を採用する。

### 1.3 版をどう揃えるか

HLS が内蔵する ormolu の版は、HLS の版だけでなく、**HLS をビルドした GHC の版**によっても変わる（HLS 2.14.0.0 の場合、GHC 9.8.4 用は 0.7.4.0、GHC 9.10.3 用は 0.8.0.2）。
Hackage の `.cabal` から分かるのは受け入れる範囲（`ormolu ^>=0.5.3 || ... || ^>=0.8`）だけで、公式の対応表は無い。

| 案 | 長所 | 短所 |
|---|---|---|
| HLS 内蔵と同じ版の ormolu を別に入れる | エディタの設定を変えずに済む | HLS や GHC を上げるたびに版を合わせ直す必要がある。ずれても気付きにくい |
| Claude Code からも HLS 経由で整形する | 版が必ず一致する | HLS にコマンドラインの整形機能が無く、LSP クライアントを書く必要がある。起動のたびにプロジェクトを読み込むため、編集ごとに数秒〜十数秒かかる |
| **HLS に外部の ormolu を使わせ、その版をプロジェクトで固定する** | エディタ・Claude Code・CI が同じコマンドを使い、結果が必ず一致する。HLS・GHC を上げても整形は変わらない | エディタ側の設定が1つ必要。devShell の仕組みが増える |

3つ目を採用する。ormolu は GHC 本体ではなく自前の構文解析器（ghc-lib-parser）を使うため、プロジェクトの GHC と版を合わせる必要は無い（プロジェクトの GHC の構文に対応していればよい）。

### 1.4 devShell に ormolu だけを入れる

GHC・cabal・HLS は ghcup で管理しており、CI も `haskell-actions/setup` で GHC を入れている（step026）。devShell に GHC まで入れると管理が二重になるため、devShell には ormolu だけを入れる。
nixpkgs は `nixpkgs-unstable` を `flake.lock` で固定する。ormolu を上げるときは `nix flake update` のコミットとして明示的に行う。

版を変えるときは `flake.nix` / `flake.lock` だけを変える（hook・エディタの設定・ci.yml は変えない）。

| 変えたい版 | 方法 |
|---|---|
| nixpkgs の最新の版 | `nix flake update` |
| nixpkgs にある別の版（`haskellPackages.ormolu_0_7_4_0` など） | `flake.nix` で `pkgs.ormolu` を差し替える |
| nixpkgs に無い版 | その版を含む nixpkgs のリビジョンを別の入力として固定する、または `haskellPackages.callHackage` で Hackage からビルドする |

どの場合も、`flake.nix` / `flake.lock` の更新をコミットしたあと、全 `.hs` を整形し直して整形だけのコミットを作り、そのハッシュを `.git-blame-ignore-revs` に追記する（1.7）。起動中のエディタは古い PATH を持ち続けるため、再起動する。

### 1.5 Claude Code の hook

- `Write`・`Edit` のあと、対象のファイルが `.hs` のときだけ整形する
- `ormolu` は `direnv exec "$CLAUDE_PROJECT_DIR"` を通して実行する。Claude Code を direnv の効いていないシェルから起動しても、devShell の版が使われる
- `ormolu` が見つからない（`direnv allow` をしていない、など）ときは、利用者に見えるエラーとして終了する（終了コード 1。Claude の作業は止めない）
- ormolu が構文エラーで整形できなかったときは、その内容を Claude に返す（終了コード 2）

### 1.6 CI

`haskell-actions/run-ormolu` を使えば Nix 無しで検査できるが、ormolu の版を ci.yml にも書くことになり、`flake.lock` と二重管理になる。
step026 で GHC の版を `cabal.project` だけで管理したのと同じ考えで、CI でも Nix を入れて devShell の ormolu を使う。ビルド・テストのジョブとは別のジョブにする（GHC が不要で、並行して実行できる）。

### 1.7 既存コードの一括整形

整形は行の内容を変えないが、git からは変更として扱われるため、`git blame` が各行の最終変更として整形のコミットを示し、その行を本当に書いたコミットが見えなくなる（動作には影響しない）。
`git blame` が整形のコミットを飛ばすよう、整形だけのコミットを先に作り、そのハッシュを `.git-blame-ignore-revs` に書く（GitHub の blame 表示はこのファイルを自動で読む。ローカルでは `git config blame.ignoreRevsFile .git-blame-ignore-revs`）。
登録したコミットの変更はすべて blame から見えなくなるため、整形以外の変更（ロジックや `flake.lock`）を同じコミットに含めない。

## 2. 変更するファイル

| ファイル | 内容 |
|---|---|
| `flake.nix` / `flake.lock` | devShell（`ormolu` のみ） |
| `.envrc` | `use flake` |
| `.gitignore` | `.direnv/`、`.claude/settings.local.json`（個人用の Claude Code 設定） |
| `.claude/settings.json` / `.claude/hooks/ormolu.sh` | Claude Code の整形 hook |
| `.github/workflows/ci.yml` | 整形を検査するジョブ |
| `.git-blame-ignore-revs` | 一括整形のコミット |
| `app/` `src/` `test/` の `.hs` | 一括整形（挙動は変わらない） |

## 3. ドキュメントの更新

| ファイル | 内容 |
|---|---|
| `CLAUDE.md` | 「コマンド」に整形のコマンド、「コーディング・テストの規約」に ormolu で整形することを加える |
| `docs/architecture.md` | 「9. テスト」の CI の節に整形の検査を加え、開発環境（devShell・エディタの設定）の節を加える |
| `docs/README.md` | step027 を一覧に加える |
| `docs/backlog.md` | 「フォーマッタの導入と CI での検査」を実現済みへ移す。HLint は残す |

## 4. スコープ外（将来の課題）

- HLint の導入と CI での検査
- `.cabal` ファイルの整形（`cabal-gild` など）
- git の pre-commit フックでの整形の検査（CI で検出できるため見送る）

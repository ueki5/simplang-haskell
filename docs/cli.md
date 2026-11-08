# コマンドラインとビルドの仕様

`simplang-haskell` コマンドの**現時点の**仕様。変更したら必ず更新する。
実装は `app/Main.hs`（引数の解析）と `src/Driver.hs`（ビルド）で、詳細は `docs/architecture.md` の「7. ドライバ」を参照。

## 1. 書式

```
simplang-haskell FILES... -o OUTPUT [-p|-i|-S|-c] [-d OUTDIR]
```

開発中は `cabal run simplang-haskell -- FILES... -o OUTPUT ...` で実行する。

| 引数 | 内容 |
|---|---|
| `FILES...` | 入力ファイル（1個以上）。拡張子で種類を判定する：`.sl` ソース / `.s` アセンブリ / `.o` オブジェクト。種類の混在・未知の拡張子はエラー |
| `-o OUTPUT` | 出力ファイル（必須）。指定したパスにそのまま出力する |
| `-p` | パースのみ（.sl → .slp）。ソースを読み込んでパースした結果 `Module`（表示用のパス `modPath`・正規化した絶対パス `modCanonical`・AST `modProgram`）を、`-i` と同じ形式で書き出す。import の解決・型検査は行わない |
| `-i` | codegen の入力の出力のみ（.sl → .sli）。codegen に渡す入力（公開シンボル `public`・static の初期値 `statics`・main 以外の fn `fns`・main `mainPart`）を、フィールド名付きで、Haskell の `Show` の表現を pretty-simple で整形した形（`pPrint` と同じ整形・色なし）で書き出す |
| `-S` | コンパイルのみ（.sl → .s） |
| `-c` | アセンブルまで（.sl / .s → .o） |
| （`-p`/`-i`/`-S`/`-c` なし） | リンクまで行い、実行ファイルを出力する |
| `-d OUTDIR` | 中間ファイルの出力先。指定すると中間ファイルを残す |
| `-h` / `--help` | 使い方を表示する |

`-p`・`-i`・`-S`・`-c` は互いに同時に指定できず、どれも入力は1つだけ。

## 2. モードと入力の組み合わせ

| モード | `.sl` | `.s` | `.o` |
|---|---|---|---|
| `-p` | 1個 → .slp | エラー | エラー |
| `-i` | 1個 → .sli | エラー | エラー |
| `-S` | 1個 → .s | エラー | エラー |
| `-c` | 1個 → .o | 1個 → .o | エラー |
| 指定なし | 1個以上 → 実行ファイル（import を再帰的にたどって一括処理） | 1個以上 → 実行ファイル | 1個以上 → 実行ファイル |

## 3. import の扱い

- `-p`：対象のソースをパースするだけで、import 先は読まない（存在しなくてもよい）。main は無くてもよい
- `-i`/`-S`/`-c`（単体コンパイル）：対象のソースと、それが**直接** import したソースの pub の定義だけを読む。main は無くてもよい
- 指定なしで入力がソース（一括処理）：入力から import を再帰的にたどった全ソースをそれぞれコンパイル・アセンブルし、まとめてリンクする。同じファイルは1回だけ処理する（import の循環は許される）
- 一括処理では、全ソースを通して pub の fn 名・static 名の重複が無いこと、main がちょうど1つあることを検査する（非公開の fn・static はファイルローカルなシンボルなので、重複してよい）

## 4. 中間ファイル

- 名前は `OUTDIR/basename.s` / `OUTDIR/basename.o`（ソースのディレクトリは考慮しない）。一括処理で入力がソースのときは、各ソースの `OUTDIR/basename.slp`（`-p` の出力と同じ内容）と `OUTDIR/basename.sli`（`-i` の出力と同じ内容）も作る。`-c` や、入力が `.s`/`.o` の一括処理では `.slp`/`.sli` は作らない。別ディレクトリにある同名のソースは衝突するため、ビルド前にエラーにする
- `-d` を省略するとシステムのテンポラリディレクトリ（`TMPDIR` があればそれ）に作り、終了時に（エラー時も）削除する
- `-d` を指定すると、ディレクトリが無ければ作成し、中間ファイルを残す

## 5. 使用例

```bash
# 一括処理（main.sl が import するソースも自動的にコンパイル・リンクされる）
simplang-haskell main.sl -o prog && ./prog

# 段階的なビルド
simplang-haskell main.sl -S -o main.s
simplang-haskell main.s  -c -o main.o
simplang-haskell lib/util.sl -c -o util.o
simplang-haskell main.o util.o -o prog

# 中間ファイル（.slp/.sli/.s/.o）を build/ に残す
simplang-haskell main.sl -o prog -d build

# パース結果を確かめる
simplang-haskell main.sl -p -o main.slp

# codegen の入力を確かめる
simplang-haskell main.sl -i -o main.sli
```

## 6. エラー

- エラーは `Error: メッセージ` を出力して終了コード 1 で終了する
- ソースごとの処理のエラー（構文・型エラーなど）にはファイル名が前置される（`main.sl: type mismatch: ...`）
- gcc が失敗した場合は、実行したコマンドと gcc の標準エラー出力を表示する

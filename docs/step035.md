# パース結果の出力（`-p` と `.slp`）における設計上の考慮点

step034 で、codegen の入力（`-i` / `.sli`）を外から確かめられるようになった。しかしその前段であるパーサの結果（AST）は、まだ確かめる手段が無い。構文の追加や変更のとき、パーサが意図どおりの AST を作っているかを見るには、テストを書くか、デバッグ用のコードを足すしかなかった。

今回、`Driver.loadModule` の出力である `Module`（表示用のパス・正規化したパス・プログラムの AST）を、step034 の `.sli` と同じく pretty-simple で整形してファイルに書き出せるようにする。

```bash
# 単体：-o にパース結果を書き出す
simplang-haskell main.sl -p -o main.slp

# 一括処理：中間ファイル build/main.slp・build/util.slp も作られる（-d を付ければ残る）
simplang-haskell main.sl -o prog -d build
```

```rust
// main.sl
fn main() -> i64 { 1 + 2 }
```

```haskell
-- main.slp の中身（modCanonical は環境によって変わる）
Module
    { modPath = "main.sl"
    , modCanonical = "/home/user/work/main.sl"
    , modProgram = Program
        { progImports = []
        , progGlobals = []
        , progFns =
            [ FnDecl Private "main" [] ( TyInt Signed W64 )
                ( []
                , Add
                    ( Lit 1 )
                    ( Lit 2 )
                )
            ]
        }
    }
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| `-p` | ソース1個を `loadModule` で読み込み・パースし、`Module` を整形したもの（末尾に改行）を `-o OUTPUT` に書き出す |
| `-p` の処理範囲 | パースまで。import の解決・型検査は行わない（`loadModule` の出力そのもの）。main は無くてもよい |
| `-p` の入力 | ソース（`.sl`）のみ。`.s`/`.o` はエラー（`-p requires a source file (.sl)`） |
| `-p`/`-i`/`-S`/`-c` | 互いに排他。どれも入力は1つだけ |
| 複数入力のエラーメッセージ | `-S/-c/-i accept only one input file` を `-S/-c/-i/-p accept only one input file` に変える |
| 一括処理（指定なし・入力がソース） | 各ソースの `.sli`/`.s`/`.o` に加え、中間ファイル `OUTDIR/basename.slp` も作る（`.slp` → `.sli` → `.s` → `.o` の順） |
| `.slp` の扱い | `.sli`/`.s`/`.o` と同じ中間ファイル。名前の衝突を事前に検査し、`-d` が無ければ終了時に削除する |
| `-i`/`-S`/`-c`、入力が `.s`/`.o` の一括処理 | `.slp` は作らない |
| 出力内容 | `Module { modPath, modCanonical, modProgram }` をそのまま出力する |
| 書式 | `.sli` と同じく、`Show` の表現を pretty-simple の `pShowNoColor` で整形したもの。読み込み（`.slp` を入力に取る）は行わない |

## 1. 考慮点

### 1.1 `-p` の処理範囲

| 選択肢 | 評価 |
|---|---|
| **パースまで（`loadModule` の出力）（採用）** | AST を確かめるという目的に必要十分。型エラーや import 先の不備があっても AST を見られるので、パーサのデバッグに使いやすい |
| import の解決・型検査まで行ってから出力する | `-i` と処理範囲がほぼ重なり、型エラーのあるソースの AST を見られなくなる |

`loadModule` は存在しないファイルと構文エラーだけを検出する。import 先が存在しなくても `-p` は成功する。

### 1.2 出力する内容

| 選択肢 | 評価 |
|---|---|
| **`Module` をそのまま（採用）** | 要望どおり `loadModule` の出力と一致する。一括処理で import をたどったとき、どのソースをどのパスとして扱ったか（`modPath`・`modCanonical`）も確かめられる |
| `modProgram`（`Program`）だけ | 環境に依存しない出力になるが、パスの解決の結果が見えない |

`modCanonical` は絶対パスなので、同じソースでも置き場所によって出力が変わる。`.slp` はデバッグ用で比較の対象にしないので、問題にしない。テストでは `canonicalizePath` で期待値を組む。

`Module` は `Show` を導出していないので導出する。テストで期待値を組むため、`Module (..)` を公開する。

### 1.3 出力先と書式

step034 の `-i` と同じ理由で、出力先は `-o OUTPUT`、書式は pretty-simple（`pShowNoColor`）とする（step034 §1.1・§1.2）。`.sli` と `.slp` の整形は同じ処理なので、`Show` を持つ値を整形する関数1つにまとめる。

### 1.4 一括処理で `.slp` を作る理由と、`-i`/`-S`/`-c` で作らない理由

step034 §1.3 の `.sli` と同じ。一括処理では各ソースのパース結果を個別に見る手段が無いので、`-d` で残すときに `.slp` も並べる。単体コンパイルでは、必要なら同じソースに `-p` を指定すればよい。

一括処理では、`collectSources` が import をたどるときに全ソースを `loadModule` で読み込んでいるので、その `Module` をそのまま書き出す（もう一度読み込まない）。

### 1.5 `-p` を他のフラグと同時に指定できない理由

step034 §1.4 と同じ。`-p` は「どこまで処理するか（`Mode`）」の1つとして `-i`/`-S`/`-c` と並べ、optparse-applicative の `flag' ... <|>` で排他にする。

## 2. 構文・AST

変更なし。

## 3. CLI・ドライバ

- `app/Main.hs`：`modeParser` に `-p`（`ParseOnly`）を加える
- `src/Driver.hs`：
  - `Mode` に `ParseOnly` を加える
  - `Module` に `Show` を導出し、`Module (..)` を公開する
  - `showIR` を、`Show` を持つ値を整形する `showPretty` にまとめ、`.sli` と `.slp` の両方に使う
  - `-p`：`loadModule` の結果を `showPretty` して `-o` に書く
  - 一括処理：中間ファイルの名前の衝突検査の対象に `.slp` を加え、各ソースについて `.slp` → `.sli` → `.s` → `.o` の順に作る
  - `classifyInputs`：`-p` は入力がソース1個のときだけ認める

## 4. 移行

- 言語仕様・生成されるアセンブリは変わらない
- `test/Spec.hs`：複数入力のエラーメッセージの期待値を `-S/-c/-i/-p accept only one input file` に変える。`-p` の出力・入力の検査、一括処理の `.slp`（`-d` の有無）、`-c` と入力が `.s` の一括処理で `.slp` を作らないことのテストを加える
- `docs/cli.md`・`docs/architecture.md`（§7 ドライバ）を更新する
- 言語機能の追加ではないので、サンプルは追加・変更しない

## 5. スコープ外（将来の課題）

- `.slp` を入力に取り、コンパイル以降だけを行う（`Read` の導出が必要）

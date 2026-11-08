# codegen の入力の出力（`-i` と `.sli`）における設計上の考慮点

コンパイラ（`Compiler.compile`）が作った中間命令列は、`CodeGen.codegen` でアセンブリへ変換されるだけで、外から確かめる手段が無かった。アセンブリ（`-S`）は x86-64 の命令に展開された後なので、型検査やコンパイラの命令選択の結果を見るには遠い。

今回、codegen に渡す4つの入力（`public`・`statics`・`fns`・`mainPart`）を、フィールド名を付けたレコードとして、pretty-simple で読みやすく整形してファイルに書き出せるようにする。

```bash
# 単体：-o に codegen の入力を書き出す
simplang-haskell main.sl -i -o main.sli

# 一括処理：中間ファイル build/main.sli・build/util.sli も作られる（-d を付ければ残る）
simplang-haskell main.sl -o prog -d build
```

```rust
// main.sl
fn main() -> i64 { 1 + 2 }
```

```haskell
-- main.sli の中身
CodegenInput
    { public = []
    , statics = []
    , fns = []
    , mainPart = Just
        ( TyInt Signed W64
        ,
            [ Push 1
            , Push 2
            , IAdd W64
            , Label ".Lfn_end0"
            ]
        )
    }
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| `-i` | ソース1個をコンパイルし、codegen の入力（レコード `CodegenInput { public, statics, fns, mainPart }`）を整形したもの（末尾に改行）を `-o OUTPUT` に書き出す |
| `-S`/`-c`/`-i` | 互いに排他。どれも入力は1つだけ |
| `-i` の入力 | ソース（`.sl`）のみ。`.s`/`.o` はエラー（`-i requires a source file (.sl)`） |
| `-i` の import | `-S`/`-c` と同じく、直接 import したソースの pub の定義だけを読む。main は無くてもよい |
| 一括処理（指定なし・入力がソース） | 各ソースの `.s`/`.o` に加え、中間ファイル `OUTDIR/basename.sli` も作る |
| `.sli` の扱い | `.s`/`.o` と同じ中間ファイル。名前の衝突を事前に検査し、`-d` が無ければ終了時に削除する |
| `-S`/`-c`、入力が `.s`/`.o` の一括処理 | `.sli` は作らない |
| 書式 | `Show` の表現を pretty-simple（`Text.Pretty.Simple`）で整形したもの。`pPrint` と同じ整形で、色のエスケープは付けない（`pShowNoColor`）。読み込み（`.sli` を入力に取る）は行わない |
| 複数入力のエラーメッセージ | `-S/-c accept only one input file` を `-S/-c/-i accept only one input file` に変える |

## 1. 考慮点

### 1.1 `-i` の出力先

| 選択肢 | 評価 |
|---|---|
| **`-o OUTPUT` にそのまま書く（採用）** | `-S`/`-c` と同じ規則。「`-o` は必須で、指定したパスにそのまま出力する」という CLI の原則を崩さない |
| `-o` を使わず `OUTDIR/basename.sli` に書く | `-i` のときだけ `-o` の要否や `-d` の意味が変わり、規則が増える |

`basename.sli` という名前は、一括処理の中間ファイルの名前としてだけ使う（`.s`/`.o` と同じ）。

### 1.2 書式

codegen の入力はすべて `Show` を導出した型（`String`・`StaticData`・`Type`・`Instr`）でできているので、`Show` の表現をそのまま使えば、内容と codegen の入力が必ず一致する。ただし `show` の1行では、命令列が長くなると読めない。

| 選択肢 | 評価 |
|---|---|
| `show` の1行 | 最小の変更だが、fn や命令の区切りが分からない |
| 独自の整形（fn ごと・1命令1行など） | 見た目を自由に決められるが、`Instr` などの型が変わるたびに整形処理も直す必要がある |
| **pretty-simple で整形（採用）** | `Show` の表現をそのまま字下げ・改行するので、型が変わっても追随する。デバッグ用にすでに依存に含めているライブラリで、`pPrint` で見慣れた形になる |

タプルのままだと、4つの要素のどれが何かが出力から分からない。codegen の入力をレコード `CodegenInput` にして `Show` を導出すれば、`public = ...`・`fns = ...` のようにフィールド名も出力される（整形の処理は変えずに済む）。

ターミナルへ表示する `pPrint` は端末なら色のエスケープを付けるが、ファイルに書く `.sli` には不要なので、同じ整形で色を付けない `pShowNoColor` を使う（その戻り値の `Text` を扱うため、`text` パッケージを依存に加える）。`.sli` を読み戻して codegen だけを行う機能は、必要になってから検討する（→ スコープ外）。

### 1.3 一括処理でも `.sli` を作る理由と、`-S`/`-c` で作らない理由

一括処理では import をたどって複数のソースをまとめて処理するため、各ソースのコンパイル結果を個別に見る手段が無い。`-d` で中間ファイルを残すとき、`.s`/`.o` と並べて `.sli` も残せば、1回のビルドで全ソースの codegen の入力を確かめられる。`-d` が無ければ他の中間ファイルと同じく削除されるので、通常のビルドの結果は変わらない。

`-S`/`-c` は単体コンパイルで、`.sli` が必要なら同じソースに `-i` を指定すればよい。`-c` の中間ファイル（`.s`）の扱いも変えない。

### 1.4 `-i` を他のフラグと同時に指定できない理由

`-i` は「どこまで処理するか（`Mode`）」の1つとして `-S`/`-c` と並べる。`-i` と `-S` を同時に指定して `.sli` と `.s` の両方を得る使い方は、出力先が `-o` 1つしかないので成り立たない。排他は既存の `-S`/`-c` と同じく、optparse-applicative の `flag' ... <|>` で表す（同時に指定すると引数の解析エラーになる）。

## 2. 構文・AST

変更なし。

## 3. CLI・ドライバ

- `app/Main.hs`：`modeParser` に `-i`（`IROnly`）を加える
- `src/Driver.hs`：
  - `Mode` に `IROnly` を加える
  - codegen の入力を表すレコード `CodegenInput { public :: [String], statics :: [StaticData], fns :: [(String, Type, [Instr])], mainPart :: Maybe (Type, [Instr]) }` を設ける（`Show` を導出。テストで期待値を組むため公開する）
  - `compileSource` を、codegen の入力までを作る `compileIR` と、そこからアセンブリを作る `genAsm`、`.sli` の中身を作る `showIR`（`pShowNoColor`）に分ける
  - `-i`：`compileIR` の結果を `showIR` して `-o` に書く
  - 一括処理：中間ファイルの名前の衝突検査の対象に `.sli` を加え、各ソースについて `.sli` → `.s` の順に作る（`toAsm` に `.sli` を作るかを引数で渡す。`-c` は作らない）
  - `classifyInputs`：`-i` は入力がソース1個のときだけ認める

## 4. 移行

- 言語仕様・生成されるアセンブリは変わらない
- `test/Spec.hs`：複数入力のエラーメッセージの期待値を `-S/-c/-i accept only one input file` に変える。`-i` の出力・入力の検査、一括処理の `.sli`（`-d` の有無）、`-c` で `.sli` を作らないことのテストを加える
- `docs/cli.md`・`docs/architecture.md`（§7 ドライバ）を更新する
- `simplang-haskell.cabal`：ライブラリとテストの依存に `text` を加える（pretty-simple はすでに依存している）
- 言語機能の追加ではないので、サンプルは追加・変更しない

## 5. スコープ外（将来の課題）

- `.sli` を入力に取り、codegen 以降だけを行う（`Read` の導出が必要）

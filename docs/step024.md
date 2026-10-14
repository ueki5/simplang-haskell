# 複数ソースファイルからのビルドにおける設計上の考慮点

step023 で関数シグネチャを宣言だけで確定させたのを前提に、ソースファイルごとにコンパイルし、他のソースの fn をシグネチャだけで呼び出せるようにする。

あわせて、コマンドラインを gcc 風（`-S` でコンパイルまで、`-c` でアセンブルまで、指定なしでリンクまで）に作り替える。

```
// main.sl
import "lib/util.sl";
fn main() -> i64 { add(sq(3), 1) }

// lib/util.sl
import "math.sl";
fn add(a: i64, b: i64) -> i64 { a + b }
fn sq(x: i64) -> i64 { mul(x, x) }

// lib/math.sl
fn mul(a: i64, b: i64) -> i64 { a * b }
```

```
$ simplang-haskell main.sl -o prog && ./prog
10
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| コマンドライン | `simplang-haskell <sourcefiles...> -o <outputfile> [-S\|-c] [-d <outdir>]` |
| `-o` | どのモードでも必須。指定したパスにそのまま出力する（`-S` なら .s、`-c` なら .o、指定なしなら実行ファイル） |
| `-S` / `-c` | 同時に指定できない。入力は1つだけ |
| 入力ファイルの種類 | 拡張子で判定する（`.sl` ソース / `.s` アセンブリ / `.o` オブジェクト）。種類の混在・未知の拡張子はエラー |
| 中間ファイル | `<outdir>/basename.s`、`<outdir>/basename.o`（ソースのディレクトリは考慮しない） |
| `-d` | 中間ファイルの出力先。既定はシステムのテンポラリディレクトリ（`getTemporaryDirectory`。`TMPDIR` があればそれ）で、終了時（エラー時も含む）に中間ファイルを削除する。`-d` を明示すると削除しない |
| import | `import "相対パス";` をファイル先頭に書く（importを書いたソースのディレクトリからの相対パス、`;` 必須） |
| 取り込む fn | **直接** import したソースに定義された fn だけ（import 先の import は取り込まない）。import 先の main は取り込まない |
| 一括処理 | `-S`/`-c` が無く入力がソースのとき、import を再帰的にたどった全ソースをコンパイル・アセンブルし、まとめてリンクする |
| main | 単体コンパイル（`-S`/`-c`）では無くてもよい。一括処理では全ソースでちょうど1つ必要 |

### モードと入力の組み合わせ

| モード | 入力 | 処理 |
|---|---|---|
| `-S` | .sl ×1 | コンパイル → `-o` |
| `-c` | .sl ×1 | コンパイル → 中間 .s → アセンブル → `-o` |
| `-c` | .s ×1 | アセンブル → `-o` |
| 指定なし | .sl ×N | import を再帰的にたどった全ソースをそれぞれコンパイル → 中間 .s → アセンブル → 中間 .o → リンク → `-o` |
| 指定なし | .s ×N | それぞれアセンブル → 中間 .o → リンク → `-o` |
| 指定なし | .o ×N | リンク → `-o` |

次の組み合わせは、することがないのでエラーにする。

- `-S` に `.s`/`.o` を入力する
- `-c` に `.o` を入力する

## 1. 考慮点

### 1.1 import は推移的に取り込まない

A が B を import し、B が C を import していても、A から C の fn は呼べない（`undeclared function`）。

- 各ソースで呼べる fn が、そのソースの import 文だけで決まる
- 推移的に取り込むと、B の内部実装のための import が A の名前空間を汚染し、名前の衝突の原因になる

一方で一括処理では、リンクに必要なため C もコンパイルする（「import を再帰的にコンパイル」）。

### 1.2 import の循環

A と B が互いに import していてもよい。

- コンパイル時に共有するのはシグネチャ（宣言）だけで、本体は各ソースが自分の分だけコンパイルする
- 一括処理でソースを集めるときは `canonicalizePath` で同一ファイルを判定し、1回だけ処理する

### 1.3 名前の衝突

| 状況 | 検出箇所 | エラー |
|---|---|---|
| 自ソースの fn と import した fn が同名 | `compileModule` | `function already declared in imported file: f` |
| import した2つのソースに同名の fn | Driver（`importedSigs`） | `main.sl: function f is defined in both a.sl and b.sl` |
| リンクする全ソースで fn が重複 | Driver（`checkLinkUnits`） | `function f is defined in both a.sl and b.sl` |
| main が無い／複数ある | Driver（`checkLinkUnits`） | `missing function: main` / `function main is defined in multiple files: ...` |

最後の2つはリンカでも検出されるが、ソースファイル名を含む分かりやすいメッセージにするため、リンク前に検査する。

`-S`/`-c` で個別にコンパイルした .o 同士の重複は、リンカ（gcc）のエラーになる。

### 1.4 中間ファイル名の衝突

中間ファイル名はソースのディレクトリを考慮せず basename だけで決まる。そのため、`x/u.sl` と `y/u.sl` を一括処理すると同じ `<outdir>/u.s` になる。黙って上書きすると誤ったリンクになるため、エラーにする（`intermediate file name conflict: ...`）。

### 1.5 シンボルの公開

- 各 fn のラベルの前に `.globl name` を出し、他のオブジェクトから呼べるようにした
- 呼び出し側（`call name`）は変更していない。アセンブラは未定義のシンボルを外部参照として扱う
- `.Ldiv_zero_error` などの `.L` ラベルはファイルローカル
- `fmt32`/`errmsg` などの rodata ラベルは `.globl` を付けていないため、各オブジェクトに重複して存在してもリンク時に衝突しない
- fn 名はそのままシンボル名になる。そのため、`puts`/`exit`/`printf` などの libc の関数と同名の fn を定義すると、ランタイムのエラー処理がそちらを呼んでしまう（step023 以前から同じ問題があり、今回はスコープ外。backlog 参照）

### 1.6 中間ファイルの後始末

- `build` は作成する直前の中間ファイルのパスを `IORef` に登録し、`finally` で削除する。そのため、コンパイルエラーや gcc の失敗で途中終了しても中間ファイルは残らない
- `-d` を明示した場合は削除しない（デバッグ用途）
- ディレクトリが無ければ作成する

## 2. 構文・AST

```
program     ::= import-decl* fn-decl*
import-decl ::= 'import' STRING ';'
```

- トークン `TImport`（予約語 `import`）と `TString String`（`"..."`、エスケープ無し）を追加した
  - 閉じられていない文字列や、途中に改行を含む文字列は `unterminated string literal`
- `Program` を `data Program = Program { progImports :: [FilePath], progFns :: [FnDecl] }` に変更した
- fn 定義より後の import は `import must appear before function definitions`

## 3. コンパイラ・コード生成

- `compileModule :: FnSigs -> [FnDecl] -> Either String ([(String, [Instr])], Maybe (Type, [Instr]))`
  - 1ソース分のコンパイル単位
  - 第1引数は import したソースの fn のシグネチャ（main は除く）で、自ソースのシグネチャとマージして呼び出しの検査に使う
  - main は任意（`Maybe`）
- `compile :: Program -> ...` は、単一ソースで完結するプログラム（main 必須）用の薄いラッパとして残した（テストで使う）
- `validateFnShapes` から main の存在検査を外し、`compile` 側へ移した
- `codegenModule :: [(String, [Instr])] -> Maybe (Type, [Instr]) -> String`
  - main が無ければ `main:` を出力しない
  - 各 fn に `.globl` を付ける
- `codegen` は `codegenModule fns (Just (ty, instrs))` の別名として残した

## 4. ドライバ（`src/Driver.hs`）

テストから呼べるよう、ライブラリのモジュールとした。`app/Main.hs` はオプションの解析だけを行う。

- `build :: BuildOpts -> IO (Either String ())`
  - `BuildOpts { inputs, output, mode = AsmOnly | ObjOnly | Link, outDir :: Maybe FilePath }`
- `classifyInputs`: 拡張子による種類の判定と、モードとの組み合わせの検証
- `loadModule`: 読み込み → tokenize → parse。エラーには `path: ` を前置する
- `importedSigs`: 直接 import したソースを読み、`buildFnSigs` でシグネチャを得る
- `collectSources`: 一括処理のときに、入力から import を再帰的にたどる
- `checkLinkUnits`: fn 名の重複と main の数を検査する
- `checkIntermediates`: 中間ファイル名の衝突を検査する
- gcc は `readProcessWithExitCode` で呼び、失敗時はその標準エラー出力をエラーメッセージに含める

## 5. 移行

- 旧オプション `-S FILE`（アセンブリの保存先）は、意味の異なる `-S`（コンパイルのみ）に置き換わった
  - 旧 `-o` 省略時の既定値（`output/basename`）は廃止し、`-o` を必須にした
  - `comp-sample.sh` は `-d output` で中間ファイルを残す形に書き換えた
- テスト
  - `parse` の結果を `Program [] [...]` で比較するようにした
  - import の字句・構文、`compileModule`、`codegenModule`、Driver（一括処理・段階的ビルド・中間ファイルの削除・各種エラー）のテストを追加した

## 6. スコープ外（将来の課題）

- 公開範囲の制御（`pub`）。現在は全ての fn が他のソースから呼べる
- 名前空間・名前マングリング（`util::add` のような修飾名、libc のシンボルとの衝突回避）
- 中間ファイルの既定の出力先（テンポラリディレクトリ直下）での、並行実行時の衝突
- 変更されたソースだけを再コンパイルする差分ビルド

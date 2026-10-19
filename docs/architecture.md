# simplang-haskell 実装の全体像

コンパイラ本体の**現時点の**設計をまとめたもの。実装を変更したら必ず更新する。
言語仕様は `docs/language.md`、コマンドラインとビルドの仕様は `docs/cli.md`、各機能を導入した経緯は `docs/stepNNN.md` を参照。

## 1. パイプライン

```
ソース (.sl)
  → tokenize       (src/Parser.hs)    字句解析                     String → [Token]
  → parse          (src/Parser.hs)    再帰下降パーサ                [Token] → Program
  → compileModule  (src/Compiler.hs)  型検査 + 定数式の評価 + 中間命令列への変換    Program → [Instr]（fn ごと）+ static の初期値
  → codegenModule  (src/CodeGen.hs)   x86-64 AT&T 構文アセンブリ    [Instr] → String
  → gcc -c         (src/Driver.hs)    アセンブル                    .s → .o
  → gcc            (src/Driver.hs)    リンク（libc の printf/puts/exit を使う）  .o... → 実行ファイル
```

コンパイルはソースファイル単位で行い、他のソースの fn はシグネチャだけを使って呼び出す（本体はリンク時に解決される）。他のソースの static は型とシンボル名だけを使って参照し、const は評価済みの値を取り込む。

## 2. モジュール

| ファイル | 責務 |
|---|---|
| `src/Parser.hs` | トークン・AST・型（`Type`/`Width`/`Signedness`）の定義、`tokenize`、`parse` |
| `src/Compiler.hs` | 中間命令 `Instr` の定義、シグネチャ表・グローバルの表の構築（定数式の評価を含む）、型検査とIR生成（`compileModule`/`compile`）、スタックVM（`run`） |
| `src/CodeGen.hs` | `Instr` → x86-64 アセンブリ（`codegenModule`/`codegen`） |
| `src/Driver.hs` | 入力の種類の判定、import の解決、各段階の実行と gcc 呼び出し、中間ファイルの管理（`build`） |
| `app/Main.hs` | コマンドライン引数の解析（optparse-applicative）と `build` の呼び出し |
| `test/Spec.hs` | Hspec のテストスイート（→ 9） |

依存は `Parser ← Compiler ← CodeGen ← Driver ← Main` の一方向。

## 3. 主要な型

```haskell
-- AST（src/Parser.hs）
data Program    = Program { progImports :: [FilePath], progGlobals :: [GlobalDecl], progFns :: [FnDecl] }
data FnDecl     = FnDecl Visibility String [(String, Type)] Type Body   -- 公開範囲, 名前, 仮引数, 戻り値型, 本体
type Body       = ([Stmt], Expr)                                        -- 文の列 + 末尾式
data GlobalDecl = GlobalDecl Visibility GlobalKind String Type Expr     -- 公開範囲, const/static, 名前, 型, 初期値
data GlobalKind = GConst | GStatic
data Visibility = Private | Public                                      -- pub の有無

-- 型
data Width      = W32 | W64                 -- 整数のビット幅 = codegen のレジスタ幅
data Signedness = Signed | Unsigned
data Type = TyInt Signedness Width          -- i32 / i64 / u32 / u64
          | TUsize | TBool | TUnit
          | TPtr Type                       -- &T（再帰的）
          | TArray Type Int                 -- [T; N]

-- コンパイラ（src/Compiler.hs）
data Loc     = LLocal Int | LStatic String | LConst Int   -- %rbp 相対オフセット / static のシンボル名 / const の値
type Env     = [Map String (Loc, Type)]     -- 変数名 → (場所, 型)。スコープのスタック（先頭が最内側、末尾がグローバル）
type FnSigs  = Map String ([Type], Type)    -- fn 名 → (仮引数の型, 戻り値型)
type Globals = Map String (Loc, Type)       -- const/static 名 → (LConst 値 / LStatic 名, 型)
data StaticInit = SInt Int | SAddr String Int   -- static の初期値の要素：整数 / アドレスの定数（シンボル名, バイトオフセット）
type StaticData = (String, Width, [StaticInit])  -- static の初期値（シンボル名, 要素の格納幅, 値の列）
data CompiledModule = CompiledModule { cmStatics :: [StaticData], cmFns :: [(String, [Instr])], cmMain :: Maybe (Type, [Instr]) }
data Instr   = Push Int | IAdd Width | ... | ICall String Int | IPop | LoadGlobal Width String | ...   -- スタックマシンの中間命令
```

## 4. パーサ

- 左再帰を除去した反復ループで左結合を実現する：`parseExpr` → `parseEquality` → `parseComparison` → `parseAdditive` → `parseMultiplicative` → `parseCast` → `parseFactor`（前置の単項演算子）→ `parsePostfixRest`（添字）→ `parsePrimary`
- `parseCast` は `as` の後の型が整数型でなければパース時にエラーにする
- 文の判別（`parseStmts`）：キーワード、または `IDENT =`（先読み2トークン）で判別する。どれでもなければ式を試し読みし、次が `=` なら場所式への代入 `SAssignPlace`、`;` なら式文 `SExpr`、それ以外なら文の列の終わり（末尾式）として読み直す
- 脱糖：fn 本体の末尾式の省略と `return;` は `UnitLit` に、`->` の省略は戻り値型 `TUnit` にする。AST 上は末尾式・return の値が常に存在する
- 型サフィックス付き整数リテラル（`9999i64`）は字句解析で1トークン `TIntSuffixed` にまとめる。サフィックスの直後が識別子の構成文字なら（`10i64x`）サフィックスとみなさない
- トップレベル（`parseItems`）は `fn` / `const` / `static` を混在して読み、`progGlobals` と `progFns` に振り分ける。先頭の `pub` は `Public`、無ければ `Private` として各定義に持たせる

## 5. 型検査と IR 生成

### 5.1 二パス構成

1. `buildFnSigs`：全 fn の宣言から `FnSigs` を作る。重複定義・最大 6 引数・`main` の引数なし（`validateFnShapes`）とシグネチャ中の配列型の禁止を検査する。本体は見ないので宣言順に依存しない
2. `buildGlobals`：全 const/static の宣言から `Globals` と static の初期値を作る（→ 5.8）
3. `compileModule importedSigs importedGlobals prog`：自ソースと import 先の fn シグネチャ（`main` を除く）・グローバルをマージし（fn・const・static は同じ名前空間で、同名はエラー）、各 fn の本体を `compileFnDecl` でコンパイルする。結果の `CompiledModule` は、`.globl` にする pub の fn・static の名前（`cmPublic`）も持つ

`compile` は main を必須とする単一ソース用のラッパで、命令列を検査するテストで使う（static の初期値は返さない）。

### 5.2 期待型の一様伝播

- 型検査とコード生成は `compileExprTyped fnSigs env expected expr` が同時に行う。期待型 `expected` を式木の子へそのまま伝播させ、`Var` の実際の型と食い違えば `type mismatch` エラーにする
- これにより、同一式内での整数型（符号・幅）の混在は必ず検出される。式ノードへの型タグ付けやボトムアップの型推論は持たない
- 型が確定しているリーフは `Var`、型サフィックス付きリテラル `LitTyped`、`BoolLit`、`UnitLit`、`Call`（戻り値型）。型なしリテラル `Lit` は整数型の `expected` ならそのまま受け入れる
- 期待型が伝播しない箇所は、`inferMaybeType`（確定した型を持つリーフから単一化して型を求める。整数リテラルだけなら `Nothing`）で被演算子の型を決める：
  - 比較演算子の被演算子（`operandType`。結果は bool で、被演算子の型とは独立）
  - キャスト `Cast e ty` の被演算子（結果型 `ty` は `expected` と厳密一致を要求）
  - 型注釈を省略した `let`（`SLetInferred`）と式文 `SExpr`（`inferType` = 未確定なら i64）
  - 配列の基底（`arrayBase`）、`*e` の場所式（`addressOf`）
- `if` / `while` の条件は `TBool` を期待型としてコンパイルする

### 5.3 変数とスコープ

- `Env` はスコープのスタック。fn 本体は `[仮引数のスコープ, グローバルのスコープ]` から始まり、文を左から右へ単一パスで処理しながら `(Env, cursor)` を引き回す（`compileStmtsFrom`）
- 名前の解決は `lookupVar` 1つで行い、見つかった場所（`Loc`）で命令を選ぶ：ローカルは `Load`/`Store`/`LoadAddr`、static は `LoadGlobal`/`StoreGlobal`/`LoadGlobalAddr`、const は `Push 値`（代入・`&` はエラー）
- `cursor` はスタック上の次の空き位置（%rbp 相対、負の方向へ伸びる）。変数は型の実サイズ（`stackBytes`。i32=4, i64=8, 配列は要素数×要素サイズ）で詰めて配置し、オフセットは `cursor - stackBytes ty`
- `{}`・if の各分岐・while の本体は空のスコープを push して再帰コンパイルし、戻ったら呼び出し前の `Env`/`cursor` で続ける。これでスコープアウトとスタック領域の巻き戻しが同時に実現され、時間的に重ならない兄弟ブロックは同じ領域を再利用する
- 同一スコープでの再宣言は `declaredLocally` でエラーにし、外側との同名（シャドーイング）は許可する。ただし let・仮引数で const/static と同名の変数は宣言できない（`checkNotGlobal`）

### 5.4 制御構文とラベル

- `CompileM = StateT Int (Either String)` の状態がラベル番号のカウンタで、`freshLabel` が `.Lif_end3` のような一意なラベルを払い出す。`Env`/`cursor` と違って巻き戻してはいけない値なので、引数ではなく State で持ち回る
- `LoopCtx`（直近のループの continue/break ラベル）と `ReturnCtx`（fn 名・戻り値型・fn 末尾ラベル）は普通の引数として渡す。`while` に入るときだけ `LoopCtx` を差し替える
- `return` と末尾式はどちらも fn 末尾ラベルへ合流する

### 5.5 場所式とポインタ

- 場所式（`&` の対象・代入の左辺）は `addressOf` が一本化して扱う。認めるのは `Var`、`Deref`、`Index` の3種類で、それ以外は `not an lvalue` エラー
- `&x` は `LoadAddr off`（static なら `LoadGlobalAddr name`）、`&*p` は `p` の値そのもの、`&a[i]` は要素アドレス
- 読み出しは `LoadInd`、場所式への書き込みは `StoreInd`
- `checkNoLocalRef`：fn の return 式・末尾式が直接 `&ローカル変数` / `&ローカル配列[i]` ならエラー（`&static` は可）。main は対象外
- `checkNoLocalStore`：static への代入（`SAssign`、`SAssignPlace (Index (Var static) _)`）で、右辺が直接ローカルを指すならエラー。main も対象
- どちらも、式が直接ローカルを指すかの判定は `isLocalRef` で共用する。中間変数を経由する形は検出しない

### 5.6 配列

- 配列変数の先頭（最下位アドレス）を `off = cursor - stackBytes ty` とし、要素 i は `off + i * elemSize`
- `let` の初期化（`compileArrayInit`）は要素ごとに静的なオフセットへ `Store` する。`[e; N]` は e を1回だけ評価して要素0へ格納し、残りはそこからコピーする
- 添字の基底は `arrayBase` が解決する：配列変数なら `LoadAddr`（static の配列なら `LoadGlobalAddr`）、`&[T; N]` 型の式（`p[i]` の自動参照外し、`(*p)[i]`）ならその値を先頭アドレスとする。`N` は型から得られる
- 要素アドレスは `indexAddr` が `基底 + 添字（期待型 usize）+ IBoundsCheck N + Push size; IMul W64; IAdd W64` で計算する。添字がリテラルならコンパイル時にも範囲を検査する

### 5.7 整数の表現と命令選択

- 整数型の判定・命令選択は `TyInt` へ直接マッチせず、`intRepr :: Type -> Maybe (Signedness, Width)` を経由する（usize は `(Unsigned, W64)` に分解される。コード生成は u64 と同一だが型としては区別される）
- 操作スタック上の値は常に 64bit で、不変条件は「**i32 は符号拡張済み、u32 はゼロ拡張済み、i64/u64/usize はそのまま**」
- 加減乗は符号によらず下位ビットが同じなので命令を共用し、u32 のみ直後に `IZext32` を付ける（`arithFixup`）。`Load W32`/`LoadInd W32` は符号拡張するので、u32 の読み出し後にも `IZext32` を付ける（`loadFixup`）
- 除算は符号なしなら `IDivU`、大小比較は符号なしなら `ICmpLtU` 等（`cmpInstr`）
- キャスト（`castInstrs`）：32bit への変換はキャスト先の符号で正規化（`ISext32` / `IZext32`）、32→64bit の拡大はキャスト元の符号で拡張、64bit 同士は命令なし
- bool と () は 32bit スロットに格納する（() は常に 0）。ポインタは指す先によらず 8 バイト（`storageWidth`）

### 5.8 const/static と定数式の評価

- `buildGlobals` が型（const はスカラー、static はスカラーとその配列）と名前の重複を検査し、初期値を評価する
- 定数式は `checkConstExpr` が AST で検査する（リテラル・変数参照・単項/二項演算・キャストのみ。static の参照は不可）
- const は `evalConst` が依存する const を深さ優先で先に評価する（評価中の const に戻ってきたら循環エラー）。参照できるのは同じソースの const だけ
- 評価は、評価済みの const だけを含む Env で `compileExprTyped` にかけて命令列にし、VM（`run`、→ 8）で実行する。型検査・ラップアラウンドは通常の式と同じになる。結果は型の正規形（i32 は符号拡張、u32 はゼロ拡張）に揃える
- static の配列は要素ごとに評価する（`[e; N]` は e を1回評価して N 個並べる）
- ポインタ型の要素は `evalAddrConst` がアドレスの定数として評価する。受け付けるのは同じソースの static の `&X`（`SAddr X 0`）と `&T[i]`（`SAddr T (i × 要素の格納幅)`。i は `evalConstExpr` で usize として評価し、範囲を検査する）。アドレスはロード時まで決まらないので VM は使わない

## 6. コード生成

- **レジスタ**：スタックマシンとして `%rax` / `%rbx` / `%rcx` を使い、値は操作スタック（`pushq`/`popq`）でやり取りする
- **即値**：`Push` は `movabsq` 経由（`pushq` の符号拡張 32bit 即値の制限を回避）
- **32bit 演算**：`addl`/`subl`/`imull`/`idivl`/`negl` の後に `cltq`/`movslq` で符号拡張して 64bit スタックへ戻す。演算ごとにラップアラウンドする
- **除算**：`cmpq $0` で除数を検査し、0 なら `.Ldiv_zero_error` へ。符号ありで除数が -1 のときは `cmove` で「被除数の符号反転・除数 1」に置き換えて割る（`最小値 / -1` で idiv が例外を起こすのを避け、最小値へラップさせる）
- **範囲検査**：`IBoundsCheck N` は添字を符号なしで N と比較し、範囲外なら `.Lindex_error` へ（負の添字も巨大な値として検出される）
- **スタックフレーム**：命令列中の `Load`/`Store`/`StoreArg`/`LoadAddr` のオフセットの最大絶対値を 16 バイトに切り上げてフレームサイズとし（`frameSize`）、プロローグで一度だけ `subq`。エピローグは `leave`
- **関数呼び出し**：SysV AMD64 ABI。引数はレジスタ（`%rdi` `%rsi` `%rdx` `%rcx` `%r8` `%r9`）で渡し、プロローグ直後に `StoreArg` でスタックへ退避する。戻り値は `%rax`
- **アライメント**：`genBody` が操作スタックの深さを左から右へ追跡し（`stackDelta`）、`call` 直前に 16 バイト境界からずれていれば `subq $8` でパディングする。if/while は文の境界（操作スタックが空）にしか現れないので、分岐の合流点で深さが食い違うことはない。`printf`/`puts`/`exit` の直前は `andq $-16, %rsp` で強制的に揃える
- **main**：他の fn と同じく `compileFnDecl` でコンパイルするが、`ret` の代わりに戻り値を型に応じたフォーマットで `printf` して終了するエピローグで終わる（`epilogue`）。そのため main は呼び出せない
- **シンボル**：pub の fn・static と main だけを `.globl` で公開する（`codegenModule` が `cmPublic` を受け取る）。非公開の fn・static はファイルローカルなシンボルになり、ほかのオブジェクトと同名でも衝突しない。`call` の対象や参照した static が未定義なら外部参照になる。`.L` ラベル（rodata の `.Lfmt64` 等を含む）はファイルローカルで、ユーザーのシンボルと衝突しない
- **static**：`.data` セクションに（pub なら `.globl` と）`.balign`・`.quad`（64bit）／`.long`（32bit）で初期値を置き（`dataSection`。アドレスの定数は `.quad X` / `.quad T+16` と書き、PIE でもロード時の再配置で解決される）、RIP 相対（`name(%rip)`）で読み書きする。32bit の読み出しは `movslq` で、u32 は `loadFixup` でゼロ拡張し直す
- **共通部**：エラー処理（`.Ldiv_zero_error`、`.Lindex_error`：メッセージを `puts` して `exit(1)`。`runtimeErrors`）と rodata（`rodataSection`）はファイルごとに1回出力する

## 7. ドライバ

`src/Driver.hs` の `build :: BuildOpts -> IO (Either String ())` がビルド全体を担う。

- 入力の種類を拡張子で判定し、`-S`/`-c` との組み合わせを検証する（`classifyInputs`）
- import の解決（`resolveImports`）：import を書いたソースのディレクトリからの相対パス。同一ファイルの判定は `canonicalizePath`
- 1ソースのコンパイル（`compileSource`）：**直接** import したソースのシグネチャとグローバル（`buildGlobals` で const を評価したもの）のうち、pub の定義（`publicNames`）だけを集めて `compileModule` に渡す（`importedSigs`。評価はソース全体で行うので、pub の const が非公開の const を参照してもよい。import 先どうしの pub の同名もここで検査する）
- リンクまでの一括処理：入力から import を再帰的にたどって全ソースを集め（`collectSources`、循環可）、pub の fn 名・static 名の重複と main がちょうど1つであることを検査する（`checkLinkUnits`。非公開の名前はファイルローカルなので対象外）
- 中間ファイル：`OUTDIR/basename.s` / `.o`。名前の衝突を事前に検査し（`checkIntermediates`）、`-d` が無ければ成功・失敗を問わず `finally` で削除する
- gcc：`gcc -c X.s -g -o X.o`（アセンブル）、`gcc OBJS... -o OUTPUT`（リンク）を `readProcessWithExitCode` で呼ぶ
- エラーは `ExceptT String IO` で伝播させ、ファイル単位の処理のエラーにはファイル名を前置する（`inFile`）

## 8. テスト用スタック VM

`Compiler.run :: [Instr] -> Either String Int` は中間命令列を直接実行する簡易 VM で、テストと定数式の評価（→ 5.8）で使う。
アドレスは %rbp=0 とみなしたオフセット値で表現し、`trunc`/`zext32`/`divU` で実機の切り詰め・拡張・符号なし除算を再現する。
ジャンプ（`Jmp`/`JmpIfZero`）・関数呼び出し（`ICall`）・引数の退避（`StoreArg`）・static の読み書き（`LoadGlobal` 等）は扱わないため、制御構文や関数・static を含む命令列は結合テストで検証する。

## 9. テスト

フレームワークは Hspec（`test/Spec.hs`）。

| カテゴリ | 内容 |
|---|---|
| tokenize / parse | 字句・構文、演算子の優先順位、エラーケース |
| compile / 意味論エラー | 生成される命令列、型エラー・未宣言変数などのコンパイルエラー |
| run（VM） | 中間命令列の実行結果 |
| codegen | プロローグ/エピローグ、各命令の x86-64 への変換 |
| 結合テスト | gcc でアセンブル・リンクして実行し、標準出力と終了コードを検証する |
| Driver | 一時ディレクトリに複数ソースを書き出し、`build` の一括処理・段階的ビルド・中間ファイルの削除・各種エラーを検証する |

結合テスト・Driver のテストは `withSystemTempDirectory` で一時ディレクトリを管理する。

### CI

GitHub Actions（`.github/workflows/ci.yml`）が、`main` への push・PR・手動実行のたびに `ubuntu-latest` 上でビルドとテストを実行する。

- GHC のバージョンは `cabal.project` の `with-compiler` から読み取る（CI 側には書かない。GHC を上げるときは `cabal.project` だけを変える）。cabal-install は最新版
- 依存パッケージのビルド結果（cabal store）を、GHC のバージョンとビルドプラン（`plan.json`）のハッシュをキーにキャッシュする
- 結合テストはランナーに入っている gcc で実行する
- 別のジョブ（`format`）で Nix を入れ、devShell の ormolu で整形を検査する（`ormolu --mode check`）。ormolu の版は ci.yml に書かず `flake.lock` から決まる

## 10. 依存パッケージ

| パッケージ | 用途 |
|---|---|
| `optparse-applicative` | コマンドライン引数の解析 |
| `process` | gcc の呼び出し（`readProcessWithExitCode`） |
| `directory` / `filepath` | import の解決、中間ファイル、テンポラリディレクトリ |
| `containers` | `Env`・`FnSigs`・`Globals` の `Map` |
| `transformers` | `StateT`（ラベル番号）、`ExceptT`（ドライバのエラー伝播） |
| `hspec` / `temporary` | テスト専用 |

## 11. 開発環境と整形

GHC・cabal・HLS は ghcup で管理する（GHC の版は `cabal.project` の `with-compiler`）。
コードの整形は ormolu で行い、その版は `flake.nix` の devShell（`ormolu` だけを含む）と `flake.lock` で固定する。

| 使う場面 | 整形の仕組み |
|---|---|
| エディタ（nvim・VS Code） | HLS の設定 `haskell.plugin.ormolu.config.external = true` で、HLS 内蔵ではなく PATH の `ormolu` を使わせ、保存時に整形する（設定は各自のエディタ側で行う） |
| Claude Code | `.claude/settings.json` の PostToolUse hook（`Write`・`Edit`）が `.claude/hooks/ormolu.sh` を実行し、書いた `.hs` を整形する。ormolu は `direnv exec` で devShell から取る。見つからなければ利用者にエラーを示し、構文エラーで整形できなければ内容を Claude に返す |
| CI | `format` ジョブで `nix develop --command ormolu --mode check`（9. テストの CI の節） |

- 初回は、プロジェクトで `direnv allow` を実行する（`.envrc` の `use flake` により、nix-direnv が devShell の PATH を載せる）。VS Code は direnv 拡張（`mkhl.direnv`）で PATH を受け取る
- 全ファイルの整形：`ormolu --mode inplace $(git ls-files '*.hs')`
- 整形だけのコミットは `.git-blame-ignore-revs` に登録し、`git blame` が飛ばすようにする。GitHub の Blame 画面はこのファイルを自動で読むが、ローカルの git は読まないため、clone ごとに一度 `git config blame.ignoreRevsFile ':(optional).git-blame-ignore-revs'` を設定する。`:(optional)` を付けないと、このファイルが無いコミット（step027 より前）で `git blame` がエラーになる

### ormolu の版を変えるとき

`flake.nix` / `flake.lock` だけを変える（hook・エディタの設定・ci.yml は変えない）。

| 変えたい版 | 方法 |
|---|---|
| nixpkgs の最新の版 | `nix flake update` |
| nixpkgs にある別の版（`haskellPackages.ormolu_0_7_4_0` など） | `flake.nix` で `pkgs.ormolu` を差し替える |
| nixpkgs に無い版 | その版を含む nixpkgs のリビジョンを別の入力として固定する、または `haskellPackages.callHackage` で Hackage からビルドする |

1. `flake.nix` / `flake.lock` の更新をコミットする
2. 全 `.hs` を整形し直し、整形だけのコミットを作る
3. 2 のハッシュを `.git-blame-ignore-revs` に追記する
4. 起動中のエディタは古い PATH を持ち続けるため、再起動する

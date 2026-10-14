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
- ポインタ型（`&`/`*`）の設計: `docs/step012.md`
- 数値リテラルの型サフィックス（i32/i64）の設計: `docs/step013.md`
- 変数宣言の型注釈省略と型推論の設計: `docs/step014.md`
- 関数の仮引数・戻り値型注釈省略と型推論の設計: `docs/step015.md`（step023で撤回）
- 整数型（i32/i64）の暗黙の型変換の設計: `docs/step016.md`（step017で撤回）
- 暗黙の型変換の撤回とキャスト（`as i32`/`as i64`）の設計: `docs/step017.md`
- 符号なし整数型（u32/u64）の設計: `docs/step018.md`
- 配列型（`[T; N]`）とusize型の設計: `docs/step019.md`
- 配列へのポインタ（`&[T; N]`）と関数での受け渡しの設計: `docs/step020.md`
- ユニット型（`()`）と式文の設計: `docs/step021.md`
- 暗黙のmainの廃止と`fn main()`の明示的な定義の設計: `docs/step022.md`
- 関数シグネチャの型注釈の必須化（step015の撤回）: `docs/step023.md`
- 複数ソースファイルからのビルド（`import`・`-S`/`-c`/リンク）: `docs/step024.md`
- 将来の課題（バックログ）: `docs/backlog.md`（stepを追加するたびに更新する）

## Build & Run

```bash
cabal build                                          # ビルド
cabal test                                           # テスト (Hspec)
cabal run simplang-haskell -- FILES... -o OUTPUT [-S|-c] [-d OUTDIR]
```

- `FILES...`: 入力ファイル（`.sl`/`.s`/`.o` のいずれか1種類。拡張子で判定し、混在はエラー）
- `-o OUTPUT`: 出力ファイル（必須）
- `-S`: コンパイルのみ（.sl ×1 → .s）。`-c`: アセンブルまで（.sl/.s ×1 → .o）。どちらも無ければリンクまで行う
- `-d OUTDIR`: 中間ファイル（`OUTDIR/basename.s`/`.o`）の出力先。省略時はシステムのテンポラリディレクトリに作って終了時に削除し、指定時は残す

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
| Driver | 一時ディレクトリに複数ソースを書き出し `build` で一括処理・段階的ビルド・中間ファイルの削除・各種エラーを検証 |

Integration テストは `withSystemTempDirectory` で一時ディレクトリを管理する。

## Key Design Notes

- **Parser**: 左再帰を除去した反復ループで左結合を実現（`parseExpr` → ... → `parseCast` → `parseFactor`）。キャスト`as`はRustと同じく単項演算子より弱く`*`/`/`より強い左結合の後置演算子で、キャスト先は整数型（`i32`/`i64`/`u32`/`u64`/`usize`）のみ（それ以外はパース時エラー）。添字`a[i]`は単項演算子より強い後置演算子（`parseFactor`＝単項 → `parsePostfixRest` → `parsePrimary`）。`let`/代入文は `parseStmts` で先読み2トークンにより判別し、キーワードにも`IDENT =`にも該当しなければ式を試し読みして次が`=`なら場所式への代入`SAssignPlace`（`a[i] = v;`/`p[i] = v;`/`(*p)[i] = v;`/`*p = v;`）、次が`;`なら式文`SExpr`、そうでなければ末尾式として読み直す。トップレベルは`import "パス";`の列と`fn`定義の列のみ（`Program [FilePath] [FnDecl]`。importはfn定義より前。`fn main()`はリンクする全ソースでちょうど1つ必須）。fn本体の末尾式は省略可能で、省略時と`return;`はパーサで`UnitLit`（`()`）へ脱糖する（AST上は末尾式・return値は常に存在する）
- **ユニット型**: `TUnit`（値は`UnitLit`のみ）はboolと同じく32bitスロットに常に0を格納する通常の値で、変数・ポインタ・配列・仮引数・戻り値に特別扱いなしで使える。`==`/`!=`のみ可。式文`SExpr`は`inferType`で型を決めてコンパイルし`IPop`（`addq $8, %rsp`）で値を捨てる（型は問わない）
- **型システム**: `Width`（`W32`/`W64`）が整数のビット幅とcodegenのレジスタ幅を兼用する型で、`Signedness`（`Signed`/`Unsigned`）と組み合わせて`TyInt Signedness Width`が`i32`/`i64`/`u32`/`u64`を表す。`Type`（`TyInt Signedness Width | TUsize | TBool | TUnit | TPtr Type | TArray Type Int`）はそれをさらにusize・bool・ポインタ（再帰的、`&`で任意段数ネスト可能）・配列まで拡張したソースレベルの論理型で、ポインタは指す先の型によらず常に`W64`（8バイト）で格納される。`TUsize`は配列の添字用で、`intRepr`により`(Unsigned, W64)`へ分解されるためコード生成はu64と同一だが、型としてはu64と区別される（混在はエラー）。整数型の判定・命令選択は`TyInt`への直接マッチではなく`intRepr`を経由する。`compileExprTyped` が期待型を式木に一様伝播させ、`Var` の実際の型と食い違えば `type mismatch` エラー。同一式内での整数型（符号の有無・幅）の混在は必ずこのチェックで検出される（式ノードへの型タグ付けやボトムアップ型推論は不要。唯一の例外が`9999i64`/`5usize`のような型サフィックス付き整数リテラル`LitTyped Int Type`で、`Var`と同様に常に確定型を持つリーフとして`expected`との一致を厳密にチェックする）。`&式`（アドレス取得）の対象は構文的には任意の式を受け付けるが、`addressOf` ヘルパーが変数参照とポインタのデリファレンス（lvalue）のみに意味論的に制限する。キャスト`Cast Expr Type`（`x as i64`/`x as usize`など）が唯一の明示的な整数型の変換（Rustと同じビットの再解釈）で、結果型`TyInt w`は`expected`と厳密一致を要求し、被演算子は`expected`とは独立に自身の推論型（型なしリテラルのみならi64）でコンパイルする。被演算子は整数型のみで、`castInstrs`が32bitへの変換はキャスト先の符号で、32→64bitの拡大はキャスト元の符号で正規化し（符号ありは`ISext32`＝`cltq`、符号なしは`IZext32`＝`movl %eax,%eax`）、64bit同士は命令なしとする。`let`の型注釈は省略可能（`SLetInferred`）で、その場合は式文の型決定にも使われている `inferType`（＝`inferMaybeType`が`Nothing`なら`i64`をデフォルトとする）で型を決定してから`SLet`と同じ処理に合流する
- **fnシグネチャ**: `FnDecl String [(String, Type)] Type Body`。シグネチャは宣言だけで確定する（複数ソースからのビルドの前提）。仮引数の型注釈は必須、`->`を省略した戻り値型はRustと同じく`()`（推論はしない。step015の推論はstep023で撤回）。`compileModule`の1パス目`buildFnSigs`が重複定義・最大6引数・`main`の引数なしの構造検証（`validateFnShapes`）と、シグネチャ中の配列型の禁止を行った上で、各`FnDecl`の型をそのまま`FnSigs`に積み、importしたソースのシグネチャとマージする（同名はエラー）。宣言順には依存しない
- **変数の環境管理**: `Env`（変数名→`%rbp`相対オフセットと型）はスコープのスタック `[Map String (Int, Type)]`（先頭が最内側）で、各fnの本体が仮引数だけのスコープから開始し左から右への単一パスで引き回す。スタックスロットは型の実サイズ（i32=4バイト、i64=8バイト）でタイトパッキング。`{}` ブロックは文としてスコープを1段push/popし、ブロックを抜けると内部で宣言した変数は不可視になる（外側と同名の再宣言＝シャドーイングは許可、同一ブロック内の再宣言は引き続きコンパイルエラー）。未宣言変数の参照もコンパイルエラー。スタックオフセットもブロックの出入りで巻き戻され、時間的に重ならない兄弟ブロックは同じオフセット範囲を再利用する
- **符号なし整数**: スタック上の不変条件は「i32は符号拡張済み、u32はゼロ拡張済み、i64/u64はそのまま」。加減乗は下位ビットが符号によらず同じなので既存命令を共用し、u32のみ直後に`IZext32`を付ける（`Load W32`/`LoadInd W32`の後も同様）。除算は`IDivU`（`divl`/`divq`）、大小比較は`ICmpLtU`等（`setb`/`setbe`/`seta`/`setae`）を使う。範囲外はラップアラウンドし、符号なしへの単項マイナスはコンパイルエラー
- **配列**: `[T; N]`（要素型はスカラー型とそのポインタ型（配列へのポインタを含む）。ネスト不可）の実体はローカル変数のみで、`let`の初期化（`[e1, ...]`/`[e; N]`、`compileArrayInit`が要素ごとに静的オフセットへ`Store`。`[e; N]`のeは1回だけ評価してコピー）、`a[i]`、`a[i] = v;`、`&a[i]`、`&a`（`&[T; N]`）が可能。配列の値としての使用（`*p`を含む）・全体代入・fnシグネチャでの`[T; N]`はエラーで、関数とは`&[T; N]`で受け渡す（戻り値もポインタのみ）。添字の基底は`arrayBase`が解決し、配列変数なら`LoadAddr`、`&[T; N]`型の式（`p[i]`の自動デリファレンス、`(*p)[i]`）ならその値を先頭アドレスとする（Nは型から得られるためポインタ経由でも範囲検査可）。代入の左辺（場所式）と`&`の対象は`addressOf`（Var/Deref/Index）で一本化し、`StoreInd`で書き込む。fnのreturn式・末尾式が直接`&ローカル変数`/`&ローカル配列[i]`ならダングリングとしてエラー（`checkNoLocalRef`。中間変数経由は検出しない簡易検査）。先頭（最下位アドレス）を`off = cursor - stackBytes ty`とし、要素iは`off + i*elemSize`。要素アドレスは`indexAddr`が`LoadAddr off`＋添字（期待型usize）＋`IBoundsCheck N`＋`Push size; IMul W64; IAdd W64`で計算し、読み出しは`LoadInd`、代入は`StoreInd`。`IBoundsCheck`は符号なし比較で範囲外なら`.Lindex_error`（"index out of bounds"でexit 1）へ飛ぶ。リテラル添字はコンパイル時にも検査する
- **コード生成**: スタック操作に `%rax` / `%rbx` / `%rcx` を使用。i32演算は32bit命令（`addl`/`subl`/`imull`/`idivl`/`negl`）の後に符号拡張（`cltq`/`movslq`）して64bitスタックへ戻し、演算のたびにラップアラウンドする。`Push` の即値は `movabsq` 経由（`pushq` の符号拡張32bit即値制限を回避）。`divq`/`idivl` 実行前に `cmpq $0` でゼロ除算チェック、エラー時は `.Ldiv_zero_error` ラベルへジャンプ。除数が-1の場合は `cmove` で「被除数を符号反転・除数を1」に置き換えてから割る（最小値/-1 で `idiv` が例外を起こすのを避け、最小値へラップアラウンドさせる）
- **スタックフレーム**: `codegen` 側で命令列中の `Load`/`Store` オフセットの最大絶対値をスキャンしてフレームサイズを決定し、プロローグで一度だけ `subq`。エピローグは `leave`（`movq %rbp,%rsp; popq %rbp`）で確保量によらず正しく巻き戻す
- **アライメント**: `call printf` / `call exit` 直前に `andq $-16, %rsp` を挿入し16バイト境界を強制
- **アセンブル・リンク**: `Driver`が`readProcessWithExitCode "gcc" [...]`で外部プロセス呼び出し（`gcc -c X.s -g -o X.o`、`gcc OBJS... -o OUTPUT`）
- **複数ソース**: ソースの先頭に`import "相対パス";`を書くと、そのソースに定義されたfnを呼べる（**直接**importしたソースのみ。推移的には取り込まない。import先のmainは除く）。`Program { progImports, progFns }`。`compileModule importedSigs fnDecls`が1ソース分のコンパイル単位でmainは任意（`compile`はmain必須の単一ソース用ラッパ）。`codegenModule`は各fnを`.globl`で公開する（`call`は未定義シンボルを外部参照として扱うので変更なし。`.L`ラベル・rodataラベルはファイルローカル）。ドライバ`src/Driver.hs`の`build`が入力種別の判定・importの解決（importを書いたソースからの相対パス、`canonicalizePath`で同一判定、循環可）・一括処理時のimportの再帰収集・fn名重複とmainがちょうど1つの検査・中間ファイル名の衝突検査・gcc呼び出し・中間ファイルの削除（`finally`）を担う
- **main**: mainも他のfnと同じく`compileFnDecl`でコンパイルし、`compile`は`(main以外のfn, mainの戻り値型, mainの命令列)`を返す。mainの命令列は通常の`ret`の代わりに戻り値を表示して終了するepilogueで終わるため、mainの呼び出しは不可（`cannot call main`）、引数も不可。戻り値型は他のfnと同じ規則（値を表示するなら`fn main() -> i64`のように明示し、省略時は`()`で何も出力しない）で、`return`も使える。mainはダングリング検査（`checkNoLocalRef`）の対象外
- **出力**: コンパイルされたバイナリは `printf` でmainの戻り値を標準出力へ表示（戻り値の型が `i64` なら `%ld`、`i32` なら `%d`、`u64`/`usize` なら `%lu`、`u32` なら `%u`、ポインタ型なら `%p`、`()` なら何も出力しない）

## Dependencies

- `optparse-applicative` — CLI 引数パース
- `process` — `readProcessWithExitCode "gcc"`
- `directory` / `filepath` — ファイルパス操作（import の解決、中間ファイル、テンポラリディレクトリ）
- `containers` — 変数環境の `[Map String (Int, Type)]`（スコープのスタック）管理
- `hspec` / `temporary` — テスト専用

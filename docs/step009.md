# 関数定義（`fn`）の導入における設計上の考慮点

`docs/step008.md` までは暗黙の `main` 本体（文の列＋末尾式）1本だけを扱ってきたが、今回はユーザーが複数の関数を定義し、相互に呼び出せるようにする。単なる新しい式・文の追加だった `step003`〜`step008` と異なり、コンパイル対象そのものが「暗黙mainの文の列＋式」から「`fn`定義の列＋暗黙mainの文の列＋式」という2階層の構造に変わるため、`Program`型・`compile`のシグネチャ・`Env`のスコープ規則（外側スコープを持たない独立コンパイル）・呼び出し規約（SysV AMD64のレジスタ渡し）・スタックアライメントの静的判定など、これまでで最も広い範囲に影響が及んだ。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 関数定義の位置 | トップレベルのみ。`fn`定義はネストしたブロック/if/while本体には出現できない |
| 仮引数 | `IDENT ':' 型` のカンマ区切り。最大6個（SysV AMD64の整数引数レジスタ数に合わせ、スタック経由の引数渡しは今回のスコープ外） |
| 戻り値 | 型を必須で明示する（`-> 型`）。**ユニット型は採用しない**ため、関数本体もプログラム全体の暗黙mainと同じく末尾式が必須（値を返さない関数は書けない） |
| 関数本体のスコープ | 完全に独立。呼び出し元やトップレベルの変数を一切参照できない（レキシカルスコープではなく、パラメータのみを起点とする独立スコープ） |
| 宣言順 | 依存しない。`fn`の**シグネチャ**（引数の型・戻り値の型）だけを本体コンパイル前の1パス目で一括収集するため、宣言順によらない相互再帰が可能 |
| `return` | 文として導入。値は必須（ユニット型を採用しないため）。早期リターンに使う。関数の外（暗黙main）で使うとコンパイルエラー |
| 末尾式とreturnの合流 | 早期`return`も末尾式のフォールスルーも、同じ関数末尾ラベルへの `Jmp`／自然な到達で合流する |
| 関数名の予約 | `main` という関数名は暗黙mainと衝突するため予約済みとしエラーにする。同名関数の再定義もエラー |
| 呼び出し規約 | SysV AMD64。整数引数は最大6個までレジスタ渡し（`%rdi`/`%rsi`/`%rdx`/`%rcx`/`%r8`/`%r9`）、戻り値は`%rax` |
| 引数の型検査 | 宣言された仮引数の型に対して実引数を厳密検査する。暗黙の型変換は行わない（`to_i64`/`to_i32`と同じ立場） |
| 式中の呼び出し | 呼び出し式は算術式・比較式の被演算子として自由にネストできる（`f(g(x)) + 1` 等） |

## 1. 文法・構文（Lexer / Parser）

- **トークン追加**: `TFn`（`fn`）、`TReturn`（`return`）、`TArrow`（`->`）、`TComma`（`,`）。`->` は `==`/`!=`/`<=`/`>=` と同じく「マルチ文字パターンを単体ガードより先に置く」方式で、`-`単体（`TMinus`）より先に判別する
- **文法**:
  ```
  program    ::= (fn-decl | stmt)* expr
  fn-decl    ::= 'fn' IDENT '(' (IDENT ':' 型 (',' IDENT ':' 型)*)? ')' '->' 型 '{' stmt* expr '}'
  stmt       ::= let-stmt | assign-stmt | block-stmt | if-stmt | while-stmt
               | break-stmt | continue-stmt | return-stmt
  return-stmt ::= 'return' expr ';'
  factor     ::= INT | 'true' | 'false' | IDENT | IDENT '(' (expr (',' expr)*)? ')'
               | '(' expr ')' | '-' factor | '!' factor | 'to_i64' factor | 'to_i32' factor
  ```
- **AST追加**:
  ```haskell
  data Expr = ... | Call String [Expr]
  data Stmt = ... | SReturn Expr
  type Body = ([Stmt], Expr)                        -- 文の列＋必須の末尾式（fn本体・暗黙mainで共通の形）
  data FnDecl = FnDecl String [(String, Type)] Type Body
  type Program = ([FnDecl], [Stmt], Expr)            -- fn定義の列＋既存の暗黙main本体
  ```
  `Body`は既存の「文の列＋末尾式」という形をfn本体と暗黙mainの両方で再利用するために独立した型として括り出した
- **`parseTopLevel`**: `parse`のエントリポイントに新設。先頭が`TFn`ならその1つを`parseFnDecl`で読んでから残りを再帰し、そうでなければ`parseStmts`で文の連続を一気に読み、止まった位置が`TFn`ならさらに読み進める。ネストしたブロック/if/while本体は従来通り`parseStmts`だけを使うため、`fn`定義がトップレベル以外に出現することはパーサレベルで構造的に排除される
- **`parseFnDecl`/`parseParamList`/`parseParam`/`parseParamListRest`**: 仮引数リストのパースは、実引数リストのパース（`parseArgList`/`parseArgListRest`、後述）と対になる新規関数群
- **`parseFactor`の呼び出し式分岐**: `TIdent name : TLParen : rest` を`TIdent name : rest`（変数参照）より先に置くガードで判別し、`Call name args`を生成する
- **`parseReturnStmt`**: `parseBreakStmt`/`parseContinueStmt`と異なり式を1つ伴う点で`parseAssignStmt`に近い形（`expr`を読んでから`;`を要求）

## 2. 意味論：型検査とスコープ

### 2.1 関数シグネチャの二パスコンパイル

```haskell
type FnSigs = Map String ([Type], Type)  -- 名前 -> (仮引数の型リスト, 戻り値の型)

buildFnSigs :: [FnDecl] -> Either String FnSigs
```

全`fn`定義から本体をコンパイルする前に`FnSigs`を一括構築する（1パス目）。名前の重複・`"main"`という予約名の使用・引数個数が6個を超える宣言はここで検出する。`FnSigs`はその後の本体コンパイル（2パス目）を通じて不変のグローバルな読み取り専用テーブルとして`inferMaybeType`/`compileExprTyped`/`compileStmtsFrom`など既存の型検査・コード生成関数すべてに引数として追加された。この分離により、`fibonacci`が自分自身を呼ぶ自己再帰はもちろん、`A`が宣言順で後に来る`B`を呼ぶような相互再帰も、`B`の本体を実際にコンパイルする前に`B`のシグネチャさえ`FnSigs`にあれば型検査・コード生成ができる

### 2.2 呼び出し式の型検査

- `inferMaybeType`に`Call name args`のケースを追加。未定義関数名や引数個数の不一致はここで検出し、成功時は常に`Just retTy`を返す（算術演算のように部分木を辿って型を単一化する必要はなく、シグネチャの戻り値型で確定するため）
- `compileExprTyped`に`Call`のケースを追加。`expected`とシグネチャの`retTy`が食い違えば型不一致エラー、引数個数が食い違えばエラー、そうでなければ各実引数をシグネチャの仮引数の型で（`expected`とは独立に）厳密検査し、`zipWithM`で命令列を連結したあとに`ICall name (length args)`を追加する

### 2.3 関数本体の独立スコープ

`compileFnDecl`は本体コンパイルを`env0 = [paramEnv]`（パラメータのみを含む1段のスコープ）から開始する。既存の`Env`は「スコープのスタック、先頭が最内側」という構造だったが、`fn`本体は暗黙mainや他の関数のスコープを一切引き継がず、常に新しい1段構成のスタックから始まる。これにより「関数は自分のパラメータとローカル変数以外を参照できない」という独立スコープ制約が、`Env`の型を変えることなく単純な初期値の選び方だけで実現される

### 2.4 `return`のコンテキスト（`ReturnCtx`）

```haskell
type ReturnCtx = Maybe (Type, String)  -- (戻り値の型, 関数末尾ラベル)
```

`docs/step007.md`で導入した`LoopCtx`（`break`/`continue`の直近の外側ループ解決）と同じ理由・同じ形で、普通の関数引数として`compileStmtsFrom`/`compileIf`/`compileWhile`に追加した。`SBlock`/`SIf`/`SWhile`の再帰呼び出しは`returnCtx`をそのまま素通しする（ループやifにネストしても外側関数の`return`が解決できるようにするため）一方、`compileFnDecl`が関数本体に入るときだけ`Just (retTy, endLabel)`に差し替える。暗黙mainのコンパイルは`returnCtx = Nothing`で開始するため、`return`をmain側で使うと`step SReturn`が`Left "return used outside function"`を返す

`LoopCtx`との違いは、`LoopCtx`はループに入るたびに差し替わるのに対し、`ReturnCtx`は関数本体に入るときの1回だけ差し替わり、その中にネストした`while`をいくつ挟んでも（`compileWhile`が`returnCtx`を素通しするため）値が変わらない点。「直近の外側ループ」と「直近の外側関数」で解決対象のネスト単位が異なることが、素通しする箇所としない箇所の違いに素直に対応している

### 2.5 `return`と末尾式の合流

`compileFnDecl`は関数ごとに`fn_end`ラベルを1つ採番する。`SReturn`は式を評価した命令列の後に`Jmp endLabel`を追加し、本体の文の列をコンパイルし終えたあとに末尾式`tailExpr`の命令列を続け、最後に`Label endLabel`を置く。早期`return`はジャンプでこのラベルに直接合流し、`return`を通らなかった場合は末尾式が自然に評価されてラベルに到達する。合流後の関数末尾は共通で「操作スタックの唯一の残り値を`%rax`へ`popq`して`leave; ret`」（`CodeGen.epilogueRet`）だけでよい

## 3. コード生成

### 3.1 `Instr`の追加

```haskell
| StoreArg Int Width Int  -- 第N引数レジスタの値をオフセットへストア（プロローグ直後のスピル専用）
| ICall String Int        -- 関数呼び出し: 操作スタックのN個の引数をレジスタへpopしてcall、戻り値を%raxからpush
```

### 3.2 関数プロローグでの引数スピル

`allocParams`が仮引数を`let`と同じ規則（宣言順に、型の実サイズでタイトパッキング）でスタックスロットに割り付ける。`compileFnDecl`はその割り付け結果から`StoreArg i (storageWidth ty) off`の列を生成し、本体の命令列の先頭に置く。`CodeGen.genInstr (StoreArg i w off)`は`i`番目の引数レジスタ（`Width`に応じて64bit/32bit版のレジスタ名を`argReg64`/`argReg32`で選択）の値をそのオフセットへストアする。パラメータを一旦ローカル変数と同じスタックスロットへスピルすることで、パラメータもボディ中では既存の`Var`/`Load`/`Store`の仕組みだけで扱える

### 3.3 複数関数のアセンブリ構造

```haskell
codegen :: [(String, [Instr])] -> Type -> [Instr] -> String
```

`.section .text`/`.globl main`をファイル全体で一度だけ出力し、各ユーザー関数のラベル・本体・`epilogueRet`を`main:`より前に並べ、最後に`main:`と暗黙mainの本体・型に応じた`epilogue`を出力する。ゼロ除算エラー処理と`.rodata`（`commonTail`）は全関数で共通のため、以前は各`epilogue`が末尾に含めていたものをファイル全体で一度だけ出力するよう切り出した。GASのラベル解決は出現順に依存しないため、関数の並び順自体に意味はない

### 3.4 呼び出し規約とスタックアライメント（`genBody`/`genCall`）

呼び出しは`popq`でN個の引数を`%rdi`〜`%r9`（`argReg64`、最大6個）へ取り出してから`call`し、戻り値`%rax`を`pushq`で操作スタックへ戻す。ここで問題になるのが**`call`直前の`%rsp`の16バイトアライメント**（SysV AMD64 ABIの要求）で、`if`/`while`と異なり呼び出し式は算術式・比較式の途中（操作スタックの深さが不定な地点）にも自由に出現しうるため、静的にアライメントを判定する仕組みが必要になった。

- `frameSize`をローカル変数用の必要量そのものではなく**16バイト境界に切り上げた値**にすることで、「プロローグ直後の`subq`完了時点」を「操作スタックの深さ0＝16バイト境界」という既知の基準点にできる
- `genBody`は`Data.List.mapAccumL`で命令列を左から右へなめながら、各命令の`stackDelta`（8バイトスロット単位の正味の増減。例: `Push`は+1、`IAdd`は-1）を積算して「その命令の直前の操作スタックの深さ」を追跡する
- `ICall name n`に到達した時点の深さ`depthAtCall`から`n`（レジスタへpopする引数の個数）を引いた`d = depthAtCall - n`が、実際に`call`を発行する瞬間の深さ。`d`が奇数なら`%rsp`は16バイト境界から8バイトずれているため、`call`の前後に`subq $8, %rsp`/`addq $8, %rsp`で8バイトのパディングを挟んで補正する（`genCall`）
- この判定が成立するのは、本プロジェクトの`if`/`while`が値を返さない文であり式の中に現れない——つまり分岐の合流点で必ず操作スタックの深さが一致する——という既存の設計（`docs/step006.md`/`docs/step007.md`）のおかげであり、単純な線形走査だけで静的に決定できる
- `genInstr`から`ICall`のケースは除去し、深さコンテキストを持つ`genBody`経由でのみ生成できるよう`error`で塞いだ（`ICall`だけは他の命令と異なり周辺の文脈が無いと正しいコードを生成できないため、誤って`genInstr`単体から呼ばれることを型ではなく実行時エラーで検出する）

## 4. VM（`run`）・`app/Main.hs` への影響

- `run`（Hspec専用の素朴なスタックマシン）: 変更なし。`StoreArg`/`ICall`は未対応のままであり、複数関数を含むプログラムは`while`/`if`と同様に常に`compile` → `codegen` → `gcc`の実行系パイプラインでテストする
- `app/Main.hs`: `compile`の戻り値が`(Type, [Instr])`から`([(String, [Instr])], Type, [Instr])`に変わったのに合わせ、`codegen finalType instrs`の呼び出しを`codegen fns finalType instrs`に変更

## 5. `docs/step008.md` からの変更点まとめ

| ファイル / 項目 | step008まで | step009での変更 |
|---|---|---|
| `Token` | `TEq`、`TLt`等 | `TFn`、`TReturn`、`TArrow`、`TComma`を追加 |
| `tokenize` | — | `fn`/`return`（識別子分岐）、`->`（`-`単体より先に判別）、`,`を追加 |
| `Expr` | `Lt`、`Ge`等 | `Call String [Expr]`を追加 |
| `Stmt` | `SBreak`、`SContinue`等 | `SReturn Expr`を追加 |
| 型 | `Program = ([Stmt], Expr)` | `Body = ([Stmt], Expr)`、`FnDecl = FnDecl String [(String, Type)] Type Body`、`Program = ([FnDecl], [Stmt], Expr)`に変更 |
| 文法 | `program ::= stmt* expr` | `program ::= (fn-decl \| stmt)* expr`、`fn-decl`/`return-stmt`を追加、`factor`に呼び出し式を追加 |
| 新規パース関数 | — | `parseTopLevel`、`parseFnDecl`、`parseParamList`、`parseParam`、`parseParamListRest`、`parseReturnStmt`、`parseArgList`、`parseArgListRest` |
| 型検査 | `inferMaybeType`/`compileExprTyped`は`Env`のみ引数 | 両方に`FnSigs`引数を追加。`Call`のケースを追加 |
| コンパイル本体 | `compileStmtsFrom :: Env -> Int -> LoopCtx -> ...` | `FnSigs`と`ReturnCtx`引数を追加。`SReturn`のケースを追加 |
| 新規コンパイル関数 | — | `buildFnSigs`、`compileProgram`、`allocParams`、`compileFnDecl` |
| `compile`の型 | `Program -> Either String (Type, [Instr])` | `Program -> Either String ([(String, [Instr])], Type, [Instr])` |
| `Instr` | `ICmpLt`等 | `StoreArg Int Width Int`、`ICall String Int`を追加 |
| `CodeGen.codegen`の型 | `Type -> [Instr] -> String` | `[(String, [Instr])] -> Type -> [Instr] -> String` |
| `CodeGen`の構造 | `prologue`/`epilogue`が`main`専用に一体化 | `prologueBody`/`epilogueRet`（関数共通）と`epilogue`（暗黙main専用）に分離。`commonTail`はファイル全体で一度だけ出力 |
| `CodeGen`の新規関数 | — | `genBody`（`mapAccumL`で操作スタック深さを追跡）、`stackDelta`、`genCall`、`argReg64`、`argReg32` |
| `frameSize` | 必要量そのまま | 16バイト境界へ切り上げ（`call`前後のアライメント判定の基準点として必要） |
| `run`（VM） | — | 変更なし（`StoreArg`/`ICall`は未対応のまま） |
| `app/Main.hs` | `codegen finalType instrs` | `codegen fns finalType instrs` |
| 依存パッケージ | — | 変更なし |

## 6. テストへの影響

- **Tokenizer**: `fn`/`return`/`->`/`,`の字句化、`->`が2文字トークンとして`-`+`>`に分割されないこと、`-`単体が引き続き`TMinus`になることの回帰確認
- **Parser**: 引数0〜複数個のfn定義、型混在パラメータ、fn本体の文の列＋末尾式、fn定義の複数並び、fn定義と暗黙main本体の文の自由な混在、末尾式が無いfn本体のエラー、`->`が無い場合のエラー、引数0〜複数個の呼び出し式、算術式中・入れ子での呼び出し式、値付きreturn文、値の無いreturn文のエラー、`;`が無いreturn文のエラー
- **CodeGen**: 変数があるときのスタックフレーム確保が16バイト境界へ切り上げられること
- **意味論エラー（compile）**: 未定義関数の呼び出し、引数個数の過不足、引数の型不一致（暗黙変換なし）、戻り値の型不一致、`main`という関数名の予約、同名関数の再定義、7個超のパラメータを持つ関数定義、関数外での`return`、宣言順に依存しない相互再帰が成功すること（二パスコンパイルの確認）
- **結合テスト（gcc実行）**: 単純な呼び出し、i32引数、bool戻り値、早期return、自己再帰（階乗）、宣言順に依存しない相互再帰、6個の引数（レジスタ渡しの上限）、呼び出しの入れ子（他の呼び出しの引数として）、式の途中（奇数深さ）でのユーザー関数呼び出しでもスタックアライメントが崩れないこと、fn定義が暗黙main本体の文と自由に混在した状態での実行結果確認（`sample/src009.sl`のフィボナッチ相当）

## 7. 今回のスコープ外とした将来の検討課題

- スタック経由の引数渡し（7個以上のパラメータを持つ関数）
- ユニット型（値を返さない関数）の導入
- クロージャ・関数を値として扱う機能（関数ポインタ、高階関数）
- `for`文、値を返す`while`式（`docs/step007.md`から引き続き据え置き）
- `run`（VM）の`StoreArg`/`ICall`対応。VM経由で複数関数プログラムをテストする必要が生じた場合に別途対応する

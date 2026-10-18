# 定数（const）とグローバル変数（static）における設計上の考慮点

トップレベルには `import` と `fn` 定義しか書けず、関数をまたいで値を共有するには引数で受け渡すしかなかった（backlog「グローバル変数・定数」、出典 step022）。今回、Rust と同じ **`const`**（コンパイル時定数）と **`static`**（静的領域に置くグローバル変数）を導入する。

```
const N: usize = 5;
const LIMIT: i64 = N as i64 * 10 + 1;
static COUNT: i64 = 0;
static TABLE: [i64; 5] = [1, 2, 3, 4, 5];

fn bump() {
    COUNT = COUNT + 1;
}

fn main() -> i64 {
    let i = 0usize;
    while i < N {
        TABLE[i] = TABLE[i] * 2;
        bump();
        i = i + 1;
    }
    TABLE[4] + COUNT + LIMIT     // 10 + 5 + 51
}
```

```
$ simplang-haskell main.sl -o prog && ./prog
66
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 構文 | `const IDENT ':' type '=' expr ';'` / `static IDENT ':' type '=' expr ';'`。型注釈は必須 |
| 書ける場所 | トップレベルのみ。import より後なら fn と混在してよく、宣言順に意味は無い（後で宣言した const も参照できる） |
| const | メモリを持たず、使う箇所に値を埋め込む。代入・アドレス取得（`&N`）はできない |
| static | `.data` セクションに置く。**常に書き換え可能**（`static mut` との区別は無い）。アドレスを取れる |
| 型 | const：整数・bool・`()`。static：整数・bool・`()` と、それらを要素とする配列。ポインタ型はどちらも不可 |
| 初期値 | 定数式（1.3）。コンパイル時に評価する |
| 名前空間 | fn・const・static は同じ名前空間を共有する。同名はエラー |
| 覆い隠し | let・仮引数で const/static と同名の変数を宣言するとエラー（Rust と同じ） |
| import | fn と同じく、**直接** import したソースの const/static を取り込む |
| ランタイムのラベル | `fmt32`・`errmsg` などの rodata のラベルを `.Lfmt32`・`.Lerrmsg` に改名し、ユーザーのシンボルと衝突しないようにする |

## 1. 考慮点

### 1.1 static を1種類にする（`static mut` を設けない）

Rust では `static` は不変で、書き換えるには `static mut`（と `unsafe`）が必要になる。不変の `static` は書き込まれないことが保証されるため、処理系は読み取り専用の `.rodata` に置ける。

simplang に同じ区別を入れようとすると、ポインタの可変性の区別（`&` と `&mut`）が無いことが問題になる。次のコードは型検査を通ってしまう。

```
static X: i64 = 1;
fn main() -> i64 {
    let p = &X;
    *p = 5;
    X
}
```

| 不変の static の扱い | 結果 |
|---|---|
| `.rodata` に置く | 実行時に保護違反（segfault）になる。「誤りはできるだけコンパイル時に検出する」に反する |
| `.data` に置く | 黙って書き換わり、「不変」が名ばかりになる |
| `&` を禁止する | 安全だが、static の配列を `&[T; N]` で関数に渡せなくなる |

いずれも中途半端であり、区別をきちんと守るには `&mut` の導入が必要になる。そこで今回は `static` を1種類だけにし、`let` に `mut` が無いのと揃えて常に書き換え可能とした。書き換えたくない値は `const` で表す。const はメモリを持たないので、上の問題は起きない。

### 1.2 const はメモリを持たない

const の参照は、評価済みの値を積む `Push v` にコンパイルする。そのため次のようになる。

- 代入はエラー（`cannot assign to constant: N`）
- `&N` はエラー（`cannot take address of constant: N`）。Rust は一時領域を作って許すが、ローカルの一時領域を返すとダングリングになる問題もあり、今回は禁止する
- const の型は、`Push` 1つで表せるスカラー（整数・bool・`()`）に限る。const の配列は、添字アクセスのためにメモリが必要になるので見送る（static の配列で代替できる）

### 1.3 定数式とその評価

const の値と static の初期値には、次の要素だけからなる**定数式**を書ける。

- 整数リテラル（型サフィックスの有無を問わない）、`true` / `false`、`()`
- 同じソースで定義した他の const の参照（import したソースの const は参照できない。1.7）
- 単項 `-` / `!`、`+ - * /`、比較演算、`as`、括弧
- static の配列の初期値に限り、配列リテラル `[c1, c2, ...]` / `[c; N]`（各要素が定数式）

static の参照、fn の呼び出し、`&` / `*`、添字は書けない（`initializer of X is not a constant expression`）。static の参照を禁止するのは、static の値は実行時に変わるため。

評価には、既存の仕組みを再利用する。

1. 定数式を AST の段階で検査する（上の要素以外があればエラー）
2. 参照する const を先に評価する。宣言順によらず参照できるよう、依存関係を深さ優先でたどる。たどる途中で同じ const に戻ってきたら循環とする（`cycle in constant definitions: A`）
3. 式を `compileExprTyped` で命令列にし、テスト用の VM（`run`）で実行して値を得る。命令列は `Push` と演算命令だけになるので、VM でそのまま実行できる
4. VM のゼロ除算は、コンパイルエラーにする（`division by zero in constant expression: X`）

型の検査は通常の式と同じ（`compileExprTyped` が期待型として宣言の型を受け取る）なので、暗黙の型変換をしない規則もそのまま働く。演算のラップアラウンドも、VM が実行時と同じく型の幅で切り詰めるため一致する。

### 1.4 static の実行時表現

static は `.data` セクションに、宣言した型の格納幅（`storageWidth`）で置く。

```
    .section .data
    .globl COUNT
    .balign 8
COUNT:
    .quad 0
    .globl TABLE
    .balign 8
TABLE:
    .quad 1, 2, 3, 4, 5
```

- 32bit の型（i32・u32・bool・`()`）は `.long` で4バイトに置く。値の正規化（読み出し時の符号拡張・ゼロ拡張）は、ローカル変数と同じ規則を使う
- アドレスは `%rbp` 相対ではなく、RIP 相対（`COUNT(%rip)`）で計算する。そのため次の命令を追加した

| 命令 | 意味 | アセンブリ |
|---|---|---|
| `LoadGlobal w name` | static の値を読んで積む | `movq name(%rip), %rax` / `movslq name(%rip), %rax` |
| `StoreGlobal w name` | 値を pop して static へ書く | `movq %rax, name(%rip)` / `movl %eax, name(%rip)` |
| `LoadGlobalAddr name` | static のアドレスを積む（`&X`、配列の添字アクセス） | `leaq name(%rip), %rax` |

- 配列の添字アクセスは、配列の先頭アドレスを積む命令が `LoadAddr` から `LoadGlobalAddr` に変わるだけで、範囲検査を含む残りはローカルの配列と共通
- `checkNoLocalRef` は `&X`（X が static）を返すことを許す。static はプログラムの終了まで存在するので、ダングリングにならない

### 1.5 グローバルを Env の最も外側のスコープとして扱う

変数の表 `Env`（スコープのスタック）の値を `(オフセット, 型)` から `(場所, 型)` に変えた。

```haskell
data Loc = LLocal Int | LStatic String | LConst Int
```

各 fn の Env の最も外側に、const/static を集めたスコープを置く。こうすると、変数の参照（`Var`）・代入・`&`・添字の基底で、名前の解決は従来どおり `lookupVar` 1つで済み、見つかった場所ごとに命令を選ぶだけでよい。

| 場所 | 読み出し | 代入 | `&` |
|---|---|---|---|
| `LLocal off` | `Load` | `Store` | `LoadAddr` |
| `LStatic name` | `LoadGlobal` | `StoreGlobal` | `LoadGlobalAddr` |
| `LConst v` | `Push v` | エラー | エラー |

### 1.6 名前の衝突と覆い隠し

Rust では fn・const・static は同じ「値の名前空間」に属し、同名の定義はエラーになる。simplang でも同じにする。fn と static は同名だとアセンブラのシンボルが衝突するため、どちらにせよ禁止する必要がある。

| 状況 | エラー |
|---|---|
| 1つのソースで fn・const・static のいずれかが同名 | `name already declared: X` |
| 自ソースの定義と import した定義が同名 | `name already declared in imported file: X` |
| import した2つのソースに同名の定義 | `main.sl: X is defined in both a.sl and b.sl` |
| リンクする全ソースで static が重複 | `X is defined in both a.sl and b.sl` |
| let・仮引数が const/static と同名 | `cannot shadow global: X` |

最後の覆い隠しについて、Rust では `let X = 1;`（X が const）は X をパターンとして扱うためエラーになり、static は「let bindings cannot shadow statics」として禁止されている。これに合わせて禁止した。グローバルと同じ名前のローカル変数を許すと、読み手が取り違えやすいことも理由である。

fn 名どうしの重複は従来どおり `function already declared: f` で検出する。

### 1.7 import

fn と同じく、**直接** import したソースの const/static だけを取り込む（step024 1.1 と同じ理由）。

- static は `.globl` で公開し、他のソースからは `X(%rip)` で参照する。アセンブラは未定義のシンボルを外部参照として扱い、リンク時に解決される
- const はシンボルを持たない。import したソースの const は、そのソースの定数式をコンパイル時に評価して値を取り込む。そのため、const はリンクする単位の間で重複してもよい（import で同時に見えなければ衝突しない）
- 定数式から参照できるのは、同じソースの const だけとした。import したソースの const まで参照できるようにすると、import 先の const を評価するためにさらにその import 先を読む必要があり、import の循環（step024 1.2）をまたいだ const の循環も検出しなければならなくなる。fn の本体からは import した const を参照できる

### 1.8 ランタイムのラベルの改名

コード生成が共通で出力する rodata のラベル（`fmt32`・`errmsg`・`strTrue` など）は、ファイル内のシンボルとして存在していた。fn では従来も同名にすると衝突したが、static を導入すると `errmsg` のような名前を付けやすくなる。そこで、`.L` を前置したファイルローカルなラベル（`.Lfmt32` など）に改名し、ユーザーのシンボルとは衝突しないようにした。

libc の関数（`puts`・`exit`・`printf`）と同名の fn や static を定義すると、ランタイムのエラー処理がそれを呼んでしまう問題は残る（step024 1.5。backlog「名前空間・名前マングリング」）。

## 2. 構文・AST

```
program     ::= import-decl* item*
item        ::= fn-decl | const-decl | static-decl
const-decl  ::= 'const' IDENT ':' type '=' expr ';'
static-decl ::= 'static' IDENT ':' type '=' expr ';'
```

- トークン `TConst`（予約語 `const`）と `TStatic`（予約語 `static`）を追加した
- 型注釈の無い `const N = 5;` は `expected ':' and type after const name: N`
- トップレベルのエラーメッセージを、const/static を含む形に改めた
  - `expected 'fn' at top level, got: ...` → `expected 'fn', 'const' or 'static' at top level, got: ...`
  - `import must appear before function definitions` → `import must appear before other definitions`
- `Program` にフィールド `progGlobals :: [GlobalDecl]` を追加した

```haskell
data GlobalKind = GConst | GStatic
data GlobalDecl = GlobalDecl GlobalKind String Type Expr
```

## 3. コンパイラ・コード生成

- `type Globals = Map String (Loc, Type)`：グローバルの表。const は評価済みの値（`LConst v`）、static はシンボル名（`LStatic name`）を持つ
- `buildGlobals :: [GlobalDecl] -> Either String (Globals, [StaticData])`：型の検査と定数式の評価を行い、表と static の初期値（シンボル名・格納幅・値の列）を作る。import したソースの表を作るのにも使う（`buildFnSigs` と同じ位置づけ）
- `compileModule :: FnSigs -> Globals -> Program -> Either String CompiledModule`：import した Globals も受け取る。結果は `CompiledModule { cmStatics, cmFns, cmMain }`
- `compile`（テスト用の単一ソースのラッパ）は戻り値の形を変えず、static の初期値は返さない
- `codegenModule :: [StaticData] -> [(String, [Instr])] -> Maybe (Type, [Instr]) -> String`：`.data` セクションを出力する。`codegen` は static を持たない形のまま残した
- ドライバの `importedSigs` は、import したソースの FnSigs と Globals の両方を集める。`checkLinkUnits` は static の重複も検査する

## 4. エラーメッセージ

| ケース | メッセージ |
|---|---|
| `N = 1;`（N が const） | `cannot assign to constant: N` |
| `&N`（N が const） | `cannot take address of constant: N` |
| 定数式に static の参照・fn の呼び出しなど | `initializer of X is not a constant expression` |
| const の循環（`const A: i64 = B; const B: i64 = A;`） | `cycle in constant definitions: A` |
| 定数式のゼロ除算 | `division by zero in constant expression: X` |
| `const P: &i64 = ...;` / `const A: [i64; 2] = ...;` | `invalid type for constant: X` |
| `static P: &i64 = ...;` | `invalid type for static: P` |
| `const X: i32 = 5i64;` | `type mismatch: expected i32, found i64`（通常の式と同じ） |
| 名前の衝突・覆い隠し | 1.6 の表 |

## 5. 移行

- `Program` のフィールドの追加に伴い、テストの `parse` の期待値を `Program [] [] [...]` の形に変えた
- 定数の評価に VM を使うため、`run` は本体のコードから使われるようになった（従来は「テストでのみ使用」）。コメントを改めた
- rodata のラベルの改名に伴い、`leaq  fmtU64(%rip)` を検査していたテストの期待値を変えた
- ソースから実行する結合テストの補助関数（`buildAndRun`）は、static の初期値も出力できるよう `compileModule` と `codegenModule` を使う形にした
- 既存のサンプルは影響を受けない。新機能のサンプルとして `sample/samp028-0〜2.sl` を追加した

## 6. スコープ外（将来の課題）

- 配列の長さへの const の使用（`[i64; N]`、`[0; N]`）。現在は型の要素数をパース時に数値で確定しているため、const の評価後に型を解決する仕組みが必要
- 不変の static と `static mut` の区別（ポインタの可変性 `&mut` の導入とあわせて）
- ポインタ型の static・const。static には定数式の初期値が必須だが、ポインタを初期化できる定数（ヌルポインタ・アドレスの定数）が無いため不可とした。`&static` を定数式として認めれば `static P: &i64 = &X;`（`.quad X`）のように書けるが、実行時の `P = &x;`（x はローカル変数）によるダングリングの検査とあわせて検討する
- const の配列
- 定数式から import したソースの const を参照すること（1.7）
- ブロック内・関数内での const/static の宣言
- 公開範囲の制御（`pub`）。現在は全ての static が他のソースから参照できる

# 公開範囲の制御（pub）における設計上の考慮点

これまで、すべての fn・static を `.globl` で公開していた。import すれば、そのソースのどの定義でも使えた（const も同様）。そのため、ライブラリの中だけで使う補助の fn や、内部の状態を持つ static も外から見え、別のソースの定義と名前が衝突することもあった（backlog「公開範囲の制御（`pub`）」、出典 step024, 028）。

今回、Rust と同じく**定義は既定で非公開**とし、`pub` を付けた定義だけを import したソースから使えるようにする。

```
// lib/util.sl
const BASE: i64 = 10;
pub const LIMIT: i64 = BASE * 2;
static CALLS: i64 = 0;

fn helper(x: i64) -> i64 {
    CALLS = CALLS + 1;
    x + BASE
}

pub fn add(x: i64) -> i64 {
    helper(x)
}
```

```
// main.sl
import "lib/util.sl";

fn main() -> i64 {
    add(1) + LIMIT        // 11 + 20
}
```

```
$ simplang-haskell main.sl -o prog && ./prog
31
```

main.sl から `helper(1)` や `BASE` を使うと、`undeclared function: helper` / `undeclared variable: BASE` になる。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 構文 | `item ::= 'pub'? (fn-decl \| const-decl \| static-decl)`。予約語 `pub` を追加する |
| 既定 | 非公開（Rust と同じ）。`pub` を付けた定義だけを、import したソースから使える |
| 同じソースの中 | 公開・非公開を問わず、従来どおり使える |
| 非公開の定義を使ったとき | 未宣言と同じエラー（`undeclared function: f` / `undeclared variable: X`） |
| シンボル | `.globl` を出すのは、pub の fn・static と main だけ。非公開の fn・static はファイルローカルなシンボルにする |
| リンク単位での重複 | 検査するのは pub の fn・static だけ。非公開の名前は、ほかのソースと重複してよい |
| import での名前の衝突 | 自ソースの定義との衝突、import したソースどうしの衝突を検査するのは、pub の定義だけ |
| main | `pub` の有無によらず `.globl main` を出す。import しても取り込まないのは従来どおり |
| 再エクスポート（`pub import`） | 対象外 |

## 1. 考慮点

### 1.1 既定を非公開にする

既定を公開にして、非公開を `priv` などで指定する案もある。この案なら、既存のソースは書き換えずに済む。

しかし「言語仕様に迷ったら Rust の仕様に合わせる」とする設計の規約に従い、Rust と同じく既定を非公開とした。外から使われることを意図した定義だけに `pub` を付けるほうが、公開の範囲を意識して書けるという利点もある。

これにより、import されるソースの定義には `pub` を書き足す必要がある（5. 移行）。import しない単一のソースは、`pub` を書かなくても従来どおり動く。

### 1.2 非公開の定義を使ったときのエラー

Rust は、非公開の定義を使うと「function `f` is private」のように、存在はするが非公開であることを伝える。

今回は、import したソースからは pub の定義だけを取り込み、非公開の定義は無いものとして扱う。そのため、未宣言と同じ `undeclared function: f` / `undeclared variable: X` になる。

非公開であることを伝えるには、import したソースの非公開の名前の表を、Compiler の名前解決（`lookupVar`・関数呼び出しの検査など）まで渡す必要がある。ローカル変数が非公開の static と同名の場合などを正しく区別するには、名前解決のあちこちに手を入れることになる。効果に対して変更が大きいので見送り、backlog に残す。

### 1.3 非公開の fn・static をファイルローカルなシンボルにする

非公開の fn・static には `.globl` を出さない。アセンブラは `.globl` の無いラベルをファイルローカルなシンボルとして扱う。同じソースの中の `call f` や `X(%rip)` はそのシンボルに解決され、ほかのオブジェクトファイルからは見えない。

| 定義 | シンボル | 同名の定義がほかのソースにあるとき |
|---|---|---|
| pub の fn・static | `.globl`（グローバル） | エラー（`f is defined in both a.sl and b.sl`） |
| 非公開の fn・static | ファイルローカル | 衝突しない |
| main | `.globl`（常に） | エラー（従来どおり） |

ファイルローカルにする利点：

- import しない限り使えない、という言語上の規則と、リンク時の振る舞いが一致する（`-c` で個別に作った .o をリンクしても、非公開の定義には外から触れない）
- ライブラリの補助の fn に、ほかのソースと名前が衝突することを気にせず名前を付けられる

ただし、libc の関数（`puts`・`exit`・`printf`）と同名の fn を定義すると、そのソースのランタイムのエラー処理がその fn を呼んでしまう問題は、非公開でも残る（そのソースの中では `call puts` がファイルローカルな `puts` に解決される）。影響する範囲がそのソースの中だけに狭まるが、解決には名前マングリングが要る（backlog「名前空間・名前マングリング」）。

### 1.4 非公開の const を参照する pub の const

const は、import したソースの定数式をコンパイル時に評価した値として取り込む（step028 1.7）。そのため、pub の const が同じソースの非公開の const を参照していても、評価はそのソースの中で完結する。

- import したソースの評価（`buildGlobals`）は、従来どおりそのソースの全ての const・static を対象に行う
- その結果から、pub の名前だけを取り込む

同様に、pub の static の初期値に、同じソースの非公開の static のアドレス（`&X`）を書いてもよい。`.quad X` は、アセンブラが「そのソースの `.data` セクションの先頭からのオフセット」として再配置を出力するので、X がファイルローカルなシンボルでもリンク時にアドレスが埋まる。

```
// a.sl
static X: i64 = 7;
pub static P: &i64 = &X;

// b.sl
import "a.sl";
fn main() -> i64 {
    *P = 5;     // X は名前では見えないが、P を通して読み書きできる
    *P
}
```

公開範囲が制御するのは名前で参照できる範囲だけで、メモリへのアクセスを制限するものではない。X の実体はプログラム全体で1つで、そのアドレスを持っていれば、ほかのソースからも読み書きできる。Rust でも、非公開の static への参照を公開の fn・static で渡せば、受け取った側はそれを通して使える。内部の状態を、意図して公開した窓口（P）を通してだけ見せる、という使い方になる。

### 1.5 main の扱い

main は、プログラムの入口として C のランタイム（`_start`）から呼ばれるため、`pub` の有無によらず `.globl` にする。Rust も `fn main` に `pub` を要求しない。`pub fn main` と書いてもよい（エラーにする理由が無い）。

import したソースの main を取り込まないのは従来どおりで、`pub fn main` であっても取り込まない。

## 2. 構文・AST

```
item ::= 'pub'? (fn-decl | const-decl | static-decl)
```

- トークン `TPub`（予約語 `pub`）を追加する
- `pub` のあとに fn・const・static 以外が続くと `expected 'fn', 'const' or 'static' after pub`
- 公開範囲を表す型を追加し、`FnDecl` と `GlobalDecl` の先頭のフィールドにする

```haskell
data Visibility = Private | Public
data FnDecl = FnDecl Visibility String [(String, Type)] Type Body
data GlobalDecl = GlobalDecl Visibility GlobalKind String Type Expr
```

## 3. コンパイラ・コード生成

- `CompiledModule` に `cmPublic :: [String]`（`.globl` にする pub の fn・static の名前）を加える
- 名前解決は変えない。自ソースの定義は、公開・非公開を問わず従来どおり参照できる
- `codegenModule` に公開するシンボルの一覧を渡し、その一覧にある fn・static にだけ `.globl` を出す。main には常に出す
- `codegen`（単一ソースのテスト用）は公開するシンボルを持たない（main だけ `.globl`）

## 4. ドライバ

- `importedSigs`：import したソースの `buildFnSigs` / `buildGlobals` の結果を、pub の名前だけに絞って返す。import したソースどうしの衝突の検査も、pub の名前だけで行う
- `checkLinkUnits`：重複の検査を pub の fn・static に限る。main の数の検査は、従来どおり全ての fn で行う
- `compileSource`：`cmPublic` を `codegenModule` に渡す

エラーメッセージは変えない。

## 5. 移行

- import されるソースの定義に `pub` を付ける
  - `sample/lib/samp024math.sl`・`samp024util.sl`・`samp028lib.sl`
  - Driver のテストで import されるソース
- リンク単位での fn・static の重複を確かめる既存のテストは、両方の定義に `pub` を付ける（非公開なら重複してよくなったため）
- `FnDecl` / `GlobalDecl` の期待値を持つテストに `Private` を加える
- 新機能のサンプルとして `sample/samp030-0.sl` と `sample/lib/samp030lib.sl` を追加する

## 6. スコープ外（将来の課題）

- 非公開の定義を使ったときに、非公開であることを伝えるエラー（1.2）
- 再エクスポート（`pub import`。import したソースの定義を、さらに import するソースへ公開する）
- 名前空間・名前マングリング（libc と同名の fn の問題。1.3）

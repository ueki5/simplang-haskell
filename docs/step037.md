# 値を返す if・while（if 式・while 式）における設計上の考慮点

step036 でブロックを式にし、if/while の本体もブロック（文の列＋末尾式）として読むようにした。しかし if/while 自体は文のままで、値を返せなかった。そのため、`let x = if c { 1 } else { 2 };` や、fn の末尾式としての if を書けなかった（backlog「値を返す `if`・`while`／`loop`」）。

今回、**if と while を式にする**。if の値は選ばれた分岐の末尾式で、while の値は常に `()` である（Rust と同じ）。左辺の無い単独の if/while は、従来どおり文として書ける。

```
fn sign(x: i64) -> i64 {
    if x < 0 { -1 } else if x == 0 { 0 } else { 1 }
}

fn main() -> i64 {
    let a = if sign(-5) < 0 { 10 } else { 20 };   // a = 10
    let i = 0;
    while i < 3 { i = i + 1; }                    // 従来どおり単独の文として書ける
    if i == 3 { a = a + 1; }                      // else の無い if も単独の文として書ける
    let u: () = while false {};                   // 値として使えば型は ()
    a + i                                         // 11 + 3
}
```

```
$ simplang-haskell main.sl -o prog && ./prog
14
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 構文 | `if` 式・`while` 式を primary として書ける。`let x = if c { 1 } else { 2 } + 3;` は `(if ...) + 3` |
| if の型 | 全分岐の末尾式の型を単一化したもの。期待型はそのまま各分岐の末尾式へ伝播する |
| else の無い if | `else { () }` と同じに扱う。値を使うなら型は `()` に限られる（`let x: i64 = if c { 1 };` は `type mismatch: expected i64, found ()`） |
| while の型 | 常に `()`（Rust と同じ。値付きの `break` は書けない） |
| 文の位置の if/while | 従来どおり書ける。step036 のブロックと同じく `;` は不要で、値は `()` でなければならない（while は常に満たす）。直後が `}` なら外側のブロックの末尾式になる |
| 定数式 | if/while は書けない |
| `if ... ;` | step036 の `{ ... };` と同じく、従来どおりパースエラー（空文が無いため） |

## 1. 考慮点

### 1.1 文と式で AST を分けない

if/while が式になると、文の位置の if（`SIf`）と式の位置の if の2つの AST ができうる。

| 選択肢 | 評価 |
|---|---|
| `SIf`/`SWhile` を残し、式の位置にだけ `If`/`While` を足す | 同じ構文に AST が2つでき、型検査・コード生成も2組になる。既存テストの書き換えは少ない |
| **`Expr` に `If`/`While` を足し、文の位置は1種類の文で包む（採用）** | Rust の構文（ExpressionWithBlock を `;` 無しで置いた文）と同じ形。step036 の `SBlock` も同じ扱いにまとめられる |

`Stmt` の `SBlock`・`SIf`・`SWhile` を `SExprWithBlock Expr` 1つにまとめる。中身は `Block`・`If`・`While` のいずれかで、値は `()` でなければならない。

### 1.2 文の位置の if/while と末尾式の区別

step036 のブロックと同じ規則にする。**後ろに `}` が続く `{}`/if/while は外側のブロックの末尾式、それ以外は文**である。

```
fn count(n: i64) {
    let i = 0;
    while i < n { i = i + 1; }   // 末尾式（型は ()）。戻り値の () と一致する
}
```

従来は最後の while も文で、末尾式は省略された `()` だった。今回は while 自体が末尾式になるが、型はどちらも `()` なので、従来のプログラムの意味は変わらない。`-> i64` の fn の最後に置いた while が `expected i64, found ()` になるのも従来と同じである（`return` で抜けるループで終わる関数を書けないのは、backlog「発散型 `!`」の課題）。

文の途中に `if c { 1 } else { 2 }` を置くと、値が `()` でないので型エラーになる（Rust と同じ）。

### 1.3 末尾の読み直しをやめる

step036 のパーサは、文の列を読む `parseStmts` が `{` で始まるブロックを読み、直後が `}` なら「文は無い」として元のトークン列のまま戻り、外側が末尾式として読み直していた。if が式になると、if の本体の末尾にさらに if を置く入れ子（`if a { if b { if c { ... } } }`）がふつうに現れる。読み直しは入れ子の1段ごとに2回読むので、深さ n で 2^n 回読むことになる。

そこで、`parseStmts` は**文の列と、末尾式として読んだ with-block 式（`Maybe Expr`）**を返す。`{`・`if`・`while` で始まる文を読み、直後が `}` ならその式をそのまま末尾式として返し、外側（`parseBlockBody`）は改めて読まない。キーワードで始まらない式（`a[0]`・`*p` など）の試し読みは従来どおりである。

### 1.4 値を使わない位置では値を積まない

式としてのコンパイル（`compileExprTyped`）では、while は最後に `()` の値（0）を積み、else の無い if は else 側で `()` を積む。文の位置や、値を捨てるブロックの末尾式でこれを使うと、`Push 0; IPop` という無駄な命令が出る。

そこで、step036 の `compileUnitBlock`（値を捨てるブロック）を **`compileUnitExpr`（値が `()` の式を、値を積まずにコンパイルする）**へ一般化する。

| 式 | コンパイル |
|---|---|
| `()` | 命令なし |
| ブロック | 文の列、続けて末尾式を `compileUnitExpr` |
| if | 各分岐の本体を `compileUnitExpr` の規則でコンパイルする（else の無い if は何もしない） |
| while | 従来の while（本体は `compileUnitExpr` の規則） |
| その他 | 期待型 `()` でコンパイルし、値を `IPop` で捨てる |

文の位置の `{}`/if/while、if/while の本体はすべてこれを使う。従来のプログラムの生成コードは変わらない。

### 1.5 分岐の合流点の操作スタックの深さ

値を返す if は、各分岐で値を積んでから `Jmp if_end` で合流する。

```
cond1; JmpIfZero next1      ← 深さ d
body1                       ← 値を積んで d+1
Jmp if_end                  ← if_end の深さとして d+1 を記録
Label next1                 ← JmpIfZero の深さ d
...
else-body                   ← d+1
Label if_end                ← フォールスルーの深さ d+1
```

step036 の `labelDepths` は、ラベルの深さをフォールスルーで決め、無ければ先に現れた `Jmp`/`JmpIfZero` の深さで決める。値を返す if はこの規則でそのまま正しく扱える。分岐の途中で return/break しても（`if c { return 1; 5 } else { 2 }`）、到達不能な部分の `Jmp` は深さが「不明」なので記録されない。コード生成の変更は要らない。

### 1.6 else の無い if

Rust と同じく、else の無い if は `else { () }` と同じに扱う。型推論では各分岐の型と `()` を単一化し、コンパイルでは else 側で期待型に対して `()` をコンパイルする。値を使う位置（`let x: i64 = if c { 1 };`）では `type mismatch: expected i64, found ()` になる。Rust の「`if` may be missing an `else` clause」に相当する専用のメッセージは設けない。

### 1.7 ダングリングポインタの簡易検査

`isLocalRef` は、ブロックと同じく if の各分岐の末尾式もたどる。

```
fn f(c: bool) -> &i64 { let x = 1; if c { &x } else { &x } }   // cannot return reference to local variable: x
```

## 2. 構文・AST

```
primary    ::= ... | block | if-expr | while-expr
if-expr    ::= 'if' expr block ('else' 'if' expr block)* ('else' block)?
while-expr ::= 'while' expr block
stmt       ::= ... | expr-with-block       （直後が '}' でない位置。値は () でなければならない）
expr-with-block ::= block | if-expr | while-expr
```

| AST | 変更 |
|---|---|
| `Expr` | `If [(Expr, Body)] (Maybe Body)`・`While Expr Body` を追加 |
| `Stmt` | `SBlock Body`・`SIf ...`・`SWhile ...` を `SExprWithBlock Expr` に統合 |

## 3. 意味論

- `If branches else`：条件を上から評価し、最初に真になった分岐の本体を評価する。値はその本体の末尾式で、どれも真でなければ else の本体（無ければ `()`）
- `While cond body`：従来どおり。値は `()`
- 期待型が `&[T]` で、if の型が `&[T; N]` なら、従来の式と同じく `&[T]` へ変換する

| ケース | メッセージ |
|---|---|
| else の無い if の値を使う（`let x: i64 = if c { 1 };`） | `type mismatch: expected i64, found ()` |
| 分岐の型が期待型と合わない（`let x: i32 = if c { 1i64 } else { 2 };`） | `type mismatch: expected i32, found i64` |
| 推論で分岐の型が合わない（`let x = if c { 1i64 } else { 2i32 };`） | `type mismatch: i64 and i32` |
| while の値を `()` 以外として使う（`let x: i64 = while c {};`） | `type mismatch: expected i64, found ()` |
| 文の途中の if の値が `()` でない（`if c { 1 } else { 2 } let y = 2;`） | `type mismatch: expected (), found integer literal` |
| 定数式に if | `initializer of N is not a constant expression` |

## 4. コンパイラ

- `compileIf` は本体のコンパイル関数を受け取り、値を返す if（`compileBlock ctx expected`）と値を捨てる if（`compileUnitExpr`）で共用する
- `compileExprTyped` の `While` は、期待型が `()` でなければ型エラー、そうでなければ従来の while の後に `Push 0` を出す
- 期待型が `()` の `If`（`()` を返す fn の最後の if など）も、値を積まない if の後に `Push 0` を1つ出す。各分岐で `()` を積むより短く、if が文だった頃の生成コードと同じになる
- `inferMaybeType` の `If` は、各分岐の本体の型（ブロックと同じく `blockEnv` で求める）を単一化する。else が無ければ `()` とも単一化する。`While` は `()`

## 5. 移行

- `SBlock`・`SIf`・`SWhile` を直接書いているテスト（parse）を `SExprWithBlock` の形へ書き換える。ブロックの最後の if/while は、外側のブロックの末尾式になる
- 従来のプログラムの意味は変わらない。生成コードは、文の位置のブロックの末尾に置いたブロック（`{ ...; { ... } }`）で、step036 が出していた `Push 0; IPop` が無くなる（`compileUnitExpr` が末尾のブロックも値を積まずにコンパイルするため。既存のサンプルでは samp006 だけが該当）。それ以外は変わらない
- `docs/language.md`：文法（if/while を式に）、文（文の位置の with-block 式）
- `docs/architecture.md`：AST、パーサの末尾式の扱い、型検査（`compileUnitExpr`・if の推論）
- `docs/backlog.md`：「値を返す `if`・`while`／`loop`」を「`loop` と値付き `break`」に縮め、if/while を実現済みへ移す。`if ...;` / `{ ...};` を追記する
- サンプル：`sample/samp037-0.sl`（冒頭の例）を追加する

## 6. スコープ外（将来の課題）

| 課題 | 補足 |
|---|---|
| `loop` と値付きの `break`（`let x = loop { break 1; };`） | break の値を合流点で受け渡す。合流点の深さは `labelDepths` で扱える |
| 発散型 `!` | `let x = if c { 1 } else { return 2; };` は、else の本体の型が `()` になるので型エラー。Rust では `!` として許される |
| `if ... ;` / `{ ... };`（値を捨てる式文） | Rust では許される。空文または with-block 式の後の `;` の扱いが要る |
| `if` に else が無いときの専用のエラーメッセージ | 現在は `type mismatch: expected T, found ()` |

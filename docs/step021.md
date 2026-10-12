# ユニット型（`()`）と式文における設計上の考慮点

step009 では「ユニット型は採用しない」としたため、関数本体にも暗黙 main にも末尾式が必須で、`return` には値が必須だった。値を返さない関数は書けず、戻り値を使わない呼び出しにも `let d = f(..);` と書く必要があった（backlog「ユニット型（値を返さない関数）と式文」）。今回、ユニット型 **`()`** と **式文 `expr;`** を導入してこれを解消する。

```
fn set_positive(p: &i64, v: i64) -> () {
    if v < 0 {
        return;
    }
    *p = v;
}
fn noop() {}
let x = 1;
set_positive(&x, 5);
set_positive(&x, -1);
let u = noop();       // u: ()
x                     // 5
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| ユニット型 | 型 `()`、値 `()` の1つだけ。型注釈（`let u: () = ();`、`&()`、`[(); N]`、仮引数・戻り値）に使える |
| fn の末尾式 | 省略可能。省略時は `()` を返す（＝末尾式 `()` とみなす） |
| `return;` | 値を省略した return。`return ();` と同じ |
| 戻り値型の省略 | 従来どおり末尾式と return 文の**両方**から推論する。末尾式の省略・`return;` は `()` の証拠になる |
| 式文 `expr;` | 任意の型の式を評価し、値を捨てる（Rust と同じ。`()` 以外も許可） |
| 暗黙 main | 末尾式を省略可能（省略時は `()`）。末尾式の型が `()` なら**何も出力しない** |
| 演算 | `==`/`!=` のみ可（常に `true`/`false`）。算術・大小比較・キャスト・条件式への使用はエラー |

## 1. 考慮点

### 1.1 省略を `()` リテラルへ脱糖する

末尾式の省略や `return;` を AST 上で `Maybe Expr` として表すと、戻り値型の推論（`resolveFnSigs` の ReturnSlot の証拠収集）、`compileFnDecl`、`compileStmtsFrom` の return 処理など、末尾式と return を扱う箇所すべてに「無い場合」の分岐が必要になる。

そこで AST に `()` リテラル `UnitLit` を追加し、パーサで次のように**脱糖**する。

| ソース | AST |
|---|---|
| fn 本体の末尾式の省略（`stmt* '}'`） | 末尾式 `UnitLit` |
| `return;` | `SReturn UnitLit` |
| プログラム全体の末尾式の省略（入力終端） | 末尾式 `UnitLit` |

`Body = ([Stmt], Expr)`・`SReturn Expr`・`Program` の形は変わらないため、型推論・コード生成は `UnitLit` の型が `()` であることを知るだけでよく、以下がすべて既存の仕組みで成り立つ。

- `-> i64` なのに末尾式を省略／`return;` → `type mismatch: expected i64, found ()`
- 型省略で `return 1i64;` があるのに末尾式を省略 → ReturnSlot の単一化で `type mismatch: i64 and ()`
- 型省略で末尾式も return の値も無い → `()` に推論される

なお、型省略で `return 1;`（無型リテラル）と末尾式の省略が混在した場合は、無型リテラルが `()` と単一化されて戻り値型が `()` に確定し、その後 `return 1;` のコンパイルで `type mismatch: expected (), found integer literal` となる。

### 1.2 `()` の実行時表現

| 選択肢 | 評価 |
|---|---|
| ゼロサイズ型（Rust） | スタックを消費しないが、`()` 型の変数・`&()`・`[(); N]`・仮引数・戻り値・式文のそれぞれで「値が無い」ことを特別扱いする必要がある。「1値＝操作スタック上の8バイト1スロット」というコード生成の前提（`genBody` の深さ計算など）も崩れる |
| **通常の値として扱う（採用）** | 値は常に 0。bool と同じく 32bit スロットに格納する（`storageWidth TUnit = W32`）。変数・ポインタ・配列・仮引数・戻り値（`%rax` = 0）がすべて特別扱いなしで動く |

採用案では `()` 型の変数も 4 バイトを消費するが、実用上の問題は小さい。ゼロサイズ化は将来の課題とする。

### 1.3 式文は任意の型を許可する

式文の値を捨てる命令として `IPop`（`addq $8, %rsp`）を追加した。式の型は `let` の型注釈省略と同じ `inferType`（無型リテラルのみなら i64）で決める。

`()` 以外の値を捨てる式文（`1 + 2;`、`add(&x, 1);`）を禁止する案もあるが、Rust も許可しており（未使用の値は lint の警告にとどまる）、副作用のある関数の戻り値を無視したい場面もあるため、許可した。配列型の式（`a;`）は値として使えないため、既存の `array cannot be used as a value` エラーになる。

### 1.4 暗黙 main の出力

末尾式の型が `()` の場合は `printf` を呼ばずに終了する（Rust の `main` が何も表示しないのと同じ）。これにより、出力を伴わない「文だけのプログラム」も書けるようになった。

## 2. 構文

```
program     ::= (fn-decl | stmt)* expr?
fn-decl     ::= 'fn' IDENT '(' params? ')' ('->' type)? '{' stmt* expr? '}'
stmt        ::= ... | expr-stmt
expr-stmt   ::= expr ';'
return-stmt ::= 'return' expr? ';'
type        ::= ... | '(' ')'
primary     ::= ... | '(' ')'
```

- `Type` に `TUnit`、`Expr` に `UnitLit`、`Stmt` に `SExpr Expr` を追加した
- 文の判別: キーワードにも `IDENT '='` にも該当しない場合に式を試し読みし、次が `=` なら `SAssignPlace`（step020）、**`;` なら `SExpr`**、それ以外は末尾式として戻る
- 末尾式の省略は「`parseStmts` の後が `}`（fn）／入力終端（プログラム）」で判定する
- `as ()` は既存の検査（キャスト先は整数型のみ）でパース時エラー

## 3. 意味論・コード生成

| 対象 | 処理 |
|---|---|
| `UnitLit`（期待型 `()`） | `Push 0` |
| `UnitLit`（期待型がそれ以外） | `type mismatch: expected T, found ()` |
| `inferMaybeType` / `resolveExprType` | `UnitLit` → `Just TUnit` |
| `SExpr e` | `inferType` で型を決め、`compileExprTyped ty e ++ [IPop]` |
| 大小比較 | bool と同じく `()` 同士はエラー（`==`/`!=` は可） |
| 暗黙 main の epilogue（`TUnit`） | `xorl %eax, %eax; leave; ret`（何も出力しない） |
| fn の戻り値 `()` | 既存どおり `popq %rax`（値は 0） |

## 4. エラーメッセージ

| ケース | メッセージ |
|---|---|
| `-> i64` で末尾式を省略／`return;` | `type mismatch: expected i64, found ()` |
| `-> ()` で `return 1i64;` | `type mismatch: expected (), found i64` |
| 型省略で `return 1i64;` と末尾式の省略が混在 | `type mismatch: i64 and ()` |
| `let x: () = 5;` | `type mismatch: expected (), found integer literal` |
| `() + ()` | `type mismatch: expected (), found arithmetic expression` |
| `() < ()` | `type mismatch: expected i32 or i64, found ()` |
| `if () {}` | `type mismatch: expected bool, found ()` |
| 暗黙 main での `return;` | `return used outside function`（従来どおり） |

## 5. テストへの影響

- 「末尾式が無いプログラム／fn 本体はエラー」「値の無い return はエラー」「末尾式の後に `;` を書くとエラー」「空入力はエラー」としていた parse のテストは、今回から正しいプログラム（`UnitLit` への脱糖・`SExpr`）になるため、期待値を変更した
- 追加したテスト:
  - parse: `()` リテラル、`()` 型注釈（`&()`・`[(); N]`）、末尾式の省略（fn・プログラム）、`return;`、式文、`as ()` のエラー
  - compile: 推論（末尾式なし・`return;` のみの fn が `()`、`let u = f();`、末尾式と return の両方からの推論）、式文の `IPop`（命令列・VM）
  - 意味論エラー: 4章の各ケース
  - Integration: `return;` による早期脱出と式文での呼び出し、`-> ()` の fn から `()` の fn を末尾式で呼ぶ、`()` 型の変数・ポインタ・比較、`()` 型の仮引数、`()` 以外の値を捨てる式文（式中の呼び出しのアライメントを含む）、何も出力しない main
- サンプル: `sample/samp021-0〜3.sl`

## 6. スコープ外（将来の課題）

- ユニット型のゼロサイズ化（1.2）
- 発散型 `!` と到達不能コードの解析。現在は `while true { ... return x; }` で終わる関数でも末尾式を省略すると `()` を返すとみなされ、`-> i64` なら型エラーになる

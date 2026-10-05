# 整数型（i32/i64）の暗黙の型変換の導入における設計上の考慮点

`docs/step002.md`でi32型を導入して以来、同一式内でのi32/i64の混在は一貫して**コンパイルエラー**としてきた（`compileExprTyped`が期待型`expected`を式木に一様伝播し、`Var`・`LitTyped`・`Call`などの確定型を持つリーフで`expected`との厳密一致を要求する。末尾式・`let`の型推論で使う`unifyType`も厳密一致を要求する）。幅の異なる値を受け渡すには、`docs/step004.md`で導入した`to_i64`/`to_i32`による明示的な変換が必須だった。

今回は、型に不整合があった場合にコンパイラが**暗黙の型変換**を挿入する機能を追加する。対象は整数型（i32/i64）同士に限り、拡大（i32→i64）・縮小（i64→i32）の両方向を暗黙に行う。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 変換の方向 | 拡大（i32→i64）・縮小（i64→i32）の**両方**を暗黙に行う。縮小時は下位32bitを符号拡張した値になる（`to_i32`および`docs/step002.md`のラップアラウンド規則と同じ） |
| 適用箇所（代入系） | 期待型が決まっている所へ値を渡す全ての箇所: `let`（型注釈あり）の初期化式、代入文、関数呼び出しの実引数、`return`文、fnの末尾式 |
| 適用箇所（演算） | 二項算術演算（`+` `-` `*` `/`）・比較演算（`==` `!=` `<` `<=` `>` `>=`）の被演算子でi32/i64が混在した場合、狭い側を広い側（i64）へ昇格させる（Cの「通常の算術変換」に相当） |
| 演算の幅 | 算術演算は**被演算子の型**で行い、結果を期待型へ変換する（例: `let z: i64 = a * b;`（a,b: i32）はi32で乗算してからi64へ拡大する。オーバーフローはi32で起きる。Cと同じ挙動） |
| 型推論への影響 | 末尾式・`let`の型注釈省略（`SLetInferred`）・fnの省略された仮引数/戻り値型の推論で、i32とi64が出会えばi64に合流する（従来はエラー） |
| 対象外 | bool↔整数、ポインタ↔整数、異なるポインタ型間（`&i32`↔`&i64`）の変換は従来どおり型エラー |
| 定数畳み込み | 行わない（リテラルの変換もコンパイル時に計算せず、実行時の`ISext32`で行う。後述5節） |
| 追加する命令 | 無し。既存の`ISext32`（`docs/step004.md`）を再利用する |
| 明示的変換 | `to_i64`/`to_i32`は引き続き使用可能。暗黙変換と同じ結果になる |

## 1. 型の合流：`unifyType`の意味の変更

`unifyType`（`src/Compiler.hs`）を「厳密一致の単一化」から「広い側への合流（join）」に変更する。

```haskell
-- 2つの型を合流させる。整数同士は幅の広い方（i64）へ昇格し、それ以外は厳密一致を要求する
unifyType :: Type -> Type -> Either String Type
unifyType t1 t2
  | t1 == t2 = Right t1
unifyType (TyInt _) (TyInt _) = Right (TyInt W64)
unifyType t1 t2 = Left ("type mismatch: " ++ typeName t1 ++ " and " ++ typeName t2)
```

- 昇格するのは**トップレベルの**`TyInt`同士のみ。`TPtr (TyInt W32)`と`TPtr (TyInt W64)`は`t1 == t2`にも2つ目の節にも該当せず、従来どおり`"type mismatch: &i32 and &i64"`となる
- `unifyMaybeType`はこの`unifyType`を呼ぶだけなので変更不要

`unifyMaybeType`を経由して、次の箇所へ自動的に波及する（いずれもコード変更は不要）。

| 利用箇所 | 変更後の挙動 |
|---|---|
| `inferMaybeType`（`combine`） | 末尾式`x + y`（x: i32, y: i64）の型がi64と推論される。`SLetInferred`も同様 |
| `operandType`（比較演算） | `x < y`の被演算子の共通型がi64になる |
| `resolveExprType`（`combine`） | fnシグネチャ推論中の式の型も同じ規則で合流する |
| `resolveOne` | 省略された仮引数に対して呼び出し箇所ごとにi32とi64が渡されていれば、その仮引数はi64に推論される（従来は`type mismatch: i32 and i64`） |

## 2. 変換命令を生成するヘルパー：`coerce`

実際の型`actual`の値（スタック先頭）を期待型`expected`へ変換する命令列を返すヘルパーを新設する。

```haskell
-- スタック先頭の actual 型の値を expected 型へ変換する命令列（整数の幅違いのみ暗黙変換する）
coerce :: Type -> Type -> Either String [Instr]
coerce actual expected
  | actual == expected = Right []
coerce (TyInt _) (TyInt _) = Right [ISext32]
coerce actual expected =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName actual)
```

- `TyInt`同士で`actual /= expected`になるのはi32→i64かi64→i32のどちらかであり、どちらも`ISext32`（`popq %rax; cltq; pushq %rax`）で正しく変換できる
  - **縮小（i64→i32）**: 下位32bitを符号拡張することがi32への切り詰めそのもの
  - **拡大（i32→i64）**: 「正しくi32として扱われた値はスタック上で常に符号拡張済み」という不変条件を信頼すれば理論上は命令不要だが、`docs/step004.md` §3と同じ理由（`fn f() -> i32 { 5000000000 }`の戻り値のように、リテラルが`Store W32`や算術命令を経由せずi32値になる経路がある）で、常に`ISext32`で明示的に正規化する
- エラーメッセージは従来の`compileExprTyped`のものと同じ形式。bool・ポインタ絡みの不一致は、従来と同じメッセージのままエラーになる

## 3. `compileExprTyped`の変更

### 3.1 確定型リーフ：厳密一致を`coerce`へ置き換える

| ノード | 従来 | 変更後 |
|---|---|---|
| `Var name` | `ty /= expected`ならエラー | `Load (storageWidth ty) off`の後に`coerce ty expected` |
| `LitTyped n w` | `expected /= TyInt w`ならエラー | `Push n`の後に`coerce (TyInt w) expected` |
| `Call name args` | `retTy /= expected`ならエラー | `ICall`の後に`coerce retTy expected`。実引数は従来どおり`zipWithM (compileExprTyped fnSigs env) paramTys args`でコンパイルするため、実引数側の変換は各リーフで自動的に行われる |
| `Lit n`（無型） | 期待型をそのまま採用 | 変更なし（変換不要） |
| `BoolLit`/`Not`/`AddrOf` | — | 変更なし（bool・ポインタは対象外） |

`compileStmtsFrom`の`SLet`/`SAssign`/`SReturn`、`compileFnDecl`の末尾式、関数の実引数は、いずれも期待型を指定して`compileExprTyped`を呼んでいるだけなので**変更不要**である。リーフで変換されることにより、代入系の全箇所で暗黙変換が自動的に有効になる。

### 3.2 算術演算：一様伝播をやめ、被演算子の型で演算する

従来は`expected`をそのまま左右の子へ伝播し、`IAdd expected`を出していた。これを、**演算の型`opTy`を被演算子から推論**する方式に変える。

```haskell
-- Add/Sub/Mul/Div 共通（expected が TBool/TPtr の場合の既存エラー節はそのまま先に置く）
compileArith :: FnSigs -> Env -> Type -> Expr -> Expr -> Expr -> (Width -> Instr) -> Either String [Instr]
compileArith fnSigs env expected node l r mkInstr = do
  -- node自身の推論型＝左右の被演算子の合流型。リテラルのみ（Nothing）なら expected を採用する
  opTy <- maybe expected id <$> inferMaybeType fnSigs env node
  case opTy of
    TyInt w -> do
      li <- compileExprTyped fnSigs env opTy l
      ri <- compileExprTyped fnSigs env opTy r
      conv <- coerce opTy expected
      Right (li ++ ri ++ [mkInstr w] ++ conv)
    other -> Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName other)
```

- `inferMaybeType`は`Add a b`に対して既に`combine a b`（左右の`unifyType`）を返すため、`opTy`は「被演算子の合流型」そのものになる。新たな推論ロジックは不要
- リテラルのみの式（`opTy`が`Nothing`）では`expected`を採用するため、`let x: i32 = 1 + 2;`のような従来の式は**従来と完全に同一の命令列**になる
- `Neg e`も同じ形（`inferMaybeType fnSigs env (Neg e)`で`opTy`を決め、`INeg w`の後に`coerce`）にする
- `opTy`がboolやポインタになる場合（`let x: i64 = 1 + true;`、`let x: i64 = p + 1;`）のエラーメッセージは、従来と同じ`"expected i64, found bool"`/`"expected i64, found &i64"`になる

#### 演算の幅と期待型が食い違う場合の挙動

```
let a: i32 = 2147483647;
let z: i64 = a + 1;   // opTy = i32 → IAdd W32 でラップ → ISext32 → z = -2147483648
let b: i64 = 1;
let w: i64 = a + b;   // opTy = i64 → a を拡大してから IAdd W64 → w = 2147483648
```

「代入先がi64だから演算もi64で行う」のではなく、「演算は被演算子の型で行い、結果を代入先へ変換する」。これはCと同じ規則であり、式の意味が代入先の型に左右されないという利点がある（代わりに、i32同士の演算結果をi64へ代入してもオーバーフローは防げない。防ぐには`to_i64(a) + 1`のように被演算子を明示的に拡大する）。

### 3.3 比較演算

`operandType`が1節で変更した`unifyMaybeType`を使うため、`x == y`（x: i32, y: i64）の被演算子の共通型はi64になる。左右の子を`opTy`でコンパイルすれば、i32側はリーフ（3.1）で`ISext32`により拡大される。`compileExprTyped`の比較の節は**変更不要**。

### 3.4 `to_i64`/`to_i32`

従来は子の期待型をi32（`to_i64`）/i64（`to_i32`）に固定していた。暗黙変換の導入後もこのままにすると、`to_i64(x)`（x: i64）で子のリーフが**暗黙にi32へ縮小**してから拡大することになり、値が黙って壊れる。そこで、子は**自身の推論型**でコンパイルし、目的の幅へ`coerce`する。

```haskell
compileExprTyped fnSigs env expected (ToI64 e) = do
  -- 子がリテラルのみ（Nothing）の場合は従来どおりi32とみなす（to_i64(5000000000) のラップ挙動を維持する）
  srcTy <- maybe (TyInt W32) id <$> inferMaybeType fnSigs env e
  case srcTy of
    TyInt _ -> do
      ei <- compileExprTyped fnSigs env srcTy e
      toI64 <- coerce srcTy (TyInt W64)
      conv <- coerce (TyInt W64) expected
      Right (ei ++ toI64 ++ conv)
    other -> Left ("type mismatch: expected i32 or i64, found " ++ typeName other)
-- ToI32 も同形（リテラルのみの場合はi64とみなし、目的の幅は TyInt W32）
```

- `to_i64(x)`（x: i32）は従来と同じ`Load W32, ISext32`
- `to_i64(x)`（x: i64）は恒等変換になる（`coerce`が`[]`を返す）。従来はエラーだった
- `let y: i32 = to_i64(x);`のように結果を別の幅の文脈で使うと、`ISext32`が2つ続く。冗長だが結果は正しく、最適化はスコープ外とする
- 結果を`expected`へ`coerce`するため、従来`expected`ごとに用意していたエラー節（`TBool`/`TPtr`/逆の幅）は`coerce`に統合できる（メッセージは同一）

### 3.5 デリファレンス（`*p`）

従来は`expected`を`TPtr expected`として子へ伝播していた。これでは`p: &i32`をi64の文脈で`*p`と使うと、ポインタ型同士の不一致（`&i64`と`&i32`）としてエラーになってしまう。ポインタ型自体は厳密一致のまま、**指す先の値**を変換するよう変更する。

```haskell
compileExprTyped fnSigs env expected (Deref e) = do
  ptrTy <-
    inferMaybeType fnSigs env e
      >>= maybe (Left "type mismatch: cannot dereference an untyped literal") Right
  case ptrTy of
    TPtr inner -> do
      ei <- compileExprTyped fnSigs env ptrTy e
      conv <- coerce inner expected
      Right (ei ++ [LoadInd (storageWidth inner)] ++ conv)
    other -> Left ("type mismatch: expected pointer, found " ++ typeName other)
```

`inferMaybeType`/`addressOf`の`Deref`と同じ判定・同じエラーメッセージになる。非ポインタのデリファレンス（`*a`、a: i64）のメッセージは`"expected &i64, found i64"`から`"expected pointer, found i64"`に変わる。

## 4. 影響を受けない箇所

| 箇所 | 理由 |
|---|---|
| Tokenizer / Parser / AST | 構文の追加・変更は無い |
| `Instr` / `codegen` / `run`（VM） | 変換には既存の`ISext32`を使うため、命令セットに追加は無い |
| `compileStmtsFrom` / `compileIf` / `compileWhile` / `compileFnDecl` / `allocParams` | 期待型を指定して`compileExprTyped`を呼ぶだけであり、変換はリーフ側で行われる |
| `resolveFnSigs`以下のシグネチャ推論 | `unifyMaybeType`経由で合流規則が自動的に適用される（1節） |
| `app/Main.hs` / printfの書式選択 | 末尾式の型が合流によりi64になる場合も、従来どおり`%ld`が選ばれる |

## 5. 明示的なスコープ外

- **bool↔整数、ポインタ↔整数、異なるポインタ型間の変換**: 従来どおり型エラー
- **定数畳み込み**: `let x: i32 = 9999i64;`のようにリテラルの変換結果がコンパイル時に分かる場合も、変換結果を事前に計算して`Push`するのではなく、`Push 9999, ISext32`を出力して実行時に変換する。結果は同じで、命令が1つ多くなるだけである。`to_i64`/`to_i32`が「常に`ISext32`で正規化する」とした`docs/step004.md`の方針とも揃う。最適化は独立した機能として将来のステップで扱う
- **冗長な`ISext32`の除去**: `to_i64`の結果をi32の文脈で使う場合などに連続する`ISext32`も、そのまま残す
- **警告**: 縮小変換で値が失われ得る場合も、警告は出さない

## 6. `docs/step015.md`までからの変更点まとめ

| ファイル / 項目 | step015まで | step016での変更 |
|---|---|---|
| `unifyType` | 厳密一致 | 整数同士はi64へ合流する |
| `coerce` | — | 新設。整数の幅違いに`ISext32`を出し、それ以外の不一致はエラー |
| `compileExprTyped`（`Var`/`LitTyped`/`Call`） | `expected`と厳密一致を要求 | `coerce`で`expected`へ変換 |
| `compileExprTyped`（算術・`Neg`） | `expected`を子へ一様伝播 | 被演算子の合流型`opTy`で演算し、結果を`coerce` |
| `compileExprTyped`（`ToI64`/`ToI32`） | 子の期待型を固定 | 子を推論型でコンパイルし、目的の幅へ`coerce`した後、結果を`expected`へ`coerce` |
| `compileExprTyped`（`Deref`） | `TPtr expected`を子へ伝播 | 子の推論型`TPtr inner`でコンパイルし、`inner`から`expected`へ`coerce` |
| 比較演算・`operandType` | 被演算子同士の厳密一致 | 変更なし（`unifyType`の変更により合流する） |
| `Instr` / codegen / VM / Parser | — | 変更なし |
| `CLAUDE.md` | 「同一式内でのi32/i64混在は必ず型チェックで検出される」 | 実装時に、型システムの記述を暗黙変換の規則に合わせて更新し、ドキュメント一覧へ`docs/step016.md`を追加する |

## 7. テストへの影響

### 7.1 カテゴリ別の影響

| カテゴリ | 影響 |
|---|---|
| Tokenizer / Parser / CodeGen / VM（`run`） | 無し |
| 意味論エラー（compile） | i32/i64混在をエラーとしていたテストを、成功（変換命令を含む命令列）へ書き換える。テストの追加 |
| Integration | 幅違いの代入をエラーとしていたテストを、実行結果の検証へ書き換える。テストの追加 |

### 7.2 期待値の書き換えが必要な既存テスト（`test/Spec.hs`）

オフセットは、テストソース中の`x: i32`を`-4`、`y: i64`を`-12`に割り付けた配置で記す。行番号はstep016着手時点のもの。

| 行 | ソース（抜粋） | 現行の期待値 | step016後の期待値 |
|---|---|---|---|
| 543 | `let z: i64 = x + y;` | `expected i64, found i32` | 成功。`… Load W32 (-4), ISext32, Load W64 (-12), IAdd W64, Store W64 (-20), Load W64 (-20)` |
| 546 | `y = x + 1;` | `expected i64, found i32` | 成功。`… Load W32 (-4), Push 1, IAdd W32, ISext32, Store W64 (-12), Load W64 (-12)`（i32で演算してから拡大） |
| 549 | 末尾式`x + y` | `i32 and i64` | 成功。型は`TyInt W64`、`… Load W32 (-4), ISext32, Load W64 (-12), IAdd W64` |
| 552 | `let z: i32 = y;` | `expected i32, found i64` | 成功。`… Load W64 (-12), ISext32, Store W32 (-16), Load W32 (-16)` |
| 565 | `let x: i32 = 9999i64;` | `expected i32, found i64` | `Right ([], TyInt W32, [Push 9999, ISext32, Store W32 (-4), Load W32 (-4)])` |
| 568 | `let x: i64 = 9999i32;` | `expected i64, found i32` | `Right ([], TyInt W64, [Push 9999, ISext32, Store W64 (-8), Load W64 (-8)])` |
| 579 | `9999i32 + 5i64` | `i32 and i64` | `Right ([], TyInt W64, [Push 9999, ISext32, Push 5, IAdd W64])` |
| 591 | 末尾式`x == y` | `i32 and i64` | 成功。型は`TBool`、`… Load W32 (-4), ISext32, Load W64 (-12), ICmpEq` |
| 594 | `let z: bool = x == y;` | `i32 and i64` | 成功（591と同じ比較の後に`Store W32 (-16)`） |
| 600 | 末尾式`x < y` | `i32 and i64` | 成功（591と同形で`ICmpLt`） |
| 615 | `to_i64(x)`（x: i64） | `expected i32, found i64` | 成功・恒等。`Right ([], TyInt W64, [Push 1, Store W64 (-8), Load W64 (-8)])` |
| 618 | `to_i32(x)`（x: i32） | `expected i64, found i32` | 成功・恒等。`Right ([], TyInt W32, [Push 1, Store W32 (-4), Load W32 (-4)])` |
| 621 | `to_i64(x)`（x: bool） | `expected i32, found bool` | エラーのまま。メッセージは`expected i32 or i64, found bool` |
| 624 | `to_i32(x)`（x: bool） | `expected i64, found bool` | エラーのまま。メッセージは`expected i32 or i64, found bool` |
| 627 | `let y: i32 = to_i64(x);`（x: i32） | `expected i32, found i64` | 成功。`… Load W32 (-4), ISext32, ISext32, Store W32 (-8), Load W32 (-8)` |
| 630 | `let y: i64 = to_i32(x);`（x: i64） | `expected i64, found i32` | 成功。`… Load W64 (-8), ISext32, ISext32, Store W64 (-16), Load W64 (-16)` |
| 642 | `let b: i64 = *a;`（a: i64） | `expected &i64, found i64` | エラーのまま。メッセージは`expected pointer, found i64` |
| 691 | `let y = x + 1i64;`（x: i32） | `i32 and i64` | 成功。yはi64に推論され、`… Load W32 (-4), ISext32, Push 1, IAdd W64, Store W64 (-12), Load W64 (-12)` |
| 733 | `f(x)`（仮引数i64、x: i32） | `expected i64, found i32` | 成功。暗黙main側は`… Load W32 (-4), ISext32, ICall "f" 1`。テスト名「暗黙変換は行わない」を改める |
| 736 | `let x: i32 = f();`（戻り値i64） | `expected i32, found i64` | 成功。`ICall "f" 0, ISext32, Store W32 (-4), Load W32 (-4)` |
| 759 | `add(1i32, 2i32)`と`add(3i64, 4i64)` | `i32 and i64` | 成功（省略された仮引数はi64に合流して推論される）。テスト名を改める |
| 911（Integration） | `let x: i64 = 9999i32;\nx` | コンパイルエラー | 実行結果`"9999"`を検証するテストへ書き換える |

### 7.3 変化しないことを確認する既存テスト（回帰確認として維持）

- **bool・ポインタ関連のエラー**: 556・559（bool↔整数リテラル）、571（`let x: bool = 9999i64`。`coerce`でも同じメッセージ）、580（`true + 1`）、583〜589（否定・比較結果の文脈）、603〜613（bool同士の大小比較）、633（`to_i64(x) == true`）、639・651（`&`の結果の誤用、ポインタの加算）、688（`let x = 5 + true`）、if/whileの条件式のエラー。いずれもメッセージを含めて不変
- **命令列が完全一致するテスト**: 574（`9999i32 + 5`は`opTy`がi32になり変換無し）、664〜684（`let`の型注釈省略、`*p`のDeref、`f()`の戻り値）、raw ASTの`ToI32 (Lit 64)`（子はリテラルのみなのでi64とみなされ、従来と同じ`Push 64, ISext32`）
- **Integrationの`to_i64`/`to_i32`**（964〜974）: 値保存・縮小のラップアラウンド・括弧なし構文・ラウンドトリップ。命令列が従来と同一になるため結果も不変
- **fnシグネチャ推論**: 自己再帰・循環検出（762〜766）、恒等関数・ポインタ引数からの推論

### 7.4 追加するテスト

意味論（compile、命令列の検証）:

- i32同士の演算結果をi64へ代入: `let a: i32 = 1;\nlet b: i32 = 2;\nlet z: i64 = a * b;\nz` → `IMul W32`の後に`ISext32`
- 単項マイナスの拡大: `let a: i32 = 1;\nlet z: i64 = -a;\nz` → `Load W32 (-4), INeg W32, ISext32`
- fn末尾式の縮小: `fn f(a: i64) -> i32 {\na\n}\nf(1)` → fn本体が`StoreArg 0 W64 (-8), Load W64 (-8), ISext32, Label …`
- `return`文の変換: `fn f(a: i32) -> i64 {\nreturn a;\n}\nf(1)` → `Load W32 (-4), ISext32, Jmp …`
- `&i32`の参照先をi64の文脈で使う: `let a: i32 = 7;\nlet p: &i32 = &a;\nlet z: i64 = *p;\nz` → `LoadInd W32, ISext32`
- ポインタ型は変換されない: `let a: i32 = 1;\nlet p: &i64 = &a;\np` → `type mismatch: expected &i64, found &i32`
- 異なるポインタ型の比較はエラー: `&i32`と`&i64`の`==` → `type mismatch: &i32 and &i64`
- boolは変換されない: `let x: i64 = true;\nx` → `type mismatch: expected i64, found bool`
- 恒等関数の推論: `fn id(x) {\nx\n}\nlet a = id(1i32);\nid(2i64)` → 成功（仮引数・戻り値ともi64）

Integration（gccでコンパイルして実行）:

- 縮小のラップアラウンド: `let x: i64 = 4294967301;\nlet y: i32 = x;\ny` → `5`
- 拡大の符号保存: `let x: i32 = -5;\nlet y: i64 = x;\ny` → `-5`
- i32演算のオーバーフロー後の拡大: `let a: i32 = 2147483647;\nlet z: i64 = a + 1;\nz` → `-2147483648`
- 混在演算の昇格（オーバーフローしない）: `let a: i32 = 2147483647;\nlet b: i64 = 1;\na + b` → `2147483648`（末尾式はi64として`%ld`で出力される）
- 関数の実引数の拡大と戻り値の縮小: `fn f(a: i64) -> i32 {\na + 1\n}\nlet x: i32 = 41;\nf(x)` → `42`
- `&i32`の参照先をi64の演算で使う: `let a: i32 = 7;\nlet p: &i32 = &a;\nlet b: i64 = 3;\n*p + b` → `10`

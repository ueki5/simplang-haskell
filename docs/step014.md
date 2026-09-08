# 変数宣言の型注釈省略と型推論の導入における設計上の考慮点

これまで`let`文は`let IDENT : type = expr ;`の形で型注釈が必須だった。今回は型注釈を省略できるようにし（`let IDENT = expr ;`）、省略時は式から型を推論する。推論しても最後まで型が確定しない場合（無型の整数リテラルのみで構成される式など）は`i64`をデフォルトとする。この規則は`compileProgram`が暗黙main末尾式の型を決定するために**既に**使っている`inferType`（後述）と全く同じであり、今回新しい推論ロジックを追加する必要はない。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 追加する構文 | `let`文のみ、型注釈`': type'`を省略可能にする（例: `let x = 5;`） |
| 対象外 | `fn`の仮引数・戻り値型の注釈は引き続き必須（`let`文のみが対象） |
| 推論規則 | 既存の`inferType`（`inferMaybeType`の結果が`Nothing`なら`i64`）をそのまま再利用する。新しい推論ロジックは追加しない |
| ASTノード | `SLet`のシグネチャは変えず、新しいコンストラクタ`SLetInferred String Expr`を追加する（`SLet`は`test/Spec.hs`内で多数参照されており、シグネチャ変更（`Type`→`Maybe Type`）は既存テストを広範囲に破壊するため採らない。`docs/step013.md`で`Lit`に対して採った方針と同じ） |
| 構文上の判別 | `let`の次のIDENTの直後が`':'`か`'='`かで`SLet`/`SLetInferred`を判別する（先読み1トークン） |

## 1. `docs/step013.md` からの変更点（要約）

| ファイル / 項目 | step013まで | step014での変更 |
|---|---|---|
| `Token` / `tokenize` | — | 変更なし（`':'`と`'='`は既存トークン） |
| `Stmt` | `SLet String Type Expr` | `SLetInferred String Expr` を追加（`SLet`はそのまま） |
| `parseLetStmt` | `IDENT ':' type '=' expr ';'` のみ受理 | IDENTの直後が`TColon`なら従来通り`SLet`、そうでなければ`TAssign`直結を`SLetInferred`として受理 |
| `compileStmtsFrom`（`step`） | `SLet`: 宣言型`ty`を`expected`として`compileExprTyped`へ渡す | `SLetInferred`: `ty <- inferType fnSigs env expr`で型を決定した上で、以降（二重定義チェック・オフセット計算・`compileExprTyped`・`Store`生成）は`SLet`と完全に同じ処理を行う |
| `inferType` / `inferMaybeType` | 暗黙main末尾式の型決定にのみ使用 | 変更なし。`SLetInferred`から再利用するのみ |
| `codegen` / `run`（VM） | — | 変更なし。`SLetInferred`は型解決後は`SLet`と同一の命令列（`compileExprTyped`の結果＋`Store`）を生成するため、命令セットに新規の種類は増えない |

## 2. なぜ新しい推論ロジックが不要か

`inferType`・`inferMaybeType`（`src/Compiler.hs`）は、`compileProgram`が暗黙main末尾式の型を決めるために既に次のように使われている：

```haskell
finalType <- lift (inferType fnSigs env expr)
exprInstrs <- lift (compileExprTyped fnSigs env finalType expr)

-- 最後まで未確定なら i64 をデフォルトとする。
inferType :: FnSigs -> Env -> Expr -> Either String Type
inferType fnSigs env expr = maybe (TyInt W64) id <$> inferMaybeType fnSigs env expr
```

`inferMaybeType`は`Var`・`LitTyped`・`BoolLit`・`AddrOf`・`Deref`・`Call`・算術演算（`combine`による子の単一化）など、既存の全ての式ノードに対して型推論を提供済みである（`docs/step002〜013`で各ノードが追加されるたびに更新されてきた）。`let`文の型注釈省略は「型注釈から得ていた`Type`を、この既存関数の戻り値で置き換える」だけの変更であり、`SLet`と`SLetInferred`は型の出所（構文 vs 推論）が違うだけで、その後の処理（二重定義チェック・スタックオフセット計算・`compileExprTyped`・`Store`生成）は完全に共通である：

```haskell
step (env, cursor, acc) (SLetInferred name expr) = do
  when (declaredLocally name env) $ lift (Left ("variable already declared: " ++ name))
  ty <- lift (inferType fnSigs env expr)
  instrs <- lift (compileExprTyped fnSigs env ty expr)
  let off = cursor - widthBytes (storageWidth ty)
  pure (insertVar name (off, ty) env, off, acc ++ instrs ++ [Store (storageWidth ty) off])
```

## 3. 副次的に生じる挙動

- `let x = 5;`：無型リテラルのみ→`inferMaybeType`が`Nothing`→`inferType`が`i64`にデフォルト
- `let x = 5i32;`：サフィックスにより`i32`確定
- `let x = true;`：`BoolLit`により`bool`確定
- `let x = &y;` / `let x = *p;`：`AddrOf`/`Deref`の推論（`addressOf`委譲）により、ポインタ型・その指す先の型がそのまま決まる
- `let x = f();`：呼び出し先のシグネチャの戻り値型で確定
- `let x = 5 + true;`：`inferMaybeType`の`combine`はBool/Intの不一致自体を検出しない（`unifyMaybeType`は`Nothing`と`Just TBool`を素直に`Just TBool`へ単一化する）ため、この時点ではエラーにならず`ty = TBool`が決まる。しかし続く`compileExprTyped fnSigs env TBool (Add (Lit 5) (BoolLit True))`は`TBool`期待での`Add`を拒否するため、結局`type mismatch: expected bool, found arithmetic expression`になる。これは暗黙main末尾式で既に起きている挙動（`inferType`→`compileExprTyped`の二段構え）をそのまま再利用したものであり、新規のエラー経路を追加したわけではない

## 4. 明示的なスコープ外

- `fn`の仮引数・戻り値型の注釈は今回のスコープ外。従来通り`':' type`が必須のまま変更しない（呼び出し規約・スタックスロット割り付けが宣言順に依存するため、引数の型推論は別途の設計を要する）
- `SAssign`（再代入）は元々型注釈を持たず、常に既存変数の型を`expected`として使うため、今回の変更は影響しない

## 5. テストへの影響

- Parser: `let x = 5;`が`SLetInferred "x" (Lit 5)`にパースされること、`let x: i32 = 5;`が従来通り`SLet`のまま変化しないこと（コロン有無の分岐の両方を確認）
- 意味論（compile）: 型注釈省略時の各種推論パターン（無型リテラルの`i64`デフォルト、サフィックス、bool、ポインタ、関数呼び出し、算術演算での型混在）の成功例、および型不一致（bool+int等）がエラーになる例
- Integration: 型注釈なしの`let`を含むプログラムをgccでコンパイル・実行し、出力値が期待通りであることを確認（`i64`デフォルト時は`%ld`で出力されることも含む）

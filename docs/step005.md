# コードブロック `{}`（変数スコープ）の導入における設計上の考慮点

現在の `Program = ([Stmt], Expr)` はフラットな「文の列 + 末尾式」であり、`let` で宣言した変数はプログラム全体で常に可視である。今回はこれに `{}` によるネストしたブロックを導入し、ブロック内で `let` した変数がブロックの外から参照できないようにする。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| ブロックの種別 | **文（Stmt）** として導入する。値を返さない。式（`factor`）としては扱わない |
| 構文 | `block-stmt ::= '{' stmt* '}'`。`stmt` は `let-stmt` / `assign-stmt` / `block-stmt` のいずれか |
| 空ブロック | `{}` は許可する（何もしない文として合法） |
| ブロックの末尾 | 式を書く必要はない（`Program` の末尾式必須ルールはトップレベルのみに適用され、ブロックには適用しない） |
| スコープ | ブロック内で `let` した変数は、ブロックを抜けると外側から参照不可（`undeclared variable` エラー） |
| シャドーイング | **許可する**。ブロック内で外側と同名の `let` をしてもエラーにならず、ブロックを抜けると外側の束縛が復元される |
| 同一ブロック内の再宣言 | 禁止のまま（同じブロック内で同名を2回 `let` するのは従来通りコンパイルエラー） |
| ブロック内からの代入 | 外側スコープの変数へ `SAssign` で書き込み可能（ブロックが外へ影響を与える唯一の手段） |
| スタックオフセット | ブロックを抜けたら消費したオフセットを巻き戻し、後続の兄弟文で再利用する |
| `Env` の表現 | フラットな `Map String (Int, Type)` から `[Map String (Int, Type)]`（スコープのスタック、先頭が最内側）に変更する |

## 1. 文法・構文（Lexer / Parser）

- **トークン追加**: `TLBrace`（`{`）、`TRBrace`（`}`）
- **文法**:
  ```
  program    ::= stmt* expr
  stmt       ::= let-stmt | assign-stmt | block-stmt
  block-stmt ::= '{' stmt* '}'
  ```
- **AST追加**: `Stmt` に `SBlock [Stmt]` を追加する。
  ```haskell
  data Stmt
    = SLet String Type Expr
    | SAssign String Expr
    | SBlock [Stmt]
  ```
- **`parseStmts` の拡張**: 先頭が `TLBrace` の場合はブロック文としてパースする分岐を追加する。
  - `{` を消費 → 再帰的に `parseStmts` で内部の文列を取得 → `}` を `expectToken` で消費（既存の `parseFactor` の `TLParen`/`TRParen` 処理と同じパターン）
  - 内部の文列の後ろに `Expr` は続かない（ブロックは値を返さないため、`parseEquality` を呼ぶ必要がない）
  - 2トークン先読みで `TLet` / `TIdent ':' TAssign` / `TLBrace` を判別する既存の分岐構造がそのまま拡張できるため、パーサ本体への影響は小さい

## 2. 意味論：スコープとシャドーイング

- **`Env` をスコープのスタックに変更する**: `type Env = [Map String (Int, Type)]`（先頭が最内側のスコープ）。トップレベルの `compile` は単一要素のリスト `[Map.empty]` から開始する。
  ```haskell
  type Env = [Map String (Int, Type)]

  lookupVar :: String -> Env -> Maybe (Int, Type)
  lookupVar _ [] = Nothing
  lookupVar name (scope : rest) =
    case Map.lookup name scope of
      Just v  -> Just v
      Nothing -> lookupVar name rest

  declaredLocally :: String -> Env -> Bool
  declaredLocally name (scope : _) = Map.member name scope
  declaredLocally _ [] = False

  insertVar :: String -> (Int, Type) -> Env -> Env
  insertVar name v (scope : rest) = Map.insert name v scope : rest
  insertVar _ _ [] = []
  ```
  「このブロックで直接宣言済みか」（`declaredLocally`＝先頭スコープだけを見る）と「今どの名前が参照できるか」（`lookupVar`＝先頭から順に探す）が同じ1つの構造から素直に導けるため、フラットな `Map` 案で必要だった「宣言済み名前集合を表す `Set` を並行して持つ」という付随的な仕組みが不要になる。
- **`compileExprTyped`／`inferMaybeType` の `Var` ケース**: `Map.lookup name env` を `lookupVar name env` に置き換える（呼び出し側のシグネチャ・呼び出し方は変わらない）。
- **`compileStmts` の一般化**: 現状 `compileStmts :: [Stmt] -> Either String (Env, [Instr])` は常に `(Map.empty, 0, [])` から `foldM` を開始しているが、これを任意の `(Env, Int)` から開始できる形に一般化する。
  ```haskell
  compileStmtsFrom :: Env -> Int -> [Stmt] -> Either String (Env, Int, [Instr])

  compileStmts :: [Stmt] -> Either String (Env, [Instr])
  compileStmts stmts = do
    (env, _cursor, instrs) <- compileStmtsFrom [Map.empty] 0 stmts
    Right (env, instrs)
  ```
  既存の `compile` からの呼び出しは無変更で済む。
- **`SLet` のコンパイル**（二重宣言チェックとシャドーイング）:
  ```haskell
  step (env, cursor, acc) (SLet name ty expr) = do
    when (declaredLocally name env) $ Left ("variable already declared: " ++ name)
    instrs <- compileExprTyped env ty expr
    let off = cursor - widthBytes (storageWidth ty)
    Right (insertVar name (off, ty) env, off, acc ++ instrs ++ [Store (storageWidth ty) off])
  ```
  `declaredLocally` は先頭スコープのみを見るため、外側と同名の `let` は自然に許可され（シャドーイング）、同一ブロック内の再宣言だけが従来通りエラーになる。`insertVar` も先頭スコープにのみ挿入するため、シャドーイングした変数は先頭スコープを pop すれば自動的に消える。
- **`SBlock` のコンパイル**: 内部の文列を「先頭に空スコープを1つ push した `env`」と、呼び出し時点の `cursor` から再帰的にコンパイルし、返ってきた `Env`/`cursor` は**破棄して呼び出し前の値をそのまま継続に使う**（＝先頭スコープを pop したのと同じ結果になる）。
  ```haskell
  step (env, cursor, acc) (SBlock innerStmts) = do
    (_, _, instrs) <- compileStmtsFrom (Map.empty : env) cursor innerStmts
    Right (env, cursor, acc ++ instrs)
  ```
  この「返り値の `Env`/`cursor` を捨てる」という1点だけで、スコープアウト（ブロック内変数が外から見えなくなる）とオフセット巻き戻し（後述）の両方が同時に実現できる。
- **ブロック内からの代入**: `SAssign` の処理は `lookupVar` に置き換わる以外は変更不要。渡された `env`（内側ブロックからは外側の束縛も含めて見える）から先頭スコープ→外側の順に探すだけで、外側スコープの変数への書き込みが自然に成立する。

## 3. スタックオフセットの巻き戻し

- `cursor`（次に割り当てる `%rbp` 相対オフセットを決めるための単調カウンタ）も `Env` と同様に、ブロックへ入る前の値を `compileStmtsFrom` に渡し、ブロックを抜けたら呼び出し前の値へ戻す。
- これにより、時間的に重ならない兄弟ブロック（同じ親スコープ内で順番に実行される複数のブロック）は同じスタックオフセット範囲を再利用できる。ループや再帰、クロージャによる変数の寿命延長がこの言語には存在しないため、「ある時点でスコープを抜けた変数のオフセットを、後続の文が再利用する」ことによるエイリアシング事故は起こり得ない（実行はプログラムテキスト上の順序どおりに1回だけ進む）。
- 一方、内部で発行された `Load`/`Store` 命令列そのものは（オフセットが再利用された場合でも）そのまま `Instr` の平坦なリストに残り続けるため、`CodeGen.frameSize` の「全命令中の最大絶対オフセットをスキャンする」ロジックは変更なしで正しく動作する。オフセット巻き戻しの効果は「フレームサイズが不要に大きくならない」という最適化としてのみ現れる。

## 4. コード生成（`src/CodeGen.hs`）・VM（`run`）・`app/Main.hs`

- いずれも変更不要。`SBlock` は最終的に既存の `Instr` の平坦なリストへ展開されるため、`codegen`／VM は「ブロックだったかどうか」を意識する必要がない。`compile` のトップレベルシグネチャ（`Program -> Either String (Type, [Instr])`）も変わらないため `app/Main.hs` への影響もない。

## 5. `docs/step004.md` からの変更点まとめ

| ファイル / 項目 | step004まで | step005での変更 |
|---|---|---|
| `Token` | — | `TLBrace`、`TRBrace` を追加 |
| `tokenize` | — | `{` → `TLBrace`、`}` → `TRBrace` |
| 文法 | `stmt ::= let-stmt \| assign-stmt` | `\| block-stmt` を追加。`block-stmt ::= '{' stmt* '}'` |
| `Stmt` | `SLet`、`SAssign` | `SBlock [Stmt]` を追加 |
| `parseStmts` | `TLet` / `TIdent ':' TAssign` の2分岐 | `TLBrace` の分岐（`parseBlock`）を追加 |
| `Env` | `Map String (Int, Type)`（フラット） | `[Map String (Int, Type)]`（スコープのスタック、先頭が最内側）に変更。`lookupVar`／`declaredLocally`／`insertVar` を新設 |
| `compileExprTyped`／`inferMaybeType` の `Var` | `Map.lookup name env` | `lookupVar name env` |
| `compileStmts` | `Map.empty, 0` 固定で `foldM` | 内部を `compileStmtsFrom :: Env -> Int -> [Stmt] -> Either String (Env, Int, [Instr])` に一般化。`compileStmts` はその `[Map.empty] 0` 起点のラッパーとして再定義 |
| 二重宣言チェック | `Map.member name env`（フラットな `env` 全体が対象） | `declaredLocally name env`（先頭スコープのみが対象。外側との同名はシャドーイングとして許可） |
| `SBlock` のコンパイル | — | `compileStmtsFrom (Map.empty : env) cursor innerStmts` で再帰コンパイルし、返り値の `Env`/`cursor` は破棄して呼び出し前の値をそのまま継続に使う（スコープアウトとオフセット巻き戻しを同時に実現） |
| `CodeGen` / `run` / `app/Main.hs` | — | 変更なし |

## 6. テストへの影響

- **Tokenizer**: `{`/`}` の字句化
- **Parser**: 空ブロック、ネストしたブロックのAST、`{` に対応する `}` が無い場合のエラー（既存の `expected closing parenthesis` に倣ったメッセージ）
- **意味論エラー（compile）**:
  - ブロックを抜けた後に内部で宣言した変数を参照 → `undeclared variable`
  - 同一ブロック内での同名 `let` の再宣言 → `variable already declared`
  - 外側と同名の `let` をブロック内で行っても**エラーにならない**ことの確認（シャドーイングが正常系であることの回帰テスト）
- **`run`（VM）**: `SBlock` はVM側には存在しない（コンパイル時に平坦化されるため）ため、VM自体への追加ケースは不要
- **結合テスト**:
  - ブロック内の `let` が外側の同名変数へ代入で反映されること（`{ x = 1; }` パターン）
  - シャドーイングされた変数がブロックを抜けると外側の値に戻ること
  - 兄弟ブロックがそれぞれ独立にローカル変数を持てること

## 7. 未決定事項

現時点で残っている未決定事項はない。ブロックを式として扱う拡張（`let x: i32 = { ... };` のように値を返すブロック）は将来の検討課題として `docs/step000.md` の設計ノートには含めず、必要になった時点で別途仕様化する。

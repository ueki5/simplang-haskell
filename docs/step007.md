# `while` / `break` / `continue` の導入における設計上の考慮点

`docs/step006.md` で導入した `if`/`else if`/`else` に続き、今回は繰り返し `while 条件式 {文}` と、ループ制御のための `break`/`continue` を導入する。`docs/step006.md` §7 が「ラベル採番の仕組み（`StateT Int (Either String)`）はループの後方分岐にもそのまま転用できる見込み」と明記していた通り、`Jmp`/`JmpIfZero`/`Label` は無変更で再利用でき、`CodeGen.hs` への変更は不要だった。新規に必要になったのは、`break`/`continue` を直近の外側ループへ正しく解決するための「ループコンテキスト」のスレッディングのみである。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| `while` の種別 | **文（Stmt）** として導入する。値を返さない（`if` と同様、値を返す `while` は今回のスコープ外） |
| 構文 | `while-stmt ::= 'while' expr '{' stmt* '}'`。`for` のような増分節は持たない |
| 条件式の型 | 常に `bool` を要求する（`compileExprTyped env TBool 式` で検査。`if` と同じ仕組みをそのまま再利用） |
| 比較演算子 | **今回のスコープ外**。既存の `==`/`!=` と算術演算のみで条件式を書く前提（`if` 導入時点でも未着手であり、今回も据え置き） |
| ループ本体のスコープ | `SBlock`/`if`分岐と同じく独立したスコープ（`Map.empty` push）としてコンパイルする。本体を抜けると内部で宣言した変数は不可視になり、スタックオフセットも巻き戻る（1回のコード生成で決まったオフセットが実行時に毎イテレーション再利用される） |
| `break`/`continue` | ループを持たない値なしの文。`break;` はループを抜け、`continue;` は本体の残りをスキップして条件式の再評価へ進む（増分節が無いため、次のイテレーションは常に条件再チェックから始まる） |
| `break`/`continue` の構文末尾 | `let`/代入文と同様に `;` を必須とする（block文/if文/while文は `}` で終わるため不要という既存の非対称性を維持） |
| `break`/`continue` のスコープ規則 | 直近の外側ループへ解決する。`if`/`{}` にネストしていても外側ループを見失わない。ループの外（トップレベルや、ループを含まない `if`/ブロックの中）で使うとコンパイルエラー |
| ループコンテキストの実装方式 | `Env`/`cursor` と同じ**普通の関数引数**として `LoopCtx = Maybe (String, String)` を `compileStmtsFrom`/`compileIf` に追加する（詳細は3節） |

## 1. 文法・構文（Lexer / Parser）

- **トークン追加**: `TWhile`（`while`）、`TBreak`（`break`）、`TContinue`（`continue`）。`tokenize` の識別子分岐に `if`/`else` と同じパターンで追加
- **文法**:
  ```
  stmt          ::= let-stmt | assign-stmt | block-stmt | if-stmt | while-stmt | break-stmt | continue-stmt
  while-stmt    ::= 'while' expr '{' stmt* '}'
  break-stmt    ::= 'break' ';'
  continue-stmt ::= 'continue' ';'
  ```
- **AST追加**:
  ```haskell
  data Stmt
    = SLet String Type Expr
    | SAssign String Expr
    | SBlock [Stmt]
    | SIf [(Expr, [Stmt])] (Maybe [Stmt])
    | SWhile Expr [Stmt]
    | SBreak
    | SContinue
  ```
  `SWhile` は `if` の1分岐と同じ形（条件式 `Expr` + 本体 `[Stmt]`）。`SBreak`/`SContinue` はデータを持たない — ジャンプ先ラベルはコンパイル時にループコンテキストから解決するため、AST上では不要
- **`parseStmts` の拡張**: 先頭が `TWhile`/`TBreak`/`TContinue` の場合はそれぞれ専用のパーサへ分岐する。いずれも既存の先頭トークン判別と衝突しない
- **`parseWhileStmt`**: `parseIfBranch` と同じ「条件式（`parseEquality`）+ `{` stmt* `}`」形状を再利用する新規関数
- **`parseBreakStmt` / `parseContinueStmt`**: `parseAssignStmt` と同じ `expectToken TSemicolon` パターンで `SBreak`/`SContinue` を生成する

## 2. 意味論：条件式の型検査とスコープ

- 条件式は `if` と同じく `compileExprTyped env TBool 式` にそのまま渡すだけでよく、新規の型検査ロジックは不要
- ループ本体のコンパイルは `SBlock`/`if`分岐と同じ「先頭に空スコープを push して再帰コンパイルし、返り値の `Env`/`cursor` は破棄して呼び出し前の値をそのまま継続に使う」パターンをそのまま再利用する

## 3. コード生成：ループコンテキストのスレッディング

### 命令列

`Instr` への追加は不要。`while` は既存の `Jmp`/`JmpIfZero`/`Label` だけで次の形へ展開する。

```
Label start
cond
JmpIfZero end
本体
Jmp start
Label end
```

- `continue` は `Jmp start`（条件式の再評価へ戻る）
- `break` は `Jmp end`（ループの外へ抜ける）

### ループコンテキストのスレッディング方式

`break`/`continue` は「直近の外側ループの (continueラベル, breakラベル)」を解決できる必要がある。`if`/`{}` にネストしていても外側ループを見失ってはならない一方、ループを抜けた後の兄弟コードや、そのループの外側にある別のループへ漏れ出してもいけない。

この情報は `docs/step006.md` で整理した「cursor 対 ラベルカウンタ」の非対称性でいうと **cursor 側の性質**を持つ：

- ラベルカウンタ（`StateT Int`）は兄弟ブロック/分岐をまたいでも巻き戻してはいけない状態 → `StateT` として `Env`/`cursor` から分離管理する必要があった
- ループコンテキストは逆に、ネストしたループに入るときだけ新しい値に**差し替わり**、そのループを抜ければ（呼び出しから戻れば）自動的に元の値に戻ってよい、スコープ的な情報

したがって新しい `StateT`/`ReaderT` を追加するのではなく、`Env`/`cursor` と同じ**普通の関数引数** `LoopCtx = Maybe (String, String)` として `compileStmtsFrom`/`compileIf` に追加した。Haskell の関数引数は呼び出しごとに字句スコープされるため、「ループに入るときだけ差し替え、戻れば自動的に元に戻る」という挙動を素の引数渡しだけで実現できる（`SBlock` が `Map.empty : env` を子呼び出しにだけ渡して戻り値を握りつぶす扱いと同型）。`ReaderT` を使っても `SWhile` の呼び出し箇所で明示的な `local` が必要になり、`StateT` ではむしろ手動の退避・復元が必要になってしまい、いずれも素の引数渡しより複雑になるだけで得るものがない。

```haskell
type LoopCtx = Maybe (String, String)  -- (continueLabel, breakLabel)

compileStmtsFrom :: Env -> Int -> LoopCtx -> [Stmt] -> CompileM (Env, Int, [Instr])
-- SBlock/SIf の再帰呼び出しは loopCtx をそのまま渡す（ネストしても外側ループを見失わない）
-- SWhile は compileWhile に委譲
-- SBreak/SContinue は loopCtx を見て Jmp を生成し、Nothing なら
--   lift (Left "break used outside loop") / lift (Left "continue used outside loop")

compileIf :: Env -> Int -> LoopCtx -> [(Expr, [Stmt])] -> Maybe [Stmt] -> CompileM [Instr]
-- 既存ロジックは不変、loopCtx を素通しするだけ

compileWhile :: Env -> Int -> Expr -> [Stmt] -> CompileM [Instr]
compileWhile env cursor cond body = do
  startLabel <- freshLabel "while_start"
  endLabel <- freshLabel "while_end"
  condInstrs <- lift (compileExprTyped env TBool cond)
  (_, _, bodyInstrs) <- compileStmtsFrom (Map.empty : env) cursor (Just (startLabel, endLabel)) body
  pure ([Label startLabel] ++ condInstrs ++ [JmpIfZero endLabel]
          ++ bodyInstrs ++ [Jmp startLabel, Label endLabel])
```

`compileWhile` は呼び出し元から `loopCtx` を受け取らず、常に自分自身の `Just (startLabel, endLabel)` を本体コンパイルへ渡す。これにより、ネストした `while` の本体に直接書かれた `break`/`continue` は必ずその内側ループへ解決され、外側ループへ漏れ出すことがない。一方、`SBlock`/`SIf` の本体コンパイルは呼び出された時点の `loopCtx` をそのまま渡すだけなので、ループ本体の中に `if { break; }` のようにネストしても、その `break` は正しく外側（＝直近の）ループへ解決される。

`compileStmts`（コンパイルの最上位エントリポイント）は初期 `loopCtx` として `Nothing` を渡すよう変更した。トップレベルや、ループを含まない `if`/ブロックの中で `break`/`continue` を使うと `Nothing` のまま `step SBreak`/`step SContinue` に到達し、コンパイルエラーとなる。

## 4. VM（`run`）・`app/Main.hs` への影響

- `app/Main.hs`: `compile` のトップレベルシグネチャは変わらないため変更不要
- `run`（Hspec専用の素朴なスタックマシン）: `if` と同じ前例により変更なし。`JmpIfZero`/`Jmp`/`Label` は未対応のままであり、`while`/`break`/`continue` を含むプログラムは常に `compile` → `codegen` → `gcc` の実行系パイプラインでテストする

## 5. `docs/step006.md` からの変更点まとめ

| ファイル / 項目 | step006まで | step007での変更 |
|---|---|---|
| `Token` | `TIf`、`TElse` 等 | `TWhile`、`TBreak`、`TContinue` を追加 |
| `tokenize` | — | `while` → `TWhile`、`break` → `TBreak`、`continue` → `TContinue` |
| 文法 | `stmt ::= ... \| if-stmt` | `\| while-stmt \| break-stmt \| continue-stmt` を追加 |
| `Stmt` | `SLet`、`SAssign`、`SBlock`、`SIf` | `SWhile Expr [Stmt]`、`SBreak`、`SContinue` を追加 |
| `parseStmts` | `TLet`/`TIdent ':' TAssign`/`TLBrace`/`TIf` の4分岐 | `TWhile`/`TBreak`/`TContinue` の3分岐を追加 |
| 新規パース関数 | — | `parseWhileStmt`、`parseBreakStmt`、`parseContinueStmt` |
| `compileStmtsFrom` の型 | `Env -> Int -> [Stmt] -> CompileM (...)` | `Env -> Int -> LoopCtx -> [Stmt] -> CompileM (...)`（`LoopCtx = Maybe (String, String)`）に変更。`SWhile`/`SBreak`/`SContinue` のケースを追加 |
| `compileIf` の型 | `Env -> Int -> [...] -> ... -> CompileM [Instr]` | `Env -> Int -> LoopCtx -> [...] -> ... -> CompileM [Instr]`（`loopCtx` を素通し） |
| `compileWhile` | — | 新規。`Instr` は既存の `Jmp`/`JmpIfZero`/`Label` のみで構成 |
| `compileStmts` | `compileStmtsFrom [Map.empty] 0 stmts` | `compileStmtsFrom [Map.empty] 0 Nothing stmts`（初期ループコンテキストを `Nothing` に） |
| `CodeGen.genInstr` | — | 変更なし（`JmpIfZero`/`Jmp`/`Label` は既存実装がラベル由来を問わず動作する） |
| `run`（VM） | — | 変更なし（`JmpIfZero`/`Jmp`/`Label` は未対応のまま） |
| `app/Main.hs` | — | 変更なし |
| 依存パッケージ | — | 変更なし |

## 6. テストへの影響

- **Tokenizer**: `while`/`break`/`continue` の字句化、それらで始まる識別子が `TIdent` のままであることの回帰確認
- **Parser**: while文のAST形状、本体への `let`/`break`/`continue` 混在、ブロック文としてのネスト、条件式が無い場合のエラー、閉じ波括弧が無い場合のエラー、`break`/`continue` に `;` が無い場合のエラー
- **意味論エラー（compile）**: while条件式が整数リテラル/整数変数の場合の型不一致エラー（`if` と同じエラー文言）、while本体を抜けた後の内部宣言変数の参照エラー、`break`/`continue` をループ外（トップレベル、または `if`/ブロックの中でループの外）で使った場合のコンパイルエラー
- **結合テスト**:
  - 基本的なカウントダウン/カウントアップループ、条件が最初から偽で本体が一度も実行されないケース
  - `break` による早期終了、`continue` による本体スキップと条件再評価
  - ネストした `while` で内側の `break`/`continue` が外側ループに影響しないこと
  - ループを抜けるたびに本体内で宣言した変数が不可視に戻ること（毎イテレーション巻き戻ることの確認）
  - ブロック内への `while` のネスト
  - 複数の `while` が連続しても互いに独立して動作すること（ラベルの一意性の間接的な確認）

## 7. 今回のスコープ外とした将来の検討課題

- 比較演算子（`<`、`<=`、`>`、`>=`）。現状 `==`/`!=` のみで、`if` 導入時点から据え置きの課題
- `for` 文（増分節を持つループ）
- 値を返す `while` 式
- `run`（VM）の分岐命令対応。VM経由で `while`/`if` を含むプログラムをテストする必要が生じた場合に別途対応する

# `if` / `else if` / `else` の導入における設計上の考慮点

現在の `Stmt` は `SLet` / `SAssign` / `SBlock`（`docs/step005.md`）のみで構成されており、コード生成の `Instr` には分岐命令が一切存在しない（命令列を先頭から末尾まで一直線に実行するだけ）。今回はこれに条件分岐 `if 式 {文} else if 式 {文} else {文}` を導入する。`else if` は0回以上、`else` は省略可能な唯一の末尾要素として扱う。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| `if` の種別 | **文（Stmt）** として導入する。値を返さない（`let x: i32 = if ... {...} else {...};` のような値を返す `if` は今回のスコープ外） |
| 構文 | `if-stmt ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?`。`else if` は複数連結可、`else` は末尾に高々1つ |
| 条件式の型 | 常に `bool` を要求する（`compileExprTyped env TBool 式` で検査。整数式や整数変数を渡すと型不一致エラー） |
| 各分岐のスコープ | `then` / 各 `else if` / `else` 本体は、`SBlock` と同じく独立したスコープ（`Map.empty` push）としてコンパイルする。分岐を抜けると内部で宣言した変数は不可視になり、スタックオフセットも巻き戻る |
| 分岐間のスタックオフセット再利用 | 実行時に高々1分岐しか実行されないため、`SBlock` の兄弟ブロックと同じ理屈でオフセット範囲を再利用してよい |
| 分岐ラベルの採番方式 | `StateT Int (Either String)` を新設し、ラベル用の単調カウンタをコンパイル全体で持ち回る（詳細は3節） |

## 1. 文法・構文（Lexer / Parser）

- **トークン追加**: `TIf`（`if`）、`TElse`（`else`）。`tokenize` の識別子分岐に `let`/`true`/`false` と同じパターンで追加
- **文法**:
  ```
  program    ::= stmt* expr
  stmt       ::= let-stmt | assign-stmt | block-stmt | if-stmt
  if-stmt    ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?
  ```
- **AST追加**: `Stmt` に `SIf [(Expr, [Stmt])] (Maybe [Stmt])` を追加する。リストの1要素目が `if` 本体、以降が `else if` の `(条件式, 本体)` 列、`Maybe` が `else` 本体（無ければ `Nothing`）
  ```haskell
  data Stmt
    = SLet String Type Expr
    | SAssign String Expr
    | SBlock [Stmt]
    | SIf [(Expr, [Stmt])] (Maybe [Stmt])
  ```
- **`parseStmts` の拡張**: 先頭が `TIf` の場合は `parseIfStmt` に分岐する。`if`/`let`/`TIdent ':' TAssign`/`{` はいずれも先頭トークンだけで一意に判別できるため、既存の2トークン先読み構造への影響はない
- **`parseIfStmt` / `parseIfBranch` / `parseIfRest`**: `parseIfBranch` が「条件式 + `{` stmt* `}`」を1分岐分パースし（`parseBlockStmt` の `{`…`}` 消費パターンを再利用）、`parseIfRest` が次のトークン列を見て `else if` の継続・末尾の `else`・分岐列の確定を再帰的に判別する。`else` の直後が `if`/`{` のどちらでもない場合は明示的にパースエラーとする（`else` の後で単に分岐列を確定させてしまうと、`else` が構文上見えなくなるバグになるため）

## 2. 意味論：条件式の型検査とスコープ

- 条件式は `compileExprTyped env TBool 式` にそのまま渡すだけでよく、bool以外の式を渡した場合は既存の型検査機構が自然にエラーを返す（新規の型検査ロジックは不要）
- 各分岐本体のコンパイルは `SBlock` と同じ「先頭に空スコープを push して再帰コンパイルし、返り値の `Env`/`cursor` は破棄して呼び出し前の値をそのまま継続に使う」パターンをそのまま再利用する。`if` 全体も値を返さない文であるため、外側から見た変数の状態は `if` に入る前と変わらない

## 3. コード生成：分岐命令とラベル採番

現状 `Instr` は分岐命令を持たないため、`Instr` に3つ追加する。

```haskell
data Instr
  = ...
  | JmpIfZero String  -- スタック先頭をpopしてゼロ判定し、偽（ゼロ）ならジャンプ
  | Jmp String         -- 無条件ジャンプ
  | Label String        -- ジャンプ先ラベルの定義
```

`CodeGen.genInstr` はそれぞれ `popq %rax; testq %rax, %rax; je LABEL` / `jmp LABEL` / `LABEL:` に変換する。ラベル名は GAS のローカルラベル慣習に合わせて `.L` プレフィックスを付ける（既存の `.Ldiv_zero_error` と同じ扱い）。

一つの `if` は次の形へ展開する（`else if` が続く場合は同じパターンを繰り返す）。

```
cond1
JmpIfZero next1
then1本体
Jmp end
Label next1
cond2
JmpIfZero next2
...
elseif本体
Jmp end
Label next2
else本体（無ければ何もしない）
Label end
```

### ラベルカウンタのスレッディング方式

同じ関数内に複数の `if`・ネストした `if` が現れるため、ラベル文字列はコンパイル時に一意なカウンタから生成する必要がある。ここで、既存の `cursor`（スタックオフセット用の単調カウンタ）とは性質が異なる点が設計上の要点だった。

- `cursor` は兄弟ブロック/分岐を抜けるたびに**巻き戻す**（オフセット再利用のため）
- ラベルカウンタは兄弟ブロック/分岐をまたいでも**巻き戻してはいけない**（巻き戻すとアセンブリ上でラベルが重複して壊れる）

この非対称性を素朴なタプル要素（`Env, Int cursor, [Instr]`）として持ち回ると、`SBlock` の実装（再帰呼び出しの戻り値の `Env`/`cursor` を握りつぶして呼び出し前の値を使う）をそのままコピーしてラベルカウンタまで一緒に握りつぶす事故が起きやすい。

これを避けるため、`compileStmtsFrom` の型を素朴な `Either String (...)` から `CompileM (...)`（`type CompileM = StateT Int (Either String)`）に変更し、ラベルカウンタを `StateT` の状態として `Env`/`cursor` から分離した。`StateT` の状態は `>>=` の鎖に沿って常に素通しされるため、`SBlock`/`SIf` がタプルの戻り値をどう扱おうと状態（ラベルカウンタ）だけは自動的に伝播し、握りつぶし事故が構造的に起こらなくなる。

他の実装方式（`mtl`/`transformers` を使わないグローバル `IORef`、AST上の位置からラベル名を導出する方式、`Data.Traversable.mapAccumM` を使う方式）も検討したが、「新規依存を最小限にしつつ、ミスが起きにくい構造にする」というこのコードベースの方針に照らして `StateT`（`transformers`、GHCのブートライブラリで新規ダウンロード不要）を採用した。

- **`freshLabel`**: `get`/`put` でカウンタをインクリメントしつつ `".L" ++ prefix ++ show n` を返す
- **`compileExprTyped`**: 条件式のコンパイル自体は分岐を持たない純粋な `Either String [Instr]` のままでよく（式の評価に制御フローは不要）、`CompileM` 側から `lift` して合成する
- **`compileStmts`**: 最終的に `evalStateT (compileStmtsFrom [Map.empty] 0 stmts) 0` でラベルカウンタを0から開始して実行し、結果は変更前と同じ `Either String (Env, [Instr])` に戻す。`compile` のトップレベルシグネチャは変更不要

## 4. VM（`run`）・`app/Main.hs` への影響

- `app/Main.hs`: `compile` のトップレベルシグネチャは変わらないため変更不要
- `run`（Hspec専用の素朴なスタックマシン）: **`JmpIfZero`/`Jmp`/`Label` を実装していない**。VMは現状、`compile` を通さず `Instr` のリストを直接渡す単純な算術式のテスト専用に使われており（`test/Spec.hs` の「run（VM）」セクション）、`if` を含むプログラムは常に `compile` → `codegen` → `gcc` の実行系パイプラインでテストする。分岐命令付きの `Instr` 列を `run` に直接渡した場合は現状 `"stack underflow"` を返す未対応の組み合わせであり、VMで制御フローが必要になった時点で別途対応する

## 5. `docs/step005.md` からの変更点まとめ

| ファイル / 項目 | step005まで | step006での変更 |
|---|---|---|
| `Token` | — | `TIf`、`TElse` を追加 |
| `tokenize` | — | `if` → `TIf`、`else` → `TElse` |
| 文法 | `stmt ::= let-stmt \| assign-stmt \| block-stmt` | `\| if-stmt` を追加。`if-stmt ::= 'if' expr '{' stmt* '}' ('else' 'if' expr '{' stmt* '}')* ('else' '{' stmt* '}')?` |
| `Stmt` | `SLet`、`SAssign`、`SBlock` | `SIf [(Expr, [Stmt])] (Maybe [Stmt])` を追加 |
| `Instr` | — | `JmpIfZero String`、`Jmp String`、`Label String` を追加 |
| `parseStmts` | `TLet` / `TIdent ':' TAssign` / `TLBrace` の3分岐 | `TIf` の分岐（`parseIfStmt`）を追加 |
| `compileStmtsFrom` の型 | `Env -> Int -> [Stmt] -> Either String (Env, Int, [Instr])` | `Env -> Int -> [Stmt] -> CompileM (Env, Int, [Instr])`（`CompileM = StateT Int (Either String)`）に変更。ラベルカウンタを状態として持ち回る |
| `compileStmts` | `compileStmtsFrom [Map.empty] 0 stmts` を直接 `Either` として評価 | `evalStateT (compileStmtsFrom [Map.empty] 0 stmts) 0` でラベルカウンタ0から実行し `Either String (Env, [Instr])` に戻す |
| `CodeGen.genInstr` | — | `JmpIfZero`/`Jmp`/`Label` を `popq+testq+je` / `jmp` / ラベル定義行に変換する分岐を追加 |
| `run`（VM） | — | 変更なし（`JmpIfZero`/`Jmp`/`Label` は未対応のまま） |
| `app/Main.hs` | — | 変更なし |
| 依存パッケージ | — | `transformers`（GHCブートライブラリ）を `library` の `build-depends` に追加 |

## 6. テストへの影響

- **Tokenizer**: `if`/`else` の字句化、`if`/`else` で始まる識別子が `TIdent` のままであることの回帰確認
- **Parser**: `else` 省略、`else if` の複数連結、`if` 本体への `let` 文の混在、`if` 文のブロック文としてのネスト、条件式が無い場合のエラー、閉じ波括弧が無い場合のエラー、`else` の後に `if`/`{` のどちらも無い場合のエラー、`else` で入力が終わる場合のエラー
- **CodeGen**: `JmpIfZero`/`Jmp`/`Label` それぞれのアセンブリ変換
- **意味論エラー（compile）**: 条件式が整数リテラル/整数変数の場合の型不一致エラー、`if`/`else if` 双方での条件式型検査、`if` 本体を抜けた後の内部宣言変数の参照エラー
- **結合テスト**:
  - `if`/`if-else`/`else if` 連鎖（最初に真になった分岐だけが実行されること）/ 条件が全て偽で `else` も無い場合に何も実行されないこと
  - `if` のネスト、ブロック内への `if` のネスト
  - 各分岐が独立したローカル変数を持てること（同名変数の再利用を含む）、分岐を抜けるとその中の変数が不可視に戻ること
  - 複数の `if` が連続しても互いに独立して動作すること（ラベルの一意性の間接的な確認）

## 7. 今回のスコープ外とした将来の検討課題

今回のif/else if/elseの実装に必要な設計判断（条件式の型検査、各分岐のスコープ、ラベル採番方式）はすべて決定済みであり、残っている「未決定事項」はない。以下は今回とは別の機能として意図的にスコープ外にしたもので、必要になった時点で別途仕様化する。

- 値を返す `if` 式（`let x: i32 = if ... {...} else {...};`）。`docs/step005.md` の「値を返すブロック」と同様の扱い
- `run`（VM）の分岐命令対応。VM経由で `if` を含むプログラムをテストする必要が生じた場合に別途対応する
- ループ（`while`/`for`）。ただしラベル採番の仕組み（`StateT Int (Either String)`）はループの後方分岐（ループ先頭へ戻るジャンプ）にもそのまま転用できる見込み

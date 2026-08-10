# コードブロック／`if`／`while`の式化における設計上の考慮点

`docs/step007.md` §7・`docs/step008.md` §7 が「今回のスコープ外」として据え置いていた「値を返す `while` 式」を出発点に、今回は `{}`ブロック・`if`・`while`をまとめて「式（`Expr`）」へ格上げする。これにより `let x: i64 = if c { 1 } else { 2 };` のような書き方が可能になり、末尾に式を持たないブロック／プログラムは `()`（unit）を返す。関数定義がこの言語にまだ存在しないため、「早期リターン（`return`）」は「何から」戻るのかを意味づける土台がなく、本ドキュメントでは意図的に見送り、Future Workにのみ記載する。

このドキュメントは実装そのものではなく、**既存実装への影響と設計上の考慮点の洗い出し**（設計分析）である。今回のセッションではソースコード（`src/`, `test/`, `app/`）は変更しない。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| block/if/whileの種別 | **式（Expr）**。末尾に式があればその値、無ければ`()`（unit）を返す |
| 文の位置に置かれたblock/if/whileの値の扱い | 文の位置（末尾式でない場所）で使う場合、本体の型は`()`でなければならない。`i64`等の値を持つblock/if/whileを文として書くと型不一致エラーとする。値を型を問わず暗黙に破棄する挙動（Rust本体の実際の規則）は**採用しない**——既存の「曖昧さを許さず型不一致は即エラー」という設計方針（i32/i64の暗黙変換なし等）との一貫性を優先する |
| プログラム全体の末尾式 | 任意化する。末尾に式が無いプログラムは`()`として成功し、画面には空文字列を表示する（現行の「末尾式必須」という制約からの変更） |
| `return` | **今回のスコープ外**。関数定義が無く「関数からの早期return」を意味づける土台が無いため、導入は関数定義が入る将来のstepへ先送りする |
| elseを持たない`if`を値が必要な文脈で使う場合の型規則 | Rust方式：then節の型が`()`である場合のみ許可し、それ以外はelse必須とする。文の位置では上記の規則により本体はもともと`()`が要求されるため、else無しifは文としては常に書ける |
| `while`式の型 | 常に`()`固定。ループ本体は0回以上実行され単一の値を持ちようがなく、breakに値を持たせる仕組みも導入しない（step007/008から引き続き据え置き） |

上記により、`return`を除いた本質は「block/if/whileの本体を、プログラム全体と同じ形（文の列＋任意の末尾式）に一般化し、式として使えるようにする」ことに絞られる。

## 1. 文法・構文（Lexer / Parser）

- **トークン追加**: なし。`{`/`if`/`while`はいずれも既存トークン（`TLBrace`/`TIf`/`TWhile`）をそのまま使う
- **本体の形の一般化**: 現在、`Program = ([Stmt], Expr)`（末尾式は必須）に対し、`SBlock [Stmt]` / `SIf`・`SWhile`の各分岐本体は素の`[Stmt]`（末尾式を持てない）。この非対称性が今回解消すべき核心。`Program`と同じ「文の列＋任意の末尾式」を表す型を導入し、ブロック本体・if各分岐・while本体・Program全体すべてで再利用する：
  ```haskell
  type Block = ([Stmt], Maybe Expr)   -- stmt* expr?（Programの末尾式を任意化した形と同一）
  type Program = Block
  ```
- **`Expr`への追加**:
  ```haskell
  data Expr
    = ... 既存 ...
    | EBlock Block
    | EIf [(Expr, Block)] (Maybe Block)
    | EWhile Expr Block
  ```
  `if`の分岐は現状 `[(Expr, [Stmt])]` だが、各本体が `Block` になる点以外は既存の分岐構造をそのまま踏襲できる
- **`Stmt`の再編**: `SBlock`/`SIf`/`SWhile`は「文の位置に置かれた式（値は`()`でなければならない）」という単一の概念に還元できる
  ```haskell
  data Stmt
    = SLet String Type Expr
    | SAssign String Expr
    | SExpr Expr    -- 文法上はEBlock/EIf/EWhileのみがここに現れる。型検査で()を強制する
    | SBreak
    | SContinue
  ```
  `SExpr`は一般の式文（Rustでいう`ExpressionStatement`）だが、パーサーが`TLBrace`/`TIf`/`TWhile`から始まる場合にのみ生成するため、実質的にblock/if/while専用のまま。将来、関数呼び出し等の副作用式が入った場合に再利用できる形になる
- **`Type`への追加**: `data Type = TyInt Width | TBool | TUnit`
- **新しい共通パースヘルパー**: 現行の`parse`（`parseStmts` → `parseEquality` → EOF検査）と同じパターンを、`}`終端のブロック本体にも使えるよう一般化した `parseBlockBody :: [Token] -> ParseResult Block` を新設する。「文の列を読み、残りが末尾式なしで終端（`}`またはEOF）ならNothing、そうでなければ`parseEquality`を試みて末尾式として採用する」処理であり、**終端トークンかどうかを先読みして判定する必要がある**（既存の`parseStmts`のように「知らない文頭トークンで自動的に止まる」だけでは、末尾式の有無と単なる構文エラーを区別できない）
- **`parseFactor`への追加**: 式の位置（`1 + { ... }`、`let x = if c {..} else {..};` 等）でblock/if/whileを受理できるよう、`parseFactor`に`TLBrace`/`TIf`/`TWhile`のケースを追加し、`parseBlockBody`を再利用して`EBlock`/`EIf`/`EWhile`を生成する
- **`parseStmts`側の扱い**: 既存の`TLBrace`/`TIf`/`TWhile`分岐は、同じ式パース関数を呼んだ上で`SExpr`にラップする形に変える。**セミコロン規則は現状維持**（`}`直後にセミコロン不要という既存の非対称性はそのまま）。文法上の曖昧さはない——`{`/`if`/`while`は元々このいずれの生成規則から現れる位置も排他的に決まるトークンであるため、既存の分岐ロジックで判別可能
- **else無しifの型検査**: パーサーは`EIf`の`Maybe Block`としてelse無しをそのまま許容する（構文レベルでは常に許可）。「値が必要な文脈でelse無しは`()`のときのみ許可」は意味論側で検査する

## 2. 意味論：型検査とUnit値の扱い

- **`compileExprTyped`の拡張**:
  - `EBlock`: 内部の文をこれまで通り`compileStmtsFrom`でコンパイルしつつ、末尾式があれば`expected`型で、なければ`expected`が`TUnit`であることを要求する（`Nothing`かつ`expected /= TUnit`は型不一致エラー）
  - `EIf`: 各分岐の`Block`を同じ`expected`型で検査する。else無しの場合は`expected == TUnit`を要求する（Rust方式）
  - `EWhile`: while式自体の型は常に`TUnit`固定（`expected /= TUnit`なら型不一致エラー）。body自身のBlockの末尾式も、実行されても使われないので`TUnit`を要求する
- **Unit値のスタック表現**: 現行の`compileExprTyped`は「あらゆる式は評価後にスタックへちょうど1値残す」という一様な不変条件の上に成り立っている（二項演算は常に2値popして1値push、等）。Unit値もこの不変条件を崩さないために、**ダミー値（例：`Push 0`）を1つpushする形で統一する**ことを推奨する。利点は`compileExprTyped`のどのケースも「型ごとに場合分けしてpushする/しない」という特殊化が不要になり、既存のシンプルな再帰構造を壊さない点。代案（Unit値を本当に0バイトでpushしない）は効率はよいが、算術演算など既存のよくテストされたコード生成パスに「オペランドの型によってpop数が変わる」という新しい分岐を持ち込むことになり非推奨
- **文の位置での破棄**: `SExpr e`をコンパイルする際、`compileExprTyped env TUnit e`で`()`型を強制した上で、ダミー値が1つスタックに残るため、**明示的に破棄する新しい命令が必要**（現行の`Instr`にはpopして捨てるだけの命令が存在しない）。`IDiscard`のような命令を追加し、`SExpr`のコンパイル結果の末尾に付与する
- **スコープ・break/continueへの影響**: `SBlock`/`SIf`/`SWhile`が担っていた「独立スコープでコンパイルし、Env/cursorの戻り値を破棄して呼び出し前の値へ巻き戻す」ロジック（`compileStmtsFrom`内の該当ケース、`compileIf`、`compileWhile`）は構造的にはそのまま維持できる。変わるのは「本体の末尾に値を残すかどうか」の1点のみで、スコープのpush/pop・オフセットの巻き戻し・`loopCtx`のスレッディング（break/continueの解決）には影響しない
- **型推論（`inferMaybeType`）への影響**: `EBlock`/`EIf`/`EWhile`の型推論ケースを追加する必要がある。特に`EIf`は分岐間の型を単一化する必要があり、既存の`unifyMaybeType`をそのまま再利用できる（`Eq`/`Neq`等ですでに使われているパターンと同型）

## 3. コード生成

- `epilogue`に`TUnit`のケースを追加する：`printf`のフォーマット文字列を使わず、空文字列＋改行を出力する（`fmt`なしで`\n`のみの文字列を`.rodata`に追加する、または`puts("")`相当の呼び出しにする）
- 新命令`IDiscard`（仮称）の`genInstr`実装：`popq %rax`のみ（結果を使わない）
- `frameSize`のオフセットスキャンには影響なし（Unit値はスタックポインタ経由のpush/popのみで`%rbp`相対オフセットを使わないため）

## 4. VM（`run`）・`app/Main.hs` への影響

- `run`（Hspec専用の素朴なスタックマシン）は現状`JmpIfZero`/`Jmp`/`Label`を実装しておらず、if/whileを含むプログラムは元々VM経由でテストされていない（step007で明示された既知のギャップ）。今回追加する`IDiscard`や`EBlock`/`EIf`/`EWhile`もVM側でのサポートは同様に対象外としてよい
- `app/Main.hs`は`compile`の返す`(Type, [Instr])`をそのまま`codegen`に渡しているだけなので、`Type`に`TUnit`が増えても変更は不要と見込まれる（実装時に要確認）

## 5. `docs/step008.md` からの変更点まとめ

| ファイル / 項目 | step008まで | step009での変更 |
|---|---|---|
| `Program` | `([Stmt], Expr)`（末尾式必須） | `Block`（`([Stmt], Maybe Expr)`、末尾式は任意）の別名に変更 |
| `Expr` | 算術・比較・論理演算のみ | `EBlock Block`、`EIf [(Expr, Block)] (Maybe Block)`、`EWhile Expr Block` を追加 |
| `Stmt` | `SBlock [Stmt]`、`SIf [(Expr, [Stmt])] (Maybe [Stmt])`、`SWhile Expr [Stmt]` | 上記3つを廃止し `SExpr Expr` に統合（`EBlock`/`EIf`/`EWhile`のラッパーとしてのみパーサーが生成） |
| `Type` | `TyInt Width \| TBool` | `TUnit` を追加 |
| `Instr` | 算術・比較・分岐命令のみ | `IDiscard`（仮称）を追加：スタック先頭を捨てるだけの命令 |
| 新規パース関数 | — | `parseBlockBody`（`stmt* expr?` を先読み付きで解析する共通ヘルパー） |
| `parseFactor` | `TLBrace`/`TIf`/`TWhile` は非対応（文専用） | `parseBlockBody`経由で`EBlock`/`EIf`/`EWhile`を生成するケースを追加 |
| `parseStmts` | `TLBrace`/`TIf`/`TWhile` からそれぞれ`SBlock`/`SIf`/`SWhile`を直接生成 | 同じ式パース関数を呼び`SExpr`でラップする形に変更 |
| `compileExprTyped` | — | `EBlock`/`EIf`/`EWhile`のケースを追加（Unit型の伝播、else無しifの`()`制約を含む） |
| `inferMaybeType` | — | `EBlock`/`EIf`/`EWhile`のケースを追加 |
| `compileStmtsFrom` | `SBlock`/`SIf`/`SWhile`のケース | `SExpr`のケースに統合。`compileExprTyped env TUnit e`で型検査し、`IDiscard`を末尾に付与 |
| `CodeGen.epilogue` | `TyInt`/`TBool`の2ケース | `TUnit`のケースを追加（空文字列を出力） |
| `CodeGen.genInstr` | — | `IDiscard` → `popq %rax`（結果未使用）を追加 |
| `run`（VM） | — | 変更なし（if/whileは元々未対応） |
| `app/Main.hs` | — | 変更なし見込み |
| 依存パッケージ | — | 変更なし |

## 6. テストへの影響

- **既存テストの期待値変更**: `test/Spec.hs`の「末尾式が無い（文だけの）プログラムはエラー」テストは、仕様変更により「`()`として成功し空文字列を表示する」という逆の期待値に書き換える必要がある
- **既存Parserテストの書き換え**: `SBlock`/`SIf`/`SWhile`のAST形を直接アサートしているテストは、`Stmt`の再編（`SExpr (EBlock ...)`等への変更）に伴い、期待するAST値をすべて書き換える必要がある
- **既存Integrationテストへの影響は限定的**: block/if/while関連の結合テスト（gcc実行で結果を比較するもの）は、現行のソース文字列がいずれも末尾式を使わない書き方のため、動作自体は変わらないはず。AST比較テストのみが修正対象
- **新規追加が必要なテスト**:
  1. block/if/whileが値を返す（`let x: i64 = { ...; 5 };`、`let y: i64 = if c { 1 } else { 2 };` 等）
  2. 文の位置で`()`でない値を使うと型エラー（`{ 1 }`を文として書くとエラーになること）
  3. else無しifを値が必要な文脈で使うと型エラー、`()`文脈でのみ許可されること
  4. while式の型が常に`()`固定であること（`let x: i64 = while c {...};`は型エラー）
  5. プログラム末尾に式がない場合に空文字列が出力されること
  6. 式の位置にネストしたblock/if/while（`1 + { 2 }`、`if a { if b { 1 } else { 2 } } else { 3 }` 等）

## 7. 今回のスコープ外とした将来の検討課題

- `return`文の導入（関数定義が言語に入った段階で改めて設計する）
- `while`にbreak時の値を持たせる拡張（Rustの`loop { break expr; }`相当）——step007/008から引き続き先送り
- 文の位置での値の明示的破棄構文（`let _ = expr;`等）——今回の型規則の副作用として、デバッグ目的で値を意図的に捨てたいケースの書き方が今回のスコープでは提供されない
- `run`（VM）へのif/while/Unit対応

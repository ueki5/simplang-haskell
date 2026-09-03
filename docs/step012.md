# ポインタ型（`&`/`*`）の導入における設計上の考慮点

`docs/step002.md`（i32型の導入＝新しい型の追加）と `docs/step004.md`（to_i64/to_i32＝新しい前置単項演算子の追加）はいずれも `Type`（`TyInt Width | TBool`）が非再帰的な直和型であることを前提にしていた。今回追加するポインタ型は初めて「型を内部に持つ型」を導入するため、上記2つのパターンを組み合わせる必要がある。加えて、実効アドレスの計算・レジスタ間接アドレッシングという、このコードベースに一度も存在しなかった概念（＝新規`Instr`とその意味論）と、「代入可能な式（lvalue）」という新しい概念（＝`&`の対象を構文的にではなく意味論的に制限する）も初めて登場する。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 追加する構文 | `&型`（ポインタ型）、`&式`（アドレス取得）、`*式`（デリファレンス） |
| 型表現 | `TPtr Type`（`Width`ではなく`Type`を再帰的にラップ。`&&i64`・`&bool`が特別扱いなしに導出される） |
| ポインタの物理格納幅 | 常に`W64`（8バイト）。指す先の型によらず一定 |
| `&式`（AddrOf）の対象 | 構文的には任意の式を受け付け、コンパイル時のlvalueチェックで`Var`と`Deref`のみを許可する（`&5`・`&(a+b)`は意味論エラー）。`&*p`（ポインタのデリファレンスへの再度の`&`）を特別扱いなしに扱えることを優先し、Stringのみを受け付ける構文的制約は採用しなかった |
| デリファレンス経由の代入 | **スコープ外**。`*p = 5;` は`SAssign`のString制約により構文エラーになる（後述） |
| ポインタ演算（`p + 1`等） | 明示的に拒否する（`compileExprTyped`の`TPtr`向け拒否節） |
| ポインタ同士の比較（`==`/`!=`、`<`/`<=`/`>`/`>=`） | 許可する。既存の`operandType`機構が`Type`の構造的等価性のみに依存するため、追加実装なしに動作する |
| ダングリングポインタ | 検出しない（C言語と同様の既知の制約として許容する） |

## 1. `docs/step011.md` からの変更点（要約）

| ファイル / 項目 | step011まで | step012での変更 |
|---|---|---|
| `Type`（`Parser.hs`） | `TyInt Width \| TBool` | `TPtr Type` を追加（再帰的） |
| `Token` | — | `TAmp`（`&`）を追加 |
| `tokenize` | — | `&`の1文字トークン化を追加（`&&`は`TAmp`2個に分割され、専用の2文字トークンは無い） |
| 文法 | `型 ::= 'i32' \| 'i64' \| 'bool'`（`let-stmt`/仮引数/戻り値型で共有） | `型 ::= '&' 型 \| 'i32' \| 'i64' \| 'bool'` に変更。`factor`に`'&' factor \| '*' factor`を追加 |
| `parseType` | `String -> Either String Type`（識別子1個をベース型へ変換） | 変更なし（ベース型変換のみを担当） |
| 型注釈のパース | `parseType`を`expectIdent`直後に直接呼ぶ（3箇所） | 新関数`parseTypeAnnotation :: [Token] -> ParseResult Type`を追加し、3箇所（`parseLetStmt`/`parseParam`/`parseFnDecl`）をこちらに差し替え |
| `Expr` | — | `AddrOf Expr`、`Deref Expr` を追加 |
| `Instr` | — | `LoadAddr Int`（実効アドレス計算）、`LoadInd Width`（レジスタ間接ロード）を追加 |
| `storageWidth`/`typeName` | `TyInt`/`TBool`のみ対応 | `TPtr`ケースを追加（`storageWidth (TPtr _) = W64`、`typeName (TPtr t) = "&" ++ typeName t`） |
| `compileExprTyped` | `TyInt`/`TBool`の2値で（ほぼ）網羅的にパターンマッチ | `AddrOf`/`Deref`用の節を追加。既存の全コンストラクタ（`Lit`/`BoolLit`/`Add`等13個）に`TPtr`向け拒否節を追加（§4参照） |
| `inferMaybeType` | — | `AddrOf`/`Deref`のケースを追加 |
| 新規ヘルパー | — | `addressOf :: FnSigs -> Env -> Expr -> Either String (Type, [Instr])`（lvalueの実効アドレス計算） |
| `codegen`（`genInstr`） | — | `LoadAddr`→`leaq off(%rbp), %rax; pushq %rax`、`LoadInd W64`→`popq %rax; movq (%rax), %rax; pushq %rax`、`LoadInd W32`→`popq %rax; movslq (%rax), %rax; pushq %rax` |
| `frameSize`/`stackDelta` | — | `LoadAddr`のオフセットをフレームサイズ計算対象に追加。両命令のスタック増減を分類（`LoadAddr`は+1、`LoadInd`は±0） |
| `epilogue`/`commonTail` | `TyInt`/`TBool`のみ | `TPtr`ケースを追加。共通の`printfEpilogue`ヘルパーへリファクタリングし、`fmtPtr: .string "%p\n"`を追加 |
| `run`（VM） | — | `LoadAddr`/`LoadInd`の評価ケースを追加（後述） |

## 2. 型表現：`TPtr Type` を選んだ理由

`Width`（`W32`/`W64`）ではなく`Type`を再帰的にラップする設計を採った。理由は次の2点:

- `Width`を拡張してポインタを含めると、算術命令（`IAdd`/`ISub`/`IMul`/`IDiv`/`INeg`）が`W32`/`W64`で網羅的にパターンマッチしている箇所（`Compiler.hs`のVM `trunc`、`CodeGen.hs`の`genInstr`）すべてに意味のない新しいケースを追加する必要が生じる。ポインタは算術演算の対象外なので、`Width`は`{W32, W64}`のまま据え置くのが自然
- `Type`を再帰的にラップすることで、`&&i64`（ポインタのポインタ）や`&bool`が特別なコードなしに導出される。`storageWidth`は`TPtr _ -> W64`という1行で「ポインタは常に8バイト」を表現でき、`typeName`も`TPtr t -> "&" ++ typeName t`の再帰1行で正しい表示名が得られる

## 3. `AddrOf`/`Deref` の型伝播：非対称性の性質が異なる

`docs/step003.md`で`Eq`/`Neq`が、`docs/step004.md`で`to_i64`/`to_i32`が、`compileExprTyped`の「`expected`を式木全体に一様伝播させる」前提をそれぞれ崩した。今回の2つの新しい演算子は、この前提との関係がそれぞれ異なる形で現れる。

### `Deref e`：一様伝播を維持する

`Deref`のノード自身の型は`expected`そのものであり、子`e`への期待型は`expected`を`TPtr`で包んだものになる。これは`to_i64`/`to_i32`（子の期待型が`expected`と無関係に固定）とは異なり、`expected`を変換して伝播する点でstep002の元々の設計と地続きである:

```haskell
compileExprTyped fnSigs env expected (Deref e) = do
  ei <- compileExprTyped fnSigs env (TPtr expected) e
  Right (ei ++ [LoadInd (storageWidth expected)])
```

### `AddrOf e`：新しい概念「lvalue」の判定が必要

`AddrOf`には子への「期待型」という概念がそもそも存在しない。`&`の対象になれるのは変数参照とポインタのデリファレンスのみ（＝lvalue）であり、それ以外（リテラル・算術式・関数呼び出し・`AddrOf`自身等）は意味論エラーになる。この判定を`compileExprTyped`本体に埋め込まず、独立したヘルパー`addressOf`に切り出した:

```haskell
addressOf :: FnSigs -> Env -> Expr -> Either String (Type, [Instr])
addressOf _ env (Var name) =
  maybe (Left ("undeclared variable: " ++ name))
        (\(off, ty) -> Right (ty, [LoadAddr off]))
        (lookupVar name env)
addressOf fnSigs env (Deref e) = do
  t <- inferMaybeType fnSigs env e
         >>= maybe (Left "type mismatch: cannot take address of a dereferenced untyped literal") Right
  case t of
    TPtr inner -> (,) inner <$> compileExprTyped fnSigs env t e
    other -> Left ("type mismatch: expected pointer, found " ++ typeName other)
addressOf _ _ e = Left ("invalid operand for &: not an lvalue: " ++ show e)
```

`&*p`（`AddrOf (Deref p)`）が特に興味深い：`*p`の**アドレス**は`p`自身の**値**と等しい（デリファレンスとアドレス取得が互いに打ち消し合う）ため、`addressOf`の`Deref`節は`LoadAddr`も`LoadInd`も発行せず、単に`p`を普通の（ポインタ型の）式としてコンパイルするだけでよい。これはC言語の`&*p == p`という性質がこの実装でも構造的にそのまま成り立つことを意味する。

`compileExprTyped`側は`addressOf`が返した「参照先の型」を`expected`と突き合わせるだけの薄いラッパーになる:

```haskell
compileExprTyped fnSigs env expected (AddrOf e) = do
  (pointeeTy, instrs) <- addressOf fnSigs env e
  if expected == TPtr pointeeTy
    then Right instrs
    else Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName (TPtr pointeeTy))
```

`inferMaybeType`（`expected`が存在しない文脈、暗黙main末尾式など向けのボトムアップ推論）にも対応するケースを追加する。`AddrOf`は`addressOf`の結果をそのまま`TPtr`で包めばよく、`Deref`は子の推論結果から`TPtr`を1枚剥がす（剥がせなければ型エラー、子が無型リテラルなら「無型のデリファレンス」として別途エラーにする）。

## 4. 既存コンストラクタの非網羅性の解消

`Type`に3つ目のコンストラクタ`TPtr`を追加すると、`compileExprTyped`内で`TyInt`/`TBool`の2値のみを列挙していた既存の節（`Lit`, `BoolLit`, `Add`/`Sub`/`Mul`/`Div`/`Neg`, `Not`, `Eq`/`Neq`/`Lt`/`Le`/`Gt`/`Ge`の外側`expected`, `ToI64`/`ToI32`）が非網羅になる。このプロジェクトは`-Wall`だが`-Werror`ではないため、対応漏れがあってもビルドは警告止まりで通り、該当コードパスを実際に踏んだときに初めて`Non-exhaustive patterns in function compileExprTyped`という実行時クラッシュとして発覚する。

対象コンストラクタそれぞれに、既存の`TBool`拒否節と同型の`TPtr`拒否節を1行追加した（`Var`と`Call`は等価比較ベースの既存チェックのため無変更で正しく動作する）:

```haskell
compileExprTyped _ _ expected@(TPtr _) (Add _ _) =
  Left ("type mismatch: expected " ++ typeName expected ++ ", found arithmetic expression")
```

この節が「ポインタ演算の明示的な拒否」を兼ねている（`let c: &i64 = b + 1;`のような式が、以前はクラッシュしていたはずの経路を通って正しく`Left`になることを回帰テストで確認済み）。

### 副次的に生じる挙動：ポインタの大小比較も許可される

`Eq`/`Neq`の被演算子型解決は`operandType`（`Type`の構造的`==`のみに依存）経由で行われており、`TPtr`が混ざっても無変更で正しく動作する。ユーザーとの相談の結果、ポインタ同士の等価比較は許可することにした。`Lt`/`Le`/`Gt`/`Ge`も同じ`operandType`機構を共有するため、要求されてはいないがポインタの大小比較（アドレス値としての比較）も実装コストゼロで同時に動作可能になる。C言語でも同一配列内のポインタ比較は合法であり、積極的に禁止する理由がないためそのまま許容している。

## 5. 新規命令とレジスタ間接アドレッシング

```haskell
| LoadAddr Int   -- ローカル変数の%rbp相対実効アドレスを計算しpushする（&lvalue 用）
| LoadInd Width  -- 操作スタック先頭のアドレスをpopし、指定幅でその先の値をロードして
                  -- （64bit正規化済みで）push する（*式 用）
```

ポインタ**値**自体のload/store（変数に格納されたアドレスの読み書き）は既存の`Load W64`/`Store W64`をそのまま流用する（`storageWidth (TPtr _) = W64`のため、変数スロットとしては通常のi64と区別が付かない）。新しいのは「実効アドレスの計算」と「レジスタの中身をアドレスとして間接参照する」の2つで、いずれもこのコードベースに前例がなかった（既存の`leaq`は`.rodata`ラベルの読み込み専用、既存の全メモリオペランドは`off(%rbp)`形式の直接アドレッシングのみ）。

```haskell
genInstr (LoadAddr off) =
  [ "    leaq  " ++ show off ++ "(%rbp), %rax"
  , "    pushq %rax"
  ]
genInstr (LoadInd W64) =
  [ "    popq  %rax"
  , "    movq  (%rax), %rax"
  , "    pushq %rax"
  ]
genInstr (LoadInd W32) =
  [ "    popq  %rax"
  , "    movslq (%rax), %rax"  -- 符号拡張ロード。Load W32と同じ不変条件（64bit正規化スロット）を維持する
  , "    pushq %rax"
  ]
```

`frameSize`の`offsetOf`に`LoadAddr`のオフセットを追加しないと、ある変数が`&`のみで参照され通常の`Load`/`Store`を一度も経由しない場合にフレーム確保量が不足しうる（今回追加した各テストケースでは他の`Store`が同じ変数に対して先に発生するため実害は出ないが、正しさのために追加した）。`stackDelta`にも両命令のスタック増減（`LoadAddr`は+1、`LoadInd`は±0＝`INeg`/`INot`と同型）を追加し、`call`前のアライメント計算を壊さないようにする。

## 6. 出力（暗黙main末尾式がポインタ型の場合）

`epilogue`の`TyInt w`ケースと`TPtr _`ケースは、使うフォーマットラベル（`fmt32`/`fmt64` vs `fmtPtr`）以外まったく同じ処理列だったため、共通ヘルパー`printfEpilogue :: String -> [String]`に括り出した。`commonTail`の`.rodata`に`fmtPtr: .string "%p\n"`を追加する。`fmtLabel :: Width -> String`自体は変更していない（ポインタの表示は指す先の幅に依存しないため、`Width`の側には手を入れず、`epilogue`側で`TPtr`を専用に分岐させている）。

## 7. VM（`run`、テスト専用スタックマシン）

`docs/step004.md`の先例に倣い、`run`にも新規命令の評価を追加した。`vars :: Map Int Int`はオフセットをキーとする抽象メモリであり、実機の`%rbp`に相当する基準点を持たないが、「アドレス」を単にオフセット値そのもの（`%rbp`をゼロとみなす）として表現すれば、実機の`leaq`/`movq (%reg)`と一貫した意味論をそのまま再現できる:

```haskell
go (LoadAddr off : rest) stack vars = go rest (off : stack) vars
go (LoadInd w : rest) (addr : stack) vars =
  case Map.lookup addr vars of
    Just v -> go rest (trunc w v : stack) vars
    Nothing -> Left "uninitialized variable"
```

## 8. 明示的なスコープ外

- **`*p = 5;`（デリファレンス代入）**: `SAssign String Expr`はString限定のまま変更していない。`*p = 5;`は`parseStmts`の2トークン先読み（`TIdent : TAssign`）にマッチしないため文として認識されず、文の列がそこで打ち切られた後、`parseExpr`が`*p`を`Deref (Var p)`として末尾式の一部とみなし、残った`TAssign`トークンで`"unexpected token: TAssign"`という汎用エラーになる（結合テストで確認済み）。クラッシュや意図しないコンパイル結果ではなく安全側のエラーである。将来これをサポートする場合は`SAssign`のターゲットをStringから一般のlvalue（`addressOf`が既に区別している`Var`/`Deref`の2種）へ拡張し、`parseStmts`の先読みロジックと`compileStmtsFrom`の代入処理を合わせて変更する必要がある
- **ポインタ演算**（`p + 1`等）: §4の`TPtr`拒否節により明示的に拒否される
- **ダングリングポインタ**: `docs/step005.md`のブロックスコープ設計により、時間的に重ならない兄弟ブロックはスタックオフセットを再利用する。ブロックを抜けた後の`&local`の使用は未検査（C言語と同様の既知の制約であり、エスケープ解析等でこれを検出する仕組みは今回追加しない）

## 9. テストへの影響

- Tokenizer: `&`のトークン化、`&`で始まる識別子との分割、`&&`が`TAmp`2個に分割されること（専用の2文字トークンが無いことの確認）
- Parser: `&型`（`&i64`/`&&i64`/`&bool`）の型注釈、`&式`/`*式`の前置演算子としてのパース、`*`の中置（乗算）/前置（デリファレンス）の位置による判別（`TMinus`のSub/Neg判別と同型の回帰テスト）、`&*p`のパース
- 意味論エラー（compile）: 未宣言変数への`&`、型不一致、非ポインタのデリファレンス、rvalue（リテラル・算術式）への`&`（"not an lvalue"）、ポインタ演算の拒否（旧来クラッシュしていたはずの経路が正しく`Left`になることの回帰確認）、`&*p`が成功すること、ポインタ同士の等価比較が成功すること
- CodeGen: `LoadAddr`/`LoadInd W64`/`LoadInd W32`の命令出力、`TPtr`末尾式での`fmtPtr`/`%p`出力
- VM（`run`）: `LoadAddr`→`LoadInd`往復での値の一致
- Integration: 仕様例（`let a:i64=0; let b:&i64=&a; let c:i64=*b; c`）、`&a`取得後の`a`への再代入が`*b`に反映されること（アドレスが実体を指していることの確認）、`&&i64`の往復、`&*p`、ポインタ型の関数引数・戻り値、暗黙main末尾式がポインタ型のときの`%p`（`0x`始まり）出力確認

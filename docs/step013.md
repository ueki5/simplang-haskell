# 数値リテラルの型サフィックス（i32/i64）の導入における設計上の考慮点

`docs/step004.md`（`to_i64`/`to_i32`）で「`expected`の一様伝播」の前提が演算子ノード（子への期待型が`expected`とは無関係に固定される）によって崩れたのに対し、今回追加する`9999i64`/`9999i32`のようなサフィックス付きリテラルは、**演算子ではなくリーフ（葉）ノード自体**が確定型を持つ初めてのケースである。これは`Lit`（無型・`expected`にそのまま従う）と対になる新しい種類のリーフとして扱う。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 追加する構文 | 整数リテラル直後（空白なし）に`i64`または`i32`を連結する（例: `9999i64`、`123i32`） |
| 対象型 | `i32`/`i64`のみ。`bool`・ポインタにはサフィックスの概念自体が存在しない |
| 既存の無型リテラル（`Lit`） | 挙動は変更しない。サフィックス無しの整数リテラルは引き続き`expected`にそのまま従う |
| ASTノード | `Lit`のシグネチャは変えず、新しいコンストラクタ`LitTyped Int Width`を追加する（`Lit`は`test/Spec.hs`内で約60箇所参照されており、シグネチャ変更は既存テストを広範囲に破壊するため採らない） |
| 型としての性質 | `LitTyped`は`Var`と同じ「常に確定した型を持つリーフ」として扱う（`inferMaybeType`で`Nothing`を返さない・`compileExprTyped`で`expected`との厳密一致を要求する） |
| 字句解析での境界判定 | `9999i64`はサフィックス付きリテラルとして1トークンに融合するが、`9999i64x`や`9999i65`のように直後に識別子構成文字が続く／サフィックスと完全一致しない場合は、従来通り`TInt`+`TIdent`の2トークンに分割する |

## 1. `docs/step012.md` からの変更点（要約）

| ファイル / 項目 | step012まで | step013での変更 |
|---|---|---|
| `Token` | — | `TIntSuffixed Int Width` を追加 |
| `tokenize` | 数字列を`span isDigit`で切り出してそのまま`TInt`化 | 切り出し直後の残り文字列に対し`matchIntSuffix`で`i64`/`i32`＋識別子境界を判定し、一致すれば`TIntSuffixed`、しなければ従来通り`TInt`を発行 |
| `Expr` | — | `LitTyped Int Width` を追加 |
| `parseFactor` | `TInt n` → `Lit n` | `TIntSuffixed n w` → `LitTyped n w` を追加（`Lit`のケースはそのまま） |
| `inferMaybeType` | `Lit _ -> Nothing`（無型） | `LitTyped _ w -> Just (TyInt w)` を追加（`Var`と同様、常に確定型） |
| `compileExprTyped` | `Lit`は`TyInt`のみ許容、`TBool`/`TPtr`はエラー（型ごとに個別のケース） | `LitTyped n w`は`Var`と同じ形の1ケースで`TBool`/`TPtr`/型不一致の`TyInt`をまとめて処理（後述） |
| `codegen`（`genInstr`） | — | 変更なし。`LitTyped`は`Lit`と同じ`Push n`命令にコンパイルされるため、生成される命令列に新規の種類は増えない |
| `run`（VM） | — | 変更なし（`Push`の評価ロジックをそのまま共有） |

## 2. `LitTyped`は演算子ではなくリーフの非対称性

`to_i64`/`to_i32`は「演算子ノード自身の型が固定で、被演算子への期待型も固定」という非対称性だったが、`LitTyped`はさらに単純で、**子を持たないリーフでありながら`Lit`と違って`Nothing`を返さない**という一点のみが`Lit`との違いになる。この性質は`Var`と完全に同じであるため、`compileExprTyped`のケースも`Var`をそのまま踏襲できる：

```haskell
compileExprTyped _ env expected (Var name) =
  case lookupVar name env of
    Nothing -> Left ("undeclared variable: " ++ name)
    Just (off, ty)
      | ty /= expected ->
          Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName ty)
      | otherwise -> Right [Load (storageWidth ty) off]

compileExprTyped _ _ expected (LitTyped n w)
  | expected == TyInt w = Right [Push n]
  | otherwise = Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName (TyInt w))
```

`Lit`が`TBool`/`TPtr _`/`TyInt _`それぞれに個別のケースを持っていた（`docs/step002.md`・`docs/step012.md`参照）のに対し、`LitTyped`は`Type`の等価性による1本のガード節で全パターン（`TBool`・`TPtr`・幅違いの`TyInt`）を一律に扱える。これは`LitTyped`の型が`TyInt Width`という単一の構造にしか成り得ず、`Var`のように「実際の型」対「期待する型」という単純な等価比較に還元できるためである。

`inferMaybeType`側も対称的に、`Var`と同じ「常に`Just`を返す」ケースを追加するだけで済み、`unifyMaybeType`/`operandType`（`docs/step003.md`のEq/Neq被演算子単一化）に無変更で参加できる：

```haskell
go (LitTyped _ w) = Right (Just (TyInt w))
```

### 副次的に生じる挙動

- `9999i64 + 5`：無型の`5`が`LitTyped`側の型（`i64`）に単一化され、全体が`i64`として確定する
- `9999i32 + 5i64`：`unifyType`が`i32`と`i64`の不一致を検出し、`type mismatch: i32 and i64`になる（`docs/step003.md`のi32/i64混在エラーと同じ経路）
- `let x: i32 = 9999i64;`：`compileExprTyped`が`expected = TyInt W32`・`LitTyped 9999 W64`のガードで不一致を検出し、`type mismatch: expected i32, found i64`になる
- `let x: bool = 9999i64;`：`TyInt W64 /= TBool`のガード不一致で`type mismatch: expected bool, found i64`になる

いずれも新規の分岐ロジックを追加したわけではなく、既存の`Var`・`unifyType`の仕組みへ新しいリーフを1種類載せただけで自然に導かれる。

## 3. 字句解析：識別子境界による誤消費の防止

サフィックスは既存の型注釈（`let x: i32 = ...`のような、常に`':'`や`'->'`の後ろに単独の`TIdent`として現れる）と異なり、リテラルへ空白なしで直接連結される。そのため、数字列を切り出した直後の残り文字列に対して次の2条件を両方満たす場合のみサフィックスとして消費する：

1. 残り文字列が`"i64"`または`"i32"`で始まる
2. その3文字の直後が識別子構成文字（`isAlphaNum`または`'_'`）でない、または入力の終端である

条件2が無いと、`9999i64x`（本来は数字列とその後の識別子`i64x`が隣接しているだけの、従来から許容されていた曖昧な入力）を誤って`TIntSuffixed 9999 W64`+`TIdent "x"`に分割してしまう。境界判定により、`9999i64x`は従来どおり`TInt 9999`+`TIdent "i64x"`のまま変化しない（回帰確認としてテスト化済み）。

`i65`のようなサポート対象外の文字列は条件1で弾かれ、従来どおり`TInt`+`TIdent`の2トークンに分割される（今回追加するのは`i32`/`i64`の2種類のみで、他の幅や符号なし整数などは対象外）。

## 4. コード生成・VMへの影響が無い理由

`LitTyped n w`は`compileExprTyped`の時点で`Push n`という、`Lit`と全く同じ命令へコンパイルされる。型情報はコンパイル時の静的チェック（`expected`との一致確認）にのみ使われ、実行時の値・命令列には一切影響しないため、`src/CodeGen.hs`・VM（`run`）はどちらも無変更で済む。

## 5. テストへの影響

- Tokenizer: `9999i64`/`123i32`が`TIntSuffixed`になること、空白なしで後続トークンと分割されること、`10abc`（無関係な識別子）・`10i64x`（境界に識別子構成文字が続く）で誤ってサフィックスとして消費されないことの回帰確認
- Parser: `TIntSuffixed`が`LitTyped`にパースされること、無型リテラルとの混在式（`Add`）でも正しくパースされること
- 意味論エラー（compile）: 宣言型と一致する場合の成功、幅不一致（i32⇄i64）・bool文脈でのエラー、無型リテラルとの混在での型固定、サフィックス同士の幅不一致エラー
- Integration: `let`を介さない単体のサフィックス付きリテラル（`%d`/`%ld`の出し分け含む）、`let`で束縛した上での演算、宣言型との不一致がコンパイルエラーになることの実行確認

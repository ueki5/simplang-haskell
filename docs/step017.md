# 暗黙の型変換の撤回とキャスト（`as i32`/`as i64`）の導入における設計上の考慮点

`docs/step016.md`では、i32/i64の幅が食い違った箇所でコンパイラが`ISext32`を自動挿入する**暗黙の型変換**を導入した。しかし暗黙の縮小（i64→i32）は値の切り詰めをソース上から見えなくするため、Rustと同様に「幅の違いは必ず明示する」方針へ戻す。

今回は次の2点を行う。

1. 暗黙の型変換を撤回し、型検査を`docs/step015.md`時点の厳密一致へ戻す
2. 明示的な型変換の構文を、`docs/step004.md`の単項演算子`to_i64 x`/`to_i32 x`からRust風のキャスト`x as i64`/`x as i32`へ置き換える（`to_i64`/`to_i32`は削除）

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 暗黙の型変換 | 撤回する。i32/i64の混在は再び`type mismatch`エラー |
| 除算 -1 の修正 | step016のブランチで行った「最小値/-1で`idiv`が例外を起こすのを回避する修正」は暗黙変換とは独立しているため**維持**する |
| `to_i64`/`to_i32` | 削除する。予約語ではなくなり、識別子として使える |
| キャスト先の型 | `i32`/`i64`のみ。`bool`・ポインタ型はパース時にエラー |
| 被演算子の型 | `i32`/`i64`のみ。bool・ポインタはコンパイル時にエラー |
| 同じ型へのキャスト | 許可する（`x as i64`、x: i64）。Rustと同じ |
| 型なしリテラルの被演算子 | i64とみなす（`inferType`の既定値と同じ規則） |
| 優先順位 | Rustと同じ。単項演算子より弱く、`*` `/`より強い。左結合で連鎖できる |
| 追加する命令 | 無し。既存の`ISext32`を再利用する |

## 1. step015への巻き戻し

step015のマージ点`834f46b`を基準に、step016で入れた次の変更を取り除く。

| 取り除くもの | 内容 |
|---|---|
| `coerce` | 整数の幅違いに`ISext32`を挿入するヘルパー |
| `compileArith` | 被演算子の合流型で演算し、結果を`coerce`する処理 |
| `compileToWidth` | `to_i64`/`to_i32`の子を推論型でコンパイルする処理 |
| `unifyType`の合流規則 | 整数同士をi64へ昇格させる節。再び厳密一致になる |
| `Var`/`LitTyped`/`Call`/`Deref`の変更 | `coerce`による変換をやめ、`expected`との厳密一致に戻す |

`src/CodeGen.hs`はstep016で除算 -1 の修正（`cmove`）しか変更していないため、そのまま残す。VM（`run`）の`IDiv`にある`b == -1`の節も残す。

## 2. 構文

```
multiplicative ::= cast (('*' | '/') cast)*
cast           ::= factor ('as' type)*        -- type は 'i32' | 'i64' のみ
factor         ::= ...（'to_i64' factor | 'to_i32' factor を削除）
```

| ソース | AST |
|---|---|
| `x as i64` | `Cast (Var "x") W64` |
| `x as i32 as i64` | `Cast (Cast (Var "x") W32) W64` |
| `-x as i64` | `Cast (Neg (Var "x")) W64` |
| `*p as i64` | `Cast (Deref (Var "p")) W64` |
| `a * b as i64` | `Mul (Var "a") (Cast (Var "b") W64)` |
| `a + b as i64` | `Add (Var "a") (Cast (Var "b") W64)` |
| `(a + b) as i64` | `Cast (Add (Var "a") (Var "b")) W64` |

- Token: `TToI64`/`TToI32`を削除し、`TAs`を追加する（`as`は予約語になる。`ask`・`as_x`は識別子のまま）
- Expr: `ToI64 Expr`/`ToI32 Expr`を削除し、`Cast Expr Width`を追加する
- `parseCast`は`parseFactor`の後に`TAs`が続く限り、既存の`parseTypeAnnotation`で型を読む。`TyInt w`以外は`"invalid cast target: only i32 or i64 is allowed"`、未知の型名は`parseTypeAnnotation`の`"unsupported type: …"`のエラーになる
- キャスト先の制限はパース時に行う。ASTは`Width`しか持てないため、不正なキャスト先が意味解析まで届くことはない

## 3. 意味論

```haskell
compileExprTyped fnSigs env expected (Cast e w)
  | expected /= TyInt w =
      Left ("type mismatch: expected " ++ typeName expected ++ ", found " ++ typeName (TyInt w))
  | otherwise = do
      srcTy <- maybe (TyInt W64) id <$> inferMaybeType fnSigs env e
      case srcTy of
        TyInt srcW -> do
          ei <- compileExprTyped fnSigs env srcTy e
          Right (ei ++ (if srcW == W64 && w == W64 then [] else [ISext32]))
        other -> Left ("invalid cast: cannot cast " ++ typeName other ++ " to " ++ typeName (TyInt w))
```

- 結果の型は常に`TyInt w`で、`expected`と厳密に一致しなければならない（`let y: i32 = x as i64;`はエラー）
- 被演算子は`expected`とは独立に、自身の推論型でコンパイルする。型なしリテラルのみの場合はi64とみなす
- `inferMaybeType`/`resolveExprType`では、被演算子によらず`Just (TyInt w)`を返す（`BoolLit`と同様に部分木を辿らない）

| 変換 | 命令 | 備考 |
|---|---|---|
| i32 → i64 | `ISext32` | 正規化済みの値なら値は変わらない |
| i64 → i32 | `ISext32` | 下位32bitの符号拡張＝切り詰め |
| i32 → i32 | `ISext32` | `docs/step004.md` §3と同じ理由で、正規化されずに届くi32値（fnがリテラルをi32として返す場合など）を再正規化する |
| i64 → i64 | 無し | 恒等変換 |
| リテラル `as i32` | `Push n, ISext32` | `5000000000 as i32`はラップした値になる |
| リテラル `as i64` | `Push n` | — |

| エラー | メッセージ |
|---|---|
| `x as bool`、`x as &i64` | `invalid cast target: only i32 or i64 is allowed`（パース時） |
| `x as foo` | `unsupported type: foo`（パース時） |
| bool・ポインタのキャスト | `invalid cast: cannot cast bool to i64` など |
| 結果の型の不一致 | `type mismatch: expected i32, found i64` など |

## 4. サンプルの書き換え

| ファイル | 書き換え後 |
|---|---|
| `sample/samp013.sl` | `a + b as i64 + c + d as i64` |
| `sample/samp014.sl` | `a + (b + c) as i64 + *p as i64` |
| `sample/samp016-1.sl` | `let e = *b + *d as i64;` |
| `sample/samp016-2.sl` | `let c:i32 = *b as i32;` |
| `sample/samp016-3.sl` | `let c:i64 = *b as i64;` |
| `sample/samp016-7.sl` | `a as i64 + b` |
| `sample/samp017-0〜5.sl` | `let a: i32 = 2147483646i64 as i32;` など |

## 5. テストへの影響

- `test/Spec.hs`をstep015の版に戻したうえで、`to_i64`/`to_i32`のテストを`as`のテストへ置き換えた（Tokenizer・Parser・意味論エラー・Integration）
- 追加したテスト：キャストの優先順位（単項演算子・`*`・`+`との関係）、連鎖、キャスト先のエラー、恒等キャスト、型なしリテラルのキャスト、ポインタのキャストのエラー、`to_i64`を識別子として使えること、キャスト無しでの幅違いの代入がエラーになること
- 除算 -1 の修正のテスト7件（CodeGen 2件・VM 2件・Integration 3件）はそのまま維持した

# Haskell の言語の版を GHC2024 にする

`.cabal` の3つのコンポーネント（library・executable・test-suite）は、`default-language: Haskell2010` のままである。
`LambdaCase`・`TupleSections`・`TypeApplications`・`ScopedTypeVariables` など、今では広く使われ、安全と分かっている拡張を使うには、ファイルごとにプラグマを書く必要があった。

GHC は `cabal.project` で 9.10.3 に固定しているので、GHC 9.10 で入った言語の版 **GHC2024** を使える。
今回、言語の版を GHC2024 にし、これらの拡張をプラグマなしで使えるようにする。

```haskell
-- import の qualified は後置きになる（ImportQualifiedPost。ormolu がこの形に揃える）
import Data.Map qualified as Map

-- \case が使える（LambdaCase）
io action =
  liftIO (try action) >>= \case
    Left e -> throwE (show (e :: IOException))
    Right a -> pure a
```

挙動を変えない、開発環境だけの変更（step027 と同じ種類）。既存コードの書き換えは、ormolu による import の再整形だけにとどめる。

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| 言語の版 | GHC2024 |
| 設定の場所 | `.cabal` の共通の stanza に1か所だけ書く。stanza の名前は `warnings` から `common-options` に変える。各コンポーネントの `default-language` は削除する |
| `base` の範囲 | 下限を `>=4.20`（GHC 9.10）に上げる。上限 `<4.21` は変えない |
| 既存コードの整形 | 全 `.hs` を ormolu で再整形する（import の書き方が変わる）。整形だけのコミットを分け、`.git-blame-ignore-revs` に登録する |
| 既存コードの書き換え | しない（`\case` の適用などは、今後そのコードを触るときに行う） |
| ormolu・GHC の版、CI・hook | 変えない |

## 1. 考慮点

### 1.1 GHC2021 と GHC2024

GHC2021 は Haskell2010 に、長年使われて安全と分かっている拡張を加えた版である（`TupleSections`・`NamedFieldPuns`・`BangPatterns`・`ScopedTypeVariables`・`TypeApplications`・`ImportQualifiedPost`・`GeneralisedNewtypeDeriving` など）。GHC2024 は、GHC2021 にさらに次の拡張を加える。

| 拡張 | 内容 |
|---|---|
| `LambdaCase` | `\case`・`\cases` |
| `DerivingStrategies` | `deriving stock`・`deriving newtype` など、導出の方法を明示する |
| `DataKinds` | データのコンストラクタを型のレベルで使う |
| `GADTs` | GADT による型の精密化（`MonoLocalBinds` も有効になる。1.3） |
| `DisambiguateRecordFields` | 同じ名前のフィールドを、パターンや構築の式で区別する |
| `ExplicitNamespaces` | import/export のリストで `type` を付けて指定する |
| `RoleAnnotations` | 型引数のロールを注釈する |

| 案 | 長所 | 短所 |
|---|---|---|
| Haskell2010 のまま | 変更が要らない | 拡張を使うたびにプラグマが要る |
| GHC2021 | GHC 9.2 以降で使える | `LambdaCase` が入っていない |
| **GHC2024** | `LambdaCase` もプラグマなしで使える。既存コードには `\case` にできる箇所がある（`Compiler.hs` の `go (Deref e)`、`Driver.hs` の `io` など） | GHC 9.10 以降に限られる（1.5）。`MonoLocalBinds` で型推論が少し変わる（1.3） |

GHC2024 を採用する。

最新の HEAD の複製を GHC2021 と GHC2024 のそれぞれに変えて試した。どちらもビルド（`-Wall` の警告 0 件）とテストが通り、違いは ormolu の整形だけだった（1.4）。

### 1.2 個別のプラグマ・`default-extensions` との比較

機能だけを見れば、`{-# LANGUAGE ... #-}` や `.cabal` の `default-extensions` で拡張を1つずつ有効にしても、同じことができる（GHC2024 は拡張の組み合わせに名前を付けたものである）。

それでも言語の版を指定するのは、次の理由による。

- 1か所で済み、拡張を使うたびに足す手間が無い
- GHC が一緒に使うことを想定している組み合わせになる（`MonoLocalBinds` と `ScopedTypeVariables` など。1.3）
- GHC2024 は、Haskell2010 にある `DatatypeContexts`・`CUSKs` を外す。個別に拡張を足す方法では、この違いまでは揃わない（このプロジェクトはどちらも使っていない）

### 1.3 `MonoLocalBinds` の影響

GHC2024 は `GADTs` を含み、`GADTs` は `MonoLocalBinds` を有効にする。
`MonoLocalBinds` のもとでは、型シグネチャの無い局所的な束縛（`let`・`where`）のうち、外側の局所変数を使うものは多相な型に一般化されない。

GADT や型族があると、`case` の分岐の中だけで成り立つ型の等式が現れる。そのため、局所的な束縛の一般化では、一番一般的な型（主要型）が決まらなくなる。GHC は、ほとんど使われていない局所的な一般化を手放し、型推論を予測しやすいものにした（Vytiniotis・Peyton Jones・Schrijvers "Let Should Not Be Generalised"、TLDI 2010）。

| 束縛 | `MonoLocalBinds` での扱い |
|---|---|
| トップレベル | これまでどおり一般化される |
| 外側の局所変数を使わない局所的な束縛 | これまでどおり一般化される |
| 外側の局所変数を使う局所的な束縛 | 一般化されない |
| 型シグネチャを付けた局所的な束縛 | シグネチャのとおり多相になる |

影響を受けるのは、「外側の変数を使う局所関数を、シグネチャ無しで、2つ以上の型で使う」場合だけである。そのときは局所関数にシグネチャを付ける。シグネチャの中で外側の型変数を指すには、外側の関数のシグネチャに `forall` を書く（`ScopedTypeVariables`。GHC2024 に含まれる）。

```haskell
f :: forall a. a -> ((a, Char), (a, Bool))
f x = (g 'a', g True)
  where
    g :: b -> (a, b) -- この a は f の a（forall が無いと別の型変数になり、エラー）
    g y = (x, y)
```

今のコードはこの一般化に頼っていない（GHC2024 の複製でビルドが通った）。

### 1.4 `ImportQualifiedPost` と整形

GHC2021 以降には `ImportQualifiedPost` が含まれ、`import Data.Map qualified as Map` と書けるようになる。この書き方では、どの行も `import` の直後がモジュール名になる。そのため、モジュール名が縦に揃い、行を単純に並べ替えるだけでモジュール名の順になる。

ormolu は `.cabal` の `default-language` を読み、この拡張が有効なときは import を後置きの形に揃える。エディタ・Claude Code の hook・CI は、どれも devShell の同じ ormolu（step027）を使うので、整形の結果は揃う。
そのため、前置きの書き方を `-Wprepositive-qualified-module` で禁止する必要は無い。

再整形で変わるのは import の行だけである（`src/CodeGen.hs`・`src/Compiler.hs`・`src/Driver.hs`・`test/Spec.hs`）。

### 1.5 GHC の下限と `base` の範囲

GHC2024 を理解できるのは GHC 9.10 以降である。
`cabal.project` で GHC を 9.10.3 に固定しているので、ローカルでも CI でも問題は無い。ただし `.cabal` の `base >=4.19.2.0` は GHC 9.8（base 4.19）も許す。GHC 9.8 でビルドすると、依存の解決は成功するのに、その後のコンパイルで `GHC2024` を理解できずに失敗する。
パッケージの記述を実態に揃えるため、下限を GHC 9.10 の base（4.20）に上げる。

### 1.6 設定の場所

`default-language` は3つのコンポーネントに同じ値を書くことになる。共通の stanza に1か所だけ書き、版を変えるときに書き換え漏れが起きないようにする。

これまでの共通の stanza の名前 `warnings` は、`-Wall` だけを入れていたときの名前で、言語の版を入れると内容と合わなくなる。そこで `common-options` に改名する。

### 1.7 一括整形のコミット

step027 と同じく、整形だけのコミットを分けて `.git-blame-ignore-revs` に登録し、`git blame` が import の行の本当の変更を示すようにする。

コミットは次の順に作る。

1. `.cabal` とドキュメントの変更
2. 全 `.hs` の整形だけ
3. 2 のハッシュの `.git-blame-ignore-revs` への登録

1 の時点では CI の format ジョブが失敗するが、PR の先頭（3）では通る。

## 2. 変更するファイル

| ファイル | 内容 |
|---|---|
| `simplang-haskell.cabal` | 共通の stanza を `common-options` に改名して `default-language: GHC2024` を加える。各コンポーネントの `default-language` を削除し、`base` の下限を `>=4.20` に上げる |
| `app/` `src/` `test/` の `.hs` | ormolu による再整形（import の書き方だけ。挙動は変わらない） |
| `.git-blame-ignore-revs` | 再整形のコミット |

## 3. ドキュメントの更新

| ファイル | 内容 |
|---|---|
| `docs/architecture.md` | 「11. 開発環境と整形」に、言語の版（GHC2024、GHC 9.10 以降が必要）、`MonoLocalBinds` の注意、import の書き方を加える |
| `docs/README.md` | step038 を一覧に加える |
| `docs/backlog.md` | 「既存コードへの GHC2024 の機能の適用」を加える |

`docs/language.md`・`docs/cli.md`・`sample/` は、言語仕様が変わらないので変更しない。

## 4. スコープ外（将来の課題）

- 既存コードへの GHC2024 の機能の適用（`\case`、`TupleSections`、`NamedFieldPuns` など）。今後そのコードを触るときに行う

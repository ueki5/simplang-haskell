# テスト用ラッパーの移動と compile/codegen の名前の整理における設計上の考慮点

1ソース分をコンパイルする関数とアセンブリを組み立てる関数が、それぞれ2つずつあった。

| 関数 | 使っている場所 | 内容 |
|---|---|---|
| `Compiler.compileModule` | `Driver`（本番のビルド）とテスト | import したソースのシグネチャ・const/static を受け取り、`CompiledModule`（static の初期値・fn・main・公開シンボル）を返す |
| `Compiler.compile` | テストだけ | import を空にして `compileModule` を呼び、main があることを検査し、結果を (main 以外の fn, main の戻り値の型, main の命令列) に組み替える |
| `CodeGen.codegenModule` | `Driver` とテスト | 公開シンボル・static の初期値・fn・main からアセンブリを組み立てる |
| `CodeGen.codegen` | テストだけ | 公開シンボルと static を空にして `codegenModule` を呼ぶ |

`compile` と `codegen` は、step024（複数ソースからのビルド）で `compileModule`/`codegenModule` を導入したときに、既存のテストを書き換えずに済むよう残したラッパーである。本番では使われていないのに、ライブラリの公開 API の中で短い方の名前を占めていた。

今回、ラッパーをテストモジュールへ移して `compileMain`/`codegenMain` とし、本番で使う関数を `compile`/`codegen` に改名する。

```haskell
-- 変更前
cm <- inFile path (compileModule sigs globals (modProgram m))
pure (codegenModule (cmPublic cm) (cmStatics cm) (cmFns cm) (cmMain cm))

-- 変更後（src/Driver.hs）
cm <- inFile path (compile sigs globals (modProgram m))
pure (codegen (cmPublic cm) (cmStatics cm) (cmFns cm) (cmMain cm))
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| `Compiler.compile`（ラッパー） | `test/Spec.hs` へ移し、`compileMain` に改名する |
| `CodeGen.codegen`（ラッパー） | `test/Spec.hs` へ移し、`codegenMain` に改名する |
| `Compiler.compileModule` | `compile` に改名する |
| `CodeGen.codegenModule` | `codegen` に改名する |
| 言語仕様・生成コード・エラーメッセージ | 変わらない |
| テストの `describe` の名前 | 段階の名前としての「compile」「codegen」はそのまま。`compileModule`/`codegenModule` を名指ししていたものは新しい名前にする |

## 1. 考慮点

### 1.1 ラッパーを残す場所

| 選択肢 | 評価 |
|---|---|
| ライブラリに残し、名前だけ変える | テストのためだけの関数がライブラリの公開 API に残る |
| ラッパーを廃止し、テストも `compile`/`codegen` を直接使う | 補助関数は無くなるが、命令列を比べる約180件と、アセンブリを検査する約60件のテストで、関係の無い引数や `CompiledModule` のフィールドを毎回扱うことになる |
| **テストモジュールへ移す（採用）** | ライブラリの公開 API は本番で使うものだけになる。テストは補助関数の名前を替えるだけで、期待値はそのまま使える |

### 1.2 名前

本番で使う方を短い `compile`/`codegen` にする。モジュール（ソース）単位で処理するのが本来の形で、`Module` を付けて区別する必要はもう無い。テスト用の補助関数は、「main を必須とし、main を中心に結果を返す」ことから `compileMain`/`codegenMain` とする。

### 1.3 `compileMain` の中身

旧 `compile` は、先頭で `validateFnShapes`（重複定義・main の引数の検査）を呼んでいた。この検査は `compile`（旧 `compileModule`）の中の `buildFnSigs` でも行っているので、`compileMain` には持ち込まない（`validateFnShapes` はライブラリの公開 API ではない）。違いが出るのは、重複定義などと main 無しが同時にあるときに、どちらのエラーを先に出すかだけで、既存のテストに影響は無い。

main の有無の検査（`missing function: main`）は、命令列を検査するテストで main を前提にしているので、`compileMain` に残す。

## 2. 構文・AST

変更なし。

## 3. コンパイラ・コード生成

- `Compiler.hs`：`compile`（ラッパー）を削除し、`compileModule` を `compile` に改名する
- `CodeGen.hs`：`codegen`（ラッパー）を削除し、`codegenModule` を `codegen` に改名する
- `Driver.hs`：呼び出しを新しい名前にする

## 4. 移行

- `test/Spec.hs`：
  - 補助関数 `compileMain`/`codegenMain` を追加し、旧ラッパーの呼び出し（`compile` 17か所、`codegen` 61か所）を置き換える
  - `compileModule`/`codegenModule` の呼び出しを `compile`/`codegen` にする
  - `describe "codegenModule"` は、命令ごとの変換を検査する既存の `describe "codegen"` と区別するため「codegen（公開シンボル・main の有無）」とする
- `docs/architecture.md`：パイプライン・モジュール表・§5.1・§6・§7 の名前を直し、§9 にテストの補助関数を記す
- サンプルへの影響はない

## 5. スコープ外（将来の課題）

なし。

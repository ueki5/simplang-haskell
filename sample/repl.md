# script/ — GHCi によるステップ実行

`simplang-haskell` のコンパイルパイプライン（トークナイズ → パース → 型推論・コンパイル → コード生成）を
GHCi 上で1ステップずつ評価しながら内部状態を観察するための資産。

## ファイル

- `init.hs` — GHCi 起動後に読み込むスクリプト。`Parser`/`CodeGen`/`Compiler` を
  ソースから `:l` して非公開識別子（`initialSlots` など）まで参照可能にし、
  検証したい `source`（simplang ソース文字列）を各パイプライン段階の関数に順に適用する
  一連の束縛（`tokens` → `parsed` → `compld` → `asmblr`）を定義する。

## 使い方

1. `init.hs` 内の `source = "..."` を検証したいコードに書き換える（他の候補行は
   `--` でコメントアウトされたサンプルとして残しておく運用）。
2. ライブラリコンポーネントを対象に `cabal repl` を起動する。

   ```bash
   cabal repl simplang-haskell
   ```

3. GHCi プロンプトでスクリプトを読み込む。

   ```
   ghci> :script script/init.hs
   ```

   `init.hs` は `src/Parser.hs src/CodeGen.hs src/Compiler.hs` を `:l` し直し、
   `:m *Parser *CodeGen *Compiler` でコンテキストへ追加してから `source` を
   評価していく。実行すると各段階の中間結果と、最終的なアセンブラ文字数が
   標準出力に表示される。

4. 各ステップを個別に確認したいときは、スクリプト実行後のプロンプトでそのまま
   束縛済みの変数を評価する。

   ```
   ghci> tokens        -- tokenize source の結果
   ghci> parsed        -- tokens >>= parse の結果
   ghci> fnDecls        -- parsed から取り出した関数宣言一覧
   ghci> stmts          -- 暗黙 main の文列
   ghci> expr           -- 暗黙 main の末尾式
   ghci> compld         -- parsed >>= compile の結果（型解決・Instr列）
   ghci> asmblr         -- compld をさらに codegen したアセンブラ文字列
   ```

   `source` を書き換えて再実行したい場合は、ファイルを編集後に GHCi 上で
   `:script script/init.hs` を再度実行すればよい（`:r` はソースファイルの
   再ロードのみで `init.hs` 自体は再実行されないため注意）。

5. `Compiler.hs` 内の `pTraceShowM (...)` / `pTraceShow (...)` 呼び出し
   （`resolveFnSigs` の固定点ループまわりに多数配置されている）はコメントアウトを
   外すと有効化され、`programEvidence` / `fnDeclEvidence` / `resolveSlots` などの
   再帰的な型推論処理が1ラウンド進むごとの中間状態（`env0`/`env1`/`evidence`/
   `resolved`/`pending` 等）が `Debug.Pretty.Simple` 経由で pretty-print される。
   型推論の挙動を1ステップずつ追いたい場合はこれらのコメントアウトを外してから
   上記の手順で `init.hs` を読み込む。不要な出力が邪魔な場合は再度コメントアウトする。

## プログラム本体（`app/Main.hs` の `main`）への引数指定

`init.hs` はライブラリコンポーネント（`Parser`/`CodeGen`/`Compiler`）のみを対象に
しており、`optparse-applicative` で CLI 引数（`FILE`/`-o`/`-S`）を解釈する
`app/Main.hs` の `main` はここには含まれない（`optparse-applicative`/`directory`/
`process` はライブラリの依存に無く、ライブラリ向け `cabal repl` セッションには
読み込めない）。`main` そのものをステップ実行・引数付きで動かしたい場合は、
実行ファイルコンポーネントを対象に別セッションを起動する。

```bash
cabal repl exe:simplang-haskell
```

GHCi プロンプトで `:main` に続けて `cabal run simplang-haskell --` と同じ形式で
引数を並べる（`execParser` が `getArgs` の代わりに `:main` の引数を読む）。

```
ghci> :main sample/src001.sl -o /tmp/out001
ghci> :main sample/src001.sl -o /tmp/out001 -S /tmp/out001.s
```

`:main` の代わりに `System.Environment.withArgs` で明示的に呼び出すこともできる。

```
ghci> import System.Environment (withArgs)
ghci> withArgs ["sample/src001.sl", "-o", "/tmp/out001"] main
```

### 引数を毎回打たずに固定する（`:set args`）

`:main file -o out` のように毎回引数を書き直すのが面倒な場合は、`:set args` で
`getArgs` が返す引数を一度だけ設定しておける。設定後は素の `main`（`:main` では
なく `Main` モジュールの `main` 束縛そのもの）を呼ぶたびに同じ引数が使われる。

```
ghci> :set args sample/src001.sl -o /tmp/out001
ghci> main
ghci> main
```

`:show args` で現在設定されている引数を確認できる。ソースを編集して `:reload`
した後も `:set args` の内容は保持されるため、`main` を呼ぶだけで再ビルド・再実行
できる。

引数を変えたいときは `:set args` を打ち直せばよい。

```
ghci> :set args sample/src002.sl -o /tmp/out002 -S /tmp/out002.s
ghci> main
```

**注意**: `:set args` は `main`（素の関数呼び出し）が内部で使う `getArgs` にのみ
反映される。`:main`（GHCi の専用コマンド、上記の「`:main` に続けて引数を並べる」
方式）は引数を省略すると `:set args` を参照せず空リストとして実行されるため、
「引数を打たずに毎回同じ値を使う」用途では `:main` ではなく素の `main` 呼び出しを
使うこと。

## `exe` 実行中に `Compiler`/`Parser` 内でブレークする

単に `cabal repl exe:simplang-haskell` しただけでは `Main` 以外（`Parser`/`CodeGen`/
`Compiler`）は事前ビルド済みのライブラリオブジェクトとして読み込まれるため、
`:break Compiler.compile` としても `Cannot set breakpoint on 'Compiler.compile':
Module 'Compiler' is not interpreted` と拒否される。実行ファイルを `:main` で
動かしながらライブラリ側の関数にブレークポイントを張るには、それらのモジュールも
ソースから `:l` してインタプリタ実行に切り替える必要がある。

1. `Compiler.hs`/`Parser.hs` が使うパッケージのうち、`exe:simplang-haskell`
   コンポーネント自身の依存に含まれないもの（`containers`/`transformers`。
   `simplang-haskell` ライブラリの依存であって実行ファイルの依存ではないため
   デフォルトでは hidden package になる）を `--repl-options` で追加公開して起動する。

   ```bash
   cabal repl exe:simplang-haskell --repl-options="-package containers -package transformers"
   ```

2. `Parser`/`CodeGen`/`Compiler`/`Main` を全てソースから `:l` し直し、
   インタプリタ（バイトコード）実行に切り替える。

   ```
   ghci> :l src/Parser.hs src/CodeGen.hs src/Compiler.hs app/Main.hs
   ```

3. `main` や内部関数を無修飾で使えるようコンテキストへ追加する（`:l` 後の
   デフォルトコンテキストは `Main` にならないため、`:main` がそのままでは
   `Variable not in scope: main` になる点に注意）。

   ```
   ghci> :m *Parser *CodeGen *Compiler *Main
   ```

4. ブレークポイントを設定してから `:main` で実行する。

   ```
   ghci> :break Compiler.compile
   ghci> :main sample/src001.sl -o /tmp/out001
   ```

   ブレークポイントにヒットすると `Stopped in Compiler.compile, ...` と表示され、
   その時点でスコープに入っているローカル変数（`fnDecls`/`stmts`/`expr` など）を
   そのままプロンプトで評価できる。`:step` で1式ずつ進め、`:continue` で次の
   ブレークポイント（またはプログラム終了）まで実行を再開する。

   ```
   ghci> fnDecls
   ghci> :step
   ghci> :continue
   ```

   スコープに入っている変数名を事前に把握していない場合は、`:show bindings` で
   その時点の全ローカル束縛（名前と型）を一覧表示できる。

   ```
   ghci> :show bindings
   ```

   `Compiler.compile` 以外にも、`Parser.tokenize` や `CodeGen.codegen` など
   `:l` した各モジュールの任意の関数にブレークポイントを張れる。

## 注意

- `cabal repl` はデフォルトでは実行できるコンポーネントを対話的に選択させようと
  するため、必ず `simplang-haskell`（ライブラリ）または `exe:simplang-haskell`
  （実行ファイル）をターゲットとして明示する。
- `init.hs` はソースファイルを直接 `:l` するため、`cabal build` でキャッシュされた
  コンパイル済みモジュールではなく編集中のソースが常に読み込まれる。
- ライブラリ向けセッション（`cabal repl simplang-haskell` + `:script script/init.hs`）
  と実行ファイル向けセッション（`cabal repl exe:simplang-haskell`）は依存パッケージ
  の集合が異なるため、同一セッション内で両方を混在させることはできない
  （`app/Main.hs` を `init.hs` 実行後のセッションに `:l` すると
  `optparse-applicative`/`directory`/`process` が hidden package エラーになる。
  逆に `exe:simplang-haskell` 側から `Compiler.hs`/`Parser.hs` を `:l` する場合は
  `containers`/`transformers` が hidden package エラーになるため、上記のとおり
  `--repl-options="-package containers -package transformers"` を付けて起動する）。

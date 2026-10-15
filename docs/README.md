# ドキュメント目次

## 現在の仕様（常に main の最新の状態と一致させる）

| ファイル | 内容 |
|---|---|
| [language.md](language.md) | 言語仕様（構文・型・意味論・main の出力） |
| [architecture.md](architecture.md) | 実装の全体像（パイプライン・モジュール・型検査・コード生成・ドライバ・テスト） |
| [cli.md](cli.md) | コマンドラインとビルドの仕様 |
| [backlog.md](backlog.md) | 将来の課題（step を追加するたびに更新する） |

## 各 step の設計記録

その step で何を決めたか・なぜそうしたかの記録。マージ後は書き換えない（撤回したときだけ追記する）。
現在の仕様と食い違う箇所は、後の step で変更されたものなので上記の文書を正とする。

| step | 内容 |
|---|---|
| [step000](step000.md) | 初期仕様（四則演算のみのコンパイラ） |
| [step001](step001.md) | 変数・代入 |
| [step002](step002.md) | i32 型 |
| [step003](step003.md) | bool 型と等価/非等価/否定演算子 |
| [step004](step004.md) | to_i64/to_i32 単項演算子（**step017 で撤回**） |
| [step005](step005.md) | コードブロック `{}`（変数スコープ） |
| [step006](step006.md) | if/else if/else |
| [step007](step007.md) | while/break/continue |
| [step008](step008.md) | 比較演算子（`<`, `<=`, `>`, `>=`） |
| [step009](step009.md) | 関数定義（fn） |
| [step010](step010.md) | リファクタリング（関数名見直し） |
| [step011](step011.md) | リファクタリング（モジュール構成見直し） |
| [step012](step012.md) | ポインタ型（`&`/`*`） |
| [step013](step013.md) | 数値リテラルの型サフィックス（i32/i64） |
| [step014](step014.md) | 変数宣言の型注釈省略と型推論 |
| [step015](step015.md) | 関数の仮引数・戻り値型注釈省略と型推論（**step023 で撤回**） |
| [step016](step016.md) | 整数型（i32/i64）の暗黙の型変換（**step017 で撤回**） |
| [step017](step017.md) | 暗黙の型変換の撤回とキャスト（`as i32`/`as i64`） |
| [step018](step018.md) | 符号なし整数型（u32/u64） |
| [step019](step019.md) | 配列型（`[T; N]`）と usize 型 |
| [step020](step020.md) | 配列へのポインタ（`&[T; N]`）と関数での受け渡し |
| [step021](step021.md) | ユニット型（`()`）と式文 |
| [step022](step022.md) | 暗黙の main の廃止と `fn main()` の明示的な定義 |
| [step023](step023.md) | 関数シグネチャの型注釈の必須化（step015 の撤回） |
| [step024](step024.md) | 複数ソースファイルからのビルド（`import`・`-S`/`-c`/リンク） |
| [step025](step025.md) | ドキュメント構成の見直し（全体像を docs へ移し、CLAUDE.md を作業規約に限定） |

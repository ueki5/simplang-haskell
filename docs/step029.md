# ポインタ型の static における設計上の考慮点

step028 では、ポインタ型の static を不可とした。static には定数式の初期値が必須だが、ポインタを初期化できる定数（ヌルポインタ・アドレスの定数）が無かったためである（backlog「ポインタ型の static・const（アドレスの定数）」）。

static のアドレスはリンク時に決まる定数である。今回、これを static の初期値として認め、ポインタ型の static を使えるようにする。

```
static COUNT: i64 = 0;
static TABLE: [i64; 4] = [1, 2, 3, 4];
static P: &i64 = &COUNT;                      // .quad COUNT
static Q: &[i64; 4] = &TABLE;                 // .quad TABLE
static R: &i64 = &TABLE[2];                   // .quad TABLE+16
static PS: [&i64; 2] = [&COUNT, &TABLE[0]];   // .quad COUNT, TABLE
static PP: &&i64 = &P;                        // .quad P

fn main() -> i64 {
    *P = 5;
    Q[1] + *R + **PP      // 2 + 3 + 5
}
```

```
$ simplang-haskell main.sl -o prog && ./prog
10
```

## 0. 方針として確定した事項

| 項目 | 決定内容 |
|---|---|
| static の型 | 整数・bool・`()` に加え、**ポインタ型**と、それらを要素とする配列（ポインタの配列を含む） |
| アドレスの定数 | ポインタ型の static（またはその配列の要素）の初期値に限り、`&X`（X は static）と `&T[i]`（T は static の配列、i は定数式）を書ける |
| 参照できる static | 同じソースで定義した static だけ |
| 添字 | 定数式で、コンパイル時に範囲を検査する |
| ダングリングの簡易検査 | static への代入（`P = e;`、`PS[i] = e;`）で、`e` が直接 `&ローカル` / `&ローカル配列[i]` ならエラー |
| const | 対象外。ポインタ型の const は引き続き不可 |

## 1. 考慮点

### 1.1 アドレスの定数をどこまで認めるか

static のアドレスは、アセンブラにシンボル（と定数のオフセット）を書けば、リンカ・ローダが埋めてくれる。

| 初期値 | アセンブリ | 採否 |
|---|---|---|
| `&X`（X は static。配列・ポインタ型の static も可） | `.quad X` | 採用 |
| `&T[i]`（T は static の配列、i は定数式） | `.quad T+オフセット` | 採用 |
| `&N`（N は const） | 複製を置く必要がある | 不採用（backlog「const のアドレス取得」） |
| `&x`（x はローカル変数） | 実行時にしか決まらない | 不可 |
| `&*P`、`&P[i]`（P はポインタ型の static） | P の値は実行時に変わる | 不可 |

`&T[i]` のオフセットは `i × 要素の格納幅` で、ローカルの配列と同じ（i32 の配列なら4バイト単位）。添字はコンパイル時に範囲を検査し、範囲外なら既存と同じ `index out of bounds: the length is N but the index is i` とする。

アドレスの定数を書けるのは、ポインタ型の static とその配列の要素の初期値だけである。整数の static の初期値（`static X: i64 = ...;`）では、従来どおり `&` を含む式は定数式ではない。アドレスの値を整数として演算する手段（ポインタから整数への `as`）が言語に無いため、アドレスの定数は「`&` 1つ」の形だけを受け付ければ足りる。

### 1.2 参照できるのは同じソースの static だけ

import したソースの static も、アドレスはリンク時に解決されるので、技術的には書ける。しかし step028 で「定数式から参照できるのは同じソースの const だけ」としたのに揃え、同じソースの static だけとした。

- `buildGlobals` は1ソース分の宣言だけで初期値を評価する、という構成を保てる
- import した static のアドレスは、実行時の代入（`P = &X;`）で設定できる

import した static のアドレスを初期値に書くことは、backlog に載せる。

### 1.3 初期値の表現

static の初期値は、これまで整数の列（`[Int]`）だった。アドレスの定数を表すため、要素を次の型に変えた。

```haskell
data StaticInit = SInt Int | SAddr String Int   -- 整数 / (シンボル名, バイトオフセット)
```

コード生成は、`SInt v` を `v`、`SAddr x 0` を `x`、`SAddr x off` を `x+off` として `.quad` に並べる。

```
P:
    .quad COUNT
R:
    .quad TABLE+16
```

`.data` 内のアドレスは、PIE（gcc の既定）でもロード時の再配置（R_X86_64_RELATIVE）で正しい値になる。コードの書き換え（テキスト再配置）は起きないので、追加のリンクオプションは要らない。

### 1.4 アドレスの定数の評価

`evalStatic` は、要素の型が `&T` なら新しい `evalAddrConst` で評価する。既存の定数式の評価（`compileExprTyped` で命令列にして VM で実行する）は使わない。命令列にすると `LoadGlobalAddr` になるが、VM は static のアドレスを扱えず、またアドレスは実行時（ロード時）まで決まらないためである。

| 初期値 | 検査 | 結果 |
|---|---|---|
| `&X` | X が同じソースの static で、型が T | `SAddr X 0` |
| `&T[i]` | T が同じソースの static の配列で、要素型が T。i は `TUsize` の定数式で、範囲内 | `SAddr T (i × 要素の格納幅)` |
| `&X`（型が違う） | | `type mismatch: expected &T, found &U` |
| `&N`（N は const） | | `cannot take address of constant: N` |
| `&y`（y が未宣言） | | `undeclared variable: y` |
| それ以外 | | `initializer of P is not a constant expression` |

添字 i の評価には、既存の `evalConstExpr` をそのまま使う（同じソースの const も参照できる）。

### 1.5 ダングリングの簡易検査

static はプログラムの終了まで存在するので、static に `&ローカル変数` を入れると、関数から戻った後でダングリングポインタになる。

```
static P: &i64 = &COUNT;
fn f() {
    let x = 1;
    P = &x;     // エラー: cannot store reference to local variable in static: P
}
```

既存の `checkNoLocalRef`（return 式・末尾式が直接 `&ローカル` ならエラー）と同じ方式の、簡易検査とした。

- 対象：左辺が static である代入。`P = e;` と、static の配列の要素への代入 `PS[i] = e;`
- 検出する右辺：直接 `&x`（x はローカル変数・仮引数）、`&a[i]`（a はローカルの配列）の形
- 検出しない形：`let p = &x; P = p;` のような中間変数の経由、`*PP = &x;`（PP: `&&i64`）のようなポインタ経由の代入

判定の部分（式が直接ローカルを指すか）は、`checkNoLocalRef` から `isLocalRef` として切り出して共用する。

main も対象にした。main が終わればプログラムも終わるので、main の中では実害は無い。しかし、例外を設けるより規則を単純にすることを優先した（return の検査で main を除外しているのは、main の戻り値を表示するまでフレームが残るという別の理由による）。

## 2. 構文・AST

変更しない。`static P: &i64 = &X;` は step028 の構文のまま書ける（`&X` は既存の `AddrOf (Var "X")`）。

## 3. コンパイラ・コード生成

- `type StaticData = (String, Width, [StaticInit])`
- `buildGlobals.checkGlobalType`：static の型にポインタ型を加えた。要素の型としても認める
- `evalStatic`：要素の型が `TPtr t` なら `evalAddrConst`、それ以外は従来どおり `checkConstExpr` + `evalConstExpr`
  - static の名前と型の表を受け取るようにした
- `compileStmtsFrom`：`SAssign` と `SAssignPlace (Index (Var x) _)` で、左辺が static かつ右辺が直接ローカルを指すならエラー
- `dataSection`：`SAddr` を `x` / `x+off` として出力する
- 実行時の読み書き（`LoadGlobal W64` / `StoreGlobal W64`）とドライバは変更しない

## 4. エラーメッセージ

| ケース | メッセージ |
|---|---|
| `static P: &i32 = &X;`（X は i64） | `type mismatch: expected &i32, found &i64` |
| `static P: &i64 = &N;`（N は const） | `cannot take address of constant: N` |
| `static P: &i64 = &5;` / `&*Q` / `&Q[0]`（Q はポインタ） | `initializer of P is not a constant expression` |
| `static R: &i64 = &T[4];`（T の長さ 4） | `index out of bounds: the length is 4 but the index is 4` |
| `static P: &i64 = &y;`（y は未宣言） | `undeclared variable: y` |
| `P = &x;`（x はローカル） | `cannot store reference to local variable in static: P` |
| `PS[0] = &a[1];`（a はローカルの配列） | `cannot store reference to local variable in static: PS` |
| `const P: &i64 = &X;` | `invalid type for constant: P`（従来どおり） |

## 5. 移行

- `StaticData` の型の変更に伴い、static の初期値を検査する既存のテストの期待値を `SInt` の形に変える
- step028 のテスト「ポインタ型のstaticはエラー」（`static P: &i64 = 1;` → `invalid type for static: P`）は削除した。同じ入力は、型ではなく初期値の誤りとして `initializer of P is not a constant expression` になり、その検査は新しいテストで行う
- 既存のサンプルは影響を受けない。新機能のサンプルとして `sample/samp029-0〜1.sl` を追加する

## 6. スコープ外（将来の課題）

- ポインタ型の const（const はメモリを持たない値として扱っているため。`&N` とあわせて検討する）
- import したソースの static のアドレスを初期値に書くこと（1.2）
- ダングリングの検査を、中間変数の経由・ポインタ経由の代入（`*PP = &x`）へ広げること。完全に防ぐにはエスケープ解析かライフタイムが必要（backlog 1.5）
- ヌルポインタ。初期値の決まらないポインタ型の static は書けない（Rust と同じく、`Option` に相当する仕組みが要る）

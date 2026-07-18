# 12. 論理ベクトル — Logical vectors

> 🌐 [English](README.ja.md) | **日本語**

> 一次情報: **R for Data Science 2e, Ch.12 "Logical vectors"**
> <https://r4ds.hadley.nz/logicals>
> データ: **nycflights13** の `flights`(全 336,776 行)+ 説明用ダミーベクトル。

論理ベクトルは最も単純な型で、各要素は `TRUE` / `FALSE` / `NA` の **3 値**しか
取りません。生データに最初から論理ベクトルがあることは稀ですが、ほぼすべての
解析の途中で作成・操作します。本章では

- **数値比較**(`< <= > >= != ==`)による作成と浮動小数点の落とし穴(`near`)
- **欠損値**の「伝染」と `is.na()`
- **ブール代数**(`& | ! xor`)・演算順序の罠・`%in%`
- **要約**(`any`/`all`・`sum`/`mean`・論理サブセット)
- **条件変換**(`if_else` / `case_when`)

を学びます。実行コードは [`Logicals.hs`](Logicals.hs)。

```sh
cd docs/tutorials/12-logical
cabal run tut-12-logical
```

> **この章は表とベクトル出力が主役です。** R4DS Ch12 で登場する図は唯一、ブール演算
> を説明する**ベン図**(`diagrams/transform.png`・概念イラスト)だけで、実データから
> 描く ggplot 図は **1 枚もありません**。本章はその概念を散文で説明し、各操作の
> **ベクトル/表出力を実データで忠実に再現**します(解説イラストは統計プロット
> ライブラリの対象外)。

> **R の関数は自前実装。** `near` / `%in%` / `if_else` / `case_when` / `any` / `all` /
> `is.na` 等、Haskell 標準に直接対応が無いものは [`Logicals.hs`](Logicals.hs) 内に
> 小ヘルパとして実装しました(CLAUDE.md「機能不足は実装で埋める」)。3 値論理は
> `Maybe Bool`(`Just True` / `Just False` / `Nothing`=NA)で表し、Kleene の
> 3 値論理に従う `andK` / `orK` / `xorK` を定義しています。

---

## 12.1 はじめに

論理ベクトルの個々の関数を説明するため、`c()` でダミーデータを作ります。自由な
ベクトルに対する操作は、`mutate()` でデータフレーム内の変数にもそのまま適用できます。

```haskell
x = [1,2,3,5,7,11,13] :: [Int]
-- x * 2
map (*2) x   -- [2,4,6,10,14,22,26]
```

`tibble(x) |> mutate(y = x * 2)` に相当:

```
 x  |  y
----|----
1   | 2
2   | 4
...
13  | 26
```

---

## 12.2 比較

数値比較は論理ベクトルを作る最も一般的な方法です。これまで `filter()` の中で
論理ベクトルを**その場で**作っては捨てていました。たとえば「昼間に出発して
ほぼ定刻に到着した便」:

| R | hgg |
|---|---|
| `filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)` | `andK`/`cmpM` で論理ベクトルを組み、`Just True` の行だけ残す |

これは近道で、`mutate()` で中間の論理変数に**名前を付けて**明示できます。複雑な
条件では、各段を名付けると読みやすく検算もしやすくなります。

```haskell
daytime  = andK (dep_time > 600) (dep_time < 2000)   -- 3 値論理 (NA は伝染)
approxOT = abs(arr_delay) < 20
```

`.keep = "used"` で使った列と新列だけを残した出力(全 336,776 行):

```
dep_time  | arr_delay  | daytime | approx_ontime
----------|------------|---------|--------------
Just 517  | Just 11    | FALSE   | TRUE
Just 533  | Just 20    | FALSE   | FALSE
...
```

`filter(daytime & approx_ontime)` の結果は **172,286 行**です。

### 12.2.1 浮動小数点比較

数値の `==` には注意が必要です。`c(1/49*49, sqrt(2)^2)` は一見 1 と 2 ですが:

```
x (R 既定 7 桁表示風)  = 1 2
x == c(1, 2)          = FALSE FALSE
print(x, digits = 16) = 0.9999999999999999 2.0000000000000004
near(x, c(1, 2))      = TRUE TRUE
```

コンピュータは固定桁数で数を保持するため `1/49` や `sqrt(2)` を正確に表せず、
計算結果がわずかにずれます。`dplyr::near()`(既定許容差 ≈ 1.49e-8)は微小差を
無視して比較します。

### 12.2.2 欠損値(比較)

欠損値は「不明」を表すので**伝染**します。不明な値が絡む演算はほぼ不明になります。

```
NA > 5    = NA
10 == NA  = NA
NA == NA  = NA
```

最も紛らわしいのは `NA == NA` が `NA` になることです。「Mary の年齢も John の年齢も
不明、二人は同い年か?」→ **不明**、と考えると腑に落ちます。

したがって `filter(dep_time == NA)` は機能しません。`dep_time == NA` は全行 `NA` と
なり、`filter()` は欠損行を自動で落とすので **0 行**になります。

```
flights |> filter(dep_time == NA):
# 全 0 行 × 19 列
```

### 12.2.3 `is.na()`

`is.na(x)` は任意の型で動き、欠損に `TRUE`・それ以外に `FALSE` を返します。

```
is.na(c(TRUE, NA, FALSE)) = FALSE TRUE FALSE
is.na(c(1, NA, 3))        = FALSE TRUE FALSE
is.na(c("a", NA, "b"))    = FALSE TRUE FALSE
```

`dep_time` が欠損の行(=欠航便)を探せます — **8,255 行**:

```
flights |> filter(is.na(dep_time)):
# 全 8,255 行 × 19 列
```

`is.na()` は `arrange()` でも有用です。`arrange()` は既定で欠損を末尾に置きますが、
`arrange(desc(is.na(dep_time)), dep_time)` とすれば欠損を**先頭**に並べられます。

---

## 12.3 ブール代数

複数の論理ベクトルは**ブール代数**で組み合わせます。R では `&`=and、`|`=or、
`!`=not、`xor()`=排他的論理和です。

> **図 12.1(R4DS のベン図)について。** R4DS は `x`・`y` を 2 円で描いたベン図で
> 各演算が選ぶ領域を示します:`x & !y`=x から y を除く / `x & y`=交わり /
> `!x & y`=y から x を除く / `x`=x 全体 / `xor(x,y)`=交わり以外すべて / `y`=y 全体 /
> `x | y`=全体。これは概念を説明する**手描きイラスト**で実データ図ではないため、
> 本章では散文で説明します(統計プロットライブラリの対象外)。

```
!is.na(x), x=c(1,NA,-15,5)        = TRUE FALSE TRUE TRUE
x < -10 | x > 0                    = TRUE NA TRUE TRUE
xor(x > 0, x < 3), x=c(1,NA,-15,5) = FALSE NA TRUE TRUE
```

> `&&` と `||` は**短絡評価**演算子で単一の `TRUE`/`FALSE` しか返しません。
> プログラミング用で、dplyr の中(=ベクトル演算)では使いません。

### 12.3.1 欠損値(ブール代数)

ブール代数での欠損のルールは一見不整合に見えます。

```
tibble(x = c(TRUE, FALSE, NA)) |> mutate(and = x & NA, or = x | NA):
  x   |  and  |  or
------|-------|-----
TRUE  | NA    | TRUE
FALSE | FALSE | NA
NA    | NA    | NA
```

`NA | TRUE` は `TRUE`(少なくとも片方が真)、`NA | FALSE` は `NA`(NA が真か偽か
不明)。`&` も同様に、`NA & FALSE` は `FALSE`(少なくとも片方が偽)、`NA & TRUE` は
`NA`(不明)。本実装の `andK`/`orK` はこの Kleene 3 値論理を符号化しています。

### 12.3.2 演算順序

演算順序は英語の語順とは違います。「11 月か 12 月に出発した便」を英語のまま
`filter(month == 11 | 12)` と書くとエラーにはなりませんが**機能しません**。

R はまず `month == 11`(これを `nov` とする)を評価し、次に `nov | 12` を計算します。
論理演算子に数値を使うと 0 以外はすべて `TRUE` に変換されるので、これは
`nov | TRUE` = **常に TRUE** となり、全行が選ばれます。

```
flights |> mutate(nov = month == 11, final = nov | 12, .keep = "used"):
month | nov   | final
------|-------|------
1     | FALSE | TRUE
...
全 336,776 行が final == TRUE (= 全行。正しくは month == 11 | month == 12)
```

### 12.3.3 `%in%`

`==` と `|` の順序ミスを避ける簡単な方法が `%in%` です。`x %in% y` は `x` と同じ
長さの論理ベクトルを返し、`x` の値が `y` のどこかにあれば `TRUE` になります。

```
1:12 %in% c(1, 5, 11)               = TRUE FALSE FALSE FALSE TRUE FALSE FALSE FALSE FALSE FALSE TRUE FALSE
letters[1:10] %in% c(a,e,i,o,u)     = TRUE FALSE FALSE FALSE TRUE FALSE FALSE FALSE TRUE FALSE
```

`%in%` は `NA` について `==` と異なるルールに従い、`NA %in% NA` は `TRUE` です。

```
c(1, 2, NA) == NA                    = NA NA NA
c(1, 2, NA) %in% NA                  = FALSE FALSE TRUE
```

これは便利な近道になります。`filter(dep_time %in% c(NA, 0800))` は `dep_time` が
欠損か 800 の行を返します(`0800` の先頭ゼロは無視され 800)— **8,803 行**。

---

## 12.4 要約

### `any()` / `all()`

主な論理要約は `any()` と `all()` です。`any(x)` は `|` に相当し `x` に 1 つでも
`TRUE` があれば `TRUE`、`all(x)` は `&` に相当し全要素が `TRUE` のときだけ `TRUE`。
`na.rm = TRUE` で欠損を除けます。日ごとに「全便が出発遅延 60 分以内か」「到着が
5 時間以上遅れた便があるか」を見ます。

```
group_by(year, month, day) |> summarize(
  all_delayed = all(dep_delay <= 60, na.rm=T),
  any_long_delay = any(arr_delay >= 300, na.rm=T)):
year | month | day | all_delayed | any_long_delay
-----|-------|-----|-------------|---------------
2013 | 1     | 1   | FALSE       | TRUE
2013 | 1     | 2   | FALSE       | TRUE
...
# 全 365 行 × 5 列
```

### 12.4.1 論理ベクトルの数値要約

論理値を数値文脈で使うと `TRUE`→1、`FALSE`→0 になります。よって `sum(x)` は
`TRUE` の**個数**、`mean(x)` は `TRUE` の**割合**になります。

```
summarize(
  proportion_delayed = mean(dep_delay <= 60, na.rm=T),
  count_long_delay = sum(arr_delay >= 300, na.rm=T)):
2013 | 1 | 1 | 0.9391408114558473 | 3
2013 | 1 | 2 | 0.9144385026737968 | 3
...
```

### 12.4.2 論理サブセット

論理ベクトルで**単一の変数を部分抽出**できます(base の `[` 演算子)。「実際に
遅れた便だけ」の平均遅延を見たいとき、先に `filter` する方法もありますが、
`arr_delay[arr_delay > 0]` のように**列内サブセット**を使えば 1 回の集約で
遅延便の平均と早着便の平均を同時に出せます。

```
summarize(behind = mean(arr_delay[arr_delay>0], na.rm=T),
          ahead  = mean(arr_delay[arr_delay<0], na.rm=T), n = n()):
2013 | 1 | 1 | 32.48156182212581 | -12.495798319327731 | 842
...
```

> 群の大きさに注意:`filter(arr_delay > 0)` してから `n()` を取ると「遅延便の数」
> ですが、列内サブセット版の `n()` は「全便数」です(上の n=842 は 1/1 の全便)。

---

## 12.5 条件変換

論理ベクトルの最も強力な使い道が**条件変換**(条件 x では A、条件 y では B)です。
道具は `if_else()` と `case_when()` の 2 つ。

### `if_else()`

条件が `TRUE` のとき第 2 引数、`FALSE` のとき第 3 引数、省略可能な第 4 引数
`missing` は入力が `NA` のとき使われます。

```
x = c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")        = "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" NA
if_else(x > 0, "+ve", "-ve", "???") = "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" "???"
if_else(x < 0, -x, x)  (= abs)      = 3 2 1 0 1 2 3 NA
if_else(is.na(x1), y1, x1)          = 3 1 2 6   -- coalesce 風
```

`true`/`false` にもベクトルを使え、混在もできます(上の `abs`・`coalesce` 風)。
0 が正でも負でもない問題は `if_else` の入れ子で解けますが、条件が増えると
読みにくくなるので `case_when()` に切り替えます。

### 12.5.1 `case_when()`

SQL の `CASE` に着想を得た構文で、`条件 ~ 出力` の対を取り、`TRUE` の出力を返します。

```
case_when(x==0~"0", x<0~"-ve", x>0~"+ve", is.na(x)~"???") = -ve -ve -ve 0 +ve +ve +ve ???
case_when(x<0~"-ve", x>0~"+ve")              = -ve -ve -ve NA  +ve +ve +ve NA    -- 不一致は NA
case_when(x<0~"-ve", x>0~"+ve", .default="???") = -ve -ve -ve ??? +ve +ve +ve ??? -- 既定値
case_when(x>0~"+ve", x>2~"big")              = NA NA NA NA +ve +ve +ve NA          -- 複数一致は最初のみ
```

両辺に変数を使い、混在もできます。`flights` の到着遅延に読みやすいラベルを付ける例:

```
mutate(status = case_when(
  is.na(arr_delay)     ~ "cancelled",
  arr_delay < -30      ~ "very early",
  arr_delay < -15      ~ "early",
  abs(arr_delay) <= 15 ~ "on time",
  arr_delay < 60       ~ "late",
  arr_delay < Inf      ~ "very late"), .keep = "used"):
arr_delay  | status
Just 11    | on time
Just 20    | late
Just (-18) | early
...
```

> `<` と `>` を混ぜると条件が重なりやすいので注意(R4DS 著者も最初の 2 回は
> 重複条件を作ったとのこと)。

### 12.5.2 互換な型

`if_else()` / `case_when()` の出力は**互換な型**でなければなりません。R では
`if_else(TRUE, "a", 1)`(文字列と数値)や `case_when(... ~ TRUE, ... ~ now())`
(論理と日時)はエラーになります。互換な主な組合せは:

- 数値と論理(`TRUE`→1, `FALSE`→0)
- 文字列と factor(factor は値域を制限した文字列とみなせる)
- 日付と日時(日付は日時の特殊形)
- `NA` は技術的には論理ベクトルだが**すべての型と互換**

> **Haskell では静的型**ゆえ、これらは「実行時エラー」ではなく「**コンパイル
> エラー**」になります。本章の `if_else` ヘルパも `true`/`false` が同型である
> 必要があり、型システムが互換性を保証します。

---

## 演習(R4DS Ch12)

R4DS の演習も本章の道具で解けます(`Logicals.hs` のヘルパで再現可能):

1. `near()` の仕組み・`sqrt(2)^2` は 2 に near か(→ `TRUE`)。
2. `dep_time`/`sched_dep_time`/`dep_delay` の欠損の関連を `is.na()`+`count()` で。
3. November/December の便を `month %in% c(11, 12)` で。
4. 偶奇判定 `if_else(x %% 2 == 0, ...)`、曜日の weekend/weekday 判定、`if_else` での絶対値。
5. `month`/`day` から米国の祝日(元日・独立記念日・感謝祭・クリスマス)を `case_when` で。

---

## まとめ

論理ベクトルは `TRUE`/`FALSE`/`NA` の 3 値だけですが大きな力を持ちます。本章では
比較演算子と `is.na()` での作成、`! & |` での結合、`any`/`all`/`sum`/`mean` での要約、
`if_else`/`case_when` での条件変換を学びました。次章以降でも論理ベクトルは繰り返し
登場します(`str_detect` で文字列マッチ、日付比較など)。

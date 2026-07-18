# 14. 文字列 — Strings

> 一次情報: **R for Data Science 2e, Ch.14 "Strings"**
> <https://r4ds.hadley.nz/strings>
> データ: **babynames**(US SSA・`year,sex,name,n,prop`・全 1,924,665 行)+ 説明用ダミー。

文字列はデータ解析でほぼ必ず触れる型です。本章では stringr(tidyverse)の `str_*`
関数群に対応する操作を学びます。

- **文字列の作成**(エスケープ・raw string・特殊文字 `\n` `\t` `\u`)
- **データから多数の文字列を作る**(`str_c` / `str_glue` / `str_flatten`)
- **文字列からデータを取り出す**(`separate_longer_*` / `separate_wider_*`・`too_few`/`too_many`)
- **文字**(`str_length`・`str_sub` の正/負 index)
- **非英語テキスト**(encoding=`charToRaw`・正規化比較 `str_equal`・locale 依存)

実行コードは [`Strings.hs`](Strings.hs)。

```sh
cd docs/tutorials/14-strings
cabal run tut-14-strings
```

> **この章は表とベクトル出力が主役です。** R4DS Ch14 で登場する唯一の視覚要素は RStudio
> オートコンプリートの**スクリーンショット 1 枚**だけで、実データから描く ggplot 図は
> **1 枚もありません**。本章はその全出力を実データ(US SSA babynames・192 万行)で
> **忠実に再現**します。

> **stringr/tidyr の関数は analyze 側に実装。** `str_c` / `str_glue` / `str_flatten` /
> `separate_longer_*` / `separate_wider_*` / `str_length` / `str_sub` / `str_equal` /
> `charToRaw` 等は、本リポジトリの統計ライブラリ
> `Hanalyze.Data.Strings`
> に実装済です(Phase 28 Ch14)。本章はそれを呼んで R4DS の各出力を再現します。
> アクセント文字の正規化比較は `unicode-transforms`(NFC)を使います。

---

## 14.2 文字列の作成

R では文字列を `"` でも `'` でも囲めます(tidyverse スタイルガイドは、文字列内に `"` を
含む場合を除き `"` を推奨)。Haskell の文字列リテラルは `"..."` のみです。

### 14.2.1 エスケープ

特殊文字はバックスラッシュ `\` でエスケープします。`'`(単一引用符)・`"`(二重引用符)・
`\`(バックスラッシュ自身)を要素に持つベクトルを作ると、見た目どおりの 1 文字になります。

| R | hgg (Haskell) |
|---|---|
| `single_quote <- '\''` | `singleQuote = "'"` |
| `double_quote <- "\""` | `doubleQuote = "\""` |
| `backslash <- "\\"` | `backslash = "\\"` |

`str_view()` は文字列の**中身**(エスケープ後の実体)を表示します。本章では同等の
`strView` ヘルパで再現します:

```
x = c(single_quote, double_quote, backslash):
[1] | '
[2] | "
[3] | \
```

### 14.2.2 Raw string

R 4.0.0+ には、過剰なエスケープを避ける **raw string** `r"(...)"` 構文があります。

> **hgg 注記**: Haskell の標準文字列リテラルに R の `r"(...)"` 相当の構文は
> **ありません**。バックスラッシュ等は通常どおりエスケープします(`"\\"` で `\` 1 文字)。
> この差は言語仕様の差であり、R4DS の概念(エスケープ地獄を避ける)は理解しておけば十分です。

### 14.2.3 その他の特殊文字

`\n`(改行)・`\t`(タブ)のほか、Unicode を `\u`(4 桁)/ `\U`(8 桁)で書けます。
Haskell では `\x00b5`(µ)・`\x1f604`(😄)のように 16 進エスケープを使います。

```
x = c("one\ntwo", "one\ttwo", "µ", "\U0001f604"):
[1] | one{\n}two
[2] | one{\t}two
[3] | µ
[4] | 😄
```

(`{\n}` `{\t}` は制御文字を見える化した表記です。実体は改行・タブです。)

---

## 14.3 データから多数の文字列を作る

### 14.3.1 `str_c()`

`str_c()` は複数のベクトルを **recycling 規則**(長さ 1 か n)で揃え、行ごとに連結します。
リテラルは長さ 1 の列として渡します。

| R | hgg |
|---|---|
| `str_c("x", "y")` | `strC [["x"],["y"]]` |
| `str_c("Hello ", c("John","Susan"))` | `strC [["Hello "], ["John","Susan"]]` |

```
str_c("x", "y")                    = ["xy"]
str_c("x", "y", "z")               = ["xyz"]
str_c("Hello ", c("John","Susan")) = ["Hello John","Hello Susan"]
```

`mutate()` の中で使うと、欠損値 `NA` は**伝播**します(`strCMaybe` で `Nothing` を伝播):

```
df |> mutate(greeting = str_c("Hi ", name, "!")):  (NA 伝播)
  name="Flora"  greeting="Hi Flora!"
  name="David"  greeting="Hi David!"
  name="Terra"  greeting="Hi Terra!"
  name=NA       greeting=NA
```

`coalesce()` で欠損を別の値に置き換えてから連結できます(hgg では `maybe d id`):

```
df |> mutate(greeting1 = str_c("Hi ", coalesce(name, "you"), "!")):
  Hi Flora!
  Hi David!
  Hi Terra!
  Hi you!
```

### 14.3.2 `str_glue()`

`str_glue()` は `{}` で変数を補間します。`{{` / `}}` でリテラルの波括弧をエスケープします。

| R | hgg |
|---|---|
| `str_glue("Hi {name}!")` | `strGlue "Hi {name}!" [("name", names)]` |

```
df |> mutate(greeting = str_glue("Hi {name}!")):
  Hi Flora!
  Hi David!
  Hi Terra!
  Hi NA!

df |> mutate(greeting = str_glue("{{Hi {name}!}}")):  (波括弧エスケープ)
  {Hi Flora!}
  {Hi David!}
  {Hi Terra!}
  {Hi NA!}
```

### 14.3.3 `str_flatten()`

`str_flatten()` はベクトルを**単一文字列**に畳み込みます(`summarize()` と相性が良い)。
hgg では `strFlatten`(= `T.intercalate`):

```
str_flatten(c("x","y","z"))        = "xyz"
str_flatten(c("x","y","z"), ", ")  = "x, y, z"
```

`group_by()` |> `summarize()` で各グループの値を連結:

```
df |> group_by(name) |> summarize(fruits = str_flatten(fruit, ", ")):
  Carmen   banana, apple
  Marvin   nectarine
  Terence  cantaloupe, papaya, mandarin
```

> **注記**: R の `str_flatten(..., last = ", and ")`(最後の区切りだけ変える)は本 helper
> では未対応です。必要なら呼び出し側で末尾要素を分けて連結します(本章の例では不使用)。

---

## 14.4 文字列からデータを取り出す

tidyr の 4 関数は `separate_[longer/wider]_[delim/position]` の規則的な名前を持ちます。

### 14.4.1 行に分ける(`separate_longer_*`)

`separate_longer_delim()` は区切り文字で 1 行を**複数行**に展開します:

| R | hgg |
|---|---|
| `df1 \|> separate_longer_delim(x, delim = ",")` | `separateLongerDelim "x" "," df1` |

```
df1 |> separate_longer_delim(x, delim = ","):   (x = "a,b,c" / "d,e" / "f" → 6 行)
 x
----
 a
 b
 c
 d
 e
 f
```

`separate_longer_position()` は固定幅で展開します:

```
df2 |> separate_longer_position(x, width = 1):  (x = "1211" / "131" / "21" → 9 行)
 x
----
 1
 2
 1
 1
 1
 3
 1
 2
 1
```

### 14.4.2 列に分ける(`separate_wider_*`)

`separate_wider_delim()` は区切り文字で 1 セルを**複数列**に分けます(行数は不変)。
`names` の `Nothing`(R の `NA`)はその piece を捨てます:

| R | hgg |
|---|---|
| `separate_wider_delim(x, ".", names = c("code","edition","year"))` | `separateWiderDelim "x" "." [Just "code", Just "edition", Just "year"]` |
| `names = c("code", NA, "year")` | `[Just "code", Nothing, Just "year"]` |

```
df3 |> separate_wider_delim(x, ".", names = c("code","edition","year")):
 code | edition | year
------|---------|-----
 a10  | 1       | 2022
 b10  | 2       | 2011
 e15  | 1       | 2015

df3 |> separate_wider_delim(x, ".", names = c("code", NA, "year")):  (NA で edition 列を捨てる)
 code | year
------|-----
 a10  | 2022
 b10  | 2011
 e15  | 2015
```

`separate_wider_position()` は固定幅(列名と文字数の組)で分けます:

```
df4 |> separate_wider_position(x, c(year=4, age=2, state=2)):
 year | age | state
------|-----|------
 2022 | 15  | TX
 2021 | 22  | LA
 2023 | 25  | CA
```

### 14.4.3 列分割の診断(`too_few` / `too_many`)

piece 数が `names` 数と合わないとき、方針を指定します。**既定はどちらも error** です。
不足は `TooFew`(`AlignStart` / `AlignEnd` / `TooFewError` / `TooFewDebug`)、過多は
`TooMany`(`DropExtra` / `MergeExtra` / `TooManyError` / `TooManyDebug`)で指定します。

`too_few = "debug"` は診断列 `{col}_ok` / `{col}_pieces` / `{col}_remainder` を付与します:

```
dfFew |> separate_wider_delim(a, "-", c("x","y","z"), too_few="debug"):
 x | y        | z        | a_ok  | a_pieces | a_remainder
---|----------|----------|-------|----------|------------
 1 | Just "1" | Just "1" | True  | 3        |
 1 | Just "1" | Just "2" | True  | 3        |
 1 | Just "3" | Nothing  | False | 2        |
 1 | Just "3" | Just "2" | True  | 3        |
 1 | Nothing  | Nothing  | False | 1        |
```

`too_few = "align_start"` は不足分を右側に `NA`(`Nothing`)で埋めます:

```
dfFew |> ... too_few="align_start":
 x | y        | z
---|----------|---------
 1 | Just "1" | Just "1"
 1 | Just "1" | Just "2"
 1 | Just "3" | Nothing
 1 | Just "3" | Just "2"
 1 | Nothing  | Nothing
```

過多のときは `too_many = "drop"`(余剰を捨てる)/ `"merge"`(余剰を最終列に再結合):

```
dfMany |> ... too_many="drop":          dfMany |> ... too_many="merge":
 x | y | z                               x | y | z
---|---|---                             ---|---|------
 1 | 1 | 1                               1 | 1 | 1
 1 | 1 | 2                               1 | 1 | 2
 1 | 3 | 5     (5-6 の 6 を捨てる)         1 | 3 | 5-6
 1 | 3 | 2                               1 | 3 | 2
 1 | 3 | 5     (5-7-9 の 7,9 を捨てる)     1 | 3 | 5-7-9
```

---

## 14.5 文字

ここから先は**実データ babynames**(192 万行)で再現します。

### 14.5.1 長さ(`str_length`)

`str_length()` は文字数(コードポイント数)を返します。NA は NA のまま:

```
str_length(c("a","R for data science", NA)) = [Just 1, Just 18, Nothing]
```

`count(length = str_length(name), wt = n)` で名前の長さ分布(`n` で加重)を出します。
**全 14 種**の長さが現れ、R4DS と完全一致します:

```
babynames |> count(length = str_length(name), wt = n):
  length        n
       2  338,150
       3  8,589,596
       4  48,506,739
       5  87,011,607
       6  90,749,404
       7  72,120,767
       8  25,404,066
       9  11,926,551
      10  1,306,159
      11  2,135,827
      12  16,295
      13  10,845
      14  3,681
      15  830
```

最長の 15 文字の名前を `count(name, wt = n, sort = TRUE)` で(異なる名前は **34 件**):

```
babynames |> filter(str_length(name) == 15) |> count(name, wt = n, sort = TRUE):
  name                n
  Franciscojavier   123
  Christopherjohn   118
  Johnchristopher   118
  Christopherjame   108
  Christophermich    52
  Ryanchristopher    45
```

(`Christopherjohn` と `Johnchristopher` は同票 118 で、tie は名前のアルファベット順で
並びます。これも R4DS の表示順と一致します。)

### 14.5.2 部分取り出し(`str_sub`)

`str_sub(string, start, end)` は **1 始まり・両端含む**で部分文字列を取ります。負の index は
末尾から(`-1` = 最終文字)、範囲外は内側にクリップします:

```
str_sub(c("Apple","Banana","Pear"), 1, 3)   = ["App","Ban","Pea"]
str_sub(c("Apple","Banana","Pear"), -3, -1) = ["ple","ana","ear"]
str_sub("a", 1, 5)                          = "a"     (範囲外はクリップ)
```

最初と最後の文字を取り出す(`mutate(first = str_sub(name,1,1), last = str_sub(name,-1,-1))`):

```
babynames |> mutate(first = str_sub(name,1,1), last = str_sub(name,-1,-1)):  (先頭 6 行)
  name       first last
  Mary       M     y
  Anna       A     a
  Emma       E     a
  Elizabeth  E     h
  Minnie     M     e
  Margaret   M     t
```

---

## 14.6 非英語テキスト

### 14.6.1 Encoding

`charToRaw()` は文字列の **UTF-8 バイト列**を 16 進で返します(hgg では `charToRaw` が
`[Word8]` を返す):

```
charToRaw("Hadley") = 48 61 64 6c 65 79
```

> **encoding の注記**: R4DS は `read_csv(..., locale = locale(encoding = "Latin1"))` 等で
> 非 UTF-8 データを読む例(`El Niño` / `こんにちは`)を挙げます。本リポジトリの
> CSV 読み込みは **UTF-8 前提**です。非 UTF-8 ソースは事前に UTF-8 へ変換してから
> 読み込む運用とし、`guess_encoding` 相当は未実装です(現代のデータはほぼ UTF-8)。

### 14.6.2 文字のバリエーション(`str_equal`)

アクセント付き文字は **合成済**(1 コードポイント `ü` = ü)と **基底+結合**
(`ü` = u + 結合分音記号)の 2 通りで表せます。見た目は同じでも符号列は異なります:

```
u = c("ü", "ü")  (どちらも見た目は ü)
str_length(u)             = [1, 2]       (合成済=1 文字・分解=2 文字)
str_sub(u, 1, 1)          = ["ü","u"]    (分解側の 1 文字目は基底 u だけ)
u[[1]] == u[[2]]          = False        (素の比較は符号列が違うので False)
str_equal(u[[1]], u[[2]]) = True         (NFC 正規化してから比較するので True)
```

hgg の `strEqual` は両辺を **Unicode NFC 正規化**(`unicode-transforms`)してから比較します。

### 14.6.3 Locale 依存の関数

`str_to_upper` / `str_sort` は**本来 locale 依存**です。hgg の `strToUpper` / `strSort`
は **既定 locale**(Unicode コードポイント順)で動きます:

```
str_to_upper(c("i","hello"))      = ["I","HELLO"]
str_sort(c("a","c","ch","h","z")) = ["a","c","ch","h","z"]
```

> **locale の注記**: R4DS はトルコ語(`str_to_upper(c("i","ı"), locale = "tr")` で
> `i` → `İ`)やチェコ語(`str_sort(..., locale = "cs")` で複合文字 "ch" が "h" の後に
> 並ぶ)の例を挙げます。これらは ICU(`text-icu`)等のロケール照合が必要で、本リポジトリ
> では **未採用**です(既定の Unicode コードポイント順のみ)。概念として、文字列の
> 大文字化・ソートは言語によって結果が変わりうる、と理解しておけば十分です。

---

## 14.7 まとめ

本章では文字列の**作成**・**連結**(`str_c`/`str_glue`/`str_flatten`)・**抽出**
(`separate_longer_*`/`separate_wider_*`)・**文字**(`str_length`/`str_sub`)・
**非英語テキスト**(encoding・正規化・locale)を扱いました。次章では、パターンマッチの
ための**正規表現**を学びます。

---

## 演習

> R4DS Ch14 の演習(14.2.4・14.3.4・14.5.3)を hgg で再現します。

### 14.2.4

**(1)** 次の中身を持つ文字列を作る:
  - `He said "That's amazing!"`
  - `\a\b\c\d`
  - `\\\\\\`

```haskell
s1 = "He said \"That's amazing!\""   -- "He said \"That's amazing!\""
s2 = "\\a\\b\\c\\d"                   -- "\a\b\c\d"  (各 \ をエスケープ)
s3 = "\\\\\\"                         -- "\\\"      (バックスラッシュ 3 個)
```

**(2)** `x <- "This is tricky"` を作って調べよ。` ` はどんな特殊文字か?
`str_view()` はどう表示するか?

> **答**: ` ` は **NO-BREAK SPACE(改行なし空白)**。見た目は普通の半角空白と区別が
> つかないが、ここで行が折り返さないことを表す。hgg で `strView ["This\x00a0is\x00a0tricky"]`
> とすると `This is tricky` と表示され(空白に見える)、通常の空白との違いはコードポイント
> `U+00A0`(`charToRaw` で `c2 a0` の 2 バイト)で判別できる。

### 14.3.4

**(1)** `str_c("hi ", NA)` と `str_c(letters[1:2], letters[1:3])` を `paste0()` と比較せよ。

> **答**:
> - `str_c("hi ", NA)` は **NA**(`strCMaybe` で `Nothing` を伝播)。R の `paste0("hi ", NA)`
>   は文字列 `"hi NA"` を返す(NA を文字 "NA" 化する)点が異なる。
> - `str_c(letters[1:2], letters[1:3])` は **長さ不一致でエラー**(recycling は長さ 1 か n のみ)。
>   `strC [["a","b"],["a","b","c"]]` も同様にエラー。R の `paste0` は短い方を recycle して
>   `"aa" "bb" "ac"` を返す(警告つき)。

**(2)** `paste()` と `paste0()` の違いは何か? `str_c()` で `paste()` を再現せよ。

> **答**: `paste0(...)` は区切りなし連結、`paste(...)` は既定で**半角空白区切り**
> (`sep = " "`)。`str_c` / `strC` は区切りなし(= `paste0` 相当)なので、`paste(a, b)` 相当は
> 間に空白リテラルを挟む: `strC [as, [" "], bs]`。

**(3)** 次の `str_c` ↔ `str_glue` を相互変換せよ:
  - `str_c("The price of ", food, " is ", price)`
  - `str_glue("I'm {age} years old and live in {country}")`
  - `str_c("\\section{", title, "}")`

> **答**:
> - `strGlue "The price of {food} is {price}" [("food",food),("price",price)]`
> - `strC [["I'm "], age, [" years old and live in "], country]`
> - `strGlue "\\section{{{title}}}" [("title",title)]`(`{{` / `}}` で literal の `{` `}` を出す)

### 14.5.3

**(1)** babynames の長さ分布を計算するとき、なぜ `wt = n` を使うのか?

> **答**: babynames の各行は `(year, sex, name)` の組で 1 行であり、`n` はその年・性別での
> **出生数**。`wt = n` 無しだと「行数」(= 名前×年×性別の延べ種類数)を数えてしまい、
> 「実際に何人がその長さの名前を付けられたか」にならない。`wt = n` で出生数を加重することで
> 人数ベースの分布になる。

**(2)** `str_length()` と `str_sub()` で各名前の**真ん中の文字**を取り出せ。偶数長はどう扱うか?

> **答**: 長さ `L` の中央位置は `mid = (L + 1) \`div\` 2`(奇数長は唯一の中央、偶数長は
> **左寄りの中央**を取る慣例)。`strSub mid mid name` で 1 文字。例:
> `Mary`(L=4)→ 位置 2 → `"a"`、`Anna`(L=4)→ 位置 2 → `"n"`、`Emma`(L=4)→ `"m"`。
> 偶数長は「2 つの中央のどちらを取るか」を決める必要があり、ここでは左側に固定した。

**(3)** babynames の名前の長さに時代トレンドはあるか? 最初/最後の文字の人気はどうか?

> **答(方針)**: `group_by(year)` で年ごとに `weighted.mean(str_length(name), n)` を取れば
> 平均長の推移が見える(20 世紀後半に向けて平均長が伸びる傾向が知られる)。最初/最後の文字は
> `str_sub(name,1,1)` / `str_sub(name,-1,-1)` で取り、`group_by(year, last) |> summarize(wt=sum(n))`
> で集計すると、女児名の語尾 "a" が近年急増する等のトレンドが見える。本リポジトリでは
> 集計値の算出までを Strings.hs で行える(可視化は Ch13 までの geom で別途描ける)。

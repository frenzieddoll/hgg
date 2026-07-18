# 14. Strings

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.14 "Strings"**
> <https://r4ds.hadley.nz/strings>
> Data: **babynames** (US SSA · `year,sex,name,n,prop` · 1,924,665 rows total) + demo data.

Strings are a fundamental data type in data analysis. This chapter learns operations corresponding to the `str_*` functions of stringr (tidyverse).

- **String creation** (escaping · raw strings · special characters `\n` `\t` `\u`)
- **Creating multiple strings from data** (`str_c` / `str_glue` / `str_flatten`)
- **Extracting data from strings** (`separate_longer_*` / `separate_wider_*` · `too_few`/`too_many`)
- **Characters** (`str_length` · `str_sub` with positive/negative indices)
- **Non-ASCII text** (encoding=`charToRaw` · normalized comparison `str_equal` · locale-dependent)

Run code: [`Strings.hs`](Strings.hs)

```sh
cd docs/tutorials/14-strings
cabal run tut-14-strings
```

> **This chapter emphasizes tables and vector output.** The only visual element in R4DS Ch14 is a single RStudio autocomplete **screenshot**, with no ggplot figures from real data. This chapter faithfully **reproduces all output** from actual data (US SSA babynames · 1.92 million rows).

> **stringr/tidyr functions are implemented on the analyze side.** `str_c` / `str_glue` / `str_flatten` /
> `separate_longer_*` / `separate_wider_*` / `str_length` / `str_sub` / `str_equal` /
> `charToRaw` and others are already implemented in this repository's statistics library
> `Hanalyze.Data.Strings`
> (Phase 28 Ch14). This chapter calls them to reproduce R4DS outputs. Accented character normalization comparison uses `unicode-transforms` (NFC).

---

## 14.2 String creation

In R, strings can be quoted with either `"` or `'` (tidyverse style guide recommends `"` except when the string contains `"`). Haskell string literals use only `"..."`.

### 14.2.1 Escaping

Special characters are escaped with backslash `\`. Single quote, double quote, and backslash itself become literal characters when escaped.

| R | hgg (Haskell) |
|---|---|
| `single_quote <- '\''` | `singleQuote = "'"` |
| `double_quote <- "\""` | `doubleQuote = "\""` |
| `backslash <- "\\"` | `backslash = "\\"` |

`str_view()` displays the **contents** of strings (after escaping). This chapter uses an equivalent `strView` helper:

```
x = c(single_quote, double_quote, backslash):
[1] | '
[2] | "
[3] | \
```

### 14.2.2 Raw strings

R 4.0.0+ has **raw string** syntax `r"(...)"` to avoid excessive escaping.

> **hgg note**: Haskell's standard string literals **do not have** syntax equivalent to R's `r"(...)"``. Backslashes and special characters are escaped normally (`"\\"` for one backslash). This is a language difference; the concept from R4DS (avoiding escaping hell) is sufficient to understand.

### 14.2.3 Other special characters

Besides `\n` (newline) and `\t` (tab), Unicode can be written with `\u` (4 digits) / `\U` (8 digits).
Haskell uses hexadecimal escapes like `\x00b5` (µ) · `\x1f604` (😄).

```
x = c("one\ntwo", "one\ttwo", "µ", "\U0001f604"):
[1] | one{\n}two
[2] | one{\t}two
[3] | µ
[4] | 😄
```

(`{\n}` and `{\t}` are visualizations of control characters; the actual values are newline and tab.)

---

## 14.3 Creating multiple strings from data

### 14.3.1 `str_c()`

`str_c()` combines multiple vectors using **recycling rules** (length 1 or n) and concatenates row by row.
Literals are passed as length-1 columns.

| R | hgg |
|---|---|
| `str_c("x", "y")` | `strC [["x"],["y"]]` |
| `str_c("Hello ", c("John","Susan"))` | `strC [["Hello "], ["John","Susan"]]` |

```
str_c("x", "y")                    = ["xy"]
str_c("x", "y", "z")               = ["xyz"]
str_c("Hello ", c("John","Susan")) = ["Hello John","Hello Susan"]
```

Within `mutate()`, missing values `NA` are **propagated** (`strCMaybe` propagates `Nothing`):

```
df |> mutate(greeting = str_c("Hi ", name, "!")):  (NA propagates)
  name="Flora"  greeting="Hi Flora!"
  name="David"  greeting="Hi David!"
  name="Terra"  greeting="Hi Terra!"
  name=NA       greeting=NA
```

`coalesce()` replaces missing values before concatenation (in hgg: `maybe d id`):

```
df |> mutate(greeting1 = str_c("Hi ", coalesce(name, "you"), "!")):
  Hi Flora!
  Hi David!
  Hi Terra!
  Hi you!
```

### 14.3.2 `str_glue()`

`str_glue()` interpolates variables using `{}`. Escape literal braces with `{{` / `}}`.

| R | hgg |
|---|---|
| `str_glue("Hi {name}!")` | `strGlue "Hi {name}!" [("name", names)]` |

```
df |> mutate(greeting = str_glue("Hi {name}!")):
  Hi Flora!
  Hi David!
  Hi Terra!
  Hi NA!

df |> mutate(greeting = str_glue("{{Hi {name}!}}")):  (brace escaping)
  {Hi Flora!}
  {Hi David!}
  {Hi Terra!}
  {Hi NA!}
```

### 14.3.3 `str_flatten()`

`str_flatten()` collapses a vector into a **single string** (works well with `summarize()`).
In hgg: `strFlatten` (= `T.intercalate`):

```
str_flatten(c("x","y","z"))        = "xyz"
str_flatten(c("x","y","z"), ", ")  = "x, y, z"
```

`group_by()` |> `summarize()` concatenates values per group:

```
df |> group_by(name) |> summarize(fruits = str_flatten(fruit, ", ")):
  Carmen   banana, apple
  Marvin   nectarine
  Terence  cantaloupe, papaya, mandarin
```

> **Note**: R's `str_flatten(..., last = ", and ")` (changing only the last separator) is not yet supported in this helper. If needed, split and concatenate the last element manually (not used in this chapter).

---

## 14.4 Extracting data from strings

tidyr's 4 functions have regular names: `separate_[longer/wider]_[delim/position]`.

### 14.4.1 Splitting rows (`separate_longer_*`)

`separate_longer_delim()` expands one row into **multiple rows** by a delimiter:

| R | hgg |
|---|---|
| `df1 \|> separate_longer_delim(x, delim = ",")` | `separateLongerDelim "x" "," df1` |

```
df1 |> separate_longer_delim(x, delim = ","):   (x = "a,b,c" / "d,e" / "f" → 6 rows)
 x
----
 a
 b
 c
 d
 e
 f
```

`separate_longer_position()` expands by fixed width:

```
df2 |> separate_longer_position(x, width = 1):  (x = "1211" / "131" / "21" → 9 rows)
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

### 14.4.2 Splitting columns (`separate_wider_*`)

`separate_wider_delim()` splits one cell into **multiple columns** by a delimiter (row count unchanged).
`Nothing` in `names` (R's `NA`) discards that piece:

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

df3 |> separate_wider_delim(x, ".", names = c("code", NA, "year")):  (NA discards edition column)
 code | year
------|-----
 a10  | 2022
 b10  | 2011
 e15  | 2015
```

`separate_wider_position()` splits by fixed width (column name and character count pairs):

```
df4 |> separate_wider_position(x, c(year=4, age=2, state=2)):
 year | age | state
------|-----|------
 2022 | 15  | TX
 2021 | 22  | LA
 2023 | 25  | CA
```

### 14.4.3 Column split diagnostics (`too_few` / `too_many`)

When piece count doesn't match name count, specify a policy. **Default is error for both.**
For shortage: `TooFew` (`AlignStart` / `AlignEnd` / `TooFewError` / `TooFewDebug`), for excess:
`TooMany` (`DropExtra` / `MergeExtra` / `TooManyError` / `TooManyDebug`):

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

`too_few = "align_start"` fills the shortage with `NA` (`Nothing`) on the right:

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

For excess, use `too_many = "drop"` (discard excess) / `"merge"` (recombine excess into final column):

```
dfMany |> ... too_many="drop":          dfMany |> ... too_many="merge":
 x | y | z                               x | y | z
---|---|---                             ---|---|------
 1 | 1 | 1                               1 | 1 | 1
 1 | 1 | 2                               1 | 1 | 2
 1 | 3 | 5     (discard 6 from 5-6)       1 | 3 | 5-6
 1 | 3 | 2                               1 | 3 | 2
 1 | 3 | 5     (discard 7,9 from 5-7-9)   1 | 3 | 5-7-9
```

---

## 14.5 Characters

From here on, we use **real data babynames** (1.92 million rows).

### 14.5.1 Length (`str_length`)

`str_length()` returns character count (code point count). NA stays NA:

```
str_length(c("a","R for data science", NA)) = [Just 1, Just 18, Nothing]
```

`count(length = str_length(name), wt = n)` produces name length distribution (weighted by `n`).
**All 14 lengths** appear and exactly match R4DS:

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

The longest 15-character names (counted by distinct names: **34 total**):

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

(`Christopherjohn` and `Johnchristopher` tie at 118; ties are sorted alphabetically by name, matching R4DS order.)

### 14.5.2 Substring extraction (`str_sub`)

`str_sub(string, start, end)` extracts substrings using **1-based, both-inclusive** indexing.
Negative indices count from the end (`-1` = last character); out-of-bounds indices are clipped:

```
str_sub(c("Apple","Banana","Pear"), 1, 3)   = ["App","Ban","Pea"]
str_sub(c("Apple","Banana","Pear"), -3, -1) = ["ple","ana","ear"]
str_sub("a", 1, 5)                          = "a"     (out of bounds clips)
```

Extract first and last character (`mutate(first = str_sub(name,1,1), last = str_sub(name,-1,-1))`):

```
babynames |> mutate(first = str_sub(name,1,1), last = str_sub(name,-1,-1)):  (first 6 rows)
  name       first last
  Mary       M     y
  Anna       A     a
  Emma       E     a
  Elizabeth  E     h
  Minnie     M     e
  Margaret   M     t
```

---

## 14.6 Non-ASCII text

### 14.6.1 Encoding

`charToRaw()` returns a string's **UTF-8 bytes** in hexadecimal (in hgg, `charToRaw` returns `[Word8]`):

```
charToRaw("Hadley") = 48 61 64 6c 65 79
```

> **Encoding note**: R4DS shows examples reading non-UTF-8 data via `read_csv(..., locale = locale(encoding = "Latin1"))` (`El Niño` / `こんにちは`). This repository assumes **UTF-8** for CSV input. Non-UTF-8 sources should be pre-converted to UTF-8 before reading; `guess_encoding` is not implemented (modern data is almost always UTF-8).

### 14.6.2 Character variation (`str_equal`)

Accented characters can be represented **precomposed** (one code point `ü` = ü) or **base + combining**
(`ü` = u + combining diaeresis). They look identical but have different byte sequences:

```
u = c("ü", "ü")  (both appear as ü)
str_length(u)             = [1, 2]       (precomposed=1 char · decomposed=2 chars)
str_sub(u, 1, 1)          = ["ü","u"]    (decomposed: first char is base u only)
u[[1]] == u[[2]]          = False        (direct comparison: different byte sequences)
str_equal(u[[1]], u[[2]]) = True         (NFC normalized before comparison)
```

hgg's `strEqual` **Unicode NFC normalizes** both sides (`unicode-transforms`) before comparing.

### 14.6.3 Locale-dependent functions

`str_to_upper` / `str_sort` are **inherently locale-dependent**. hgg's `strToUpper` / `strSort`
operate in **default locale** (Unicode code point order):

```
str_to_upper(c("i","hello"))      = ["I","HELLO"]
str_sort(c("a","c","ch","h","z")) = ["a","c","ch","h","z"]
```

> **Locale note**: R4DS shows Turkish examples (`str_to_upper(c("i","ı"), locale = "tr")` yields `i` → `İ`) and Czech (`str_sort(..., locale = "cs")` sorts composite "ch" after "h"). These require ICU (`text-icu`) locale collation, **not adopted** in this repository (Unicode code point order only). As a concept: string uppercase and sorting can vary by language.

---

## 14.7 Summary

This chapter covered string **creation** · **concatenation** (`str_c`/`str_glue`/`str_flatten`) · **extraction**
(`separate_longer_*`/`separate_wider_*`) · **characters** (`str_length`/`str_sub`) ·
**non-ASCII text** (encoding · normalization · locale). The next chapter covers **regular expressions** for pattern matching.

---

## Exercises

> Reproduce R4DS Ch14 exercises (14.2.4 · 14.3.4 · 14.5.3) in hgg.

### 14.2.4

**(1)** Create strings with these contents:
  - `He said "That's amazing!"`
  - `\a\b\c\d`
  - `\\\\\\`

```haskell
s1 = "He said \"That's amazing!\""   -- "He said \"That's amazing!\""
s2 = "\\a\\b\\c\\d"                   -- "\a\b\c\d"  (escape each \)
s3 = "\\\\\\"                         -- "\\"        (3 backslashes)
```

**(2)** Create `x <- "This is tricky"` and investigate. What is the special character ` `?
How does `str_view()` display it?

> **Answer**: ` ` is **NO-BREAK SPACE** (space that prevents line breaks). It looks identical to a regular space but signals no break here. In hgg, `strView ["This\x00a0is\x00a0tricky"]` displays as `This is tricky` (space-like), distinguishable from regular space by code point `U+00A0` (`charToRaw` returns `c2 a0` as 2 bytes).

### 14.3.4

**(1)** Compare `str_c("hi ", NA)` and `str_c(letters[1:2], letters[1:3])` with `paste0()`.

> **Answer**:
> - `str_c("hi ", NA)` returns **NA** (`strCMaybe` propagates `Nothing`). R's `paste0("hi ", NA)` returns `"hi NA"` (converting NA to the character "NA"), different behavior.
> - `str_c(letters[1:2], letters[1:3])` is an **error** (recycling only for length 1 or n).
>   `strC [["a","b"],["a","b","c"]]` errors similarly. R's `paste0` recycles the shorter: `"aa" "bb" "ac"` (with warning).

**(2)** What's the difference between `paste()` and `paste0()`? Reproduce `paste()` with `str_c()`.

> **Answer**: `paste0(...)` concatenates without separator, `paste(...)` uses **space as default separator**
> (`sep = " "`). `str_c` / `strC` have no separator (= `paste0`), so `paste(a, b)` equivalent: `strC [as, [" "], bs]`.

**(3)** Convert between `str_c` ↔ `str_glue` (bidirectional):
  - `str_c("The price of ", food, " is ", price)`
  - `str_glue("I'm {age} years old and live in {country}")`
  - `str_c("\\section{", title, "}")`

> **Answer**:
> - `strGlue "The price of {food} is {price}" [("food",food),("price",price)]`
> - `strC [["I'm "], age, [" years old and live in "], country]`
> - `strGlue "\\section{{{title}}}" [("title",title)]` (`{{` / `}}` emit literal `{` `}`)

### 14.5.3

**(1)** When computing babynames length distribution, why use `wt = n`?

> **Answer**: Each row in babynames is one `(year, sex, name)` combination; `n` is the **birth count** for that year/sex. Without `wt = n`, you'd count rows (= name × year × sex combinations), not "how many people got that name length." Weighting by `n` gives per-person distribution.

**(2)** Extract the **middle character** of each name using `str_length()` and `str_sub()`. How to handle even length?

> **Answer**: For length `L`, middle position: `mid = (L + 1) \`div\` 2` (odd length has one middle; even length takes **left-center** by convention). Extract with `strSub mid mid name`. Examples:
> `Mary` (L=4) → position 2 → `"a"`, `Anna` (L=4) → position 2 → `"n"`, `Emma` (L=4) → `"m"`.
> Even length requires deciding which of two centers to take; here we fixed left.

**(3)** Is there a time trend in babynames name length? How popular are first/last characters?

> **Answer (approach)**: `group_by(year)` with `weighted.mean(str_length(name), n)` shows average length trend over time (known: average length increases toward late 20th century). For first/last characters, extract via `str_sub(name,1,1)` / `str_sub(name,-1,-1)`, then `group_by(year, last) |> summarize(wt=sum(n))` reveals trends like rising popularity of "a" as ending for female names recently. This repository supports aggregation in Strings.hs (visualization via Ch13 geoms separately).

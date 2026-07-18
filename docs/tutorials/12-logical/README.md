# 12. Logical Vectors

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.12 "Logical vectors"**
> <https://r4ds.hadley.nz/logicals>
> Data: **nycflights13** `flights` (all 336,776 rows) + dummy vectors for explanation.

Logical vectors are the simplest type—each element holds only **3 values**: `TRUE` / `FALSE` / `NA`.
Raw data rarely starts with logical vectors, but nearly all analyses create and manipulate them mid-flow.
This chapter covers:

- **Numerical comparison** (`< <= > >= != ==`) creation and floating-point pitfalls (`near`)
- Missing values **"propagate"** and `is.na()`
- **Boolean algebra** (`& | ! xor`), operator precedence pitfalls, `%in%`
- **Summaries** (`any`/`all`, `sum`/`mean`, logical subsetting)
- **Conditional transforms** (`if_else` / `case_when`)

Execution code is in [`Logicals.hs`](Logicals.hs).

```sh
cd docs/tutorials/12-logical
cabal run tut-12-logical
```

> **This chapter stars tables and vector output.** R4DS Ch.12 contains only one figure—a conceptual
> **Venn diagram** (`diagrams/transform.png`)—and **zero ggplot plots of real data**. We explain the
> concepts in prose and **faithfully reproduce vector/table outputs of each operation** with real data
> (explanation diagrams are outside statistical plot libraries' scope).

> **R functions self-implemented.** Functions like `near` / `%in%` / `if_else` / `case_when` / `any` /
> `all` / `is.na` lacking direct Haskell equivalents are implemented as small helpers in
> [`Logicals.hs`](Logicals.hs) (CLAUDE.md's "fill gaps with implementation"). 3-valued logic uses
> `Maybe Bool` (`Just True` / `Just False` / `Nothing`=NA) and defines `andK` / `orK` / `xorK`
> following Kleene's 3-valued logic.

---

## 12.1 Introduction

To explain individual logical vector functions, we create dummy data with `c()`. Operations on raw
vectors apply directly to dataframe variables via `mutate()`.

```haskell
x = [1,2,3,5,7,11,13] :: [Int]
-- x * 2
map (*2) x   -- [2,4,6,10,14,22,26]
```

Equivalent to `tibble(x) |> mutate(y = x * 2)`:

```
 x  |  y
----|----
1   | 2
2   | 4
...
13  | 26
```

---

## 12.2 Comparison

Numerical comparison is the most common way to create logical vectors. So far we've created logical
vectors **on-the-fly** inside `filter()` and thrown them away. Example: "flights departing daytime
and arriving roughly on schedule":

| R | hgg |
|---|---|
| `filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)` | Build logical vectors with `andK`/`cmpM`, keep rows where `Just True` |

That's a shortcut; we can use `mutate()` to **name intermediate logical variables**. For complex
conditions, naming each stage aids readability and checking.

```haskell
daytime  = andK (dep_time > 600) (dep_time < 2000)   -- 3-valued logic (NA propagates)
approxOT = abs(arr_delay) < 20
```

Output from `.keep = "used"` keeping used and new columns (all 336,776 rows):

```
dep_time  | arr_delay  | daytime | approx_ontime
----------|------------|---------|--------------
Just 517  | Just 11    | FALSE   | TRUE
Just 533  | Just 20    | FALSE   | FALSE
...
```

`filter(daytime & approx_ontime)` yields **172,286 rows**.

### 12.2.1 Floating-point comparison

Numerical `==` needs care. `c(1/49*49, sqrt(2)^2)` appear to be 1 and 2, but:

```
x (R default 7-digit display style) = 1 2
x == c(1, 2)                        = FALSE FALSE
print(x, digits = 16)               = 0.9999999999999999 2.0000000000000004
near(x, c(1, 2))                    = TRUE TRUE
```

Computers hold numbers with fixed precision, so `1/49` and `sqrt(2)` can't be exactly represented;
calculation results drift slightly. `dplyr::near()` (default tolerance ≈ 1.49e-8) ignores tiny
differences.

### 12.2.2 Missing values (comparison)

Missing values represent "unknown", so **they propagate**. Operations involving unknown values become
nearly unknown.

```
NA > 5    = NA
10 == NA  = NA
NA == NA  = NA
```

Most confusing: `NA == NA` yields `NA`. Thinking "Mary's age is unknown, John's age is unknown—are
they the same age?" → **unknown** makes sense.

So `filter(dep_time == NA)` doesn't work. `dep_time == NA` produces `NA` for all rows, and `filter()`
automatically drops missing rows, yielding **0 rows**.

```
flights |> filter(dep_time == NA):
# 0 rows × 19 columns
```

### 12.2.3 `is.na()`

`is.na(x)` works on any type, returning `TRUE` for missing and `FALSE` otherwise.

```
is.na(c(TRUE, NA, FALSE)) = FALSE TRUE FALSE
is.na(c(1, NA, 3))        = FALSE TRUE FALSE
is.na(c("a", NA, "b"))    = FALSE TRUE FALSE
```

Find rows where `dep_time` is missing (= cancelled flights)—**8,255 rows**:

```
flights |> filter(is.na(dep_time)):
# 8,255 rows × 19 columns
```

`is.na()` also helps with `arrange()`. `arrange()` by default puts missing at the end, but
`arrange(desc(is.na(dep_time)), dep_time)` puts missing **first**.

---

## 12.3 Boolean algebra

Multiple logical vectors combine via **Boolean algebra**. In R: `&`=and, `|`=or, `!`=not, `xor()`=XOR.

> **On Figure 12.1 (R4DS Venn diagram).** R4DS shows Venn diagrams with 2 circles `x` and `y`,
> highlighting regions for each operation: `x & !y` = x minus y / `x & y` = intersection /
> `!x & y` = y minus x / `x` = x all / `xor(x,y)` = all but intersection / `y` = y all /
> `x | y` = all. This is a **conceptual hand-drawn sketch**, not a real-data figure, so we explain
> in prose (outside statistical plot library scope).

```
!is.na(x), x=c(1,NA,-15,5)        = TRUE FALSE TRUE TRUE
x < -10 | x > 0                    = TRUE NA TRUE TRUE
xor(x > 0, x < 3), x=c(1,NA,-15,5) = FALSE NA TRUE TRUE
```

> `&&` and `||` are **short-circuit** operators returning only a single `TRUE`/`FALSE`. They're for
> programming, not for dplyr (vector operations).

### 12.3.1 Missing values (Boolean algebra)

Missing value rules in Boolean algebra appear inconsistent.

```
tibble(x = c(TRUE, FALSE, NA)) |> mutate(and = x & NA, or = x | NA):
  x   |  and  |  or
------|-------|-----
TRUE  | NA    | TRUE
FALSE | FALSE | NA
NA    | NA    | NA
```

`NA | TRUE` is `TRUE` (at least one is true); `NA | FALSE` is `NA` (NA's truth is unknown). Similarly
for `&`: `NA & FALSE` is `FALSE` (at least one is false); `NA & TRUE` is `NA` (unknown). This
implementation's `andK`/`orK` encode this Kleene 3-valued logic.

### 12.3.2 Operator precedence

Operator precedence differs from English word order. Writing "flights departing in November or
December" as English `filter(month == 11 | 12)` causes no error but **doesn't work**.

R first evaluates `month == 11` (call it `nov`), then `nov | 12`. With logical operators, numbers
→ `TRUE` except 0, so this becomes `nov | TRUE` = **always TRUE**, selecting all rows.

```
flights |> mutate(nov = month == 11, final = nov | 12, .keep = "used"):
month | nov   | final
------|-------|------
1     | FALSE | TRUE
...
All 336,776 rows have final == TRUE (= all rows; correct form: month == 11 | month == 12)
```

### 12.3.3 `%in%`

Simple way to avoid `==` and `|` precedence mistakes: `%in%`. `x %in% y` returns a logical vector
matching `x`'s length; `TRUE` where `x`'s value appears in `y`.

```
1:12 %in% c(1, 5, 11)               = TRUE FALSE FALSE FALSE TRUE FALSE FALSE FALSE FALSE FALSE TRUE FALSE
letters[1:10] %in% c(a,e,i,o,u)     = TRUE FALSE FALSE FALSE TRUE FALSE FALSE FALSE TRUE FALSE
```

`%in%` follows different rules for `NA` than `==`; `NA %in% NA` is `TRUE`.

```
c(1, 2, NA) == NA                    = NA NA NA
c(1, 2, NA) %in% NA                  = FALSE FALSE TRUE
```

This becomes a convenient shortcut. `filter(dep_time %in% c(NA, 0800))` returns rows where `dep_time`
is missing or 800 (leading zero in `0800` ignored)—**8,803 rows**.

---

## 12.4 Summaries

### `any()` / `all()`

Key logical summaries are `any()` and `all()`. `any(x)` is equivalent to `|`: `TRUE` if `x` has any
`TRUE`; `all(x)` equivalent to `&`: `TRUE` only if all elements are `TRUE`. `na.rm = TRUE` excludes
missing. Daily: "all flights delayed ≤60 min?" and "any flights delayed ≥5 hours?"

```
group_by(year, month, day) |> summarize(
  all_delayed = all(dep_delay <= 60, na.rm=T),
  any_long_delay = any(arr_delay >= 300, na.rm=T)):
year | month | day | all_delayed | any_long_delay
-----|-------|-----|-------------|---------------
2013 | 1     | 1   | FALSE       | TRUE
2013 | 1     | 2   | FALSE       | TRUE
...
# 365 rows × 5 columns
```

### 12.4.1 Numerical summaries of logical vectors

In numerical context, logical values convert: `TRUE`→1, `FALSE`→0. So `sum(x)` counts `TRUE`
**occurrences**; `mean(x)` is the **proportion** of `TRUE`.

```
summarize(
  proportion_delayed = mean(dep_delay <= 60, na.rm=T),
  count_long_delay = sum(arr_delay >= 300, na.rm=T)):
2013 | 1 | 1 | 0.9391408114558473 | 3
2013 | 1 | 2 | 0.9144385026737968 | 3
...
```

### 12.4.2 Logical subsetting

Use logical vectors to **partially extract a single variable** (base's `[` operator). To see average
delay for actually-delayed flights, `filter` first works, but **column subsetting** lets you compute
delayed and early arrival averages in one summarize.

```
summarize(behind = mean(arr_delay[arr_delay>0], na.rm=T),
          ahead  = mean(arr_delay[arr_delay<0], na.rm=T), n = n()):
2013 | 1 | 1 | 32.48156182212581 | -12.495798319327731 | 842
...
```

> Watch group size: `filter(arr_delay > 0)` then `n()` gives "count of delayed flights"; column
> subsetting's `n()` is "total flight count" (above n=842 is all flights for 1/1).

---

## 12.5 Conditional transforms

Logical vectors' most powerful use is **conditional transformation** (if condition x, then A; if y,
then B). Two tools: `if_else()` and `case_when()`.

### `if_else()`

Condition `TRUE` → 2nd arg; `FALSE` → 3rd arg; optional 4th arg `missing` used when input is `NA`.

```
x = c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")        = "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" NA
if_else(x > 0, "+ve", "-ve", "???") = "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" "???"
if_else(x < 0, -x, x)  (= abs)      = 3 2 1 0 1 2 3 NA
if_else(is.na(x1), y1, x1)          = 3 1 2 6   -- coalesce-like
```

`true`/`false` can be vectors; mixing is OK (above `abs` and coalesce-like). Nesting `if_else` for
"0 is neither positive nor negative" works, but as conditions grow, readability drops; switch to
`case_when()`.

### 12.5.1 `case_when()`

SQL `CASE`-inspired syntax taking `condition ~ output` pairs, returning output for the first `TRUE`.

```
case_when(x==0~"0", x<0~"-ve", x>0~"+ve", is.na(x)~"???") = -ve -ve -ve 0 +ve +ve +ve ???
case_when(x<0~"-ve", x>0~"+ve")              = -ve -ve -ve NA  +ve +ve +ve NA    -- non-match is NA
case_when(x<0~"-ve", x>0~"+ve", .default="???") = -ve -ve -ve ??? +ve +ve +ve ??? -- default value
case_when(x>0~"+ve", x>2~"big")              = NA NA NA NA +ve +ve +ve NA          -- first match wins
```

Both sides can use variables; mixing is OK. `flights` arrival delay with readable labels:

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

> Mixing `<` and `>` easily creates overlapping conditions (R4DS author admits doing so in early
> tries).

### 12.5.2 Compatible types

`if_else()` / `case_when()` output must be **compatible types**. In R, `if_else(TRUE, "a", 1)` (string
and number) or `case_when(... ~ TRUE, ... ~ now())` (logical and datetime) error. Main compatible
combos:

- Numeric and logical (`TRUE`→1, `FALSE`→0)
- String and factor (factor ≈ string with restricted range)
- Date and datetime (date is datetime special case)
- `NA` is technically logical but **compatible with all types**

> **Haskell is statically typed**, so these become **compile errors**, not runtime. This chapter's
> `if_else` helper requires `true`/`false` be the same type; the type system guarantees compatibility.

---

## Exercises (R4DS Ch.12)

R4DS exercises are solvable with this chapter's tools (`Logicals.hs` helpers enable reproduction):

1. How `near()` works; is `sqrt(2)^2` near 2? (→ `TRUE`).
2. Relate missing in `dep_time`/`sched_dep_time`/`dep_delay` via `is.na()`+`count()`.
3. November/December flights via `month %in% c(11, 12)`.
4. Parity check `if_else(x %% 2 == 0, ...)`, weekend/weekday, `if_else` absolute value.
5. US holidays (New Year, Independence Day, Thanksgiving, Christmas) from `month`/`day` via `case_when`.

---

## Summary

Logical vectors hold only 3 values—`TRUE`/`FALSE`/`NA`—yet carry great power. We learned creation via
comparison and `is.na()`, combination with `! & |`, summaries via `any`/`all`/`sum`/`mean`, and
conditional transforms with `if_else`/`case_when`. Logical vectors recur throughout later chapters
(string matching via `str_detect`, date comparison, etc.).

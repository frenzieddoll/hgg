# 13. Numbers

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.13 "Numbers"**
> <https://r4ds.hadley.nz/numbers>
> Data: **nycflights13** `flights` (all 336,776 rows) + dummy vectors.

We systematically survey what's possible with numerical vectors: string-to-number conversion, `count()`,
numerical transforms (recycling, remainder, rounding, binning, cumulative), general transforms (rank,
offset, consecutive ID), and numerical summaries (center, quantile, spread, distribution, position).
Execution code is in [`Numbers.hs`](Numbers.hs).

```sh
cd docs/tutorials/13-numbers
cabal run tut-13-numbers
```

> **High-level API by default.** R4DS numerical functions are implemented as **hanalyze public APIs**
> (avoiding re-implementation in every demo):
> - **Descriptive statistics** `mean`/`median`/`quantile`(R type-7)/`sd`/`var`/`IQR` →
>   `Hanalyze.Stat.Descriptive` (Phase 65)
> - **dplyr verbs** `min_rank`/`lag`/`lead`/`cumsum`/`cut`/`consecutive_id` →
>   `Hanalyze.Data.Transform` (Phase 66)
> - **`summarise`/`mutate`/`groupBy`** (DataFrame-coupled, symmetric to plot's `df |>>`) →
>   `Hanalyze.Data.Wrangle` (Phase 67)
>
> Only small items like string-to-number (`parse_number`) and `pmin/pmax` are tutorial-local.

---

## 13.1 Making numbers

Two string-to-number functions (tutorial local helper in `Numbers.hs`). `parseDouble` takes numeric
strings directly (`read`, scientific notation `1e3` too); `parseNumber` strips currency symbols, commas,
`%`, etc., extracting the numeric part.

```haskell
map (show . parseDouble) ["1.2", "5.6", "1e3"]          -- R parse_double
map (show . parseNumber) ["$1,234", "USD 3,513", "59%"]  -- R parse_number
```

Output:

```
parse_double(c("1.2","5.6","1e3"))          = 1.2 5.6 1000.0
parse_number(c("$1,234","USD 3,513","59%")) = 1234.0 3513.0 59.0
```

## 13.2 Counting

`count()` is exploration staple. Reproduce via `groupBy [...] |> summarise [ "n" =: nOf ]`:

```haskell
flights |> groupBy ["dest"] |> summarise [ "n" =: nOf ]                  -- count(dest)
flights |> groupBy ["dest"] |> summarise [ "n" =: nOf, "delay" =: meanOf "arr_delay" ]
```

`sort = TRUE` equivalent is reordering by `n` descending. Top destinations: ORD (17,283), ATL
(17,215), LAX (16,174), etc. `n_distinct(carrier)` (carrier count), weighted counts `sum(distance)`,
missing count `sum(is.na(dep_time))` (= cancelled count) work similarly.

| R | hgg |
|---|---|
| `count(dest)` | `groupBy ["dest"] \|> summarise ["n" =: nOf]` |
| `summarize(delay = mean(arr_delay, na.rm=T))` | `meanOf "arr_delay"` (na.rm default) |
| `summarize(carriers = n_distinct(carrier))` | Text columns → distinct count via `Set` (Wrangle v1's `nDistinctOf` is numeric-only) |
| `count(tailnum, wt = distance)` | `summarise ["miles" =: sumOf "distance"]` |

## 13.3 Numerical transforms

### 13.3.1 Arithmetic and recycling rules

Vectors of different length **recycle** (repeat) the shorter. `x / 5` uses `5` four times. Recycling
also applies to `==`, so `filter(month == c(1,2))` is a trap: it matches odd rows = month 1, even rows
= month 2 (correct: `month %in% c(1,2)`).

### 13.3.2 pmin / pmax (row-wise) vs min / max (summary)

`pmin(x, y)` is **row-wise** minimum (tutorial local helpers `pmin'`/`pmax'` = `zipWith`); `min(x, y)`
is **single whole value**. Output:

```
pmin(x,y,na.rm=T) = 1.0 2.0 7.0      pmax(x,y,na.rm=T) = 3.0 5.0 7.0
min(x,y,na.rm=T)  = 1.0              max(x,y,na.rm=T)  = 7.0   (= easy to confuse)
```

### 13.3.3 Remainder `%/%` and `%%`

R's `%/%` (integer division) and `%%` (remainder) are Haskell's `div` / `mod`. Decompose
`sched_dep_time` to hours and minutes (R's `mutate(hour = …, minute = …)` equivalent; actual code
uses `insertVector`):

```haskell
let schedV  = colPlain @Int "sched_dep_time" flights
    hourMin = DF.insertVector "minute" (V.fromList (map (`mod` 100) schedV))
            $ DF.insertVector "hour"   (V.fromList (map (`div` 100) schedV))
            $ DF.select ["sched_dep_time"] flights
```

R's `1:10 %/% 3` / `1:10 %% 3`:

```haskell
map (`div` 3) [1..10]   -- %/%
map (`mod` 3) [1..10]   -- %%
```

Combine with cancellation rate `is.na(dep_time)` proportion by time, viewing **cancellation rate by
departure time** (`fig1`):

![cancellation rate vs departure time](fig1-prop-cancelled.svg)

Cancellation rate rises from ~0.5% morning to ~4% around 7 PM, then drops sharply toward midnight
(point size = flight count).

### 13.3.4 Rounding (banker's rounding)

`round(x)` rounds to nearest integer. 2nd arg specifies digits (`round(x,-1)` = tens place).
`round` uses **round half to even**, so `round(c(1.5, 2.5))` = `2 2` (both round to even). Haskell's
`round` already uses half-to-even; digit specification is a local helper `roundTo`. Output:

```
round(123.456)=123  round(.,2)=123.46  round(.,1)=123.5  round(.,-1)=120  round(.,-2)=100
floor(123.456)=123  ceiling(123.456)=124
```

### 13.3.5 Binning `cut`

Divide numerical vectors into discrete boxes (`Data.Transform`'s `cut`/`cutLabels`; default is
right-closed interval `(a, b]`; out-of-range is `Nothing`=NA).

```haskell
Tr.cut [0,5,10,15,20] [1,2,5,10,15,20]                       -- bin index
Tr.cutLabels ["sm","md","lg","xl"] [0,5,10,15,20] [1,2,5,10,15,20]
```

Output:

```
cut(x, breaks=c(0,5,10,15,20)) = bin 1 1 1 2 3 4
With labels (sm/md/lg/xl)      = sm sm sm md lg xl
```

### 13.3.6 Cumulative `cumsum`

`Data.Transform`'s `cumsum`/`cumprod`/`cummin`/`cummax`/`cummean`. `Tr.cumsum [1..10]` =
`1 3 6 … 55`.

## 13.4 General transforms

### 13.4.1 Rank

`minRank` is fundamental (ties: 1,2,2,4). Descending via `Data.Ord.Down`. Also `rowNumber` /
`denseRank` / `percentRank` / `cumeDist`. `*NA` variants rank while preserving NA (`Nothing`)
(`Data.Transform`):

```haskell
let xrk = [Just 1, Just 5, Just 5, Just 17, Just 22, Nothing] :: [Maybe Int]
Tr.minRankNA xrk                       -- min_rank(x)
Tr.minRankNA (map (fmap Down) xrk)     -- min_rank(desc(x))
Tr.rowNumberNA xrk                     -- row_number(x), plus denseRankNA / percentRankNA / cumeDistNA
```

Output:

```
x = c(1,5,5,17,22,NA)
min_rank(x)   = 1 2 2 4 5 NA      min_rank(desc(x)) = 5 3 3 2 1 NA
row_number(x) = 1 2 3 4 5 NA      dense_rank(x)     = 1 2 2 3 4 NA
percent_rank  = 0 .25 .25 .75 1 NA  cume_dist       = .2 .6 .6 .8 1 NA
```

Combine `row_number()` with `%%` / `%/%` to divide data into same-size groups.

### 13.4.2 Offset `lag` / `lead`

Reference previous/next value (ends filled with NA). `x - lag(x)` gives difference from previous;
`x == lag(x)` finds change points.

### 13.4.3 Consecutive ID `consecutive_id`

Assign new group ID whenever argument changes. `c("a","a","a","b","c","c",…)` →
`1 1 1 2 3 3 …`.

## 13.5 Numerical summaries

### 13.5.1 Center — mean vs median

Mean is sensitive to outliers; median is robust. Compare daily departure delay mean and median.

```haskell
flights |> groupBy ["year","month","day"]
        |> summarise [ "mean"   =: meanOf "dep_delay"
                     , "median" =: medianOf "dep_delay"
                     , "n"      =: nOf ]
```

![daily mean vs median](fig2-mean-vs-median.svg)

All points fall **below** the diagonal `y = x` (median < mean). Flights may be hours late but rarely
hours early, so distribution skews right, pulling the mean up.

R4DS's `geom_abline(slope=1, intercept=0)` (= `y=x` reference line) renders directly with hgg's
public API `refIdentity` (just compose with `<>`; see [api-guide 04-decoration reference
lines](../../api-guide/04-decoration.md#guides)):

```haskell
dayDelay |>> theme ThemeGrey <> layer (scatter "mean" "median") <> refIdentity
```

> Reference lines: `refIdentity` (=`y=x`) / `refHorizontal c` (=`geom_hline`) / `refVertical x`
> (=`geom_vline`) / `refLine (RefLinear slope intercept)` (=arbitrary `geom_abline`). All draw lines
> across the **entire plot area**, not data-dependent.

### 13.5.2 Min, max, quantiles

`quantile(x, 0.95)` is the 95th percentile. Ignore extreme 5% delays (R type-7, matches R).

```haskell
flights |> groupBy ["year","month","day"]
        |> summarise [ "max" =: maxOf "dep_delay", "q95" =: quantileOf 0.95 "dep_delay" ]
```

### 13.5.3 Spread — `sd` / `IQR`

`IQR(x)` = `quantile(x,.75) - quantile(x,.25)`. Airport-pair distances should be constant, but
examining distance IQR by `group_by(origin, dest)` (using `D.iqrL`) reveals **EGE** (EWR/JFK origin)
uniquely has IQR > 0—a data quirk. Output (filtered to `iqr > 0`, 2 rows):

```
EWR EGE  distance_iqr 1.0  n 110
JFK EGE  distance_iqr 1.0  n 103
```

### 13.5.4 Distribution

Look at distribution before relying on summaries. Departure delay distribution is extremely
right-skewed, needing magnification. **Main** binds `flights` directly, plots column `"dep_delay"`
with `histogram` (`dep_delay` is `Maybe Int` with missing, but resolver handles NA internally—no
need to extract raw). Magnified side uses ggplot's `filter |> ggplot` pattern: **filter DataFrame**
with `DF.filterJust` + `DF.filterWhere` before binding:

```haskell
let ddZoom = flights |> DF.filterJust  "dep_delay"
                     |> DF.filterWhere (F.col @Int "dep_delay" .< (120 :: DF.Expr Int))
saveSVG "fig3-dist.svg" $ subplots
  [ bakeSpec (toResolver flights) (theme ThemeGrey <> layer (histogram "dep_delay" <> binWidth 15) <> title "full (binwidth 15)")
  , bakeSpec (toResolver ddZoom)  (theme ThemeGrey <> layer (histogram "dep_delay" <> binWidth 5)  <> title "dep_delay < 120 (binwidth 5)") ]
  <> subplotCols 2
```

![dep_delay distribution (full / <120 zoom)](fig3-dist.svg)

Left: extreme right skew (massive spike near 0), right (<120 zoom): peak just below 0
(=most flights depart minutes early), then steep drop.

> **patchwork equivalent**: To give each panel different data, make each panel a `bakeSpec
> (toResolver df) spec` ("finished figure with data baked in"), then lay out with `subplots`/`hconcat`
> (= making separate plots in ggplot then composing with patchwork). Layout options: `hconcat`
> (horizontal) / `vconcat` (vertical), or `a <-> b` (horizontal) / `a <:> b` (vertical).
>
> **Missing columns**: `Maybe` columns like `dep_delay` plot directly with `histogram "dep_delay"`
> (resolver carries NA as NaN, consumed side excludes = `na.rm` equivalent, handled in plot fix).

Verify subgroups match overall shape. Overlaying 365 days of frequency polygons:

![365 days frequency polygon](fig4-freqpoly-365.svg)

365 lines nearly overlap, forming a **thick black band** showing shared pattern (sharp peak just below
0 + right tail) = same summary works for every day.

### 13.5.5 Position — `first` / `last` / `nth`

Value at specific position (tutorial helper in `Numbers.hs` fetches 1st/5th/last non-NA per group).
Daily first, 5th, last departure time. Output (1/1 row):

```
2013-01-01  first_dep 517  fifth_dep 554  last_dep 2356
```

### 13.5.6 Combo with mutate (group standardization)

Summary functions work with `mutate()` via recycling. Z-score via `(x - mean(x)) / sd(x)`
(`Data.Wrangle`'s `mutate` + `zscoreOf`):

```haskell
DF.fromNamedColumns [ ("x", DF.fromList ([2,4,4,4,5,5,7,9] :: [Double])) ]
  |> mutate [ "zscore" =: zscoreOf "x" ]
```

Also writeable: `x / sum(x)` (proportion), `(x - min(x)) / (max(x) - min(x))` (scale to [0,1]),
`x / first(x)` (indexed to first value).

---

## Exercises (R4DS Ch.13)

R4DS exercises solve with this chapter's tools (`Numbers.hs` API enables reproduction):

1. How `near()` works; is `sqrt(2)^2` near 2?
2. Expand `count()` to `group_by`+`summarize`+`arrange`.
3. Continuous time from `%/%` / `%%` (decimal hours or minutes from midnight).
4. Round `dep_time`/`arr_time` to 5-minute units.
5. Top 10 delayed flights via rank functions.
6. Use `lag()` to explore correlation with prior hour's average delay.

---

## Summary

We covered numerical vector creation (`parse_number`), counting (`count`/`n_distinct`), transforms
(recycling, `%/%`, rounding, `cut`, `cumsum`, rank, `lag`/`lead`), and summaries
(`mean`/`median`/`quantile`/`sd`/`IQR`/`first`/`last`/`nth`). Descriptive statistics and dplyr verbs
are implemented as hanalyze public APIs (`Stat.Descriptive` / `Data.Transform` / `Data.Wrangle`),
enabling direct DataFrame composition with `summarise`/`mutate`/`groupBy`.

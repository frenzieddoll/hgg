# 03. Data Transformation (R4DS 2e Ch.3 "Data transformation")

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.3 "Data transformation"**
> <https://r4ds.hadley.nz/data-transform>
> Data: **nycflights13** `flights` (**all 336,776 rows**) and **Lahman** `Batting`.
> Sources in [`../_data/_raw/SOURCE.md`](../_data/_raw/SOURCE.md).

We map dplyr verbs (`filter` / `arrange` / `distinct` / `count` / `mutate` / `select` /
`rename` / `relocate` / `group_by` + `summarize` / `slice_*`) **1:1 to dataframe**.
R4DS draws only **one plot** in the text (a case study scatter of batting statistics),
so other examples are shown as printed result tables (tibbles). We use **complete data**
(no sampling), matching R4DS values. Executable code is in [`DataTransform.hs`](DataTransform.hs).

```sh
cd docs/tutorials/03-data-transform
cabal run tut-03-data-transform     # prints operation results, generates batters.svg
```

Missing values (cancellation delays for `dep_delay` / `arr_delay` / `air_time`, etc.)
are read as `Maybe Int`. R arithmetic propagates NA (`gain = NA`, etc.), so `mutate`
uses `F.nullLift2` to **propagate NA while preserving rows** (same 336,776 rows as R).

---

## §3.1 Getting Started — Inspecting Flights

```haskell
flightsRaw <- DF.readCsv "../_data/_raw/flights.csv"
let flights = flightsRaw |> DF.exclude ["rownames"]   -- remove row number column from CSV conversion
```

`flights` has 336,776 rows × 19 columns. The equivalent of `glimpse(flights)` is
`DF.describeColumns` (name, type, count, and missing count for each column).
Missing values appear in `dep_time`/`dep_delay` (8,255 each), `arr_time` (8,713),
`arr_delay`/`air_time` (9,430 each), and `tailnum` (2,512), which become `Maybe` types.

## §3.2 Rows — filter() / arrange() / distinct()

**`filter(dep_delay > 120)`** — Flights departing 2+ hours late.
R's `filter` drops NA as false, so `filterJust` to exclude missing then compare:

```haskell
flights |> DF.filterJust "dep_delay"
        |> DF.filterWhere (F.col @Int "dep_delay" .> (120 :: DF.Expr Int))
-- → 9,723 rows
```

**`filter(month == 1 & day == 1)`** (842 rows), **`filter(month == 1 | month == 2)`**
(51,955 rows). The shorthand `%in%` from `|` and `==` becomes `filterBy` + `elem`:

```haskell
flights |> DF.filterWhere (F.col @Int "month" .== 1 .&& F.col @Int "day" .== 1)  -- 842
flights |> DF.filterBy (`elem` [1, 2]) (F.col @Int "month")                       -- 51,955
```

**`arrange(desc(dep_delay))`** — Flights with greatest delays first.
The top is **HA 51 (1,301 minutes late)**, matching R4DS.

```haskell
flights |> DF.sortBy [ DF.Desc (F.col @Int "dep_delay") ]
```

| year | month | day | dep_delay | carrier | flight |
|---|---|---|---|---|---|
| 2013 | 1 | 9 | 1301 | HA | 51 |
| 2013 | 6 | 15 | 1137 | MQ | 3535 |
| 2013 | 1 | 10 | 1126 | MQ | 3695 |

> ★ `dataframe` stores nullable columns as "base type (`Int`) vector + null bitmap".
> Thus sort type annotations use the **base type `@Int`** (`@(Maybe Int)` doesn't match
> the type and sorting won't work).

**`distinct(origin, dest)`** — Unique origin–destination pairs: **224** (matching R4DS).
**`count(origin, dest, sort = TRUE)`** — Routes by flight count:

```haskell
flights |> DF.groupBy ["origin", "dest"]
        |> DF.aggregate [ F.countAll `F.as` "n" ]
        |> DF.sortBy [ DF.Desc (F.col @Int "n") ]
```

| origin | dest | n |
|---|---|---|
| JFK | LAX | 11262 |
| LGA | ATL | 10263 |
| LGA | ORD | 8857 |

## §3.3 Columns — mutate() / select() / rename() / relocate()

**`mutate(gain = dep_delay - arr_delay, speed = distance / air_time * 60)`** —
Propagate `Maybe` on both sides with `F.nullLift2` (matching R arithmetic, no row drop).

```haskell
let gainE  = F.nullLift2 (\d a -> d - a)
               (F.col @(Maybe Int) "dep_delay") (F.col @(Maybe Int) "arr_delay") `F.as` "gain"
    speedE = F.nullLift2 (\d a -> fromIntegral d / fromIntegral a * 60 :: Double)
               (F.col @Int "distance") (F.col @(Maybe Int) "air_time") `F.as` "speed"
flights |> DF.deriveMany [gainE, speedE]
```

First row: `gain = 2 - 11 = -9`, `speed = 1400/227*60 = 370.0` (matches R4DS).
`.before = 1` / `.after = day` reorder columns (use `DF.select` to specify ordering),
`.keep = "used"` keeps only columns used in computation (`DF.select` for projection).

**`select`** variations:

```haskell
flights |> DF.select ["year", "month", "day"]                 -- by name
flights |> DF.selectBy [ DF.byNameRange ("year", "day") ]     -- range year:day
flights |> DF.exclude ["year", "month", "day"]                -- exclude !year:day
flights |> DF.selectBy [ DF.byProperty isChar ]               -- where(is.character)
  where isChar c = "Text" `T.isInfixOf` T.pack (columnTypeString c)
flights |> DF.select ["tailnum"] |> DF.rename "tailnum" "tail_num"  -- select and rename
```

`where(is.character)` returns 5 text columns (`carrier` / `tailnum` / `origin` / `dest` /
`time_hour`). **`rename(tail_num = tailnum)`** uses `DF.rename`;
**`relocate`** uses `DF.select` to reorder columns (e.g.,
`relocate(starts_with("arr"), .before = dep_time)` moves `arr_time` / `arr_delay`
before `dep_time`).

## §3.4 Pipes — Chaining Multiple Verbs

Combine `filter |> mutate |> select |> arrange` in one pipe (fastest flights to IAH):

```haskell
flights |> DF.filterWhere (F.col @Text "dest" .== F.lit ("IAH" :: Text))
        |> DF.deriveMany [speedE]
        |> DF.selectBy [ DF.byNameRange ("year", "day"), DF.byName "dep_time"
                       , DF.byName "carrier", DF.byName "flight", DF.byName "speed" ]
        |> DF.sortBy [ DF.Desc (F.col @Double "speed") ]
```

R4DS's nested and intermediate-object versions produce the same results (piping is clearest).

## §3.5 Grouping — group_by() / summarize() / slice_*()

**`group_by(month) |> summarize(avg_delay = mean(dep_delay, na.rm = TRUE), n = n())`**

```haskell
let avgByMonth = flights |> DF.filterJust "dep_delay" |> DF.groupBy ["month"]
                         |> DF.aggregate [ F.mean (F.col @Int "dep_delay") `F.as` "avg_delay" ]
    nByMonth   = flights |> DF.groupBy ["month"] |> DF.aggregate [ F.countAll `F.as` "n" ]
DF.innerJoin ["month"] avgByMonth nByMonth |> DF.sortBy [ DF.Asc (F.col @Int "month") ]
```

| month | avg_delay | n |
|---|---|---|
| 1 | 10.04 | 27004 |
| 6 | 20.85 | 28243 |
| 7 | 21.73 | 29425 |
| 12 | 16.58 | 28135 |

Values match R4DS. We compute mean (NA-ignored) and count (all rows) **separately then
`innerJoin`** because this version of `dataframe`'s grouped aggregation mixes missing
slots as 0 (see LIMITATIONS).

**`group_by(dest) |> slice_max(arr_delay, n = 1)`** — Most-delayed arrival per destination
(keep all ties). Since `dataframe` lacks `slice_max`, we find max per dest then restore via
`innerJoin`. Same as R4DS: **105 destinations → 108 rows** (ties +3, plus `LGA` with
all-NA `arr_delay` (1 cancelled flight) kept with default `na_rm = FALSE`):

```haskell
let arrNN   = flights |> DF.filterJust "arr_delay"
    destMax = arrNN |> DF.groupBy ["dest"]
                    |> DF.aggregate [ F.maximum (F.col @Int "arr_delay") `F.as` "arr_delay" ]
    tied    = DF.innerJoin ["dest", "arr_delay"] arrNN destMax
-- Add all-NA dest (LGA) rows to match R's na_rm=FALSE result of 108 rows
```

**`group_by(year, month, day) |> summarize(n = n())`** yields 365 rows (2013's days).
dplyr's message about "peeling off the last group" doesn't appear in `dataframe`
(groups are always explicit). `.by` is per-operation grouping, equivalent to
`groupBy` + `aggregate`.

## §3.6 Case Study: Aggregation and Sample Size (★ The Only Plot in This Chapter)

Using Lahman's batting data, plot batting average `performance = sum(H)/sum(AB)` against
at-bats `n = sum(AB)`.

```haskell
batting <- DF.readCsv "../_data/_raw/batting.csv"
let batters = batting |> DF.groupBy ["playerID"]
                      |> DF.aggregate [ F.sum (F.col @Int "AB") `F.as` "n"
                                      , F.sum (F.col @Int "H")  `F.as` "hits" ]
                      |> DF.derive "performance"
                           (F.toDouble (F.col @Int "hits") / F.toDouble (F.col @Int "n"))
saveSVGBoundStats "batters.svg" $
  (batters |> DF.filterWhere (F.col @Int "n" .> (100 :: DF.Expr Int)))
    |>> theme ThemeGrey <> layer (scatter "n" "performance" <> color (fromHex "#000000") <> alpha 0.1)
     <> layer (statSmooth "n" "performance" 8 <> color (fromHex "#3366FF"))
     <> xLabel "n" <> yLabel "performance"
```

![batters](batters.svg)

Two patterns emerge, matching R4DS:
1. Players with fewer at-bats show greater batting average variance (law of large numbers).
2. Positive correlation between average and at-bats (teams give more plate appearances to better hitters).

Naively sorting with `arrange(desc(performance))` puts players with **extreme few at-bats**
at the top (`n = 1`–`2` with `performance = 1.0`). This teaches us: "always pair aggregates
with counts."

## What We Can't Do / Faithfully Recorded Differences (LIMITATIONS)

No approximations, substitutions, or sampling. The following are `dataframe` 1.3 constraints,
honestly recorded:

- **✗ `distinct()` (all columns)**: Crashes with `fromMaybeVec: Nothing slot` if any column
  has NA (version constraint). We show `distinct(origin, dest)` for NA-free columns.
- **✗ `distinct(origin, dest, .keep_all = TRUE)`**: `dataframe`'s `distinct` only removes
  complete duplicate rows; there's no direct way to "uniquify by columns while keeping the
  first occurrence's full row". We record this one example as unimplemented.
- **△ Grouped mean with NA**: This version's grouped `meanMaybe` mixes missing slots as 0,
  downbiasing the mean. We use `filterJust` + `F.mean` for correct NA-ignoring means,
  count separately, then `innerJoin` (results match R).
- **△ NA positions in sorting**: R always places NA at the end; `dataframe`'s null slots sort
  as base-type defaults. The top displayed rows' order matches R.
- **△ R's runtime error examples**: Cases like `filter(month = 1)` (misused `=`) or
  `filter(month == 1 | 2)` ("works but wrong") can't be reproduced in Haskell since
  types/syntax prevent them (compiler rejects them). Comments explain the correspondence.

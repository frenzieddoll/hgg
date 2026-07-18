# 05. Data Tidying — Tidy Data and Pivoting

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.5 "Data tidying"**
> <https://r4ds.hadley.nz/data-tidy>
>
> Data: **tidyr** `table1`/`table2`/`table3` (tuberculosis incidence), `billboard` (2000 hit charts),
> `who2` (WHO TB diagnoses), `household` (family and child birth years),
> `cms_patient_experience` (US CMS patient survey).
> who2 / household / cms_patient_experience are tidyr package `.rda` files (R serialization format)
> **converted directly from bytes to CSV** (no fabrication or sampling; expanded via R-independent parser).
> Sources listed in [`../_data/_raw/SOURCE.md`](../_data/_raw/SOURCE.md).

The executable code is [`DataTidying.hs`](DataTidying.hs).

```sh
cd docs/tutorials/05-data-tidying
cabal run tut-05-data-tidying     # prints operation results, generates tb-cases.svg / billboard-ranks.svg
```

`dataframe` 1.3 lacks `pivot_longer` / `pivot_wider`. We define generic helpers
**capturing what pivoting does** via `Cell` (an intermediate type absorbing column type differences):
`pivotLongerG` / `pivotLongerValueG` / `pivotWiderG` (not hardcoded to a specific table;
column and row counts come from data).

---

## Three Rules of Tidy Data

1. **Each variable is a column**
2. **Each observation is a row**
3. **Each cell is a single value**

The same TB data can be represented three ways. `table1` is tidy (country × year is one row,
`cases`/`population` are columns):

```
  country   | year | cases  | population
Afghanistan | 1999 | 745    | 19987071
Afghanistan | 2000 | 2666   | 20595360
Brazil      | 1999 | 37737  | 172006362
...
```

`table2` has `cases` and `population` split into **long format** (`type`/`count`), untidy:

```
  country   | year |    type    |   count
Afghanistan | 1999 | cases      | 745
Afghanistan | 1999 | population | 19987071
...
```

`table3` has **two values in one cell** (`rate`: `745/19987071`), untidy.

> R4DS figures like `tidy-1.png` and `variables.png` are **hand-drawn explanatory diagrams**,
> not R code output, so this tutorial doesn't reproduce them (only code-generated plots).

## Tidy Data Enables Easy Computation

`table1` is tidy, so we can compute directly with `mutate` / `group_by`:

| R | hgg / dataframe |
|---|---|
| `mutate(rate = cases / population * 10000)` | `DF.derive "rate" (F.toDouble (F.col @Int "cases") / F.toDouble (F.col @Int "population") * 10000)` |
| `group_by(year) \|> summarize(total = sum(cases))` | `DF.groupBy ["year"] \|> DF.aggregate [F.sum (F.col @Int "cases") \`F.as\` "total_cases"]` |

Yearly totals: **1999: 250,740 / 2000: 296,920** (matches R4DS).

### Figure 1 — TB Cases Over Time (`tb-cases.svg`)

Plot tidy `table1` as lines + points. Overlay lines (color-coded by country) with
points (color + shape by country), and set x-axis breaks to 1999/2000 only
(equivalent to `scale_x_continuous(breaks=c(1999,2000))`):

```haskell
table1 |>> theme ThemeGrey <> layer (line "year" "cases" <> colorBy "country")
       <> layer (scatter "year" "cases" <> colorBy "country" <> shapeBy "country")
       <> palette okabeIto
       <> xAxis (axisBreaksAt [1999, 2000])
```

![TB Cases](tb-cases.svg)

China dominates (>200k both years), Brazil rises from ~40k to ~80k, Afghanistan barely visible
at this scale—exactly matching R4DS's observation.

| R | hgg |
|---|---|
| `geom_line(aes(group = country))` | `layer (line "year" "cases" <> colorBy "country")` |
| `geom_point(aes(color = country, shape = country))` | `layer (scatter "year" "cases" <> colorBy "country" <> shapeBy "country")` |
| `scale_x_continuous(breaks = c(1999, 2000))` | `xAxis (axisBreaksAt [1999, 2000])` |

---

## `pivot_longer` — Wide to Long

Most real data is untidy. When **column names hold variable values**, use `pivot_longer`
to reshape to long format.

### Column Names Contain Data — `billboard`

`billboard`: one row per song, weeks 1–76 ranks in **76 columns** `wk1`–`wk76`
(untidy, 317 songs). Reshape to one row per song × week.

```haskell
let wkCols  = filter ("wk" `T.isPrefixOf`) (DF.columnNames billboardRaw)
    parseWk = read . T.unpack . T.drop 2          -- "wk12" -> 12 (parse_number equivalent)
    bbLong  = pivotLongerG (\c -> [("week", CI (parseWk c))]) "rank" True
                           ["artist","track"] wkCols billboardRaw
```

Result: **(317, 80) → (5,307, 4)** (matches R4DS). With `values_drop_na = TRUE`,
drop NA (out-of-chart weeks). Since `dataframe` stores empty cells in a validity bitmap,
the helper **reads columns as `@(Maybe Int)` first** to capture NA as `Nothing`
(reading `@Int` directly ignores validity and returns 0—the trap described later).

| R | hgg |
|---|---|
| `pivot_longer(starts_with("wk"), names_to="week", values_to="rank", values_drop_na=TRUE)` | `pivotLongerG (\c -> [("week", CI (parseWk c))]) "rank" True ["artist","track"] wkCols` |
| `mutate(week = parse_number(week))` | helper's `parseWk` (`"wk12" → 12`) |

### Figure 2 — Rank Progression (`billboard-ranks.svg`)

Long format lets us draw rank over weeks as lines. Overlay gray lines per song,
and use `reverseY` (= `scale_y_reverse()`) to place rank 1 at the top:

```haskell
bbLong |>> theme ThemeGrey <> layer (line "week" "rank" <> linetypeBy "track" <> color (fromHex "#888888") <> alpha (85/255))
       <> reverseY
```

![Rank Progression](billboard-ranks.svg)

Most songs **drop from top 100 within 20 weeks**—R4DS's observation is immediately apparent.

> With 317 songs, color-coding per song would be chaotic. Instead, we use `linetypeBy "track"`
> (fixed gray) for grouping. Equivalent to R's `geom_line(aes(group = track))`.

### How Pivoting Works (Toy DataFrame)

Verify with a small example. `id` repeats for each value column; column names become
new variable values; cell values stack vertically:

```haskell
let toyL = DF.fromNamedColumns [("id", …["A","B","C"]), ("bp1", …[100,140,120]), ("bp2", …[120,115,125])]
pivotLongerG (\c -> [("measurement", CT c)]) "value" False ["id"] ["bp1","bp2"] toyL
-- id  measurement value
-- A   bp1         100
-- A   bp2         120
-- B   bp1         140  …
```

### Multiple Variables in Column Names — `who2` (`names_sep`)

`who2` (7,240 rows × 58 columns) column names like `sp_m_014` combine **three pieces**
(diagnosis `sp` / gender `m` / age group `014`) separated by `_`. Use `names_sep`
to split into three variables:

```haskell
let who2Vals = filter (`notElem` ["country","year"]) (DF.columnNames who2)
who2Long = pivotLongerG (\c -> zip ["diagnosis","gender","age"] (map CT (T.splitOn "_" c)))
                        "count" False ["country","year"] who2Vals who2
```

Result: **(7,240, 58) → (405,440, 6)** (matches R4DS). Here we **don't use `values_drop_na`**
(matching R4DS), so unreported years remain as `count = NA` (Afghanistan 1980's first rows
are all NA).

| R | hgg |
|---|---|
| `names_to = c("diagnosis","gender","age"), names_sep = "_"` | `\c -> zip ["diagnosis","gender","age"] (map CT (T.splitOn "_" c))` |

### Variable Names and Values Mixed in Column Names — `household` (`.value` Sentinel)

`household` (5 families) column names like `dob_child1` mix **variable name `dob`** with
**variable value `child1`**. Using the special value `names_to = c(".value", "child")`,
the first part (`dob`/`name`) becomes the **output column name**; the second part
(`child1`/`child2`) becomes the `child` column value:

```haskell
pivotLongerValueG "_" "child" True ["family"]
  ["dob_child1","dob_child2","name_child1","name_child2"] household
-- family child  dob        name
-- 1      child1 1998-11-26 Susan
-- 1      child2 2000-01-29 Jose
-- 2      child1 1996-06-22 Mark   ← family 2 has 1 child; child2 row is all NA, dropped
-- …                                  (5×2 − 1 = 9 rows)
```

With `values_drop_na = TRUE`, drop `child2` rows (all `.value` NA) for 1-child families,
yielding **9 rows** (matches R4DS).

| R | hgg |
|---|---|
| `names_to = c(".value", "child"), names_sep = "_", values_drop_na = TRUE` | `pivotLongerValueG "_" "child" True ["family"] …` |

---

## `pivot_wider` — Long to Wide

When one observation spans multiple rows, use `pivot_wider` to reshape wide.

### Return to Tidy Form — `table2`

Pivot `table2`'s `type` (cases/population) to columns to get back `table1`'s tidy shape:

```haskell
pivotWiderG ["country","year"] "type" "count" table2
-- → country year cases population  (= table1)
```

### Specify `id_cols` — `cms_patient_experience`

`cms_patient_experience` (500 rows): one organization spans 6 rows (one per survey item).
`distinct(measure_cd, measure_title)` yields 6 items (`CAHPS_GRP_1`, `_2`, `_3`, `_5`, `_8`, `_12`).
Use `id_cols = starts_with("org")` to uniquely identify organizations; pivot `measure_cd`
to columns with `prf_rate` as values:

```haskell
pivotWiderG ["org_pac_id","org_nm"] "measure_cd" "prf_rate" cms
```

Result: **(500, 5) → (95, 8)** (matches R4DS: 95 organizations, 2 ID columns, 6 measure columns).
`org_pac_id` is a **zero-padded ID** (`0446157747`), so we declare `Text` in the schema
at CSV read to prevent truncation (`readCsvWithSchema` + `schemaType @Text`).

| R | hgg |
|---|---|
| `pivot_wider(id_cols=starts_with("org"), names_from=measure_cd, values_from=prf_rate)` | `pivotWiderG ["org_pac_id","org_nm"] "measure_cd" "prf_rate" cms` |

### How `pivot_wider` Works and Duplicate Cells (Toy DataFrame)

Combinations absent from input become NA (`B`'s `bp3` is missing):

```haskell
pivotWiderG ["id"] "measurement" "value" toyW
-- id  bp1 bp2 bp3
-- A   100 120 105
-- B   140 115 NA
```

When `(id, measurement)` combinations **appear multiple times**, R's `pivot_wider` warns of
list-columns. Our typed-column implementation can't create list-columns. Following R4DS's
recommendation, we detect duplicates with `group_by(id, measurement) |> summarize(n = n()) |> filter(n > 1)`
and report them (`A`/`bp1` has `n=2`).

---

## Correspondence Table for This Chapter (Summary)

| tidyr / dplyr | dataframe / hgg |
|---|---|
| Tidy data's 3 rules | (Design principle: column=variable, row=observation, cell=value) |
| `pivot_longer(names_to=…, values_to=…)` | Custom `pivotLongerG` (wide → long) |
| `pivot_longer(names_sep="_")` | `pivotLongerG`'s `nameExpand` with `T.splitOn "_"` |
| `pivot_longer(names_to=c(".value", …))` | Custom `pivotLongerValueG` (.value sentinel) |
| `pivot_wider(names_from=…, values_from=…, id_cols=…)` | Custom `pivotWiderG` |
| `values_drop_na = TRUE` | helper's `dropNA` parameter |
| `parse_number("wk12")` | `read . T.unpack . T.drop 2` |
| `scale_y_reverse()` | `reverseY` |
| `scale_x_continuous(breaks=…)` | `xAxis (axisBreaksAt […])` |
| `geom_line(aes(group = g))` | `line … <> colorBy "g"` (color) / `<> linetypeBy "g"` (gray) |

## Faithful Reproduction: Recorded Differences (Honest Account)

- **NA display**: `dataframe` prints nullable columns as `Maybe a`, so where R shows `NA`,
  we show `Nothing`, and `63` as `Just 63.0` (**value identical**; just a display convention difference).
- **Empty cell reading (trap)**: `dataframe` stores empty cells via validity bitmap; reading `@Int`
  directly ignores validity and returns `0`. To correctly drop/keep NA, **read as `@(Maybe Int)` first**
  (`readCells`). We discovered this when billboard had 20,605 rows instead of 5,307.
- **Zero-padded ID**: `org_pac_id` looks numeric but is text (`0446157747`). Inferring the type
  truncates it, so we explicitly declare `Text` via `readCsvWithSchema`.
- **List-columns**: Duplicate cells in `pivot_wider` become list-columns in R, but our typed-column
  implementation can't create them. Following R4DS's recommendation, we detect duplicates
  with `group_by`/`summarize`/`filter` instead.

---

Previous: [`04-workflow-style`](../04-workflow-style/README.md) (Ch.4 Workflow: code style).
Next: R4DS Ch.6 "Workflow: scripts and projects" (no plots; code tutorial coming).

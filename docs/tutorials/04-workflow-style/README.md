# 04. Workflow: Code Style (R4DS 2e Ch.4 "Workflow: code style")

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.4 "Workflow: code style"**
> <https://r4ds.hadley.nz/workflow-style>
> Data: **nycflights13** `flights` (complete).

R4DS Ch.4 is a **plot-free** "code style" chapter (all R code in the text is `eval: false`
style examples; plots are RStudio screenshots only). Here we map rules from
[tidyverse style guide](https://style.tidyverse.org) **to Haskell / this project's conventions
(`CLAUDE.md`)**, showing "**Strive for** / **Avoid**". Executable code is in
[`WorkflowStyle.hs`](WorkflowStyle.hs) (running the Strive version for verification).

```sh
cd docs/tutorials/04-workflow-style
cabal run tut-04-workflow-style
```

This file set itself exemplifies the project's style (2-space indentation, `camelCase`,
`-- ===` section lines, aligned `=`, one verb per pipe).

---

## §4.2 Names

R: Variable names are lowercase + digits + `_` (snake_case). This project uses **`camelCase`**
(functions/bindings) / **`PascalCase`** (types/constructors). Like R, prefer long descriptive
names over short abbreviations.

```haskell
-- Strive for: descriptive, camelCase
shortFlights = flights |> DF.filterJust "air_time"
                       |> DF.filterWhere (F.col @Int "air_time" .< 60)

-- Avoid: abbreviations, all caps
sf = ...        -- unclear what abbreviation means later
```

## §4.3 Spaces

R: Space around binary operators (`+ - == <` …), except `^`; space around assignment `<-`.
No spaces inside parentheses in function calls; space after commas. Haskell is the same.

```haskell
-- Strive for
z = (a + b) ^ 2 / d

-- Avoid
z=( a+b )^2/d
```

**Aligning** `=` is equally effective in Haskell. When creating multiple columns,
aligning `=` (`` `F.as` `` in dataframe) vertically improves readability
(`dep_time` is in HHMM format, so R's `%/% 100`·`%% 100` become dataframe's `div`·`mod`):

```haskell
flights |> DF.deriveMany
  [ F.nullLift2 (\dist t -> fromIntegral dist / fromIntegral t :: Double)
      (F.col @Int "distance") (F.col @(Maybe Int) "air_time") `F.as` "speed"
  , F.nullLift (\t -> t `div` 100) (F.col @(Maybe Int) "dep_time") `F.as` "dep_hour"
  , F.nullLift (\t -> t `mod` 100) (F.col @(Maybe Int) "dep_time") `F.as` "dep_minute" ]
```

## §4.4 Pipes

R: Place `|>` with space before it, at end of line. **One verb per line**.
Functions with named arguments (`mutate` / `summarize`): **one argument per line, extra 2-space indent**;
closing `)` on its own line, aligned to function name. Dataframe's `|>` follows the same convention:

```haskell
-- Strive for
flights
  |> DF.filterJust "arr_delay"
  |> DF.filterJust "tailnum"
  |> DF.groupBy ["dest"]
  |> DF.aggregate [ F.countAll `F.as` "n" ]

-- Avoid
flights|>DF.filterJust "arr_delay"|>DF.groupBy["dest"]|>DF.aggregate[F.countAll `F.as` "n"]
```

With multiple named arguments (aggregations), split one per line, aligning list brackets:

```haskell
-- Strive for (one aggregation per line)
byTail =
  flights
    |> DF.groupBy ["tailnum"]
    |> DF.aggregate
         [ F.mean (F.col @Int "arr_delay") `F.as` "delay"
         , F.countAll                      `F.as` "n" ]
```

> ★ This `group_by(tailnum)` example is a **formatting model**. `tailnum` is originally `Maybe Text`
> (has missing), and this version of `dataframe` **crashes when `groupBy` on originally-nullable columns**.
> So the executable demo ([`WorkflowStyle.hs`](WorkflowStyle.hs)) uses R4DS's other §4.4 example
> `… |> count(dest)` (`dest` is non-null) instead (formatting rules are identical).

## §4.5 ggplot2

R: ggplot's `+` follows the same pipe formatting (treat `+` like `|>`).
In hgg, we bind data with `|>>` and layer with `<>` (= ggplot's `+`).
Formatting rules are the same (one layer per line; if args are long, one per line):

```haskell
-- Strive for (|>> binds data, <> layers, one layer per line)
delayByMonth
  |>> layer (line    "month" "delay")
   <> layer (scatter "month" "delay")

-- Long-argument layers, one per line
plotData
  |>> layer (statSmooth "distance" "speed" 8
               <> color (fromHex "#FFFFFF")
               <> stroke 4)
   <> layer (scatter "distance" "speed")
```

The switch from R's `|>` → `+` corresponds to this library's switch `|>>` (data bind) → `<>`
(layer combine). ★ R4DS doesn't generate plots in this chapter (examples are `eval: false`),
so this tutorial also produces no plots, only the aggregated source (monthly average delay).

## §4.6 Section Markers (Sectioning Comments)

R: Divide scripts with section comments like `# Load data ----------`.
This project's convention is **`-- ===` lines** (`CLAUDE.md`).
Sections in [`WorkflowStyle.hs`](WorkflowStyle.hs) exemplify this.

```haskell
-- =========================================================================
-- §4.2 Names — variable naming
-- =========================================================================
```

## What We Can't Do / Faithfully Recorded Differences

- **△ `groupBy` on originally-nullable columns**: Columns like `tailnum` (originally `Maybe Text`)
  with missing values cause this version of `dataframe` to crash when `groupBy` is applied.
  The executable demo (§4.4) substitutes R4DS's other example using non-null `dest`
  (formatting rules themselves are identical).
- **R's runtime behavior**: R4DS style examples are `eval: false` (demonstration, not executed).
  This chapter runs only the Strive version; the Avoid version is shown for contrast.

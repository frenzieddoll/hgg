# DataFrame Integration — `df |>> layer …` Write with Column Names

> 🌐 **English** | [日本語](06-dataframe.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 layers](02-layers.md) | [03 encoding & scale](03-encoding-scale.md) | [04 decoration](04-decoration.md) | [05 backends](05-backends.md) | **06 dataframe** | [07 analyze](07-analyze.md) | [08 3d](08-3d.md) | [09 appendix](09-appendix.md)

Using `hgg-frame`, write like ggplot2: **dataframe + column names**. The spec contains only column "names" (`scatter "x" "y"`); actual data is resolved at bind time with `(|>>)`.

Structure of this page:
**[3 ways to prepare df](#df-sources)** | **[Draw with column names](#by-column-name)** |
**[CSV (Hackage dataframe)](#df-csv)** | **[Column validation (pure function)](#df-validate)**

> ⚠️ Operator is **`|>>`** (not `|>`). Hackage `dataframe` exports `|>`, so collision. No collision when using `import DataFrame` together. `|>>` is weaker than `<>` (`infixl 1`) so `df |>> (layer a <> layer b)` works. (4 operator roles `<>` / `|>` / `|>>` / `|->` listed at [operator quick reference](README.md#operator-quick-reference).)

> 📝 **Why `"x"` works (column name literal)**: `"weight"` in `scatter "weight" "mpg"` is `ColRef`. `ColRef` has `IsString` instance (`fromString = ColByName . T.pack`), so **with `{-# LANGUAGE OverloadedStrings #-}`**, `"weight"` auto-converts to `ColByName "weight"`. Without the extension, must write `ColByName "weight"` explicitly. All examples in this guide assume `OverloadedStrings`.

### 3 Ways to Prepare df (`class PlotData`) {#df-sources}

Left side of `(|>>)` can be any `instance PlotData df`. Standard 3 options:

| df type | Required package | Use case |
|---|---|---|
| `Map Text ColData` | core only (zero deps) | Turn local vector into df |
| `[(Text, ColData)]` | core only (zero deps) | Assoc-list preserving order |
| `DataFrame` | `hgg-dataframe` | Hackage `dataframe` (CSV reading, manipulation) |

Column values are `ColData` = `NumData (Vector Double)` / `TxtData (Vector Text)`. Helper shortcuts simplify:

```haskell
import           Graphics.Hgg.Easy             -- re-export Spec (scatter/layer/…/ColData)
import           Graphics.Hgg.Frame            ((|>>), BoundPlot, bpDiagnostics)
import           Graphics.Hgg.Backend.SVG      (saveSVGBound)
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V
import           Data.Text       (Text)

num :: [Double] -> ColData ; num = NumData . V.fromList
txt :: [Text]   -> ColData ; txt = TxtData . V.fromList

df :: M.Map Text ColData
df = M.fromList
  [ ("x",     num [1,2,3,4,5,6,7,8,9,10])
  , ("y",     num [2.1,3.9,6.0,7.7,10.2,11.8,14.1,15.9,18.2,20.0])
  , ("size",  num [2,8,3,9,4,7,5,6,3,8])
  , ("group", txt (take 10 (cycle ["A","B"]))) ]
```

> ⚠️ df column values are **`ColData` (`NumData`/`TxtData`)**. `inline`/`inlineCat` are **`ColRef`** for mark arguments (`scatter (inline xs) ys`), can't use for df values (different type). For df, use above `num`/`txt` (= `NumData`/`TxtData`).

## What you can draw from df = All marks and encodings by "column name" {#by-column-name}

Spec side uses column "names" only (`scatter "x" "y"`). **Any mark, any encoding uses column names**.

```haskell
-- (a) Scatter + color-code by group + size by size column
df |>> ( layer (scatter "x" "y" <> colorBy "group" <> sizeBy "size" <> alpha 0.85)
       <> title "df |>> scatter (color/size by column name)" )
```
![df scatter](images/df/01-scatter-color-size.svg)

```haskell
-- (b) Overlay: separate layers of same df with <> ([04 decoration](04-decoration.md#overlay) same)
df |>> ( layer (scatter "x" "y" <> size 6)
       <> layer (line "x" "y" <> color (fromHex "#d62728") <> stroke 1) )
```
![df overlay](images/df/02-overlay.svg)

```haskell
-- (c) facet: partition by column (free/fixed scale also set via VisualSpec)
df |>> ( layer (scatter "x" "y" <> colorBy "group" <> size 6) <> facet "group" )
```
![df facet](images/df/03-facet.svg)

```haskell
-- (d) Bar chart: category column + grouping + position adjustment
dfB |>> ( layer (bar "cat" "val" <> colorBy "grp" <> position PosDodge) )
```
![df bar dodge](images/df/04-bar-dodge.svg)

Similarly, all marks in [02 layers](02-layers.md#index) like `boxplot "y"` / `histogram "x"` / `violin "y" <> groupBy "g"` / `heatmap "c" "r" "v"` etc. can be drawn from df using column name arguments.

### Hackage `dataframe` (CSV etc.) {#df-csv}

Add `hgg-dataframe` and `DataFrame` becomes df directly
(`instance PlotData DataFrame`). No intermediate JSON conversion needed.

```haskell
import qualified DataFrame              as DF
import           Graphics.Hgg.DataFrame ()   -- expose the instance

main = do
  df <- DF.readCsv "cars.csv"
  saveSVGBound "out.svg" (df |>> layer (scatter "weight" "mpg" <> colorBy "origin"))
```

#### Handling missing values (`Maybe` / NA) columns {#nullable-columns}

Beyond `Double` / `Int` / `Text` columns, **`Maybe Double` / `Maybe Int` columns (= columns with NA · empty CSV cells or missing values) draw directly by column name**. Like ggplot's `aes(col)` + `na.rm`, missing values handled internally, so **no need to extract raw vectors** (`columnAsList` → `catMaybes` → `fromNamedColumns` rebuild).

```haskell
-- dep_delay is Maybe Int (many NA) but drawable directly by column name
flights |>> layer (histogram "dep_delay" <> binWidth 15)
```

Behavior (resolver `dfResolver` + render side):

| Mark | Missing (NA) handling |
|---|---|
| Single-column (`histogram` / `freqpoly` / `density` / `boxplot` / `ecdf` …) | NA **dropped** in aggregation (= `na.rm = TRUE`) |
| Multi-column (`scatter` / `line`) | **Rows where x or y is NA dropped** (row alignment preserved · ggplot row-wise na.rm) |
| Axis range | NA ignored for min/max |

Internally NA carried as `NaN` (preserving column length, not breaking row alignment); range / binning / point rendering exclude `NaN`. **Non-NULL columns are no-op, completely identical to before**.

> R `filter(col < x)` equivalent (= narrow rows) is **on DataFrame side**: `df |> DF.filterJust "col" |> DF.filterWhere (F.col @Int "col" .< (x :: DF.Expr Int))` (same as `filter |> ggplot` · combine with patchwork-style [subplot](04-decoration.md#subplots)).

### Column validation at bind time (pure function, no exceptions) {#df-validate}

`(|>>)` is **pure** (no exceptions thrown). At bind time, validates column names and places result **as values** in `BoundPlot`'s `bpDiagnostics` (detects missing columns, type mismatch, empty df). `saveSVGBound` / `renderBound` report Error-severity diagnostics to stderr during rendering (rendering doesn't stop). To bypass validation completely, extract `(Resolver, VisualSpec)` with `unBound` and pass directly to existing `saveSVG`.

```haskell
let bp = df |>> layer (scatter "x" "wieght")   -- typo!
bpDiagnostics bp
-- [PlotError (ColumnNotFound "wieght" …) (DiagnosticContext {dcLayer = Just 0, dcMark = Just MScatter})]
```

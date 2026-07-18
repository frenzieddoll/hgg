# 01. Data Visualization (R4DS 2e Ch.1 "Data visualization")

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.1 "Data visualization"**
> <https://r4ds.hadley.nz/data-visualize>
> Data: **palmerpenguins** `penguins` (344 individuals. Source in
> [`../_data/_raw/SOURCE.md`](../_data/_raw/SOURCE.md))
>
> Detailed specifications for marks used (signature, encoding, all options) are in
> [API Reference 02 layers](../../api-guide/02-layers.md).

> "The simple graph has brought more information to the data analyst's mind than any other device."
> — John Tukey

hgg implements the **grammar of graphics** —— a unified system for describing and composing
visualizations. At its core is a single idea: **visualization is the process of mapping
variables in data to visual attributes (aesthetics) such as position, color, size, and shape**.
Once you master this one system, you can create a wide variety of plots using the same approach
and quickly iterating.

In this chapter, we first build a scatter plot while introducing two fundamental components:
**aesthetics** (mapping data to visual attributes) and **marks** (graphical elements). We then
visualize distributions of a single variable and relationships among two or more variables,
ending with plot saving and common pitfalls.

Using the complete penguins dataset (344 rows), we reproduce R4DS plots by configuring mark
types, color and shape mappings, and parameters (binwidth, position, facet) using hgg's
`layer (mark ...)` to match each R4DS figure. In hgg, we bind data with `|>>`, layer marks
like `scatter`/`bar`/`boxplot` using `layer`, and specify colors and shapes via `colorBy`/`shapeBy`
within the mark. Below is a walkthrough following R4DS's progression with **explanation →
code → plot** laid out in sequence. The complete executable code is in [`Visualize.hs`](Visualize.hs).

```sh
cd docs/tutorials/01-visualize
cabal run tut-01-visualize    # generates 01-teaser.svg ... 24-facet-island.svg
```

## Missing Values (One Subtlety First)

`flipper_length_mm` and `body_mass_g` have missing values (2 rows), represented in dataframe
as `Maybe Int`. hgg **reads `Maybe` columns by name directly, and both marks and stats
(like regression lines) automatically drop `Nothing` (NA)** (equivalent to R's `na.rm`).
Thus subsequent plots read `raw` directly without explicit filtering.

To explicitly drop missing rows, you can use `DF.filterJust` (equivalent to R4DS's
*"removing 2 rows containing missing values"*):

```haskell
-- Explicit removal (optional): drop NA rows to get a plain Int column
let cleaned = raw |> DF.filterJust "flipper_length_mm"
                  |> DF.filterJust "body_mass_g"
-- But subsequent plots read raw directly (mark/stat auto-exclude NA)
```

---

## §1.1 Teaser Plot (Chapter Opening Motivating Plot)

R4DS opens each chapter with "this is what you'll be able to create by chapter's end."
It's a complete plot showing the relationship between body mass and flipper length,
colored and shaped by species, with a regression line (`statLm`) and formatted title/labels
(same as `09-final.svg`).

We use a colorblind-safe **Okabe-Ito palette** (`palette okabeIto`).
In this chapter, only this teaser and the final figure have formatted labels and colorblind palette;
intermediate plots use axis labels as-is and default coloring (matching how R4DS applies `labs()`
and `scale_color_colorblind()` only to these two).

```haskell
saveSVGBoundStats "01-teaser.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> palette okabeIto
      <> title "Body mass and flipper length"
      <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
      <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
      <> legendTitle "Species"
      <> theme ThemeGrey
```

![teaser](01-teaser.svg)

---

## §1.2 Building a Scatter Plot Step by Step

In hgg, we create a plot by binding data with `|>>` to a spec and layering `layer (mark ...)`
on top of it. Without any marks, the canvas remains empty. Corresponding to R4DS's progression
from "empty panel → axes → plot", we start with no marks.

**No marks (`purePlot` = empty spec)** — With nothing layered, we get an empty panel only.
Since no columns are specified, axes default to the range 0–1 (this is what hgg shows for an
empty spec).

```haskell
saveSVG "02-empty.svg" $
  purePlot
```

![empty](02-empty.svg)

**Axis labels only (still no marks)** — Adding `xLabel`/`yLabel` labels the axes, but scale
is determined by marks, so ticks remain at default (0–1) and no points appear until we add a mark
(this corresponds to R4DS's "axes only, no plot" step; hgg differs in that columns are only
determined once a mark is added).

```haskell
saveSVGBound "03-empty-axes.svg" $
  raw |>> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

![empty axes](03-empty-axes.svg)

**Adding `layer (scatter ...)`** — Our first scatter plot.
A scatter mark given column names creates axes spanning the data range and produces points.
Body mass and flipper length show positive correlation.

```haskell
saveSVGBound "04-scatter.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

![scatter](04-scatter.svg)

**Adding `colorBy "species"`** — Color-coding by species. When a categorical variable is
mapped to the color channel, hgg automatically assigns a color to each level and generates
a legend (equivalent to ggplot's scaling). **Levels for color, x-axis, point shape, and
facet are all alphabetically ordered** (here: Adelie / Chinstrap / Gentoo), using the default
hue palette (3 colors).

```haskell
saveSVGBound "05-color.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![color](05-color.svg)

**Overlaying regression line `layer (statLm ...)`** — Applying `colorBy "species"` to both
scatter and regression layers means the grouping affects both, so **3 regression lines appear,
one for each species**.

```haskell
saveSVGBoundStats "06-smooth-species.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorBy "species")
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![smooth species](06-smooth-species.svg)

**Applying `colorBy` to scatter layer only** — The regression line spans all data as **1 line**
(ggplot's default blue). This shows how placing the same aesthetic **on different layers
changes the plot's meaning** (applying globally to all layers vs. to a specific mark only).

```haskell
saveSVGBoundStats "07-smooth-global.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![smooth global](07-smooth-global.svg)

**`colorBy "species"` + `shapeBy "species"`** — In addition to color, distinguish species
by point shape.

```haskell
saveSVGBoundStats "08-shape.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![shape](08-shape.svg)

**Finishing the plot** — Polish with `title`/`subtitle`/`xLabel`/`legendTitle`, and apply
the colorblind-safe Okabe-Ito palette via `palette okabeIto` (same as teaser).

```haskell
saveSVGBoundStats "09-final.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> palette okabeIto
      <> title "Body mass and flipper length"
      <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
      <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
      <> legendTitle "Species"
      <> theme ThemeGrey
```

![final](09-final.svg)

---

## §1.3 The hgg Pattern

Now that we've seen how to build a scatter plot, let's consolidate the **recurring pattern**
we'll see in subsequent plots (corresponding to §1.3 in R4DS where code transitions to
concise form).

An hgg plot always takes this shape:

```haskell
data |>> layer (mark columns… <> aesthetic…) <> decoration…
```

- **`|>>`** — Plot bind operator combining DataFrame and spec. Data on the left, spec on the right.
  Different from `|>` (DataFrame's forward pipe, equivalent to R4DS's `|>`).
- **`layer (mark ...)`** — A single graphical layer. Give a **mark** like `scatter`/`bar`/`histogram`
  column names and aesthetics. Layering multiple `layer`s with `<>` builds multipart plots.
- **`<>`** — Monoid combining operator joining aesthetics within a mark, between layers, and
  between decorations. Combine attributes like `colorBy "species" <> alpha 0.85`.
- **Decorations** — Functions like `xLabel`/`title`/`legendTitle`/`theme`/`palette`. Added
  outside layers with `<>`.

Depending on the plot's contents, use one of three output functions:

| Function | When to use |
|---|---|
| `saveSVG` | Plots without column names (inline data with literal values) |
| `saveSVGBound` | `df \|>> spec` using **column names** (typical for this chapter) |
| `saveSVGBoundStats` | Plus **stats** like `statLm`/`statSmooth` |

Remembering this pattern, you simply swap out marks and aesthetics to create scatter,
bar, histogram, boxplot—all using the same structure. All subsequent sections follow
this pattern.

---

## §1.4 Single Variable Distributions

**`bar` mark (count by species)** — Count for categorical variable. Since bars require
aggregation, first compute with `DF.aggregate` (values unchanged). x is in alphabetical order.

```haskell
let bySpecies = raw |> DF.groupBy ["species"]
                    |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

saveSVGBound "10-bar-species.svg" $
  bySpecies |>> layer (bar "species" "n")
              <> xLabel "species" <> yLabel "count"
              <> theme ThemeGrey
```

![bar species](10-bar-species.svg)

**Sort by descending count** — (Adelie 152 > Gentoo 124 > Chinstrap 68).
Use `scaleXDiscreteLimits` to specify level order explicitly (equivalent to R4DS's `fct_infreq`).

```haskell
saveSVGBound "11-bar-infreq.svg" $
  bySpecies |>> layer (bar "species" "n")
              <> scaleXDiscreteLimits ["Adelie", "Gentoo", "Chinstrap"]
              <> xLabel "species" <> yLabel "count"
              <> theme ThemeGrey
```

![bar infreq](11-bar-infreq.svg)

**`histogram` mark (`binWidth 200`)** — Distribution of continuous variable (body mass).
`binWidth` (R4DS's `binwidth`) determines bin boundaries and bar heights.

```haskell
saveSVGBound "12-histogram-bw200.svg" $
  raw |>> layer (histogram "body_mass_g" <> binWidth 200)
       <> xLabel "body_mass_g" <> yLabel "count"
       <> theme ThemeGrey
```

![histogram bw200](12-histogram-bw200.svg)

**`binWidth 20`** — Too fine, jagged (over-resolved).

```haskell
saveSVGBound "13-histogram-bw20.svg" $
  raw |>> layer (histogram "body_mass_g" <> binWidth 20)
       <> xLabel "body_mass_g" <> yLabel "count"
       <> theme ThemeGrey
```

![histogram bw20](13-histogram-bw20.svg)

**`binWidth 2000`** — Too coarse, 3 bins (information lost).

```haskell
saveSVGBound "14-histogram-bw2000.svg" $
  raw |>> layer (histogram "body_mass_g" <> binWidth 2000)
       <> xLabel "body_mass_g" <> yLabel "count"
       <> theme ThemeGrey
```

![histogram bw2000](14-histogram-bw2000.svg)

**`density` mark** — Smooth curve representation of distribution.

```haskell
saveSVGBound "15-density.svg" $
  raw |>> layer (density "body_mass_g")
       <> xLabel "body_mass_g" <> yLabel "density"
       <> theme ThemeGrey
```

![density](15-density.svg)

---

## §1.5 Relationships Among Two or More Variables

**`boxplot` mark (species × body mass)** — Categorical × continuous.

```haskell
saveSVGBound "16-boxplot.svg" $
  raw |>> layer (boxplot "body_mass_g" <> groupBy "species")
       <> xLabel "species" <> yLabel "body_mass_g"
       <> theme ThemeGrey
```

![boxplot](16-boxplot.svg)

**`density` + `colorBy "species"`** — Three density curves, one per species.

```haskell
saveSVGBound "17-density-color.svg" $
  raw |>> layer (density "body_mass_g" <> colorBy "species")
       <> xLabel "body_mass_g" <> yLabel "density"
       <> legendTitle "species"
       <> theme ThemeGrey
```

![density color](17-density-color.svg)

**`densityFill True` + `alpha 0.5`** — Filled density curves.

```haskell
saveSVGBound "18-density-fill.svg" $
  raw |>> layer (density "body_mass_g" <> colorBy "species"
                 <> densityFill True <> alpha 0.5)
       <> xLabel "body_mass_g" <> yLabel "density"
       <> legendTitle "species"
       <> theme ThemeGrey
```

![density fill](18-density-fill.svg)

**`bar` + `colorBy "species"` (island × species)** — Two categorical variables.
Default (stack) stacks bars. First level (Adelie) stacks at the top.

```haskell
let byIslandSpecies = raw |> DF.groupBy ["island", "species"]
                          |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

saveSVGBound "19-bar-stack.svg" $
  byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosStack)
                    <> xLabel "island" <> yLabel "count"
                    <> legendTitle "species"
                    <> theme ThemeGrey
```

![bar stack](19-bar-stack.svg)

**`position PosFill`** — Normalize each island to 1 (proportions). Y-axis label remains default.

```haskell
saveSVGBound "20-bar-fill.svg" $
  byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosFill)
                    <> xLabel "island" <> yLabel "count"
                    <> legendTitle "species"
                    <> theme ThemeGrey
```

![bar fill](20-bar-fill.svg)

**`yLabel "proportion"`** — Change y-axis label to "proportion".

```haskell
saveSVGBound "21-bar-fill-proportion.svg" $
  byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosFill)
                    <> xLabel "island" <> yLabel "proportion"
                    <> legendTitle "species"
                    <> theme ThemeGrey
```

![bar fill proportion](21-bar-fill-proportion.svg)

**Plain scatter plot (start of §1.5.3)** — Baseline before adding three variables.

```haskell
saveSVGBound "22-scatter-plain.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

![scatter plain](22-scatter-plain.svg)

**`colorBy "species"` + `shapeBy "island"`** — Three variables (color = species, shape = island).

```haskell
saveSVGBound "23-scatter-shape-island.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "island" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![scatter shape island](23-scatter-shape-island.svg)

**`facetWrap "island" 3`** — Split by island into small panels (panels also alphabetically ordered).

```haskell
saveSVGBound "24-facet-island.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> facetWrap "island" 3
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![facet island](24-facet-island.svg)

---

## §1.6 Saving Plots

Once you've created a plot, you'll want to save it to a file for use elsewhere.
That's what the `saveSVG` family of functions does. Throughout this chapter we've used
`saveSVGBound` / `saveSVGBoundStats`. These save the spec to an SVG file.

```haskell
saveSVGBound "penguin-plot.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g")
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
```

**Plot dimensions** are properties of the spec (not backend arguments), given as `width` /
`height` / `aspectRatio`. For reproducible code, it's best to specify dimensions explicitly:

```haskell
saveSVGBound "penguin-plot.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g")
      <> width 640 <> height 480
```

**Output format** changes by swapping the backend. The API is symmetric across SVG/PDF/PNG:

| Format | Functions | import |
|---|---|---|
| SVG | `saveSVG` / `saveSVGBound` | `Hgg.Plot.Backend.SVG` |
| PDF | `savePDF` / `savePDFBound` | `Hgg.Plot.Backend.PDF` |
| PNG | `savePNG` / `savePNGBound` | `Hgg.Plot.Backend.Rasterific` (supports Japanese labels) |

See [API Reference 05 backends](../../api-guide/05-backends.md) for details.

---

## §1.7 Common Pitfalls

When you start writing code, something will usually trip you up. Don't worry ——
everyone encounters these. Start by comparing your code to this chapter's code
**character by character**. Haskell is strict about types and syntax;
a single character mismatch changes the result.

- **Matching parentheses**: Check that every `(` has a closing `)` and every `"` has a closing `"`.
- **Operator precedence**: `|>>` has lower precedence than `<>`. Combine aesthetics within
  a mark with `<>`, add decorations outside layers with `<>` ——
  getting this nesting wrong causes type errors. When in doubt, parenthesize at layer boundaries.
- **Required columns for marks**: `scatter` requires two columns (x, y);
  `histogram`/`density` require one. Missing columns cause type errors.
- **Column name typos**: Check that column names passed to `|>>` (like `"flipper_length_mm"`)
  actually exist in the DataFrame. Non-existent names are caught at runtime.
- **`OverloadedStrings` / `TypeApplications`**: To use string literals as column names,
  enable `OverloadedStrings`; for `F.col @Text`, enable `TypeApplications`
  (see `{-# LANGUAGE ... #-}` at the top of `Visualize.hs`).

Still stuck? Check the [API Reference](../../api-guide/README.md) for the mark's signature
and examples, then carefully read the type error message. The answer is often hiding there.

---

## §1.8 Summary

In this chapter, we learned the fundamentals of data visualization with hgg.
Our starting point: **"Visualization is the process of mapping data variables to visual attributes
like position, color, size, and shape."** From there, we learned to layer plots using `<>`,
building complexity and refinement step by step. We visualized single-variable distributions
(bars, histograms, density) and relationships among two or more variables (boxplots, colored
density curves, stacked bars, 3-variable scatter plots) using additional aesthetic mappings
and `facetWrap` for panel subdivision. Finally, we saw how to save plots as SVG/PDF/PNG.

Visualization appears repeatedly throughout this tutorial suite. For more advanced control over
encoding, scale, and theme, see [API Reference 03 encoding & scale](../../api-guide/03-encoding-scale.md) /
[04 decoration](../../api-guide/04-decoration.md); for integration with statistical modeling,
see [07 analyze](../../api-guide/07-analyze.md).

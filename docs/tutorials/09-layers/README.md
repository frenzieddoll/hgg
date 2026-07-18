# 09. Layers

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.9 "Layers"**
> <https://r4ds.hadley.nz/layers>
> Data: **mpg** (234 vehicles, `../_data/mpg.csv`) and **diamonds** (53,940 observations,
> `../_data/_raw/diamonds.csv`). Both included with ggplot2.

Chapter 9 "Layers" of R4DS **reproduces all figures (32 total) shown in the text, in order**. This
chapter is central to the Visualize part, diving deep into layered grammar of graphics (aesthetic
mappings, geometric objects, facets, statistical transformations, position adjustments, coordinate
systems). Below follows R4DS's flow with **explanation → code → figure** walkthroughs. Complete
execution code is in [`Layers.hs`](Layers.hs).

```sh
cd docs/tutorials/09-layers
cabal run tut-09-layers    # generates 01-aes-color.svg .. 32-coord-polar.svg
```

## Fidelity note (measuring differences from R4DS and recording honestly)

hgg is not a ggplot2 clone, so some figures won't exactly match R. We **record measured differences
honestly, without approximation or omission**, noting them below (and repeating in each figure's
note):

- **geom_smooth**: R defaults to loess (n<1000). hgg's `statSmooth` uses **B-spline smoothing**
  (knot count 6). Curve shapes roughly agree, but not bit-identical to loess.
- **Stat grouping drives off color aesthetic only** (`Bridge.Stat.groupColumn` checks only
  `ColorByCol`). R's "`linetype=drv` for 3 lines" and "`group=drv` for 3 grey lines" are unsupported;
  grouped smooths are **represented by the color version** (§9.3).
- **shape**: R's 26 pch reference figure (`fig-shapes`) is R-internal. hgg's `MarkShape` has
  **8 types** (circle/square/triangle/diamond/cross/spade/heart/club), so we substitute a list of
  the 8 usable types (§9.2).
- **Aesthetic mapping alpha to a variable unsupported** (R also discourages alpha for discrete).
  §9.2's size/alpha pair shows only the size version.
- **Bar color (stroke) and fill not separated** (`color` = fill color). §9.6's "color vs fill" is
  consolidated to 1 figure.
- **Maps** (`map_data("nz")` + `geom_polygon` + `coord_quickmap`) unimplemented. R4DS itself states
  "this book doesn't address maps", so we touch the concept only in a correspondence table (§9.7).
- **Category order** defaults to ggplot factor order = alphabetical. Ordered factor (cut / clarity)
  is made explicit via `scaleXDiscreteLimits` / `colorCats`.

---

## 9.2 Aesthetic mappings

`mpg` is fuel economy data for 234 vehicles. We examine the relationship between `displ` (displacement)
and `hwy` (highway fuel economy), color-coded by categorical `class` (vehicle class). Mapping variables
to aesthetics (`color` / `shape` / `size` / `alpha`) inside `aes()` lets ggplot2 create scales and
legends.

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> colorBy "class" <> alpha 0.9)
      <> xLabel "displ" <> yLabel "hwy" <> legendTitle "class"
```

![Color by class](01-aes-color.svg)

Replacing `color` with `shape` makes each class a different plot character. R limits shape to 6 types
maximum, so the 7th (suv) doesn't render and triggers a warning; **hgg's `MarkShape` has 8 types, so
all 7 classes get shapes** (rendering more than R. Honest record).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> shapeBy "class" <> alpha 0.9)
```

![Shape by class](02-aes-shape.svg)

Similarly, `size` (point size) can be mapped. Mapping unordered categories to ordered aesthetics is
generally unwise, as it implies nonexistent ranking (R warns).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> sizeBy "class" <> alpha 0.6)
```

![Size by class](03-aes-size.svg)

> **Honest record**: R then shows `aes(alpha = class)` (transparency mapped), but **mapping alpha to
> a variable is unsupported in hgg** (R also discourages alpha for discrete). The size version above
> handles the example of mapping unordered categories to ordered aesthetics.

Aesthetics can also be **fixed to a value** **outside `aes()`** (as geom function arguments). Color
then carries no variable information, just visual change.

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> color (fromHex "#0000ff") <> alpha 0.9)
```

![All points fixed to blue](04-aes-blue.svg)

> **Honest record (fig-shapes)**: R shows a reference figure of 26 numbered shapes (pch 0–25), which
> explains R's internal pch system. hgg's `MarkShape` has 8 types. We list the 8 usable types
> (shapes are fixed by name via `shapeMapEntry`).

![hgg's 8 shapes](05-shapes.svg)

---

## 9.3 Geometric objects

Even with the same x, y, and data, changing **geom (geometric object)** changes appearance. We compare
points (`geom_point`) and smooth curves (`geom_smooth`).

```haskell
-- points
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
-- smooth (with confidence band)
mpg |>> theme ThemeGrey <> layer (statSmoothCI "displ" "hwy" 6 <> color (fromHex "#3366FF"))
```

![geom_point](06-geom-point.svg)
![geom_smooth](07-geom-smooth.svg)

> **Honest record**: R's `geom_smooth` default is loess. hgg's `statSmooth` uses **B-spline**
> (knot 6), so curvature at the ends differs slightly from loess.

Smooth curves can be grouped. R changes line type with `linetype=drv` for 3 lines; **hgg's stat
grouping drives off the color aesthetic**, so we use `colorBy "drv"` for 3 lines (distinguished by
color. Honest record).

```haskell
mpg |>> theme ThemeGrey <> layer (statSmoothCI "displ" "hwy" 6 <> colorBy "drv")
```

![3 smooth curves by drv](08-smooth-color-drv.svg)

**Layering** geoms lets one plot hold multiple layers. Having local mapping per layer is key to grammar
of graphics. We color both points and smooths by `drv` (R uses points=color, smooth=linetype; we use
color for both).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> colorBy "drv" <> alpha 0.9)
      <> layer (statSmoothCI "displ" "hwy" 6 <> colorBy "drv")
```

![Points and smooth overlaid by drv](09-point-smooth-drv.svg)

Classic local mapping example: color points by `class`, keep smooth as **single line for all data**.
`geom_point` mapping affects only that layer; `geom_smooth` doesn't receive it.

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> colorBy "class" <> alpha 0.9)
      <> layer (statSmoothCI "displ" "hwy" 6 <> color (fromHex "#3366FF"))
```

![Points=class, smooth=overall](10-point-class-smooth.svg)

Layers can use **different data**. Plot all points, then highlight only 2-seaters (`class ==
"2seater"`) with red points and red hollow circles (equivalent to local `data` argument).

```haskell
let twoSeater = mpg |> DF.filterBy (== "2seater") (F.col @Text "class")
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
      <> layer (scatter (inline displ2) (inline hwy2) <> color (fromHex "#ff0000"))
      <> layer (scatter (inline displ2) (inline hwy2)
                 <> color (fromHex "#ff0000") <> hollow <> size 9 <> stroke 1.2)
```

![2seater highlighted](11-2seater.svg)

Changing geoms reveals different distribution aspects. View `hwy` distribution via histogram / density
/ boxplot.

```haskell
mpg |>> theme ThemeGrey <> layer (histogram "hwy" <> binWidth 2)   -- bimodal, right tail
mpg |>> theme ThemeGrey <> layer (density "hwy")
mpg |>> theme ThemeGrey <> layer (boxplot "hwy")                    -- 2 outliers
```

![histogram](12-histogram.svg)
![density](13-density.svg)
![boxplot](14-boxplot.svg)

Extension package geoms also work. R uses **ggridges**' `geom_density_ridges` to stack densities by
category vertically. hgg's `ridge` is equivalent (mapping same `drv` to `y` / `fill` / `color`, with
`alpha 0.5` for transparency).

```haskell
mpg |>> theme ThemeGrey <> layer (ridge "hwy" <> colorBy "drv" <> alpha 0.5) <> legendOff
```

![ridgeline plot](15-ridges.svg)

---

## 9.4 Facets

**Facets** split a plot into small subplots by categorical variable. `facet_wrap(~cyl)` wraps by one
variable (R displays `cyl` as discrete labels "4".."8"; we convert `cyl` to Text too).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
      <> facetWrap "cyl_f" 2
```

![facet_wrap(~cyl)](16-facet-wrap-cyl.svg)

For two variables, `facet_grid(rows ~ cols)` creates a 2D grid: rows=`drv`, columns=`cyl` (empty cells
for unobserved combinations: 5-cylinder×4WD, 4/5-cylinder×FF, etc.).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
      <> facetGrid "drv" "cyl_f"
```

![facet_grid(drv ~ cyl)](17-facet-grid-drv-cyl.svg)

By default, all facets share the same scale. `scales="free"` frees y per row and x per column
(`facetScales FacetFree`).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
      <> facetGrid "drv" "cyl_f" <> facetScales FacetFree
```

![facet_grid + scales=free](18-facet-grid-free.svg)

---

## 9.5 Statistical transformations

Bar plots don't render raw data—they **compute new values**. `geom_bar(aes(x=cut))` computes `count`
per `cut` in `diamonds`. hgg's `bar` takes aggregated y as height, so we first do the `stat_count`
equivalent via `groupBy + countAll` (values unchanged: Fair 1610 .. Ideal 21551).

```haskell
let byCut = diamonds |> DF.groupBy ["cut"] |> DF.aggregate [ F.countAll `F.as` "n" ]
byCut |>> theme ThemeGrey <> layer (bar "cut" "n") <> scaleXDiscreteLimits cutOrder
```

![Count per cut](19-bar-cut.svg)

R4DS explains the raw data → stat(aggregate) → plot flow with this figure (quoting R4DS original):

![stat mechanism](images/visualization-stat-bar.png)

**Three reasons to make stat explicit**:

1. Override default stat. R counts with `count(cut)` then uses `geom_bar(stat="identity")` to render
   raw `n` as height. hgg's `bar` takes aggregated y as-is, so plotting the same aggregation with
   `y=n` is equivalent.

   ```haskell
   byCut |>> theme ThemeGrey <> layer (bar "cut" "n") <> scaleXDiscreteLimits cutOrder
   ```

   ![stat=identity](20-bar-identity.svg)

2. Override default mapping of computed variable to aesthetic. Plot **proportion** (`after_stat(prop)`)
   as height, not count. Create `prop = n / total` as a derived column.

   ```haskell
   let byProp = byCut |> DF.derive "prop"
                  (F.lift (\k -> fromIntegral k / total) (F.col @Int "n"))
   byProp |>> theme ThemeGrey <> layer (bar "cut" "prop") <> scaleXDiscreteLimits cutOrder
   ```

   ![proportion](21-bar-prop.svg)

3. Front-and-center statistical transform. R's `stat_summary` draws min–max vertical lines and median
   point per cut for `depth`. dataframe lacks median aggregation, so we extract `depth` per cut,
   compute exactly in Haskell, and recreate with `lineRange` (lines) + `scatter` (median points)
   (`lineRange` takes continuous x, so we position cut at 0..4 numeric positions and relabel ticks).

   ```haskell
   let depthStats = [ (minimum ds, median ds, maximum ds) | c <- cutOrder, let ds = ... ]
   DF.empty |>> theme ThemeGrey <> layer (lineRange (inline cutXs) (inline mids) (inline halves) <> stroke 1.5)
            <> layer (scatter (inline cutXs) (inline meds) <> size 7)
            <> xAxis (axisBreaksLabeled (zip cutXs cutOrder))
   ```

   ![stat_summary (depth)](22-stat-summary.svg)

---

## 9.6 Position adjustments

Bars get color via `color` (stroke) / `fill` (fill). **hgg doesn't separate stroke and fill**
(`color` = fill color), so R's `color=drv` and `fill=drv` produce the same figure (honest record).
We show the fill-color version.

```haskell
let byDrv = mpg |> DF.groupBy ["drv"] |> DF.aggregate [ F.countAll `F.as` "n" ]
byDrv |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "drv") <> legendOff
```

![Color by drv](23-bar-fill-drv.svg)

Mapping `fill` to another variable (`class`) makes bars **stack automatically** (default = stack).
Each colored rectangle represents a `drv` × `class` combination.

```haskell
let byDrvClass = mpg |> DF.groupBy ["drv","class"] |> DF.aggregate [ F.countAll `F.as` "n" ]
byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosStack)
```

![stack (default)](24-bar-stack-class.svg)

Besides stacking, there are 3 other `position` options: `"identity"` / `"fill"` / `"dodge"`.

- `position = "identity"` places each object at its exact position. Bars overlap, so we lower `alpha`
  to transparent to show overlap.

  ```haskell
  byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosIdentity <> alpha 0.2)
  ```

  ![identity (transparent)](25-bar-identity.svg)

- `position = "fill"` normalizes each stack to height 1, making proportions easier to compare.

  ```haskell
  byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosFill)
  ```

  ![fill](26-bar-fill.svg)

- `position = "dodge"` places overlapping objects side-by-side, easing individual value comparison.

  ```haskell
  byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosDodge)
  ```

  ![dodge](27-bar-dodge.svg)

For scatter plots, `"jitter"` is effective. The first scatter shows only 126 of 234 observations
(values are rounded, points overlap = **overplotting**).

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy")
```

![overplotting](28-scatter-overplot.svg)

`position = "jitter"` adds tiny noise to each point, scattering overlaps (`jitterX`/`jitterY`).
Small scales lose accuracy, but large scales reveal distribution well.

```haskell
mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> jitterX 0.02 <> jitterY 0.02)
```

![jitter](29-jitter.svg)

---

## 9.7 Coordinate systems

The default coordinate system is Cartesian. Here we vary the system starting from a bar plot of
`clarity` (diamond clarity).

```haskell
let byClarity = diamonds |> DF.groupBy ["clarity"] |> DF.aggregate [ F.countAll `F.as` "n" ]
byClarity |>> theme ThemeGrey <> layer (bar "clarity" "n" <> colorBy "clarity" <> colorCats clarityOrder)
            <> scaleXDiscreteLimits clarityOrder <> legendOff
```

![clarity bar plot](30-bar-clarity.svg)

`coord_flip()` swaps x and y, making horizontal bars (`coordFlip`).

```haskell
byClarity |>> theme ThemeGrey <> layer (bar "clarity" "n" <> colorBy "clarity" <> colorCats clarityOrder)
            <> scaleXDiscreteLimits clarityOrder <> coordFlip <> legendOff
```

![coord_flip](31-coord-flip.svg)

`coord_polar()` uses polar coordinates, showing the bar plot–**Coxcomb chart** relationship
(`coordPolar`).

```haskell
byClarity |>> theme ThemeGrey <> layer (bar "clarity" "n" <> colorBy "clarity" <> colorCats clarityOrder)
            <> scaleXDiscreteLimits clarityOrder <> coordPolar <> legendOff
```

![coord_polar (Coxcomb)](32-coord-polar.svg)

> **Honest record (maps)**: R shows one more, `coord_quickmap()` (aspect ratio correction for maps),
> with `map_data("nz")` + `geom_polygon`. **hgg lacks both polygon geom and map projection**
> (R4DS itself states "this book doesn't go deep on maps"). We address the concept only in a
> correspondence table:
>
> | R | Role | hgg |
> |---|---|---|
> | `map_data("nz")` | Map boundary data | Unimplemented (no geographic data fetch) |
> | `geom_polygon` | Closed path fill | Unimplemented (future candidate) |
> | `coord_quickmap()` | Lat/lon aspect ratio correction | Unimplemented |

---

## 9.8 The layered grammar of graphics

Grammar of graphics rests on the insight that **any** plot can be uniquely described by combining
**data, geom, mapping, stat, position, coordinate system, facet, theme**. R4DS explains the flow
from raw data to plot with this figure (quoting R4DS original):

![grammar of graphics](images/visualization-grammar.png)

Adding position, stat, coordinate, and facet to the template lets us build tens of thousands of plots:

```
ggplot(data = <DATA>) +
  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>), stat = <STAT>, position = <POSITION>) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>
```

In hgg, we compose `layer (mark <> aes... <> position ...) <> coord... <> facet...` with `<>`, then
bundle to data via `df |>> spec` (all figures in this chapter exemplify this).

---

## Related

- Complete code: [`Layers.hs`](Layers.hs)
- R4DS original: <https://r4ds.hadley.nz/layers>
- Previous chapter: [`../08-getting-help`](../08-getting-help) (Whole Game close)

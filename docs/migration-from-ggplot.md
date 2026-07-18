# ggplot2 → hgg Migration Guide

> 🌐 **English** | [日本語](migration-from-ggplot.ja.md)

For experienced ggplot2 users, this shows **function mappings** for fastest rewriting.
For learning the grammar step-by-step, see [API Reference (api-guide/)](api-guide/README.md).
For broader comparison including matplotlib and philosophy, see [comparison.md](comparison.md).

## Three Core Principles

1. **`+` → `<>`**. ggplot's `p + geom_point() + theme_minimal()` becomes
   `purePlot <> layer (...) <> theme ...`.
2. **`<>` has two levels**. Aesthetics within a geom (`color` / `size` …) are **inside** `layer (geom <> aes)`,
   while theme / scale / facet / labs are **outside**. → [api-guide 03 decoration](api-guide/04-decoration.md).
3. **`aes(x, y)` data comes two ways**: direct value `scatter (inline xs) (inline ys)` or
   DataFrame + column name `df |>> layer (scatter "x" "y")` ([api-guide 05 dataframe](api-guide/06-dataframe.md)).

---

## Mapping Table

### Plot Structure

| ggplot2 | hgg |
|---|---|
| `ggplot(d, aes(x, y))` | `purePlot` (+ geom below) |
| `+ geom_point()` | `<> layer (scatter "x" "y")` |
| `+ geom_*()` another layer | `<> layer (...)` add another |
| `ggsave("out.svg", p)` | `saveSVG "out.svg" spec` / with df: `saveSVGBound "out.svg" bound` |

### Geoms

| ggplot2 | hgg |
|---|---|
| `geom_point` | `scatter` |
| `geom_line` | `line` |
| `geom_col` / `geom_bar` | `bar` |
| `geom_step` | `step` |
| `geom_text` / `geom_label` | `geomText` / `geomLabel` |
| `geom_histogram` | `histogram` |
| `geom_boxplot` | `boxplot` / `boxplotBy` |
| `geom_violin` | `violin` |
| `geom_density` | `density` |
| `stat_ecdf` | `ecdf` |
| `geom_ribbon` | `band` |
| `geom_errorbar` / `geom_pointrange` | `errorY` / `pointRange` |
| `geom_smooth(method="lm")` | `statLm` (analyze bridge) or `regressionLineCI` |
| `geom_smooth(method="lm", level=0.99)` | `statLmLevel "x" "y" 0.99` |
| `geom_smooth()` (smooth, no band) | `statSmooth` (analyze bridge) |
| `geom_smooth()` (smooth + band) | `statSmoothCI` |
| `geom_smooth(method="lm", formula=y~poly(x,2))` | `statPoly "x" "y" 2` |
| `geom_smooth(aes(color=g))` (by group) | `statLm … <> color "g"` |
| `plot(lm)` #1 (residuals vs fitted) | `statResid "x" "y"` |
| `geom_tile` / `geom_bin2d` | `heatmap` / `bin2d` |
| `geom_contour` | `contour` |

### Aesthetics (inside layer with `<>`)

| ggplot2 | hgg |
|---|---|
| `aes(color = g)` | `<> color "g"` (or `color (inlineCat gs)`) |
| `aes(size = s)` | `<> sizeBy "s"` |
| `aes(shape = g)` | `<> shapeBy "g"` |
| `aes(linetype = g)` | `<> linetypeBy "g"` |
| `alpha = 0.7` (fixed) | `<> alpha 0.7` |
| `color = "red"` (fixed) | `<> colorStatic "#dc2626"` |
| `position = "dodge" / "stack" / "fill"` | `<> position PosDodge / PosStack / PosFill` |

### Scales (outside plot with `<>`)

| ggplot2 | hgg |
|---|---|
| `scale_color_manual(values = …)` | `scaleColorManual [("a","#…")]` |
| `scale_color_gradient2()` | `scaleColorGradient2 lo mid hi midPt` |
| `scale_size(range = …)` | `scaleSize lo hi` |
| `scale_x_log10()` | `logAxis` (x-axis adjustment) |
| `scale_x_reverse()` | `reverseX` |

### Facet / Coord / Theme / Labs (outside plot with `<>`)

| ggplot2 | hgg |
|---|---|
| `facet_wrap(~g, ncol = n)` | `facetWrap "g" n` |
| `facet_grid(r ~ c)` | `facetGrid "r" "c"` |
| `coord_flip()` | `coordFlip` |
| `coord_polar()` | `coordPolar` |
| `theme_minimal()` | `theme ThemeMinimal` |
| `theme(legend.position = "bottom")` | `legendPos LegendBottom` |
| `theme(legend.position = "none")` | `legendOff` |
| `labs(title=, x=, y=)` | `title … <> xLabel … <> yLabel …` (or `labs (emptyLabs{…})`) |

> **All values** that each setting can take (`Position` / `ThemeName` / `LineType` / `LegendPosition` …)
> are enumerated in [api-guide 03 enum table](api-guide/04-decoration.md#enum-tables).

---

## Minimal Contrasting Example

ggplot2:

```r
ggplot(d, aes(weight, mpg, color = origin)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  facet_wrap(~cyl, ncol = 3) +
  theme_minimal() +
  labs(title = "MPG vs weight")
```

hgg (DataFrame + stat-in route):

```haskell
df |>> ( layer (scatter "weight" "mpg" <> color "origin" <> size 3)
       <> layer (statLm "weight" "mpg")          -- geom_smooth(method="lm")
       <> facetWrap "cyl" 3
       <> theme ThemeMinimal
       <> title "MPG vs weight" )
```

Render via stat-in path's `saveSVGBoundStats` ([api-guide 06 analyze (route 2 stat-in)](api-guide/07-analyze.md)).
To overlay a fitted model without `statLm`, use `toPlot (lmModel x y)` (route 1, same 06 analyze).

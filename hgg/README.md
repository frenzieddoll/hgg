# hgg — a grammar of graphics for Haskell

A Haskell-native declarative plotting library. Like ggplot2 and Vega-Lite it
follows the **grammar of graphics** philosophy: plots are built by **monoid
composition** — `purePlot <> layer (mark …) <> settings …`. It pairs with the
statistical library [**hanalyze**](https://hackage.haskell.org/package/hanalyze)
(hanalyze = analysis / hgg = visualization), so fitted models — regression, GLM,
GP, survival, time series, Bayesian HBM — can be overlaid directly onto plots.

> **Status**: practical, pre-1.0 (API stabilising). SVG / PDF / PNG / Jupyter backends work today.

This is the **umbrella package**: depending on `hgg` brings in the core
(`hgg-core`), the dataframe binding (`hgg-frame`) and the SVG backend
(`hgg-svg`), and a single `import Graphics.Hgg` covers the whole default
experience. A gallery with generating code is on
[GitHub](https://github.com/frenzieddoll/hgg#gallery).

## Installation

Add `hgg` to your `build-depends`:

```
build-depends: hgg
```

Optional backends are enabled with manual cabal flags — in your
`cabal.project`:

```
constraints: hgg +pdf +png +latex +3d
```

| Flag | Pulls in | Gives you |
|---|---|---|
| `pdf` | `hgg-pdf` | PDF output (`Graphics.Hgg.Backend.PDF`) |
| `png` | `hgg-rasterific` | PNG output, Japanese fonts supported (`Graphics.Hgg.Backend.Rasterific`) |
| `latex` | `hgg-latex` | LaTeX/TikZ output (`Graphics.Hgg.Backend.LaTeX`) |
| `3d` | `hgg-3d` | 3D plots, CPU projection (`Graphics.Hgg.ThreeD`) |

The umbrella is a convenience, not a requirement: you can instead depend on
the individual packages (`hgg-svg`, `hgg-pdf`, `hgg-rasterific`, `hgg-latex`,
`hgg-3d`, `hgg-ihaskell`, `hgg-custom`, `hgg-analyze-bridge`) and skip `hgg`
entirely — for Jupyter inline display use `hgg-ihaskell`.

## Quick start

The shortest form is one line (one figure, no decisions beyond the data).

```haskell
import Graphics.Hgg

main :: IO ()
main = quickScatter "scatter.svg" [1,2,3,4,5] [1,4,9,16,25]
```

To add decorations, use the Easy helpers (direct values + `overlay`).

```haskell
import Graphics.Hgg

main :: IO ()
main = saveSVG "easy.svg" $
     overlay [ points [1,2,3,4,5] [1,4,9,16,25] ]
  <> title "y = x²" <> xLabel "x" <> yLabel "y"
  <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
```

To work with **column names**, bind a data source with `|>>` (the idiomatic
style). Put a value that has columns on the left of `|>>` (below: inline
`[(name, ColData)]`) and refer to columns by name in the spec on the right.
`|>>` binds more loosely than `<>`, so no outer parentheses are needed even
with several layers.

```haskell
import Graphics.Hgg
import qualified Data.Vector as V
import Data.Text (Text)

main :: IO ()
main = saveSVGBound "bound.svg" $
     cols |>> layer (scatter "x" "y")
  <> title "y = x²" <> xLabel "x" <> yLabel "y"
  where
    cols = [ ("x", NumData (V.fromList [1,2,3,4,5]))
           , ("y", NumData (V.fromList [1,4,9,16,25])) ] :: [(Text, ColData)]
```

## A taste of the grammar

A plot is the empty `purePlot` plus `layer (mark …)` pieces combined with `<>`.
Data is bound with `|>>`; colour and shape are given inside the mark with
`colorBy`/`shapeBy` (below, `raw` is palmerpenguins).

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

The full step-by-step walkthrough (all 24 figures) is in the
[R for Data Science, chapter 1 tutorial](https://github.com/frenzieddoll/hgg/blob/master/docs/tutorials/01-visualize/README.md).

## What you can do

- **Layer/mark declarative API** — scatter, line, bar, histogram, boxplot, violin,
  density, band, forest, heatmap, contour, vector field, DAG, MCMC diagnostics, …
- **DataFrame integration** — write `df |>> layer (scatter "x" "y")` with column names
  (NA rows are dropped automatically, i.e. `na.rm`)
- **Backends** — SVG / PDF / PNG (Japanese fonts supported) / LaTeX (TikZ) / Jupyter (iHaskell) inline
- **3D** — response surfaces (RSM) and generic 3D plots (CPU projection)
- **Statistical integration** — `toPlot` / `statLm` / HBM extractors draw
  hanalyze's fitted models directly
- **Full decoration set** — themes / scales / facets / subplots / coordinate systems /
  reference lines / legends (ggplot-alike)

## Documentation

- [Gallery + full README](https://github.com/frenzieddoll/hgg#readme)
- [API Reference](https://github.com/frenzieddoll/hgg/blob/master/docs/api-guide/README.md) — organised by topic
- [Tutorial: R for Data Science, chapter 1](https://github.com/frenzieddoll/hgg/blob/master/docs/tutorials/01-visualize/README.md)
- [Getting Started](https://github.com/frenzieddoll/hgg/blob/master/docs/getting-started.md) /
  [ggplot2 Migration Guide](https://github.com/frenzieddoll/hgg/blob/master/docs/migration-from-ggplot.md)

## License

BSD-3-Clause (same as hanalyze).

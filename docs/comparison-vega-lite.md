# Comparison with Vega-Lite

> 🌐 **English** | [日本語](comparison-vega-lite.ja.md)

[Comparison with matplotlib/ggplot2](comparison.md) was framed along the imperative/declarative axis. Here we compare hgg with **Vega-Lite, the closest relative** to hgg. Both embrace:

> "Assemble **data + marks + encodings as a declarative spec (AST)** and let the renderer draw it"

This grammar of graphics is implemented **spec-centric** (rather than as a state machine like matplotlib). hgg's `VisualSpec` corresponds to Vega-Lite's JSON spec.

This guide focuses on two axes where hgg is tightly integrated: **DataFrame integration** and **statistical engine (hanalyze) integration**.

> ⚠ hgg APIs here are confirmed implementations (`hgg-frame` / `hgg-analyze-bridge` / analyze `Hanalyze.Plot`). Vega-Lite references are based on v5 official syntax. Details may change with versions.

## In a nutshell

- **Same "declarative spec" philosophy** — Vega-Lite uses JSON, hgg uses `VisualSpec` (Haskell ADT). Vega-Lite's `layer` array corresponds to hgg's `<>` (Monoid composition).
- **Critical difference: "where is statistics computed?"** — Vega-Lite's viz runtime (JS) has built-in `transform` for limited statistics (regression/loess/density). hgg uses **the real statistical library hanalyze** to compute, flowing results into the spec. Models like GLMM / GP credible bands / survival curves / Bayesian workflows that don't exist in viz runtimes become plots directly.
- **Vega-Lite's strengths: interactivity and ecosystem** (selection / tooltip / pan-zoom, Altair, browser). hgg is currently **static output** (SVG/PNG/WebGL/PDF) with no interactivity.

## The same plot in two styles

Scatterplot + linear regression line.

### Vega-Lite (JSON spec, regression via built-in transform)

```json
{
  "data": {"values": [{"x": 1, "y": 2.1}, {"x": 2, "y": 3.9}]},
  "layer": [
    {"mark": "point", "encoding": {
      "x": {"field": "x", "type": "quantitative"},
      "y": {"field": "y", "type": "quantitative"}}},
    {"mark": "line", "transform": [{"regression": "y", "on": "x"}],
     "encoding": {"x": {"field": "x"}, "y": {"field": "y"}}}
  ]
}
```

The regression line is computed by the Vega runtime (JS) via `transform.regression`.

### hgg (VisualSpec, regression computed by hanalyze)

```haskell
-- (a) stat-in: stats are ordinary layers like geom. Bridge delegates fit to hanalyze (df seen once)
saveSVGBoundStats "out.svg" $ df |>> (layer (scatter "x" "y") <> layer (statLm "x" "y"))

-- (b) model-out: fit in hanalyze first, convert to plot with toPlot
let fit = lmModel xs ys           -- Real fit from Hanalyze.Model.*
df |>> (layer (scatter "x" "y") <> toPlot fit)   -- regression line + confidence band
```

Numbers in `lm`/`toPlot` are computed by **hanalyze's `fitLM`** (not Vega's JS regression). Confidence bands come from hanalyze's `confidenceBand`.

## Axis 1: DataFrame integration

| Aspect | Vega-Lite | hgg |
|---|---|---|
| Data supply | `data.values` (inline JSON) / `data.url` (CSV/JSON/TSV) | `class PlotData` + `(|>>)` bind (`BoundPlot`) |
| Column reference | `encoding.x.field: "x"` (string) | `scatter "x" "y"` (column name, `Resolver` resolves) |
| Direct df | Python: **Altair** converts pandas → spec (separate layer) | `instance PlotData DataFrame` **binds Hackage `dataframe` directly** |
| Validation | At spec compile time (vega) | At bind time with `validatePlotWith` — **column name validation → diagnostics carried as values** (`bpDiagnostics`) |
| Type safety | JSON → runtime | `PlotData` typeclass, Haskell type checking (column names are strings but structure is typed) |

Key point: Vega-Lite either "carries data in JSON" or converts pandas with Altair. hgg accepts `dataframe` directly as a `PlotData` instance, **validates column names at bind time** and carries diagnostics as values in `BoundPlot` (pure path, no exceptions). Note: `|>` is taken by Hackage `dataframe`, so the bind operator is **`|>>`**.

## Axis 2: Statistical engine (hanalyze) integration ★

This is the biggest differentiator.

| Aspect | Vega-Lite (`transform`) | hgg |
|---|---|---|
| Regression | `regression` (linear/log/exp/pow/quad/poly) | `lm`/`statLm` / `LMModel` `toPlot` (with CI band) |
| Polynomial regression | `regression` (method=poly) | `statPoly` / `SplineSpec` (arbitrary degree) |
| Smoothing | `loess` | `statSmooth` (B-spline) / `SplineModel` `toPlot` |
| Density | `density` (KDE) | histogram/density layer (core) |
| Quantile | `quantile` | `QuantileModel` `toPlot` (multiple τ) |
| Aggregation | `aggregate`/`bin`/`window` | df side + core's `bin2d` etc |
| **Weighted regression (WLS)** | ✗ (no weight in regression) | **`weighted ws (lm "x" "y")`** (√w scaling, WLS CI, weighted R²) |
| **GLM** | ✗ (not in built-in transform) | **`GLMModel` `toPlot`** (μ curve + asymmetric credible band) |
| **GP** | ✗ | **`GPResult` `toPlot`** (mean + credible band) |
| **Survival** | ✗ | **`KMResult`/`CRFit` `toPlot`** (KM step / competing CIF) |
| **Time series forecasting** | ✗ | **`ForecastModel` `toPlot`** (AR prediction interval) |
| **Bayesian (HBM)** | ✗ | **HBM extractors** (`epred`/`traceOf`/`forestOf`/`ppcOf`/`dagOf`/`marginalsOf`), NUTS posterior |
| **Mixed effects** | ✗ | hanalyze `GLMM` (Phase 48: random intercept+slope), fit with `glmmF`. `toPlot` coming |
| **Trees/multivariate** | ✗ | `RandomForest` (importance bar) / `PCAResult` (scree) `toPlot` |
| Computing substrate | **Vega runtime (JS)** | **hanalyze (real statistical library)** |

Two integration paths:

1. **model-out (`toPlot`)**: Fit model in hanalyze → convert to plot with `toPlot :: m -> VisualSpec`. `class Plottable` has **14 model instances** (LM/GLM/GP/Spline/GAM/Robust/MultiFit/Quantile/MCMCChain/KM/CRFit/Forecast/PCA/RandomForest) plus **WLS (`WeightedLMModel`), grouped fits (`GroupedFit` — N curves overlaid by group color), HBM extractors** (`ForestSpec`/`PPCSpec`/`DagSpec` etc). Band semantics are statistically correct per model (Wald band / GP credible band / AR prediction interval / Bayesian 94% HDI / quantile is the line itself). Unified verb `df |-> spec` (learn arbitrary model from any data source).

2. **stat-in (`statLm`/`statSmooth`/`statPoly`/`statResid`)**: ggplot style: `layer (scatter "x" "y") <> layer (statLm "x" "y")` (stats are ordinary layers, df seen once). Rendering: bridge's `saveSVGBoundStats` delegates computation to hanalyze via `resolveStats`. Backward edge (plot→analyze) isolated in bridge package to keep core dependency-free (core has pure tags `MStatLM` only). ★ Plus **geom_smooth-style options**: `statColor`/`statFill`/`statLinetype`/`statLinewidth`/`statAlpha` (appearance), `statLabel` (legend), `statEquation`/`statR2` (equation/R² annotation), `statLevel` + `interval CI|PI` (confidence/prediction interval switch). = Vega-Lite's `regression` lacks uncertainty, group color faceting, and equation annotation—hgg makes all declarative.

★ Vega-Lite's `regression` only computes the **fitted curve** in JS; it has no confidence intervals, prediction intervals, standard errors, model diagnostics, or weighting. hgg renders **statistically correct uncertainty** from hanalyze's `confidenceBand`/`predictionBandAt`/`predictGlmMuWithCI` etc. as bands, and makes CI/PI switching, group overlays, equation/R² annotations, and WLS all declarative.

## Axis 3: Basic plot coverage

Legend: ✓ = native / △ = via transform/workaround / ✗ = impractical. hgg references `MarkKind` implementation (`hgg-core` Spec.hs, 47 types).

| Basic plot | Vega-Lite | hgg (`MarkKind`) |
|---|---|---|
| Scatterplot | ✓ `point` | ✓ `MScatter` |
| Line | ✓ `line` | ✓ `MLine` |
| Bar (grouped/stacked/100%) | ✓ `bar` | ✓ `MBar` (dodge/stack/fill = position adjustment) |
| Area / Band | ✓ `area` | ✓ `MBand` |
| Histogram | ✓ `bar`+`bin` | ✓ `MHistogram` |
| Pie / Donut | ✓ `arc` | ✓ `MPie` |
| Step | ✓ `line(step)` | ✓ `MStep` |
| Heatmap | ✓ `rect` | ✓ `MHeatmap` |
| 2D bin (geom_bin2d) | ✓ `rect`+`bin` | ✓ `MBin2d` |
| Text / Label | ✓ `text` | ✓ `MText` / `MLabel` |
| Box plot | ✓ `boxplot` (composite) | ✓ `MBox` |
| Error bar / Interval | ✓ `errorbar`/`errorband` | ✓ `MLineRange` / `MPointRange` / `MCrossbar` |
| Strip (tick) | ✓ `tick` | ✓ `MStrip` |
| Density (KDE) | △ `density` transform | ✓ `MDensity` |
| ECDF | △ `window` transform | ✓ `MEcdf` |
| Stem (bar with point) | △ (`rule`+`point`) | ✓ `MStem` |
| QQ plot | ✗ | ✓ `MQQ` |
| Contour | ✗ (possible in Vega) | ✓ `MContour` (marching squares) |

Key point: **Basic plots roughly match**. Vega-Lite has `boxplot`/`errorbar` as composites; density/ECDF via transforms. hgg has QQ, contour, stem as **native marks** (Vega-Lite needs workarounds or can't do them).

## Axis 4: Advanced plot coverage

| Advanced plot | Vega-Lite | hgg (`MarkKind`) |
|---|---|---|
| Violin | △ (hand-compose `density`+`area`) | ✓ `MViolin` |
| Ridge line (joyplot) | △ (facet + hand-compose) | ✓ `MRidge` |
| Raincloud | ✗ | ✓ `MRaincloud` |
| Swarm (beeswarm) | ✗ | ✓ `MSwarm` |
| Parallel coordinates | △ (`fold`+`repeat`) | ✓ `MParallel` |
| Waterfall | △ (`window` hand-compose) | ✓ `MWaterfall` |
| Scatterplot matrix (SPLOM/pairs) | △ (`repeat`) | ✓ `pairs` |
| Regression + CI | △ (`regression` transform, **no CI**) | ✓ `MRegression` (hanalyze CI band) |
| Forest plot | ✗ | ✓ `MForest` |
| Funnel plot | ✗ | ✓ `MFunnel` |
| Autocorrelation (ACF) | ✗ | ✓ `MAutocorr` |
| MCMC trace / density | ✗ | ✓ `MTrace` / `MCMC` (Bayesian diagnostics) |
| ESS (effective sample size) | ✗ | ✓ `MEss` |
| Model DAG (graph) | ✗ | ✓ `MDAG` (HBM structure) |
| Wafer map | ✗ | ✓ `MWaferMap` (semiconductor) |
| 3D scatterplot | ✗ | ✓ `MScatter3D` (`hgg-3d`, CPU projection) |
| Custom shapes (trump cards etc) | ✗ (shapes limited) | ✓ `MShClub`/`MShSpade`/`MShHeart`/… |

Key point: **Advanced, statistical, and domain-specific plots are where hgg excels**. Vega-Lite's high-level grammar means violin/parallel coordinates/waterfall need hand-composition; forest, ACF, MCMC diagnostics, DAG, wafer map, 3D are outside its scope. hgg has these as **mark types for standard statistical/engineering workflows** (especially Bayesian diagnostics, DOE, semiconductor).

## Axis 5: Grammar comparison

| Grammar element | Vega-Lite | hgg |
|---|---|---|
| Spec form | JSON object | `VisualSpec` (Haskell ADT, value) |
| Layer composition | `"layer": [...]` (array) | `<>` (Monoid) |
| Encoding | Channel record `encoding: {x, y, color, size, shape, opacity, text, tooltip, order, detail, …}` | Combinator functions `scatter "x" "y"` + `colorBy`/`sizeBy`/… |
| Type annotation | `type: quantitative/nominal/ordinal/temporal` explicit | Inferred from column values (numeric/categorical), limited `temporal` type |
| Aggregation | Inline in encoding (`aggregate: "mean"`, `bin`, `timeUnit`) | Stat marks (`MStatMean`/`MStatMedian`) or df preprocessing |
| Transform | `transform: [...]` (filter/calculate/aggregate/window/fold/pivot/regression/loess/density/quantile) | df preprocessing + stat marks + hanalyze integration (`statLm`/`statSmooth`/`toPlot`) |
| Multi-panel | `facet` / `repeat` / `concat` / `hconcat` / `vconcat` (generic) | `facet` (free/fixed scale) + **`hconcat`/`vconcat` + operators `<->`/`<:>` (concat equivalent, flattens same-direction chains)** + `subplots`/`subplotCols` (arbitrary grid, nestable) + **`repeatFields` (field repetition = repeat equivalent, explicit)** |
| Scale / axis / legend | `scale`/`axis`/`legend` + `resolve` | scale + `coord_flip` + `scale_*_reverse` + polar (coord) |
| Conditional encoding | `condition` (selection-driven) | None (static) |
| Interactivity | `params` / `selection` (point/range/pan-zoom) | None |
| Theme | `config` | Theme (light/dark + academic palette + ggplot preset + element-level override) |
| Composition algebra | JSON nesting (algebra implicit) | **Monoid laws** (`<>` associativity, identity) guaranteed by type |

Grammar philosophy differences:
- **Multi-panel**: hgg once had only `facet` and was weaker, but `subplots`/`subplotCols` (arbitrary grid for mixed specs, **nestable**) plus thin wrappers **`hconcat`/`vconcat` + operators `<->`(horizontal)/`<:>`(vertical)** now give **concat/hconcat/vconcat equivalents** with much smaller gap. Operators flatten same-direction chains so `(a <-> b <-> c) <:> d` writes asymmetric layouts like "3 columns in row 1 + full row 2" in one line (e.g., HBM diagnostics dashboard with "cells in columns" double nesting). **`repeatFields`** (list of fields, each exploded into `subplots`) gets `repeat` equivalence; multi-panel gap nearly closed (only missing Vega's `{repeat: …}` syntactic sugar in encoding).
- **Encoding style**: Vega-Lite writes all channels as a **uniform record** (high discoverability). hgg uses **combinators + Monoid** (compositional/type-safe per Haskell, but available channels are individual functions).
- **Transform locus**: Vega-Lite completes aggregation/regression/KDE **inside spec transform** (self-contained, reproducible). hgg: simple shaping on df side, **statistics delegated to hanalyze** (real statistical engine—see Axis 2).
- **Interactivity grammar**: Vega-Lite has `selection`/`params` as first-class citizens (its greatest strength). hgg is static output with no interactivity grammar.
- **Algebraic guarantees**: hgg's `<>` carries Monoid laws in the type, so sub-plots can be safely rearranged and reused (JSON nesting has no such guarantee).

## Concept mapping

| Goal | Vega-Lite | hgg |
|---|---|---|
| Plot spec | JSON object | `VisualSpec` |
| Layer overlap | `"layer": [...]` | `<>` (Monoid) |
| Mark | `"mark": "point"` | `scatter` / `line` / `bar` / … (`MarkKind`) |
| Encoding | `"encoding": {"x": …}` | `scatter "x" "y"` etc as arguments |
| Facet | `"facet"`/`"repeat"` | `facet` (free/fixed scale) / `repeatFields` (repeat equivalent) |
| Mixed panel layout | `"concat"`/`"hconcat"`/`"vconcat"` | `hconcat [..]` / `vconcat [..]` / operators `a <-> b` (horiz) / `a <:> b` (vert) / `subplots [..] <> subplotCols n` (nestable) |
| Scale | `"scale": {…}` | scale / `coord_flip` / `scale_*_reverse` |
| Theme | config / theme | Theme (light/dark + academic palette + ggplot preset) |
| Regression | `transform: regression` | `lm` / `toPlot (lmModel …)` |
| Data | `data.values`/`url` | `df |>> …` (`PlotData`) |
| Output | SVG / Canvas / PNG (vega) | SVG / WebGL / Canvas(PS) / PDF (planned) / PNG (Rasterific) |
| Interactivity | selection / tooltip / zoom | (none, static) |

## Strengths and gaps

### hgg's strengths
- **Real statistical engine integration**: Models like GLM/GP/survival/forecasting/mixed-effects that don't exist in viz runtime builtins become plots. Uncertainty (CI/credible band/prediction interval) is statistically correct.
- **Type and purity**: `VisualSpec` is a Haskell value, Monoid. Bind validation flows as values (no exceptions).
- **Direct df binding via typeclass**: `dataframe` accepted as `PlotData` instance (no intermediate JSON conversion).
- **Multiple backends**: SVG/WebGL/Canvas/PDF/PNG from one spec. HS/PS parity maintained by golden tests.

### Where Vega-Lite excels — split into two categories

We scanned the [Vega-Lite gallery](https://vega.github.io/vega-lite/examples/) and categorized examples hgg can't reproduce (code and full list at [vega-lite-gallery.md](vega-lite-gallery.md)).

**A. Out of spec by design (intentionally out-of-scope, no implementation planned)** — features users don't use, so we don't include:
- **Geography / maps** (geoshape / projection): entire Maps section.
- **Interactivity** (selection / hover / brush / pan-zoom / crossfilter / widgets): entire Interactive section.
- **Images / isotypes** (image mark): Image-based Scatter / Isotype.

**B. Future work (genuinely missing, room for implementation)** — excluding A, things currently not native:
- **Multi-panel is implemented**: `concat`/`hconcat`/`vconcat` equivalents via `subplots` (mixed spec grid, **nestable**); `repeat` equivalent via **`repeatFields`** (field list → each view exploded into `subplots`). Only missing: Vega's `{repeat: …}` syntactic sugar in encoding.
- **Streamgraph implemented** (Phase 52.D2): `stream x y <> color "series"` draws centered (silhouette, baseline=-Σy/2) stacked area. Wiggle minimization (ThemeRiver) not yet.
- **Horizon Graph / Trail (variable-width line) / Mosaic / Ternary / Candlestick / Bullet**: no dedicated mark.
- **Comprehensive declarative transform** (`fold`/`pivot`/`window` etc): hgg assumes preprocessing on df side.
- **Mixed-effects `toPlot`**: **implemented** (Phase 52.D3) — `GLMMResultRE` (`glmmF` fit) has `Plottable` instance drawing random effect **caterpillar plot** (BLUP sorted per group). ★CI band currently absent (points only): conditional variance / `n_j` not stored (future storage will enable bands).
- **Ecosystem** (Altair/browser etc) also favors Vega-Lite, but reflects design philosophy difference.

## Summary

Vega-Lite and hgg share the design philosophy **"assemble declarative spec and hand to renderer"** and are closest relatives. Five-axis assessment:

- **Basic plots (Axis 3)**: Nearly equivalent. hgg natively has QQ, contour, stem.
- **Advanced plots (Axis 4)**: **hgg excels** (violin/parallel coordinates/forest/ACF/MCMC diagnostics/DAG/wafer map/3D). Vega-Lite's high-level grammar means hand-composition or out-of-scope.
- **Grammar (Axis 5)**: Vega-Lite's strengths: **interactivity (`selection`)** and **inline transform** as first-class. hgg's strengths: **algebraic guarantee of Monoid composition** and **type safety**. Multi-panel now nearly matched via `subplots` (nestable) with concat/hconcat/vconcat equivalents (only missing `repeat` auto-expansion).
- **DataFrame integration (Axis 1)**: hgg binds `dataframe` via typeclass + validation at bind time.
- **Statistics integration (Axis 2)**: hgg computes via hanalyze (real library) ↔ Vega-Lite via JS transform.

Clear division of labor:

- **Interactive exploration / web distribution / pandas ecosystem** → Vega-Lite (Altair).
- **Real statistical models / advanced/engineering plots / type safety / Haskell data pipelines / static high-quality output** → hgg.

Especially via hanalyze integration (`toPlot` 14 models + stat-in bridge), **"fit a model and render with statistically correct uncertainty"** is hgg's unique strength.

# hgg — Haskell Grammar of Graphics

> 🌐 **English** | [日本語](README.ja.md)

> **Status: Documentation Preview (Code Coming Soon)**
> This repository currently publishes **documentation and API reference only**.
> The Haskell library itself will be released later.

**hgg** is a declarative plotting library for Haskell. Like ggplot2 and Vega-Lite,
it follows the **Grammar of Graphics** philosophy, building plots with monoid composition:
`purePlot <> layer (mark …) <> settings …`. It pairs with the statistical library
[**hanalyze**](https://hackage.haskell.org/package/hanalyze) (hanalyze = analysis / hgg = visualization),
letting you overlay fitted models (regression, GLM, GP, survival, time series, Bayesian HBM) directly onto plots.

## What You Can Do (Highlights)

- **Layer/mark-based declarative API** (scatter, line, bar, histogram, boxplot, violin, density,
  band, forest, heatmap, contour, vector field, DAG, MCMC diagnostics …)
- **DataFrame integration** — write `df |>> layer (scatter "x" "y")` using column names
- **Multiple backends** — SVG / PDF / PNG (with Japanese font support) / Jupyter (iHaskell) inline
- **3D plotting** — response surface (RSM), generic 3D (CPU projection + WebGL)
- **Statistical integration** — `toPlot` / `statLm` / HBM extractors to visualize hanalyze models

## Documentation

- 📚 **[API Reference](docs/api-guide/README.md)** — comprehensive topic-organized reference
- [Tutorials](docs/tutorials/README.md) — recreating examples from *R for Data Science 2e*
- [Getting Started](docs/getting-started.md) / [ggplot2 Migration Guide](docs/migration-from-ggplot.md)
- [Vega-Lite Comparison](docs/comparison-vega-lite.md) / [Module Structure](docs/modules.md)

## License

[BSD-3-Clause](LICENSE) (same as hanalyze).

---

*hgg — declarative Grammar-of-Graphics plotting for Haskell. Library code coming soon.*

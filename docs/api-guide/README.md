# hgg API Reference

> 🌐 **English** | [日本語](README.ja.md)

Comprehensive reference organized by topic. Learning pathways are covered by [tutorials](../tutorials/README.md); here we prioritize **terminology and completeness as a dictionary**.

> **Terminology**: composition unit = **layer**, plot type = **mark** (type `MarkKind`: `scatter`/`line`/`bar`/…), entire plot = **`VisualSpec`**. "geom" is used only as a cross-reference for ggplot users (native concepts here are layer / mark). 3D is a separate type system ([08 3d](08-3d.md)).

## Page index

| Page | Content |
|---|---|
| [01 quickstart](01-quickstart.md) | 30 seconds to one plot + 3 layers of API (Easy / Grammar / DataFrame) |
| [02 layers](02-layers.md) | Layers and marks (plot catalog, mark reference) |
| [03 encoding & scale](03-encoding-scale.md) | Channels (color / size / shape / static color) + scale / palette + axes (position scale) |
| [04 decoration](04-decoration.md) | Labels / theme / facet / subplot / coordinates / reference lines / overlay / advanced layering |
| [05 backends](05-backends.md) | Backend choice (SVG / PDF / PNG / Jupyter) and save functions |
| [06 dataframe](06-dataframe.md) | DataFrame integration (`df \|>> layer …`, nullable columns) |
| [07 analyze](07-analyze.md) | analyze integration (`toPlot` / `statLm` / HBM extractors) |
| [08 3d](08-3d.md) | 3D (separate types `Layer3D` / `VisualSpec3D`, response surfaces, general 3D) |
| [09 appendix](09-appendix.md) | Appendix (layer/page selection / ggplot migration / library extension) + API quick reference |
| [10 custom marks](10-custom-marks.md) | Custom marks (`customMark`) — add new plot types without modifying core |

## 3 layers of writing (any import gives you all lower layers)

| Layer | Module | Role |
|---|---|---|
| **0. Quick** | `Hgg.Plot.Quick` | `IO` one-shot. `quickScatter "out.svg" xs ys` |
| **1. Easy** | `Hgg.Plot.Easy` | `[Double]` direct pass + `overlay` default |
| **2. Grammar** | `Hgg.Plot.Spec` | ggplot-like channels + `<>` composition (primary) |

## Two golden rules

1. Plots are shaped **`purePlot <> layer (mark …) <> settings …`**. Add components with `<>`.
2. `<>` is two-layered: marks/appearance return **`Layer`** and compose **inside** `layer (…)`, titles/theme/facet return **`VisualSpec`** and compose **outside**. → [Layering rules](04-decoration.md#overlay).

## Operator quick reference

Only 4 operators build plots. Understand their roles here (individual pages cover detail).

| Operator | Role | Reference |
|---|---|---|
| `<>` | **Compose** spec / layer / settings (Monoid) | [Two golden rules](#two-golden-rules) |
| `\|>` | DataFrame **forward transform** (Hackage `dataframe`, groupBy/aggregate etc) | [06 dataframe](06-dataframe.md) |
| `\|>>` | **Bind** df to plot (pure function creating `BoundPlot`, no file I/O) | [06 dataframe](06-dataframe.md#by-column-name) |
| `\|->` | **Fit model** from df by column name | [07 analyze](07-analyze.md#fit-data) |

Save functions (`saveSVG` / `saveSVGBound` / `saveSVGBoundStats` × SVG/PDF/PNG) are listed in
[05 backends](05-backends.md#be-svg).

## Related documentation

- **Live gallery** — from reference to real data examples:
  [README gallery](../../README.md) (click plots to see relevant entries)
- [Tutorials (R4DS recreation etc)](../tutorials/README.md) — learning pathway
- [R4DS Chapter 1](../tutorials/01-visualize/README.md) (all 24 penguins plots + code)

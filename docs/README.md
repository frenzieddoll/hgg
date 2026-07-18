# hgg Documentation

> 🌐 **English** | [日本語](README.ja.md)

Declarative plot library for Haskell and PureScript. This is the entry point for users.

## Reading Order

0. **[tutorials/](./tutorials/README.md)** ─ Hands-on tutorials based on *R for Data Science 2e*
   (using mpg data: load → explore → transform → wrangle → visualize → model across 19 chapters • task-oriented entry point)
1. **[getting-started.md](./getting-started.md)** ─ Installation, quickest way to produce one plot, choosing a backend
2. **[API Reference (api-guide/)](./api-guide/README.md)** ─ Complete API organized by topic (layers/marks, decoration, backends, dataframe, analyze, 3D)
3. **[modules.md](./modules.md)** ─ Package structure and "what's possible" checklist (chart types, coordinate systems, themes …)
4. **[comparison.md](./comparison.md)** ─ Correspondence and differences with matplotlib / ggplot2
5. **[migration-from-ggplot.md](./migration-from-ggplot.md)** ─ Line-by-line migration guide for ggplot2 users (function mapping table)
6. **[comparison-vega-lite.md](./comparison-vega-lite.md)** ─ Vega-Lite comparison (dataframe integration & hanalyze statistical engine as axes)
7. **[vega-lite-gallery.md](./vega-lite-gallery.md)** ─ Vega-Lite gallery examples recreated (28 examples with code + classification of non-drawable examples)
8. **../design/gallery/README.md** ─ Visual gallery (30+ SVG, one-liner code example per chart type)

## Runnable Samples

Numbered tutorials live in `hgg-svg/examples/`. Each is self-contained and produces 1–2 SVG files:

| Run Command | Content | Doc |
|---|---|---|
| `cabal run tutorial-01-easy` | Easy API to draw a scatter plot | [api-guide §Easy](./api-guide/01-quickstart.md#easy) |
| `cabal run tutorial-02-grammar` | Grammar (ggplot-style) with color grouping + scale | [api-guide §Grammar](./api-guide/01-quickstart.md#grammar) |
| `cabal run tutorial-03-overlay` | Overlay multiple layers | [api-guide §Overlay](./api-guide/04-decoration.md#overlay) |
| `cabal run tutorial-04-distribution` | violin / box (group comparison) | [modules §Distribution](./modules.md) |
| `cabal run tutorial-05-theme` | Light/dark theme variants | [modules §Theme](./modules.md) |

> Output directory is the current working directory. In the repo, example outputs are stored in `design/tutorial/` for reference.

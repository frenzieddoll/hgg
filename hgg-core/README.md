# hgg-core

The backend-independent core of [`hgg`](https://github.com/frenzieddoll/hgg) —
plot spec / data / layout / render primitives.

Depends only on `base` / `vector` / `text` / `containers`. Backends
(SVG / PDF / Rasterific / LaTeX / ...) are implemented as separate packages.

## Module structure

| Module | Role |
|---|---|
| `Hgg.Plot.Spec` | `VisualSpec` ADT (declarative spec for every chart kind) |
| `Hgg.Plot.Data` | `PlotData` ADT (Vector-based, DataFrame-independent) |
| `Hgg.Plot.Layout` | `computeLayout` pure function (viewport / scale / axis) |
| `Hgg.Plot.Render` | `Primitive` ADT + `Renderer` class (backend interface) |
| `Hgg.Plot.Easy` | matplotlib-style `scatter` / `bar` / `line` / `histogram` |

See the [repository README](https://github.com/frenzieddoll/hgg) for a
gallery and getting-started guide.

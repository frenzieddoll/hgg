# hgg-core

The backend-independent core of [`hgg`](https://github.com/frenzieddoll/hgg) —
plot spec / data / layout / render primitives.

Depends only on `base` / `vector` / `text` / `containers`. Backends
(SVG / PDF / Rasterific / LaTeX / ...) are implemented as separate packages.

## Module structure

| Module | Role |
|---|---|
| `Graphics.Hgg.Spec` | `VisualSpec` ADT (declarative spec for every chart kind) |
| `Graphics.Hgg.Data` | `PlotData` ADT (Vector-based, DataFrame-independent) |
| `Graphics.Hgg.Layout` | `computeLayout` pure function (viewport / scale / axis) |
| `Graphics.Hgg.Render` | `Primitive` ADT + `Renderer` class (backend interface) |
| `Graphics.Hgg.Easy` | matplotlib-style `scatter` / `bar` / `line` / `histogram` |

See the [repository README](https://github.com/frenzieddoll/hgg) for a
gallery and getting-started guide.

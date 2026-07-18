# Getting Started

> 🌐 **English** | [日本語](getting-started.ja.md)

## Installation

> ⚠️ hgg is **not yet published to Hackage / npm** (public release after official version).
> Currently use it as a local package within the repository.

The `cabal.project` has `packages:` entries for each package, resolved by `cabal build`.
To use from your own project, add the required packages to `build-depends`:

```cabal
build-depends:
    hgg-core    -- Spec / Layout / Palette / DAG (pure Haskell, only base+vector+text+containers)
  , hgg-svg     -- SVG backend (when using saveSVG)
```

For PureScript (frontend), add `hgg-canvas` to your spago dependencies (Halogen / web-canvas).

## Quick Start ─ One Plot in Seconds

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Hgg.Easy                 -- Layer 1: introductory API (Spec also re-exported)
import Graphics.Hgg.Backend.SVG (saveSVG)

main :: IO ()
main = saveSVG "quick.svg" $
     overlay [ points [1,2,3,4,5] [1,4,9,16,25] ]   -- Pass [Double] directly
  <> title "y = x²"
  <> xLabel "x" <> yLabel "y"
  <> width 600 <> height 400
```

```bash
cabal run tutorial-01-easy    # Run nearly identical code → tutorial-01-easy.svg
```

Key points:

- **Only the final `saveSVG` has a side effect**. Plots are built as pure values (`VisualSpec`).
- **Compose with `<>`**. `title` and `width` both return `VisualSpec`, so just add them via Monoid.
- Overlay must be **wrapped in `overlay [...]`** (avoid the pitfall of directly adding `scatter <> line`.
  See [api-guide/](./api-guide/README.md) and `design/monoid-semantics.md` for details).

## Choosing a Backend

The core (`hgg-core`) is backend-agnostic. Choose a package based on your output target:

| Goal | Package | Function / Entry | Status |
|---|---|---|---|
| Write to SVG file | `hgg-svg` | `saveSVG` (simple) / `saveSVGWith` (Resolver) / `saveSVGBound` (df) | ✅ Production-ready |
| Interactive browser drawing (Halogen) | `hgg-canvas` (PureScript) | Canvas backend | ✅ In active use |
| 3D with CPU projection (SVG/PDF/PNG) | `hgg-3d` | scatter3D / surface3D etc. | ✅ |
| 3D in browser with orbit/zoom | `hgg-canvas` (WebGL2) | `showBrowser` | ✅ |
| Jupyter (iHaskell) inline rendering | `hgg-ihaskell` | `display` (SVG inline) | 🧪 Experimental |
| Write to PDF / PNG | `hgg-pdf` / `hgg-rasterific` | ─ | 🚧 Placeholder |

Entry point usage (details in [api-guide 05 dataframe](./api-guide/06-dataframe.md)):

- `saveSVG :: FilePath -> VisualSpec -> IO ()` ─ No Resolver needed (inline only plots). **Usually this**.
- `saveSVGWith :: FilePath -> Resolver -> VisualSpec -> IO ()` ─ Pass `Resolver` for plots with `ColByName`.
- `saveSVGBound :: FilePath -> BoundPlot -> IO ()` ─ Save DataFrame `df |>> spec`.
- For SVG text instead of files, use `renderSVG` / `renderSVGWith`.

## Using in Jupyter (iHaskell)

Import `hgg-ihaskell` and plot values render **inline** in cells (matplotlib-style, currently **SVG only**; PNG/PDF forthcoming).
Rendering uses the SVG backend's `renderSVG`, passed to iHaskell's `svg` display helper — minimal wiring, and the `ihaskell` dependency is isolated in this package (core/svg are dependency-free).

```haskell
:set -XOverloadedStrings
import Graphics.Hgg.Easy
import Graphics.Hgg.IHaskell (DisplayPlot(..))   -- Instance + DisplayPlot

-- Plots with only inline columns can be placed at the end of a cell for rendering
layer (scatter (inline [0,1,2,3]) (inline [0,1,4,9])) <> title "demo"
```

For plots using column name references (`ColByName`), a `Resolver` is needed, so wrap in `DisplayPlot`:

```haskell
import qualified Data.Vector as V
let spec     = layer (scatter (ColByName "x") (ColByName "y"))
    resolver "x" = Just (NumData (V.fromList [0,1,2,3,4]))
    resolver "y" = Just (NumData (V.fromList [3,1,4,1,5]))
    resolver _   = Nothing
DisplayPlot (resolver, spec)
```

- `DisplayPlot` is **provisional** until DataFrame integration (spec-2's `BoundPlot`) is complete.
- Working notebook = `design/ihaskell/demo.ipynb`. Run `cabal run ihaskell-demo-svg` to write cells' plots as SVG (same rendering path).
- Tested build: GHC 9.6.7 with `ihaskell-0.13.0.0`. Register these packages in your kernel environment.

## Next Steps

- API Reference (layers, marks, decoration, backends, dataframe, analyze, 3D) → **[api-guide/](./api-guide/README.md)**
- What's drawable checklist → **[modules.md](./modules.md)**
- Migrating from matplotlib / ggplot → **[comparison.md](./comparison.md)**

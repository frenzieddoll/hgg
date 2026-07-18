# Quickstart — 30 seconds to a plot + 3 API layers

> 🌐 **English** | [日本語](01-quickstart.ja.md)

> [📚 Index](README.md) | **01 quickstart** | [02 layers](02-layers.md) | [03 encoding & scale](03-encoding-scale.md) | [04 decoration](04-decoration.md) | [05 backends](05-backends.md) | [06 dataframe](06-dataframe.md) | [07 analyze](07-analyze.md) | [08 3d](08-3d.md) | [09 appendix](09-appendix.md)

Fastest path to one plot in hgg, plus the **3 API layers** (Easy / Grammar / DataFrame). 
Settings reference: [02 layers](02-layers.md) / [03 encoding & scale](03-encoding-scale.md) / [04 decoration](04-decoration.md).
Backend choice: [05 backends](05-backends.md).
DataFrame column-name binding: [06 dataframe](06-dataframe.md).
Regression/GLM/HBM plotting: [07 analyze](07-analyze.md) is your dictionary.

Page structure:
**[30 seconds (Quick layer)](#quickstart-30s)** | **[Easy layer](#easy)** | **[Grammar layer](#grammar)**

> **Two golden rules (memorize these)**
>
> 1. Plots follow **`purePlot <> layer (mark …) <> settings …`**. Add components with `<>`.
>    (`mark` = plot type like `scatter`/`line`/`bar`…; type is `MarkKind`).
> 2. `<>` has **two kinds**: marks/appearance return **`Layer`** and compose **inside** `layer (…)`,
>    titles/theme/facet return **`VisualSpec`** and compose **outside**.
>    → Type tells you placement ([layering rules](04-decoration.md#overlay) explained later).

| Layer | Module | Role |
|---|---|---|
| **0. Quick** | `Hgg.Plot.Quick` | `IO` one-shot. `quickScatter "out.svg" xs ys` |
| **1. Easy** | `Hgg.Plot.Easy` | `[Double]` direct pass + `overlay` default |
| **2. Grammar** | `Hgg.Plot.Spec` | ggplot-like channels + `<>` composition (primary) |
| **3. Typed** | `Hgg.Plot.Spec` + `Resolver` | Typed channels / scales / Resolver-controlled encoding |
| **4. Low-level** | `Hgg.Plot.Render` | Direct `Primitive` (custom backends, special rendering) |

> `Easy` re-exports `Spec` + value-passing helpers. `Quick` re-exports `Easy` + `IO` save helpers.
> **Any import gives you all lower-layer features**.

---

## 30 seconds to a plot (Quick layer) {#quickstart-30s}

One plot, data only, zero configuration. Uses `hgg-svg`'s `Hgg.Plot.Quick`.

```haskell
import Hgg.Plot.Quick

main :: IO ()
main = do
  quickScatter "scatter.svg" [1,2,3,4,5] [1,4,9,16,25]
  quickLine    "line.svg"    [1,2,3,4,5] [2,3,1,5,4]
  quickBar     "bar.svg"     [1,2,3]     [10,20,15]
  quickHist    "hist.svg"    [1,1,2,3,3,3,4,5,5]
```

`quickScatter / quickLine / quickBar :: FilePath -> [Double] -> [Double] -> IO ()`,
`quickHist :: FilePath -> [Double] -> IO ()`. Multiple geoms on one plot:
`quickPlot :: FilePath -> [Layer] -> IO ()`.

---

## Easy layer — pass values directly {#easy}

Pass `[Double]` without `inline`, wrap overlays in `overlay`, add decoration with `<>`.

```haskell
import Hgg.Plot.Easy
import Hgg.Plot.Backend.SVG (saveSVG)
import Hgg.Plot.Unit (px, (*~))   -- px sizing (default unit mm, omit for 6.5×4in)

main :: IO ()
main = saveSVG "easy.svg" $
     overlay [ points [1,2,3,4,5] [1,4,9,16,25] ]
  <> title "y = x²" <> xLabel "x" <> yLabel "y"
  <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
```

![Easy layer output](images/lesson1-easy.svg)

Value-passing helpers: `points` / `lineXY` / `bars` / `hist` / `plotY` (index as x).
Overlay: `overlay [layer list]` (alias `plots`).
→ Live example: `cabal run tutorial-01-easy`

---

## Grammar — write ggplot-style {#grammar}

Start with `purePlot` (empty plot), add `layer (mark …)`, build channels with `inline` (numeric) /
`inlineCat` (categorical). Aesthetics compose **inside** the mark with `<>`.

```haskell
import Hgg.Plot.Spec
import Hgg.Plot.Backend.SVG (saveSVG)
import Data.Text (Text)

main :: IO ()
main = do
  let xs = inline    [1,2,3,4, 1,2,3,4]
      ys = inline    [2,3,1,4, 3,1,4,2]
      gs = inlineCat (concatMap (replicate 4) (["alpha","beta"] :: [Text]))
  saveSVG "grammar.svg" $
       purePlot
    <> layer (scatter xs ys <> colorBy gs <> size 6)   -- ← aesthetics inside layer
    <> scaleColorManual [("alpha","#1B9E77"), ("beta","#D95F02")]  -- ← scale outside
    <> legend
    <> title "scale_color_manual"
```

![Grammar output](images/lesson2-grammar.svg)

Equivalent to ggplot2: `ggplot(d, aes(x,y,color=g)) + geom_point(size=6) +
scale_color_manual(...)`.
→ Live example: `cabal run tutorial-02-grammar`

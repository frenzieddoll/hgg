# 10 custom marks — Add your own marks

> 🌐 **English** | [日本語](10-custom-marks.ja.md)

For plot types not supported by built-in marks (`scatter` / `line` / `bar` / `box` / …), **custom mark** (`customMark`) is the extension point to define your own **without editing the library core** (`MarkKind` enum).
Reading this page alone, you can build new marks from start to finish.

> **When to use**: When you want to draw plots not supported by existing marks (dendrogram, custom annotations, specialized diagrams, etc.).
> Serious marks needing deep integration with scale / legend / color require core additions ([09 appendix](09-appendix.md) library extension),
> but types like "just place lines and text yourself" benefit most from custom marks. Same philosophy as ggplot's "Extending ggplot2" and matplotlib's Artist helpers (`scipy…dendrogram` etc.).

## 1. 30-second version — Minimal custom mark

`customMark id drawFn` returns `Layer`. `drawFn :: RenderCtx -> [Primitive]` is the core —
"receive context (`RenderCtx`), return list of shapes (`Primitive`)."

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Hgg.Easy
import Graphics.Hgg.Primitive (Primitive(..), Point(..), solid)
import Graphics.Hgg.Spec      (RenderCtx(..), customMark)

-- Just draw one diagonal from bottom-left (0,0) to top-right (1,1)
diagonal :: RenderCtx -> [Primitive]
diagonal ctx =
  let p x y = uncurry Point (rcProjectXY ctx x y)   -- data coords → screen px
  in [ PLine (p 0 0) (p 1 1) (solid (rcColor ctx) 1.5) ]

main :: IO ()
main = saveSVG "diag.svg" $
     purePlot
  <> layer (scatter (inline [0,1]) (inline [0,1]) <> alpha 0.0)  -- Reserve axis range (see below)
  <> layer (customMark "diagonal" diagonal)                       -- ★ no core edits
  <> title "my first custom mark"
```

Three key points:

1. **`rcProjectXY ctx x y`** converts data coords to screen px (auto-follows scale, log, polar, flip).
2. `Primitive` constructors (`PLine` etc.) create shapes.
3. Put `customMark "id" drawFn` in `layer (…)` and compose with other layers via `<>`.

## 2. RenderCtx — Context passed to draw function

The only argument `drawFn` receives. Everything needed for drawing comes from here.

| Field | Type | Purpose |
|---|---|---|
| `rcProjectXY` | `Double -> Double -> (Double, Double)` | **Data coords (x,y) → device px**. Follows axis scale / coordinate system. Nearly always used |
| `rcPlotArea` | `Rect` | Plot drawing area (px). `Rect {rX,rY,rW,rH}`. For clipping or area-relative placement |
| `rcResolver` | `Resolver` | Column name → data. Fetch columns bound to layer (§5) |
| `rcColor` | `Text` | Theme default line / point color |
| `rcFill` | `Text` | Theme default fill color |
| `rcTextColor` | `Text` | Theme default text color |
| `rcAxisColor` | `Text` | Theme default axis color |

`RenderCtx(..)` imported from `Graphics.Hgg.Spec`.

## 3. Primitive — Drawing shape vocabulary

Backend-agnostic drawing commands returned by `drawFn`. Import from `Graphics.Hgg.Primitive` (or re-export from `Graphics.Hgg.Render`).
All coordinates are **device px** (= `rcProjectXY` output).

| Constructor | Arguments | Meaning |
|---|---|---|
| `PLine` | `Point Point LineStyle` | Line segment |
| `PPath` | `[PathSegment] FillStyle (Maybe StrokeStyle)` | Polyline / curve / polygon (`MoveTo`/`LineTo`/`CurveTo`/`ClosePath`) |
| `PRect` | `Rect FillStyle (Maybe StrokeStyle)` | Rectangle |
| `PCircle` | `Point Double FillStyle (Maybe StrokeStyle) (Maybe Text)` | Circle (last is hover label, `Nothing` if not needed) |
| `PText` | `Point Text TextStyle` | Text |

Style constructors:

```haskell
solid :: Text -> Double -> LineStyle          -- Solid line: solid "#333" 1.5
FillStyle   { fsColor :: Text, fsOpacity :: Double }
StrokeStyle { ssColor :: Text, ssWidth   :: Double }
TextStyle   { tsColor, tsSize, tsFamily, tsAnchor, tsRotate, tsWeight, tsItalic }
Point x y                                     -- Point in device px
```

`TextStyle` example: `TextStyle (rcTextColor ctx) 10 "sans-serif" AnchorMiddle 0 "normal" False`
(`AnchorMiddle` / `AnchorStart` / `AnchorEnd`, `tsRotate` in degrees CCW).

## 4. Step-by-step — Build dendrogram as a "normal mark"

Build a simple dendrogram from two columns: `"leaf"` (x position of leaves) and `"height"` (node height).
Draw merges as Π-shaped elbows. Final form: **`dendrogram "leaf" "height"`** used like `scatter x y`.
Complete example at `hgg-svg/examples/CustomMarkDemo.hs` (`cabal run custom-mark-demo`).

```haskell
import Graphics.Hgg.Easy
import Graphics.Hgg.Primitive (Primitive(..), Point(..), TextStyle(..), TextAnchor(..), solid)
import Graphics.Hgg.Spec (RenderCtx(..), ColRef, ColData(..), customMark, encX, encY, resolveNum)
import qualified Data.Vector as V

-- ① draw: Read "leaf"/"height" columns via rcResolver, build elbows (4-leaf fixed for simplicity)
dendroDraw :: RenderCtx -> [Primitive]
dendroDraw ctx =
  let num nm = maybe [] V.toList (resolveNum (rcResolver ctx) nm)  -- Read column
      p x y  = uncurry Point (rcProjectXY ctx x y)                 -- data→px
      ls     = solid (rcColor ctx) 1.5
      -- Π-shaped elbow merging children at height hy (from base heights by1/by2)
      elbow cx1 by1 cx2 by2 hy =
        [ PLine (p cx1 by1) (p cx1 hy) ls
        , PLine (p cx2 by2) (p cx2 hy) ls
        , PLine (p cx1 hy)  (p cx2 hy) ls ]
      ts = TextStyle (rcTextColor ctx) 10 "sans-serif" AnchorMiddle 0 "normal" False
  in case (num "leaf", num "height") of
       ([x0,x1,x2,x3], [_,h1,h2]) ->               -- leaf=[0,1,2,3], height=[0,1,2]
         let m01 = (x0+x1)/2; m23 = (x2+x3)/2
         in concat
              [ elbow x0 0 x1 0 h1                  -- Leaves 0,1 → height h1
              , elbow x2 0 x3 0 h1                  -- Leaves 2,3 → height h1
              , elbow m01 h1 m23 h1 h2              -- (0,1) and (2,3) → height h2 = root
              , [ PText (p x (-0.12)) nm ts
                | (x, nm) <- zip [x0,x1,x2,x3] ["A","B","C","D"] ] ]
       _ -> []

-- ② Named combinator = first-class mark. Bundle x/y columns, auto-compute axis range.
dendrogram :: ColRef -> ColRef -> Layer
dendrogram x y = customMark "dendrogram" dendroDraw <> encX x <> encY y

-- ③ Usage side = like scatter x y. Columns supplied via resolver (here: dat) / df |>>.
main :: IO ()
main = saveSVGWith "dendro.svg" dat $
     purePlot
  <> layer (dendrogram "leaf" "height")
  <> xLabel "leaf" <> yLabel "merge height"
  where dat "leaf"   = Just (NumData (V.fromList [0,1,2,3]))  -- Leaf x positions
        dat "height" = Just (NumData (V.fromList [0,1,2]))    -- Node heights (leaf baseline 0 / 1 level / root)
        dat _        = Nothing
```

Key points:

- **`dendrogram "leaf" "height"` has same shape as `scatter x y`**. Inside: `customMark … <> encX <> encY`.
- Draw reads columns via **`rcResolver`** (§5). Data flows from `saveSVGWith` resolver or `df |>>`.
- **`encX`/`encY` auto-compute axis range** (§4.1 below). Without them, default `[0,1]` puts shapes outside frame.

### 4.1 How first-class marks work — `encX` / `encY`

`encX` / `encY` are **mark-independent x/y column bundlers** (`Graphics.Hgg.Spec`):

```haskell
encX :: ColRef -> Layer          -- Bundle x column only
encY :: ColRef -> Layer          -- Bundle y column only
```

Composing `customMark id draw <> encX x <> encY y`, axis range auto-computes from `lyEncX` / `lyEncY` (`RangeOf` scans),
data flows via `df |>>` (§5). Each custom mark has its own `draw`, so like `dendrogram` above,
**write mark-specific wrapper per mark** (no need for generic bundler helpers).

When `encX` / `encY` bundled, **axis range auto-computes** (`RangeOf` scans `lyEncX` / `lyEncY`),
data flows via `df |>>` (§5). Each custom mark has unique `draw`, so naturally
**write per-mark wrapper** (no generic bundle helper needed).

## 5. Two ways to feed data

- **Closures** (example above): Write leaf coords directly in draw function. Haskell strength. Shortest.
- **From columns**: Read column bound to layer via `rcResolver`. Data-driven on screen.

```haskell
import Graphics.Hgg.Spec (resolveNum)

fromCols :: RenderCtx -> [Primitive]
fromCols ctx =
  case (resolveNum (rcResolver ctx) "x", resolveNum (rcResolver ctx) "y") of
    (Just xs, Just ys) ->
      [ PCircle (uncurry Point (rcProjectXY ctx x y)) 3
                (FillStyle (rcColor ctx) 1) Nothing Nothing
      | (x, y) <- zip (V.toList xs) (V.toList ys) ]
    _ -> []
-- Pass columns via resolver:
--   saveSVGWith "out.svg" myResolver (… customMark "c" fromCols …)
-- DataFrame: df |>> supplies resolver directly (combine with §4.1 first-class mark):
--   saveSVGBound "out.svg" (df |>> (purePlot <> layer (dendrogram "leaf" "height")))
```

`df |>>` bundles df's column resolver (`dfResolver df`) into the plot. When custom mark's `draw` reads the same column names
via `rcResolver`, DataFrame data flows with no extra wiring.

## 6. Also draw on canvas (PureScript) — Parity

Custom mark closures are **Haskell-only** (functions can't JSON-serialize). Serializing plot to JSON for canvas (PureScript)
sends only `id` and options; closure drops. To draw same mark on canvas too, **hand-register draw function in PureScript registry** with same `id`
(current HS↔PS hand-mirroring practice). Unregistered → canvas **skips** (= Haskell-only = SVG / PDF / Rasterific only).

```purescript
import Graphics.Hgg.Custom (registerMark)          -- RenderCtx also from here
import Graphics.Hgg.Render.Common (Primitive(..), Point(..))

-- Hand-write to return same shapes as HS drawFn
dendroDrawPS :: Json -> RenderCtx -> Array Primitive
dendroDrawPS _opts ctx =
  let p x y = case ctx.projectXY x y of { r -> Point r.x r.y }  -- projection returns {x,y}
  in [ PLine (p 0.0 0.0) (p 0.0 1.0) (solid ctx.color 1.5), … ]

main = do
  registerMark "dendrogram" dendroDrawPS   -- Once at app startup
  …
```

PureScript `RenderCtx` has same fields as HS (`projectXY` / `plotArea` / `resolver` / `color` / `fill` /
`textColor` / `axisColor`). Only difference: **projection returns `{ x, y }` record** (PS convention).
Options (`customMarkWith` JSON) become draw function's first arg `Json`.

## 7. Promote to core

When well-designed and "you want it built-in," promotion is designed to cost nothing:

- HS: Place draw function in library module, swap `customMark "id"` call with that library function
  (public API, `id` unchanged, so user code stays compatible).
- PS: Call `registerMark "id" drawPS` in library initialization.

Add constructor + renderer to core `MarkKind` only if deep integration with scale / legend / color is needed (traditional path).
Custom marks are a lightweight channel for types not needing that.

## 8. API quick reference

```haskell
-- Graphics.Hgg.Spec (Easy also OK)
customMark     :: Text -> (RenderCtx -> [Primitive]) -> Layer
customMarkWith :: Text -> Value -> (RenderCtx -> [Primitive]) -> Layer   -- With options for PS
encX           :: ColRef -> Layer   -- Bundle x column (mark-independent, first-class mark / auto axis range)
encY           :: ColRef -> Layer   -- Bundle y column

data RenderCtx = RenderCtx
  { rcProjectXY :: Double -> Double -> (Double, Double)
  , rcPlotArea  :: Rect
  , rcResolver  :: Resolver
  , rcColor, rcFill, rcTextColor, rcAxisColor :: Text }
```

```purescript
-- Graphics.Hgg.Custom (PureScript)
registerMark :: String -> (Json -> RenderCtx -> Array Primitive) -> Effect Unit
type RenderCtx = { projectXY :: Number -> Number -> { x :: Number, y :: Number }
                 , plotArea :: Rect, resolver :: Resolver
                 , color, fill, textColor, axisColor :: String }
-- PureScript also has isomorphic customMark / customMarkWith in Graphics.Hgg.Spec
```

## Related

- Minimal example: `hgg-svg/examples/CustomMarkDemo.hs` (`cabal run custom-mark-demo`).
- Design rationale (why closures, why Primitive as leaf): `specification/phases/phase-51-custom-mark-extension.md`.
- References: ggplot "Extending ggplot2" / matplotlib Artist / scipy dendrogram (helper function style).
- Adding serious marks to core: [09 appendix](09-appendix.md) library extension.

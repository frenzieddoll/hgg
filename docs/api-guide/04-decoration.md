# Decoration — Labels / theme / facet / subplot / coordinates / reference lines / overlay

> 🌐 **English** | [日本語](04-decoration.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 layers](02-layers.md) | [03 encoding & scale](03-encoding-scale.md) | **04 decoration** | [05 backends](05-backends.md) | [06 dataframe](06-dataframe.md) | [07 analyze](07-analyze.md) | [08 3d](08-3d.md) | [09 appendix](09-appendix.md)

Figure-wide settings (all are `VisualSpec` · `<>` **outside** `purePlot <> … <> this`) are organized by topic. Mark appearance and channels are in [03 encoding & scale](03-encoding-scale.md#encoding); color and size scale and axis control are also there.

Structure of this page (topic index):
**[Title & Labels](#labels)** | **[theme](#theme)** |
**[facet](#facet)** | **[subplot](#subplots)** | **[Coordinates](#coord)** |
**[Legend, reference lines, helpers](#guides)** | **[Enum values quick reference](#enum-tables)** |
**[Layering correctly](#overlay)** | **[Advanced figures](#advanced-layering)**

> Each topic follows the pattern: **setting function table (type · meaning)** → **example (code + figure)**. All settings with fixed values (`position` / `theme` / `legendPos` etc.) are consolidated in [Enum values quick reference](#enum-tables).

## Title & Labels {#labels}

| Setting | Type (what to pass) | Meaning |
|---|---|---|
| `title` / `subtitle` / `caption` / `tag` | `Text -> VisualSpec` | Title / subtitle / caption / tag |
| `xLabel` / `yLabel` | `Text -> VisualSpec` | Axis labels |
| `legendTitle` | `Text -> VisualSpec` | Legend title |
| `labs` | `Labs -> VisualSpec` | Batch-specify labels (`emptyLabs { labsTitle = Just …, … }`) |
| `width` / `height` | `Length -> VisualSpec` | Figure size. Bare literals are **pt** (`width 600` = 600pt · `Num Length` default unit) |
| `widthMm` / `heightMm` | `Double -> VisualSpec` | Specify figure size in **mm** (`widthMm 180` = 180mm) |
| `widthUnit` / `heightUnit` | `Length -> VisualSpec` | Explicit unit. `widthUnit (7 *~ inch)` / `widthUnit (800 *~ px)` |
| `dpi` | `Double -> VisualSpec` | DPI for raster output (default 96). px=pt×dpi/72. PDF ignores this |
| `aspectRatio` | `Double -> VisualSpec` | Aspect ratio |

> **pt unit system**: Figure size is laid out in physical pt space; DPI is applied once at output boundary to convert to device px. Bare literals for `width`/`height` are **pt** (`Num Length`'s default `fromInteger` unit). For mm, use `widthMm`/`heightMm`. Default unspecified figure is **6.5×4in (468×288pt · landscape)**. For px-based legacy output, use `widthUnit (N *~ px)`. SVG is vector, so crisp at FHD+/HiDPI (independent of px count).

Example (batch `labs`):

```haskell
purePlot <> layer (scatter xs ys <> size 6)
  <> labs (emptyLabs { labsTitle = Just "title", labsSubtitle = Just "subtitle"
                     , labsCaption = Just "caption", labsX = Just "x axis", labsY = Just "y axis" })
```

![3c labs](images/s3c-labs.svg)

## theme — `theme :: ThemeName -> VisualSpec` {#theme}

```haskell
<> theme ThemeMinimal
```

**13 choices** for `ThemeName`. Gallery showing the same scatter in each theme:

| Theme | Appearance |
|---|---|
| `ThemeDefault` / `ThemeMinimal` | Default / minimal frameless |
| `ThemeDark` / `ThemeLight` | Dark / light |
| `ThemeGrey` / `ThemeBW` | Grey panel / black & white |
| `ThemeClassic` / `ThemeVoid` / `ThemeLinedraw` | Axis lines only / frameless / fine lines |
| `ThemeNoir` / `ThemeLumen` | Brand dark / light |
| `ThemeCanvas` / `ThemeCanvasDark` | Parchment (light / dark) |

![theme gallery (13 representative themes)](images/s3e-theme-gallery.svg)

> **theme and series colors (palette) are independent**: `theme` sets the **overall appearance** — panel background, grid, axis lines, text color. Series colors for color-coding groups with `colorBy` are determined by [palette / scaleColorManual](03-encoding-scale.md#scale) and combine with theme on a separate axis (any palette can overlay any theme). Extract default series colors for a theme with `themeSeriesPalette :: ThemeName -> [Text]` ([03 encoding & scale](03-encoding-scale.md#scale)).

```haskell
-- Apply one theme
purePlot <> layer (scatter xs ys <> color (fromHex "#38bdf8") <> size 6) <> theme ThemeDark
```

![3e theme ThemeDark](images/s3e-theme.svg)

> **Default is ggplot-aligned**: Plot title is **left-aligned** (all themes), legend is **right · vertically centered** ([Legend section](#guides)). `ThemeGrey` matches ggplot `theme_grey()` down to grey panel + white grid + tick grey20 + black title + legend key grey95. All are customizable via element override below.

### Element-level overrides

After `theme` preset, override individual elements with `<>` (ggplot `theme(...)` equivalent). All return `VisualSpec`:

| Setting | Type (what to pass) | Meaning |
|---|---|---|
| `themeGrid` | `Bool -> VisualSpec` | Grid lines on/off |
| `gridColor` / `panelFill` / `plotBg` | `Text -> VisualSpec` | Grid color / panel background / overall background (color hex) |
| `axisColor` / `textColor` / `stripFill` | `Text -> VisualSpec` | Axis line color / text color / strip background (color hex) |
| `themeAxisLine` / `panelBorder` / `themeStrip` | `Bool -> VisualSpec` | Axis lines (bottom/left) / plot frame / facet strip on/off |
| `themeAxisTextAngle` | `Double -> VisualSpec` | Tick label rotation (degrees) |
| `titleHjust` | `Double -> VisualSpec` | Plot title horizontal alignment (`0`=left [default] · `0.5`=center · `1`=right) |
| `titleColor` / `tickColor` / `legendKeyBg` | `Text -> VisualSpec` | Title text color / axis tick mark color / legend key background (color hex; `""` for no fill) |
| `titleFont` / `axisLabelFont` / `tickFont` / `legendFont` | `FontSpec -> VisualSpec` | Font for each text (compose with combinator below) |

**Fonts** are composed with combinators (`fontSize`/`fontFamily`/`fontWeight`/`fontItalic`/`fontColor`) returning `FontSpec` and combined with `<>` to pass to `titleFont` etc. (empty default is `emptyFontSpec`):

```haskell
-- Override elements on ThemeMinimal base: bold 18px title, grey 10px ticks, frame, grid off, 45° ticks
purePlot <> layer (scatter xs ys <> size 6)
  <> theme ThemeMinimal
  <> titleFont (fontSize 18 <> fontWeight "bold")   -- compose FontSpec with <>
  <> tickFont  (fontSize 10 <> fontColor "#64748b")
  <> panelBorder True
  <> themeGrid False
  <> themeAxisTextAngle 45
```

![Element-level theme override (bold title, frame, grid off, 45° ticks)](images/s3e-theme-override.svg)

For facet figures ([facet](#facet)), the strip (header bar) is also customizable:

```haskell
purePlot <> layer (scatter "x" "y" <> colorBy "g") <> facet "g"
  <> themeStrip True <> stripFill "#eef2ff"          -- set strip to light blue background
```

![facet strip background (stripFill)](images/s3e-theme-strip.svg)

→ Working example: `cabal run tutorial-05-theme`

> Each font setter has a `ThemeOverride` equivalent via `theme*Font` (`themeTitleFont`/`themeAxisLabelFont`/`themeTickFont`/`themeLegendFont`). In rendering, override (`theme*Font`) takes priority, but **layout character height is only affected by `titleFont` series**, so prefer `titleFont` for standalone use.

### theme and subplot relationship

When combined with [subplot](#subplots), **placing `theme` outside (outside subplots) propagates to all panels** (each panel is rendered with that theme). To use different themes per panel, add `theme` individually inside each panel's `VisualSpec` (the [theme gallery](#theme) above is an example of this "per-panel theme").

```haskell
-- Outer theme: all panels are ThemeDark
subplots [ layer (scatter "x" "y") <> title "scatter"
         , layer (bar "g" "y")     <> title "bar" ]
  <> subplotCols 2 <> theme ThemeDark
```

![theme × subplots: outer theme propagates to all panels](images/s3e-theme-subplots.svg)

## facet (partitioning) {#facet}

Partition data by one column value and arrange in multiple panels (ggplot `facet_*` equivalent).
**All options**:

```haskell
<> facet "g"                      -- Simple partition by column g (1 row N cols)
<> facetWrap "g" 3                -- Partition by g and wrap at 3 cols
<> facetCols 3                    -- Specify column count only (use with facet)
<> facetGrid "row" "col"          -- 2D cross-layout (row × col)
<> facetScales FacetFreeY         -- Free axes per panel (FacetFixed[default]/FacetFreeX/FacetFreeY/FacetFree)
<> facetSpace SpaceFree           -- Proportional panel sizes to data range for free axes (facetGrid only)
```

| Function | Type (what to pass) | Role (ggplot equivalent) |
|---|---|---|
| `facet` | `ColRef -> VisualSpec` | Simple partition by column (`facet_wrap(~g)`) |
| `facetWrap` | `ColRef -> Int -> VisualSpec` | Partition by column, wrap at n cols (`facet_wrap(~g, ncol=n)`) |
| `facetCols` | `Int -> VisualSpec` | Column count only · use with `facet` (`ncol=n`) |
| `facetGrid` | `ColRef -> ColRef -> VisualSpec` | 2D r × c layout (`facet_grid(r ~ c)`) |
| `facetScales` | `FacetScales -> VisualSpec` | Axis sharing mode (`scales="free_y"` etc. · enum below) |
| `facetSpace` | `FacetSpace -> VisualSpec` | Panel size allocation · grid only (`space="free"`) |

> `FacetScales` = `FacetFixed` / `FacetFreeX` / `FacetFreeY` / `FacetFree`.
> `FacetSpace` = `SpaceFixed` / `SpaceFreeX` / `SpaceFreeY` / `SpaceFree`.
> To arrange completely separate specs in panels (independent figures, not facet), use [subplot](#subplots).

Example (`facetWrap "g" 2`). Facet columns are name references, so supply `"g"` via `Resolver` (or DataFrame):

```haskell
-- r is a Resolver returning "x"/"y"/"g"
saveSVGWith "out.svg" r $
  purePlot <> layer (scatter "x" "y" <> colorBy "g" <> size 6) <> facetWrap "g" 2
```

![3f facetWrap](images/s3f-facet.svg)

## subplot (independent figure composition) {#subplots}

Where `facet` **partitions data by one column**, `subplots` **composes completely separate `VisualSpec`s** (not in ggplot; like matplotlib `subplots` / patchwork). Marks and axes differ per figure.

```haskell
<> subplots [ spec1, spec2, spec3 ]   -- Compose list of independent figures
<> subplotCols 2                       -- Wrap at 2 cols (default is 1 row N cols)
```

| Function | Type (what to pass) | Role |
|---|---|---|
| `subplots` | `[VisualSpec] -> VisualSpec` | Arrange each `VisualSpec` as independent panel |
| `subplotCols` | `Int -> VisualSpec` | Wrap column count |
| `selectPanels` | `[Text] -> VisualSpec` | Select + reorder panels by title name |
| `repeatFields` | `[Text] -> (Text -> VisualSpec) -> VisualSpec` | Iterate field names and generate views (Vega-Lite `repeat`) |
| `hconcat` / `vconcat` | `[VisualSpec] -> VisualSpec` | Horizontal / vertical composition (operators `<->` / `<:>` too) |

**Field iteration (`repeatFields`)**: To apply the same plot template to multiple fields without manually arranging in `subplots`, use `repeatFields` (Vega-Lite `repeat` equivalent · explicit form). Field name is passed to generator function, so each view uses different columns:

```haskell
<> repeatFields ["height", "weight", "age"] (\f -> layer (hist f) <> title f)
<> subplotCols 3                                  -- Arrange in 3 cols
```

**Panel name selection (`selectPanels`)**: Inverse of `repeatFields`: from completed panel list, **select subset by name** (= each panel's `title`). Enumeration order becomes display order (selection + reordering combined). Use for multi-parameter diagnostic grids (e.g., HBM trace from analyze integration) showing only parameters of interest:

```haskell
<> subplots panels <> selectPanels ["b1_0", "b1_1", "sigma"] <> subplotCols 1
-- Unmatched title names are ignored. Unspecified uses all panels as before.
```

![3f-2 selectPanels: select c, a from 4 panels](images/s3f2-select-panels.svg)

Each panel is complete figure, so `title` / `theme` / marks are combined per panel with `<>`:

```haskell
saveSVG "dash.svg" $
  subplots [ layer (scatter "x" "y") <> title "scatter"
           , layer (line    "x" "y") <> title "line"
           , layer (bar     "g" "y") <> title "bar" ]
  <> subplotCols 3 <> title "dashboard"
```

![3f-2 subplots: compose independent figures](images/s3f2-subplot.svg)

**Nesting (nested subplots)**: When panel content itself contains `subplots`, creates nested grid. Asymmetric layouts like one main figure left, small figures 2-high right:

```haskell
subplots [ layer (scatter "x" "y") <> title "main"
         , subplots [ layer (histogram "x") <> title "x distribution"
                    , layer (histogram "y") <> title "y distribution" ] <> subplotCols 1 ]
<> subplotCols 2
```

![3f-2 nested subplots: main figure + nested marginal distribution](images/s3f2-nested.svg)

> Working nested dashboard example with HBM diagnostics in one frame is at [HBM plotting with analyze integration](07-analyze.md#hbm-plotting).

**Concat composition (`hconcat` / `vconcat` + operators)**: Thin wrappers around `subplots` + `subplotCols` providing Vega-Lite `hconcat`/`vconcat` equivalent and patchwork-style infix operators.

| Function / Operator | Role |
|---|---|
| `hconcat [a, b, c]` | Horizontal (1 row n cols · `subplots ss <> subplotCols (length ss)`) |
| `vconcat [a, b]` | Vertical (n rows 1 col · `subplots ss <> subplotCols 1`) |
| `a <-> b` | Horizontal merge operator (`infixl 6`) |
| `a <:> b` | Vertical merge operator (`infixl 6` · same precedence as `<->`) |

Operators **flatten same-direction chains**. `a <-> b <-> c` becomes 3-equal-width columns (binary nesting doesn't split left cell into `a,b`); mixing directions nests. For example, **row 1 with 3 cols, row 2 full-width** (3× row 1 cell width) in one line:

```haskell
saveSVG "concat.svg" $
  (a <-> b <-> c) <:> d          -- = vconcat [hconcat [a, b, c], d]
```

![3f-2 concat: (a <-> b <-> c) <:> d](images/concat.svg)

> **Alignment (unified grid)**: Compositions with nesting or spans internally flatten to **single unified grid**, assigning each panel `(row, rowspan, col, colspan)`. This means **panel edges align across rows** — in the example above, row 2 full-width `d` left edge aligns with row 1 left `a` (col0), and `d` spans 3 columns for full width. Nested subplots also expand to fill outer grid cell.

> **Advanced helpers (normally not needed)**: `selectedSubplots :: VisualSpec -> [VisualSpec]` extracts panels after `selectPanels`. `bakeSpec :: Resolver -> VisualSpec -> VisualSpec` bakes Resolver into spec (internal for subplot / HBM extractors). `applyDiscreteLimits` resolves discrete limits. `freeScaleX`/`freeScaleY` (`FacetScales -> Bool`) and `freeSpaceX`/`freeSpaceY` (`FacetSpace -> Bool`) predicates for facet are for testing `facetScales` ([facet](#facet)).

## Coordinates {#coord}

```haskell
<> coordFlip          -- x↔y flip (horizontal bar etc.)
<> coordPolar         -- polar coordinates (x = angle)
<> coordPolarY        -- polar coordinates (y = angle)
<> reverseX           -- reverse x axis
<> reverseY           -- reverse y axis (y version of reverseX)
<> coordCartesianX lo hi   -- zoom display range x-only (out-of-range data kept)
<> coordCartesianY lo hi   -- zoom display range y-only
<> coordCartesian x0 x1 y0 y1   -- specify all 4 sides
```

> **Types**: `coordFlip` / `coordPolar` / `coordPolarY` / `reverseX` / `reverseY` are `VisualSpec` (no arguments).
> `coordCartesianX` / `coordCartesianY :: Double -> Double -> VisualSpec`,
> `coordCartesian :: Double -> Double -> Double -> Double -> VisualSpec` (x0 x1 y0 y1).
> `coordCartesian*` **changes only visible range, keeps data** (ggplot `coord_cartesian(xlim=)` equivalent).
> Different from `axisRange` which drops out-of-range rows (see [Helpers](#guides) below).

Example (`coordFlip` for horizontal bar):

```haskell
purePlot <> layer (bar (inlineCat ["A","B","C"]) (inline [3,7,5])) <> coordFlip
```

![3g coordFlip](images/s3g-coord.svg)

## Legend, reference lines, helpers {#guides}

```haskell
<> legend                         -- Legend ON
<> legendOff                      -- OFF
<> legendPos LegendBottom         -- Position (Right/Bottom/None/Inside*)
<> legendNcol 2                   -- Legend in 2 cols (legendNrow 1 for rows)
<> legendReverse                  -- Reverse legend order
<> guideColorNone                 -- Hide color legend only
<> refIdentity                    -- y=x line
<> refHorizontal 0                -- Horizontal line y=0
<> refVertical 1.0                -- Vertical line x=1
<> refLine (RefLinear 2 1)        -- Arbitrary y = 2x + 1
<> marginalX                      -- x marginal histogram (marginalY / marginal too)
```

> **Types**: `legend` / `legendOff` / `legendReverse` / `guideColorNone` / `refIdentity` / `marginalX` / `marginalY` / `marginal` are `VisualSpec` (no arguments).
> `legendPos :: LegendPosition -> VisualSpec` ([enum](#enum-tables)) / `legendNcol` / `legendNrow :: Int -> VisualSpec` / `refHorizontal` / `refVertical :: Double -> VisualSpec` / `refLine :: ReferenceLine -> VisualSpec`.

> **Default legend position is `LegendRightCenter`** (panel right · vertically centered = ggplot `legend.position="right"` equivalent). For top alignment use `legendPos LegendRight`, bottom use `LegendBottom`.

Example (`refHorizontal` + `refVertical` + `legend`):

```haskell
purePlot <> layer (scatter xs ys <> colorBy gs <> size 6)
  <> refHorizontal 2.5 <> refVertical 2.5 <> legend
```

![3h refHorizontal / refVertical + legend](images/s3h-guides.svg)

> **Legend width is automatic (content-based)**: Right (`LegendRight`) / bottom (`LegendBottom`) legend widths follow **longest label** automatically (not fixed reservation). Short labels (`x` / `y` etc.) tighten right margin and expand plot area; long labels and full-width characters don't overflow. Width estimation uses per-script advance approximation (full-width = 1.0em · uppercase ≈ 0.70em · thin `i`/`l` ≈ 0.30em etc.), unified across backends (SVG / PNG / PDF / Canvas identical).

### Annotations, insets, marginal distribution

`annotText` / `annotArrow` / `annotRect` / `annotLine` are **added directly as `VisualSpec`** with `<>` (lower-level `annotate` taking `Annotation` also exists). Insets use `inset` / `insetAt` / `insetElement`; marginals use `marginalX` / `marginalY`:

```haskell
purePlot <> layer (scatter "x" "y")
  <> annotText 2.0 5.0 "outlier"                      -- Text at (x,y)
  <> annotArrow 1.5 4.5 2.0 5.0                       -- Arrow (x0,y0)→(x1,y1)
  <> annotRect 0 0 1 1 "region A"                     -- Rectangle + label
  <> marginalX                                        -- x marginal histogram
  <> insetAt 0.7 0.7 0.25 0.25 (layer (histogram "x"))   -- 25% small figure at (0.7,0.7) upper-right
```

![annotText / annotArrow / annotRect / marginalX / insetAt](images/s3h-annotate.svg)

> **`Pos` version** (`annotTextP` / `annotArrowP` / `annotRectP` / `annotLineP`): Pass coordinates as `Pos` instead of raw `Double`, mixing npc (`PNpc 0.95` = panel 95%), data values (`PNative 3.0`), or absolute lengths per axis. Use for panel-relative annotations like "right edge npc, y is data value".
> Examples: `annotTextP (PNpc 0.95) (PNative 3.0) "R²"`,
> `annotRectP (PNpc 0.0) (PNative 1.0) (PNpc 1.0) (PNative 2.0) "grey"` (x full-width, y data band 1..2).

> Related types: `LegendSpec` (legend) / `Annotation` / `AnnotCoord` (annotations, coordinates) / `Inset` (insets) / `MarginalKind` / `MarginalSpec` (marginal distribution) / `Labs` ([Title & Labels](#labels) batch).

## Enum values quick reference {#enum-tables}

Settings with fixed values (`position` etc.) are listed **completely** here. Definition = ultimate truth is module **`Hgg.Plot.Spec`** (if values grow, source is authoritative).

| Setting Function | Type | All possible values |
|---|---|---|
| `position` | `Position` | `PosIdentity` / `PosDodge` / `PosStack` / `PosFill` |
| `linetype` / `linetypeBy` | `LineType` | `LtSolid` / `LtDashed` / `LtDotted` / `LtDotDash` / `LtLongDash` / `LtTwoDash` |
| `theme` | `ThemeName` | `ThemeDefault` / `ThemeMinimal` / `ThemeDark` / `ThemeLight` / `ThemeGrey` / `ThemeBW` / `ThemeClassic` / `ThemeVoid` / `ThemeLinedraw` / `ThemeNoir` / `ThemeLumen` / `ThemeCanvas` / `ThemeCanvasDark` (13 types) |
| `facetScales` | `FacetScales` | `FacetFixed` / `FacetFreeX` / `FacetFreeY` / `FacetFree` |
| `legendPos` | `LegendPosition` | `LegendRight` / `LegendBottom` / `LegendNone` / `LegendInsideTopRight` / `LegendInsideTopLeft` / `LegendInsideBottomRight` / `LegendInsideBottomLeft` |
| Coordinates (`coordFlip` / `coordPolar` …) | `Coord` | `CoordCartesian` / `CoordFlip` / `CoordPolarX` / `CoordPolarY` |
| `refLine` | `ReferenceLine` | `RefIdentity` / `RefHorizontalAt c` / `RefVerticalAt c` / `RefLinear slope intercept` |

> Examples: Stacked bar `<> position PosStack`, side-by-side `<> position PosDodge`, 100% stacked `<> position PosFill`. Dashed line `<> linetype LtDashed`. Legend inside top-right `<> legendPos LegendInsideTopRight`.

## Layering correctly (how `<>` works) {#overlay}

The two levels of `<>` matter for **layering**.

```haskell
-- ✅ Overlay 2 marks: wrap each in layer and <>
purePlot
  <> layer (scatter xs ys <> alpha 0.85 <> size 5)
  <> layer (line    xs fit <> color (fromHex "#dc2626") <> stroke 2)

-- ❌ This doesn't overlay (scatter and line properties merge into 1 mark)
purePlot
  <> layer (scatter xs ys <> line xs fit)
```

Why: `scatter` / `line` return `Layer`; `<>` on `Layer` means **property composition for same layer** (overwrite color, width), not "overlay two figures". Layering requires wrapping each in `layer` to make `VisualSpec`, then combining. Later layer appears on top.

![3j Layering output (scatter + regression line)](images/lesson4-overlay.svg)

Easy layer's `overlay [a, b]` (= `foldMap layer`) abbreviates this pattern.

> **Color & legend consistency**: When overlaying layers with `colorBy (ColByName …)`, glyph colors and legend swatches come from **union of all layer categories** (palette assigned by first appearance order). Colors don't re-assign per layer, staying consistent with legend. Specify order explicitly with `colorCats [..]` if needed.

## Advanced figures (stacking settings) {#advanced-layering}

Stacking settings with `<>` loads one figure with many encodings / decorations. Below is an example stacking **continuous color gradient + point size encoding + regression line overlay + reference line + theme + labs + legend** (combinations of settings in this page). Written with df integration ([06 dataframe](06-dataframe.md) detailed), encodings use just column names, reusing the same column (`"y"`) for color:

```haskell
import           Hgg.Plot.Easy             -- re-export Spec (scatter/layer/ColData…)
import           Hgg.Plot.Frame            ((|>>))
import           Hgg.Plot.Backend.SVG      (saveSVGBound)
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

num :: [Double] -> ColData ; num = NumData . V.fromList

-- Bundle x / y / sz (point size) / fit (regression prediction) into one df
df :: M.Map Text ColData
df = M.fromList [ ("x", num xs), ("y", num ys), ("sz", num sz), ("fit", num fit) ]

main :: IO ()
main = saveSVGBound "advanced.svg" $
  df |>>
     ( layer ( scatter "x" "y"            -- scatter
               <> colorContinuousBy "y"     -- continuous color (Viridis gradient, reuse y column)
               <> sizeBy "sz"             -- point size by column values
               <> alpha 0.85 )
     <> layer ( line "x" "fit"            -- regression line overlay
                <> color (fromHex "#ef4444") <> stroke 2 )
     <> scaleSize 4 16                     -- size range
     <> refHorizontal 1.0                  -- horizontal reference line
     <> theme ThemeMinimal
     <> legend
     <> labs (emptyLabs
          { labsTitle    = Just "Continuous color + size + regression + reference"
          , labsSubtitle = Just "colorContinuousBy / sizeBy / line overlay / refHorizontal"
          , labsCaption  = Just "Stack settings with <>"
          , labsX = Just "x", labsY = Just "y" }) )
```

![Advanced figure (stacking settings)](images/advanced.svg)

Key point: **encoding (color, size) inside mark with `<>`**, **scale, theme, reference lines, labs outside with `<>`** ([02 layers](02-layers.md) return type rules). Overlay one mark per layer ([Layering](#overlay)). → Figure generation code is `hgg-svg/examples/DocFigures.hs` (run `cabal run doc-figures` to regenerate all guide figures).

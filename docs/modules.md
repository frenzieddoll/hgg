# Modules and What's Possible

> 🌐 **English** | [日本語](modules.ja.md)

## Package Structure

Core is backend-agnostic (base / vector / text / containers only). Choose a backend package for your output target.

| Package | Role | Language | Status |
|---|---|---|---|
| `hgg-core` | Spec / Layout / Render abstraction / Palette / DAG | Haskell (pure) | ✅ |
| `hgg-svg` | SVG backend (`saveSVG` etc.) | Haskell | ✅ Production-ready |
| `hgg-3d` | 3D plot (CPU projection, mplot3d equivalent) | Haskell | ✅ |
| `hgg-pdf` | PDF backend (HPDF, Latin only) | Haskell | ✅ (Phase 17) |
| `hgg-rasterific` | PNG backend (Rasterific + FontyFruity, Japanese support) | Haskell | ✅ (Phase 22) |
| `hgg-dataframe` | Hackage dataframe → Resolver bridge | Haskell | ✅ |
| `hgg-analyze-bridge` | hanalyze direct plot bridge | Haskell | ✅ |
| `hgg-doe` | DoE-specific helper (MainEffects/Interaction/ResponseSurface) | Haskell | ✅ |
| `hgg-canvas` | Halogen / web-canvas + WebGL2 3D backend | PureScript | ✅ In active use |
| `hgg-doe-canvas` | PS DoE-specific helper | PureScript | ✅ |

### Core Main Modules

| Module | Responsibility |
|---|---|
| `Hgg.Plot.Spec` | Declarative plot specification (Grammar API). geom / channel / scale / theme / facet |
| `Hgg.Plot.Easy` | Introductory API. Re-exports Spec + `[Double]` direct-pass helpers + `overlay` |
| `Hgg.Plot.Layout` | Domain computation, track placement, facet layout |
| `Hgg.Plot.Render` | `Primitive` (geometric primitives) and backend abstraction |
| `Hgg.Plot.Palette` | Hgg brand + academic palettes, sequential/diverging/cyclical |
| `Hgg.Plot.DAG` | Graph plots (graphviz dot equivalent, Sugiyama layout) |
| `Hgg.Plot.Validate` | Spec validity checking |

PureScript (`hgg-canvas`) carries the **same ADT**, guaranteeing JSON round-trip fidelity via aeson ↔ Argonaut.
Swap backends without changing the spec.

---

## What's Drawable

> One-liner code examples + output SVG for each chart type in **../design/gallery/README.md**.

### Basic Charts
scatter / line / bar / histogram / box / heatmap / pie / waterfall / contour / step / stem
/ parallel coordinates / band (CI ribbon)

### Distributions
violin / strip / swarm / raincloud / ridge (joyplot)
→ Group comparison: `cabal run tutorial-04-distribution`

### Statistical Specialties
density (KDE) / trace (MCMC) / pairs plot / regression line + CI / stat line (mean/median)
/ forest / ESS / Q-Q plot / **DAG** (graphviz equivalent)

### 3D (mplot3d equivalent + interactive)
- CPU projection (SVG/PDF/PNG): scatter3D / line3D / wireframe3D / surface3D
- WebGL interactive (browser): orbit / zoom / pan
- Launch with `showBrowser` in one line

### DoE (`hgg-doe`)
mainEffects / interaction / responseSurface

### Coordinate Systems (Phase 10/11)
`coordFlip` (swap XY) / `coordCartesian` (zoom without discarding data) / `coordPolar` (rose plots etc.)

### Axes
linear / log / format (Integer/Decimal/Exponent) ✅, sqrt / time are spec-defined (Layout in progress)

### Decoration
facet (Trellis, free scales supported) / subplots (1D/2D grid) / pairs / inset axes
/ annotation (text/arrow/rect/line) / dual Y axis / marginal hist+density / legend chip (4 positions)

### Position Adjustment
`PosDodge` (side-by-side) / `PosStack` (stacked) / `PosFill` (100% stacked) / identity

### Themes & Color Palettes
- Themes: `ThemeDefault` / `ThemeMinimal` / `ThemeDark` / `ThemeLight`
 / `ThemeCanvas` (parchment, light) / `ThemeCanvasDark`
- Series palette: ggplot standard hue_pal + academic palettes + Hgg brand (7 character-based systems)
- Element-wise override with `ThemeOverride`
→ Light/dark variants: `cabal run tutorial-05-theme`

### Fonts
`titleFont` / `axisLabelFont` / `tickFont` / `legendFont` + `fontSize` / `fontFamily`
/ `fontWeight` / `fontColor` (composable with `<>`. Currently title + axis label in Render)

### Scale
`scaleColorManual` (category → color dict) / `scaleColorGradient2` (diverging) / `scaleSize` (size range)
/ `colorContinuous` / `sizeBy` / `*_reverse` equivalent (range swap)

---

## Feature Stability

| Category | Content |
|---|---|
| **Stable** | SVG backend basic charts, distributions, statistics, coordinate systems, themes, facet (= gallery entries) |
| **Experimental** | sqrt/time axes (Layout in progress), tick/legend font wiring, true stacked-pie, PDF backend (Phase 17, Latin only), PNG backend (Phase 22, Japanese support) |
| **Planned** | Hackage / npm release (after official version) |

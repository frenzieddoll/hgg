# Changelog for `hgg-core`

## 0.2.0.0 — 2026-08-13

- **Breaking**: theme constructors `ThemeCanvas` / `ThemeCanvasDark` renamed
  to `ThemeParchment` / `ThemeParchmentDark`.
- Generalized coordinate systems: ternary coordinates (`ternaryScatter` /
  `ternaryLine` marks, `encZ` channel), polar start angle / direction /
  rotation, polygon clipping; crossbar renders through the projection layer
  (correct in polar coordinates).
- Theme extensions (cowplot parity): base-size scaling, font family, plot
  margins, subplot relative widths and tags, three new presets; grid and
  axis line widths are now theme-controlled.
- Fixes: facets now split inline (non-DataFrame) layer data; boxplot
  outliers are included in the axis domain.
- Bilingual (English / Japanese) haddock across the public API.

## 0.1.0.0 — 2026-07-18

First public release on Hackage.

- Backend-independent core: `VisualSpec` / layers / marks / encoding / scales / themes.
- Depends only on base / vector / text / containers.

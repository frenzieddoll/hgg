# Changelog for `hgg`

## 0.2.0.0 — 2026-08-13

- Version lockstep with the hgg 0.2 package set (exact pins on hgg-core /
  hgg-frame / hgg-svg 0.2.0.0; the `pdf` / `png` / `latex` / `3d` flags pull
  in the 0.2 backends).
- **Breaking** (inherited from hgg-core): theme constructors `ThemeCanvas` /
  `ThemeCanvasDark` renamed to `ThemeParchment` / `ThemeParchmentDark`.

## 0.1.0.0 — 2026-07-19

First public release on Hackage.

- Umbrella package: depends on `hgg-core` / `hgg-frame` / `hgg-svg`
  (exact-pinned) and exposes the single module `Graphics.Hgg`, a thin
  re-export of the Easy API (`Graphics.Hgg.Quick` / `Easy` / `Spec`),
  the dataframe binding (`Graphics.Hgg.Frame`), SVG save
  (`Graphics.Hgg.Backend.SVG`) and units (`Graphics.Hgg.Unit`).
- Manual cabal flags `pdf` / `png` / `latex` / `3d` pull in the optional
  backend packages (`hgg-pdf` / `hgg-rasterific` / `hgg-latex` / `hgg-3d`).

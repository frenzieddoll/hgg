# Appendix

> 🌐 **English** | [日本語](09-appendix.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 layers](02-layers.md) | [03 encoding & scale](03-encoding-scale.md) | [04 decoration](04-decoration.md) | [05 backends](05-backends.md) | [06 dataframe](06-dataframe.md) | [07 analyze](07-analyze.md) | [08 3d](08-3d.md) | **09 appendix**

Page structure:
**[A: Which layer/page to use](#appendix-layers)** | **[B: For ggplot2 users](#appendix-ggplot)** |
**[C: For library extension](#appendix-extend)**

## A: Which layer/page to use {#appendix-layers}

```
One plot, REPL                                    → 01 quickstart (Quick / Easy)
Color by group, multiple marks, ggplot experience  → 01 quickstart (Grammar) + 02 layers / 04 decoration
2D fields (contour / bin2d)                       → 02 layers (mark index)
Backend choice, save functions                    → 05 backends
CSV / DataFrame by column name                    → 06 dataframe (df |>> …)
Regression, GLM, HBM plotting                     → 07 analyze (analyze integration)
3D response surface (surface / ground contour)    → 08 3d (hgg-3d)
Continuous gradient / scale control               → 04 decoration (scale) + Resolver
Custom backend / new mark rendering               → Appendix C (library extension)
```

## B: For ggplot2 users {#appendix-ggplot}

Correspondence with ggplot2 concepts is noted on each page as **cross-references with geom/aes**
([02 layers](02-layers.md) mark catalog, [03 encoding & scale](03-encoding-scale.md) channels,
[04 decoration](04-decoration.md) theme/scale/facet). `aes()` ↔ `colorBy`/`shapeBy`/… inside marks,
`+` ↔ `<>`, `geom_*()` ↔ `scatter`/`bar`/… marks.

## C: For library extension (backend / new marks) {#appendix-extend}

Not needed for normal plotting. Read this only when **extending the library itself** — writing a new output backend, rendering something the existing marks don't support.

- **Writing a backend** = Use `Graphics.Hgg.Render`'s
  `renderToPrimitives :: Resolver -> Layout -> VisualSpec -> [Primitive]` to lower the spec to geometric primitive sequences
  (`PLine` / `PRect` / `PCircle` / `PPath` / `PText` / `PClipPush`/`PClipPop` / `PTransformPush`/`PTransformPop`),
  then **fold** them into your target format. **Template = `hgg-svg`'s `Backend/SVG.hs`**
  (`renderToPrimitives` → `primsToSvg` stringifies + wraps in header/footer). Pre-built helpers
  `renderPrimitivesSVG` / `savePrimitivesSVG` (Primitive sequence → SVG) are exported.
- **Compose `Primitive` directly** = when rendering something not in any backend, hand-build it.

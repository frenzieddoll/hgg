# matplotlib / ggplot2 Comparison

> 🌐 **English** | [日本語](comparison.ja.md)

> For comparison with **Vega-Lite** (especially on DataFrame integration and hanalyze statistical
> engine integration), see [comparison-vega-lite.md](comparison-vega-lite.md).

## In One Sentence

- **Philosophy is ggplot2-oriented** ─ "data + aesthetic mapping + geom + scale" grammar.
  Composition with `<>` corresponds to `+` in ggplot2.
- **Unlike matplotlib: stateless** ─ No implicit "current figure" like matplotlib's pyplot.
  Plots are pure values (`VisualSpec`), with side effects only at final save.
- **Coverage** ─ Not aiming to recreate all matplotlib features, but to cover **standard statistical workflow**
  (scatter/line/bar/distribution/regression/facet/3D etc.).

---

## Same Plot, Three Styles

Draw a "scatter plot grouped by category + axis labels + title."

### matplotlib (imperative, stateful)

```python
import matplotlib.pyplot as plt
for g, sub in df.groupby("group"):
    plt.scatter(sub.x, sub.y, label=g)
plt.legend(); plt.title("by group")
plt.xlabel("x"); plt.ylabel("y")
plt.savefig("out.svg")
```

### ggplot2 (declarative, grammar-based)

```r
ggplot(df, aes(x, y, color=group)) +
  geom_point(size=6) +
  scale_color_manual(values=c(alpha="#1B9E77", beta="#D95F02")) +
  labs(title="by group", x="x", y="y")
```

### hgg (declarative, pure)

```haskell
purePlot
  <> layer (scatter (inline xs) (inline ys) <> color (inlineCat gs) <> size 6)
  <> scaleColorManual [("alpha","#1B9E77"), ("beta","#D95F02")]
  <> legend
  <> title "by group" <> xLabel "x" <> yLabel "y"
```

→ `cabal run tutorial-02-grammar`

---

## Concept Mapping

| Goal | matplotlib | ggplot2 | hgg |
|---|---|---|---|
| Plot foundation | `plt.figure()` (implicit) | `ggplot(d, aes())` | `purePlot` |
| Scatter plot | `plt.scatter` | `geom_point` | `scatter` / `points` |
| Line plot | `plt.plot` | `geom_line` | `line` / `lineXY` |
| Bar plot | `plt.bar` | `geom_col` | `bar` / `bars` |
| Histogram | `plt.hist` | `geom_histogram` | `histogram` / `hist` |
| Group by color | `c=`, loop | `aes(color=g)` | `<> color (inlineCat gs)` |
| Overlay | Consecutive `plt.*` calls | `+ geom_*()` | `<> layer (...)` |
| Color scale | `cmap=` | `scale_color_*` | `scaleColorManual` / `scaleColorGradient2` |
| Small multiples | `plt.subplots` | `facet_wrap` / `facet_grid` | `facet*` |
| Theme | `plt.style.use` | `theme_*` | `theme Theme*` |
| Axis label | `plt.xlabel` | `labs(x=)` | `xLabel` |
| Flip/polar coords | Separate API | `coord_flip` / `coord_polar` | `coordFlip` / `coordPolar` |
| Save | `plt.savefig` | `ggsave` | `saveSVG` |
| 3D | `mplot3d` | (limited) | `hgg-3d` + `showBrowser` |

---

## Strengths / What's Not Yet Ready

### Strengths

- **Pure & composable** ─ Partial specs (theme-only, axes-only) are reusable values. Easier to test.
- **HS / PS same ADT** ─ Backend (Haskell) and frontend (PureScript) share the same spec,
  round-tripping via JSON. Server-generated and browser-interactive plots match exactly.
- **3D is browser-interactive** ─ WebGL2 orbit/zoom/pan. Beyond mplot3d's static projection.
- **Statistical plot breadth** ─ violin / raincloud / ridge / trace / ESS / forest / DAG and more,
  built-in (matplotlib requires extra libraries for these).

### Not Yet Ready (Planned / Experimental)

- PDF / PNG backends are placeholders (SVG / Canvas / WebGL are production).
- sqrt / time axes are spec-defined but full Layout support is in progress.
- matplotlib-style low-level artist manipulation requires direct `Primitive` layer writing (Layer 4).

> Single source of truth for per-backend/chart implementation status: `design/parity-table.md`.

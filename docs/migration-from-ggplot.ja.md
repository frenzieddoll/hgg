# ggplot2 → hgg 移行ガイド

> 🌐 [English](migration-from-ggplot.md) | **日本語**

ggplot2 経験者が最短で書き換えられるよう、 関数の **対応表** を示す。
文法を手取り足取り学ぶなら [API リファレンス (api-guide/)](api-guide/README.ja.md)、 思想面・matplotlib も含む広い比較は
[comparison.ja.md](comparison.ja.md) を参照。

## 3 つの原則

1. **`+` → `<>`**。 ggplot の `p + geom_point() + theme_minimal()` は
   `purePlot <> layer (...) <> theme ...`。
2. **`<>` は 2 階層**。 geom の中の aesthetic (`color` / `size` …) は `layer (geom <> aes)` の **中**、
   theme / scale / facet / labs は **外**。 → [api-guide 03 装飾](api-guide/04-decoration.ja.md)。
3. **`aes(x, y)` のデータは 2 通り**: 値直渡し `scatter (inline xs) (inline ys)` か、
   DataFrame + 列名 `df |>> layer (scatter "x" "y")` ([api-guide 05 dataframe](api-guide/06-dataframe.ja.md))。

---

## 対応表

### 図の骨格

| ggplot2 | hgg |
|---|---|
| `ggplot(d, aes(x, y))` | `purePlot` (+ 下の geom) |
| `+ geom_point()` | `<> layer (scatter "x" "y")` |
| `+ geom_*()` をもう 1 つ重ね | `<> layer (...)` を追加 |
| `ggsave("out.svg", p)` | `saveSVG "out.svg" spec` / df なら `saveSVGBound "out.svg" bound` |

### geom

| ggplot2 | hgg |
|---|---|
| `geom_point` | `scatter` |
| `geom_line` | `line` |
| `geom_col` / `geom_bar` | `bar` |
| `geom_step` | `step` |
| `geom_text` / `geom_label` | `geomText` / `geomLabel` |
| `geom_histogram` | `histogram` |
| `geom_boxplot` | `boxplot` / `boxplotBy` |
| `geom_violin` | `violin` |
| `geom_density` | `density` |
| `stat_ecdf` | `ecdf` |
| `geom_ribbon` | `band` |
| `geom_errorbar` / `geom_pointrange` | `errorY` / `pointRange` |
| `geom_smooth(method="lm")` | `statLm` (analyze bridge) または `regressionLineCI` |
| `geom_smooth(method="lm", level=0.99)` | `statLmLevel "x" "y" 0.99` |
| `geom_smooth()` (平滑、 帯なし) | `statSmooth` (analyze bridge) |
| `geom_smooth()` (平滑 + 帯) | `statSmoothCI` |
| `geom_smooth(method="lm", formula=y~poly(x,2))` | `statPoly "x" "y" 2` |
| `geom_smooth(aes(color=g))` (群別) | `statLm … <> color "g"` |
| `plot(lm)` #1 (残差 vs fitted) | `statResid "x" "y"` |
| `geom_tile` / `geom_bin2d` | `heatmap` / `bin2d` |
| `geom_contour` | `contour` |

### aesthetic (layer の **中**で `<>`)

| ggplot2 | hgg |
|---|---|
| `aes(color = g)` | `<> color "g"` (or `color (inlineCat gs)`) |
| `aes(size = s)` | `<> sizeBy "s"` |
| `aes(shape = g)` | `<> shapeBy "g"` |
| `aes(linetype = g)` | `<> linetypeBy "g"` |
| `alpha = 0.7` (固定) | `<> alpha 0.7` |
| `color = "red"` (固定) | `<> colorStatic "#dc2626"` |
| `position = "dodge" / "stack" / "fill"` | `<> position PosDodge / PosStack / PosFill` |

### scale (図の **外**で `<>`)

| ggplot2 | hgg |
|---|---|
| `scale_color_manual(values = …)` | `scaleColorManual [("a","#…")]` |
| `scale_color_gradient2()` | `scaleColorGradient2 lo mid hi midPt` |
| `scale_size(range = …)` | `scaleSize lo hi` |
| `scale_x_log10()` | `logAxis` (x 軸の調整) |
| `scale_x_reverse()` | `reverseX` |

### facet / coord / theme / labs (図の **外**で `<>`)

| ggplot2 | hgg |
|---|---|
| `facet_wrap(~g, ncol = n)` | `facetWrap "g" n` |
| `facet_grid(r ~ c)` | `facetGrid "r" "c"` |
| `coord_flip()` | `coordFlip` |
| `coord_polar()` | `coordPolar` |
| `theme_minimal()` | `theme ThemeMinimal` |
| `theme(legend.position = "bottom")` | `legendPos LegendBottom` |
| `theme(legend.position = "none")` | `legendOff` |
| `labs(title=, x=, y=)` | `title … <> xLabel … <> yLabel …` (or `labs (emptyLabs{…})`) |

> 各設定の取りうる値 (`Position` / `ThemeName` / `LineType` / `LegendPosition` …) の **全列挙** は
> [api-guide 03 列挙型早見表](api-guide/04-decoration.ja.md#enum-tables)。

---

## 最小の対比例

ggplot2:

```r
ggplot(d, aes(weight, mpg, color = origin)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  facet_wrap(~cyl, ncol = 3) +
  theme_minimal() +
  labs(title = "MPG vs weight")
```

hgg (DataFrame + stat-in ルート):

```haskell
df |>> ( layer (scatter "weight" "mpg" <> color "origin" <> size 3)
       <> layer (statLm "weight" "mpg")          -- geom_smooth(method="lm")
       <> facetWrap "cyl" 3
       <> theme ThemeMinimal
       <> title "MPG vs weight" )
```

描画は stat-in 経路の `saveSVGBoundStats` ([api-guide 06 analyze (ルート 2 stat-in)](api-guide/07-analyze.ja.md))。
`statLm` を使わず fit 済みモデルを重ねるなら `toPlot (lmModel x y)` (ルート 1、 同 06 analyze.ja.md)。

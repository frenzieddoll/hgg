# Vega-Lite 例ギャラリーの再現

[Vega-Lite examples gallery](https://vega.github.io/vega-lite/examples/) の各セクション
代表例を hgg で再現したコード集。 実際に動く executable
`hgg-svg/examples/VegaLiteGallery.hs`
として実装・レンダリング検証済み。

```sh
cabal run vega-lite-gallery   # → design/vega-lite-gallery/*.svg (28 枚) を生成
```

> 比較の総評・5 軸評価は [comparison-vega-lite.md](comparison-vega-lite.md) を参照。
> 本書は **「実際に描けるものはコードで示し、 描けないものは描けないと明記する」** 方針。

## 再現できる例 (コード付き)

すべて inline データで自己完結 (facet のみ Resolver)。 `purePlot <> layer (...) <> …` で構築し
`saveSVG` で出力する。

### Bar Charts

```haskell
-- Simple Bar Chart
purePlot <> layer (bar (inlineCat ["A","B","C","D","E"]) (inline [28,55,43,91,81]))

-- Grouped Bar Chart (dodge) / Stacked / Normalized は position で切替
let gcat = inlineCat (concatMap (replicate 3) ["A","B","C"])
    ggrp = inlineCat (take 9 (cycle ["x","y","z"]))
    gval = inline [3,5,2, 4,1,6, 2,3,4]
purePlot <> layer (bar gcat gval <> color ggrp <> position PosDodge)   -- 群並び
purePlot <> layer (bar gcat gval <> color ggrp <> position PosStack)   -- 積み上げ
purePlot <> layer (bar gcat gval <> color ggrp <> position PosFill)    -- 100% 積み上げ
```

| Simple | Grouped (dodge) | Stacked | Normalized |
|---|---|---|---|
| ![](images/vega-lite/01-bar-simple.svg) | ![](images/vega-lite/02-bar-grouped.svg) | ![](images/vega-lite/03-bar-stacked.svg) | ![](images/vega-lite/04-bar-normalized.svg) |

### Histograms / Density / Cumulative

```haskell
purePlot <> layer (histogram (inline vals))   -- Histogram
purePlot <> layer (density   (inline vals))   -- Density Plot
purePlot <> layer (ecdf      (inline vals))   -- Cumulative Frequency Distribution
```

| Histogram | Density Plot | ECDF |
|---|---|---|
| ![](images/vega-lite/05-histogram.svg) | ![](images/vega-lite/06-density.svg) | ![](images/vega-lite/07-ecdf.svg) |

### Scatter & Strip

```haskell
purePlot <> layer (scatter (inline sx) (inline sy) <> size 6)                     -- Scatterplot
purePlot <> layer (scatter (inline sx) (inline sy) <> color grp <> sizeBy sz)     -- Bubble Plot
purePlot <> layer (strip (inlineCat (replicate n "v")) (inline vals))             -- 1D Strip Plot
```

| Scatterplot | Bubble Plot | Strip Plot |
|---|---|---|
| ![](images/vega-lite/08-scatter.svg) | ![](images/vega-lite/09-bubble.svg) | ![](images/vega-lite/10-strip.svg) |

### Line Charts

```haskell
purePlot <> layer (line (inline lx) (inline ly) <> stroke 2)                      -- Line Chart
-- Multi Series = line レイヤを複数重畳
purePlot <> layer (line lx l1 <> colorStatic "#1f77b4") <> layer (line lx l2 <> colorStatic "#d62728")
purePlot <> layer (step (inline lx) (inline sy) <> stroke 2)                      -- Step Chart
```

| Line Chart | Multi Series | Step Chart |
|---|---|---|
| ![](images/vega-lite/11-line.svg) | ![](images/vega-lite/12-line-multi.svg) | ![](images/vega-lite/13-step.svg) |

### Area Charts

```haskell
-- Area Chart = band を 0..y で塗り、 line を重ねる
purePlot <> layer (band (inline ax) (inline (replicate n 0)) (inline ay) <> alpha 0.5)
         <> layer (line (inline ax) (inline ay) <> stroke 2)
```

![Area Chart](images/vega-lite/14-area.svg)

#### Streamgraph (中心化積層 area)

```haskell
-- color aes で系列分割。 各 x で系列を積層、 baseline=-(Σy)/2 (silhouette 中心化)
purePlot <> layer (stream (inline t) (inline value) <> color (inlineCat series) <> alpha 0.85)
```

![Streamgraph](images/streamgraph.svg)

### Table-based / Circular

```haskell
purePlot <> layer (heatmap hx hy hv)                          -- Table Heatmap (x/y は category)
purePlot <> layer (pie (inlineCat cats) (inline vals))        -- Pie Chart
purePlot <> layer (bar cats vals <> color cats) <> coordPolar -- Radial Plot (polar bar)
```

| Table Heatmap | Pie Chart | Radial Plot |
|---|---|---|
| ![](images/vega-lite/15-heatmap.svg) | ![](images/vega-lite/16-pie.svg) | ![](images/vega-lite/17-radial.svg) |

### Advanced Calculations

```haskell
-- Linear Regression (plot-core 内蔵 OLS。 信頼帯付きや GLM/GP は hanalyze toPlot 経由)
purePlot <> layer (scatter (inline sx) (inline sy))
         <> layer (regressionLine (inline sx) (inline sy) <> colorStatic "#d62728")

purePlot <> layer (geomQQ (inline vals))                                   -- QQ Plot
purePlot <> layer (parallelCoords [inline c1, inline c2, inline c3])       -- Parallel Coordinates
purePlot <> layer (waterfall (inlineCat steps) (inline deltas))            -- Waterfall Chart
```

| Linear Regression | QQ Plot | Parallel Coordinates | Waterfall |
|---|---|---|---|
| ![](images/vega-lite/18-regression.svg) | ![](images/vega-lite/19-qq.svg) | ![](images/vega-lite/20-parallel.svg) | ![](images/vega-lite/21-waterfall.svg) |

### Error Bars & Box Plots

```haskell
-- Error Bars Showing Confidence Interval (pointRange = x, y, err)
purePlot <> layer (pointRange (inline xs) (inline ys) (inline errs))
purePlot <> layer (boxplot (inline vals))                                  -- Box Plot (Tukey 1.5 IQR)
```

| Error Bars (CI) | Box Plot |
|---|---|
| ![](images/vega-lite/22-errorbar.svg) | ![](images/vega-lite/23-boxplot.svg) |

### Distributions

```haskell
purePlot <> layer (violin    cat val)     -- Violin Plot
purePlot <> layer (swarm     cat val)     -- Swarm (beeswarm)
purePlot <> layer (raincloud cat val)     -- Raincloud
purePlot <> layer (ridge     cat val)     -- Ridgeline
```

| Violin | Swarm | Raincloud | Ridgeline |
|---|---|---|---|
| ![](images/vega-lite/24-violin.svg) | ![](images/vega-lite/25-swarm.svg) | ![](images/vega-lite/26-raincloud.svg) | ![](images/vega-lite/27-ridge.svg) |

### Faceting (Trellis)

```haskell
-- 列名参照ゆえ Resolver と saveSVGWith を使う
saveSVGWith "trellis.svg" resolver $
  purePlot <> layer (scatter "x" "y" <> color "g" <> size 6) <> facet "g"
```

![Trellis Scatter Plot](images/vega-lite/28-trellis-scatter.svg)

**計 28 例をレンダリング検証済み**。 画像は `docs/images/vega-lite/01..28-*.svg` (上に埋め込み)。
SVG ソースは `cabal run vega-lite-gallery` で `design/vega-lite-gallery/` に再生成できる。

## 描けない例の分類

Vega-Lite ギャラリーのうち hgg で再現していないものを 2 つに分ける。

### A. 仕様として載せない (意図的に対象外)

**この 3 系統は今後も実装予定がない** (利用者が使わないため)。 spec として持たない。

| 系統 | 該当 Vega-Lite 例 | 理由 |
|---|---|---|
| **地理 / 地図** | Maps セクション全部 (Choropleth / Zipcode・Airport dots / Tube Lines / Projection explorer / Earthquakes / Wind Vector Map …) | geoshape / projection を spec に持たない方針 |
| **対話** | Interactive / Interactive Multi-View セクション全部 (Hover / Brush / Pan-Zoom / Crossfilter / Widgets / Minimap / Dynamic Legend …) | 静的出力に専念。 selection/params の対話文法は持たない方針 |
| **画像 / アイソタイプ** | Image-based Scatter Plot / Isotype Dot Plot (image・emoji) / Isotype Grid | image mark を持たない方針 |

### B. 今後の課題 (真に描けない・実装余地あり)

仕様外の上記を除き、 **現状ネイティブに描けないが将来実装し得る**もの。

| 不足機能 | 該当 Vega-Lite 例 | メモ |
|---|---|---|
| **repeat 演算子のみ** | Repeat-and-Layer | ★concat 系 (Vertical/Horizontal/Nested View Concatenation) は **`subplots`/`subplotCols` で対応済** (異種 spec の任意グリッド・**入れ子可**)。 残るは `repeat` (1 フィールドリストの自動反復) の専用演算子のみ (現状は手で `subplots` に並べる) |
| ~~**Streamgraph**~~ | Streamgraph | ✅ **`stream x y <> color "series"` で対応** (Phase 52.D2)。 中心化 (silhouette、 baseline=-Σy/2) 積層 area。 wiggle 最小化 (ThemeRiver) は未対応 |
| **Horizon Graph** | Horizon Graph | 折り返し帯の専用描画が無い |
| **Trail mark (可変幅線)** | Line Chart with Varying Size (trail) | 線幅エンコードが無い (WebGL は 1px 固定) |
| **Mosaic Chart** | Mosaic Chart with Labels | 可変幅積み上げバーが無い |
| **Ternary chart** | Ternary chart | 重心座標系が無い |
| **Candlestick / Bullet** | Candlestick Chart / Bullet Chart | 専用マークが無い (layered で近似は要手組み) |
| **Gantt (ranged bar)** | Gantt Chart | 区間は `lineRange`/`crossbar` で近似可能だが、 ranged **bar** はネイティブでない |

> これらは [comparison-vega-lite.md](comparison-vega-lite.md) の「未整備」 とも対応。 B 群は
> マーク追加・合成演算子追加で対応余地があり、 今後の Phase 候補。

## 補足: hgg 固有の強み (Vega-Lite に無い)

逆に Vega-Lite のギャラリーに無く hgg が持つもの: **MCMC trace / ESS / 自己相関** (ベイズ
診断)、 **モデル DAG** (HBM 構造)、 **フォレスト / ファンネル** (メタ解析)、 **ウェハマップ** (半導体)、
**3D 散布**、 そして **hanalyze 連携の `toPlot`** (GLM/GP/生存/予測を統計的に正しい不確実性つきで描く)。
詳細は [comparison-vega-lite.md 軸 4](comparison-vega-lite.md)。

# レイヤとマーク ─ 描けるグラフ一覧 + encoding

> [📚 索引](README.md) ｜ [01 quickstart](01-quickstart.md) ｜ **02 layers** ｜ [03 decoration](03-decoration.md) ｜ [04 backends](04-backends.md) ｜ [05 dataframe](05-dataframe.md) ｜ [06 analyze](06-analyze.md) ｜ [07 3d](07-3d.md) ｜ [08 appendix](08-appendix.md)

合成単位は **layer**、 描画の種類は **mark** (型名 `MarkKind`)、 個別関数は `scatter`/`line`/`bar`/…。
「何が設定できるか」 を **戻り型**で分けて並べる:

- **`Layer` を返す** = `layer (mark <> これ)` の**中**に足す (= その mark の見た目・channel)
- **`VisualSpec` を返す** = `purePlot <> … <> これ` の**外**に足す (= 図全体の設定。 [03 decoration](03-decoration.md))

> ggplot2 利用者向け: 本ライブラリの **mark** は ggplot の `geom_*` に相当する (例: `scatter` =
> `geom_point`)。 ただし「geom」 は ggplot 方言で Wilkinson の Grammar of Graphics には無いため、
> 本リファレンスでは native の **mark / layer** を用い、 「geom」 は ggplot 相互参照のみで使う。

## mark / layer 一覧 (描けるグラフ) ─ いずれも `Layer` {#marks}

```haskell
-- 基本 (x, y)
scatter  x y      line     x y      bar      x y      step  x y
geomText x y      geomLabel x y     stem     x y      ecdf  x         -- ecdf は 1 列

-- 分布
histogram x       boxplot  x        boxplotBy g x     density   x
densityNorm x     violin   g x      strip    g x      swarm     g x
raincloud g x     ridge    g x

-- 区間 / エラー
band  x lo hi     lineRange x lo hi pointRange x y e  crossbar x y w
forest e lo hi    funnel   e se

-- 積層 area / 時系列
stream x y        -- streamgraph (中心化積層 area)。 `<> color "series"` で系列分割

-- 集計 / 統計
statFunction      statMean          statMedian        regressionLine
regressionLineCI  histogramWide

-- 2 次元密度 / 行列
heatmap x y z     contour x y z     bin2d x y z       pie cat val
waterfall x y     pairs [cols]      parallelCoords [cols]

-- ベクトル場 (vector field)
quiver x y u v    -- 各 (x,y) に成分 (u,v) の矢印。 <> arrowColorByMagnitude で |u,v| 連続色

-- MCMC / ベイズ診断
trace  / traceLines / autocorr x / ess x / chain …

-- DAG (HBM ModelGraph 等)
dag / dagFromLists / dagFromListsWithPlates
```

`x y` は `ColRef` (= `inline […]` か、 DataFrame 利用時は列名 `"weight"`)。
完全な mark 一覧と必須 channel は `docs/modules.md` / `design/gallery/` を参照。
2 次元場の `contour`(等高線) / `bin2d`(塗り) / `heatmap`(カテゴリ grid) の使い分けは
このすぐ下「2 次元場の mark」 にコード + 図付きで示す。

デモ (`bar`):

```haskell
purePlot <> layer (bar (inlineCat ["A","B","C"]) (inline [3,7,5]))
  <> xLabel "群" <> yLabel "値"
```

![3a geom: bar](images/s3a-geom.svg)

**2 次元場の mark (`contour` / `bin2d` / `heatmap`)** ─ 連続 `(x, y, z)` の場を描く mark。
`contour` と `bin2d` は **同じ引数**のまま、 線 (等値線) で描くか grid セルを塗るかが違うだけ
(カテゴリ grid の塗りは `heatmap`)。 scatter/line と同じく `layer` で重ねる:

```haskell
-- 正則格子上の z = 2 つの山
let grid = [ (xi, yi) | xi <- [0.0, 0.4 .. 6.0], yi <- [0.0, 0.4 .. 6.0] ]
    gx = inline (map fst grid); gy = inline (map snd grid)
    gz = inline [ exp (-(((x-3)**2)+((y-3)**2))/4)
                + 0.4 * exp (-(((x-1.2)**2)+((y-4.5)**2))/1.5) | (x, y) <- grid ]
purePlot <> layer (contour gx gy gz)    -- marching squares の等値線 (z を等分・Viridis 色)
purePlot <> layer (bin2d   gx gy gz)    -- 同データを grid セルの連続色で塗る (= geom_bin2d)
```

| `contour` (等高線図) | `bin2d` (binned heatmap) |
|---|---|
| ![contour](images/contour.svg) | ![bin2d](images/bin2d.svg) |

df なら `df |>> layer (contour "x" "y" "z")` のように列名でも描ける ([05 dataframe](05-dataframe.md))。

**ベクトル場 (`quiver`)** ─ 各点 `(x, y)` に成分 `(u, v)` の矢印を描く (= matplotlib
`quiver`)。 勾配場・流れ場・力場・residual の方向表現に。 矢印長は **autoscale**
(最長矢印がデータ対角の ~8%) で、 `arrowScale s` で倍率を掛けられる。
`arrowColorByMagnitude` で矢印を `|u,v|` の連続色 (viridis) にする:

```haskell
purePlot <> layer (quiver gx gy gu gv)                        -- 単色
purePlot <> layer (quiver gx gy gu gv <> arrowColorByMagnitude
                                      <> arrowScale 1.2)       -- 大きさ連続色 + 倍率
```

![quiver (vector field)](images/quiver.svg)

df なら `df |>> layer (quiver "x" "y" "u" "v")` ([05 dataframe](05-dataframe.md))。 3D 版は
[07 3d](07-3d.md) の `quiver3D`。

## mark ごとの見た目・encoding ─ `Layer` (mark の中で `<>`) {#encoding}

| 設定 | 意味 | 例 |
|---|---|---|
| `color c` | カテゴリ列で色分け | `<> color gs` |
| `colorStatic "…"` | 単色固定 | `<> colorStatic "#dc2626"` |
| `colorContinuous c` | 連続値 gradient | `<> colorContinuous zs` |
| `alpha a` | 不透明度 (0–1) | `<> alpha 0.7` |
| `size s` | 点サイズ | `<> size 6` |
| `stroke w` | 線幅 | `<> stroke 2` |
| `sizeBy c` | 値で点サイズを変える | `<> sizeBy ssz` |
| `shapeBy c` | カテゴリで形を変える | `<> shapeBy gs` |
| `linetype lt` | 線種固定 (`LtDashed` 等) | `<> linetype LtDashed` |
| `linetypeBy c` | カテゴリで線種 | `<> linetypeBy gs` |
| `position p` | bar の積み方 (下記) | `<> position PosStack` |
| `errorX e` / `errorY e` | エラーバー列 | `<> errorY se` |
| `hoverCols […]` | hover tooltip 列 | `<> hoverCols ["id","note"]` |
| `connect* …` | 点の接続線 (順序/群/色/幅) | `<> connectOrder ord` |

`position` の値: `PosIdentity` / `PosDodge` (横並び) / `PosStack` (積上げ) /
`PosFill` (100%積上げ)。

デモ (`color` + `position PosDodge`):

```haskell
purePlot
  <> layer (bar bx bv <> color bg <> position PosDodge)   -- bx=群, bg=系列
  <> legend
```

![3b position dodge + color](images/s3b-aes.svg)

# 付録

> [📚 索引](README.md) ｜ [01 quickstart](01-quickstart.md) ｜ [02 layers](02-layers.md) ｜ [03 decoration](03-decoration.md) ｜ [04 backends](04-backends.md) ｜ [05 dataframe](05-dataframe.md) ｜ [06 analyze](06-analyze.md) ｜ [07 3d](07-3d.md) ｜ **08 appendix**

## A: どの層・どのページを使うか {#appendix-layers}

```
1 枚だけ・REPL                          → 01 quickstart (Quick / Easy)
群で色分け・複数 mark・ggplot 経験あり    → 01 quickstart (Grammar) + 02 layers / 03 decoration
2 次元場 (contour / bin2d)               → 02 layers (mark 一覧)
backend の選択・保存関数                  → 04 backends
CSV / DataFrame を列名で                 → 05 dataframe (df |>> …)
回帰・GLM・HBM を描く                     → 06 analyze (analyze 連携)
3D 応答曲面 (surface / 床面 contour)     → 07 3d (hgg-3d)
連続値 gradient / scale 制御             → 03 decoration (scale) + Resolver
新 backend / mark に無い描画             → 付録 C (ライブラリ拡張)
```

## B: ggplot2 ユーザの方へ {#appendix-ggplot}

ggplot2 からの逐語的な移行 (関数の対応表) は
**[ggplot2 → hgg 移行ガイド](migration-from-ggplot.md)** に分離した。 思想面・matplotlib も
含む比較は [comparison.md](comparison.md) を参照。 最も近い類縁である **Vega-Lite との比較**
(宣言的 spec・統計連携・多パネル合成 `subplots`・WLS/HBM など) は
[comparison-vega-lite.md](comparison-vega-lite.md) を参照。

## C: ライブラリを拡張する人へ (backend / 新 mark) {#appendix-extend}

通常の作図では不要。 新しい出力 backend を書く、 既存 mark に無い描画をする等、
**ライブラリ自体を拡張する**ときだけ読む。

- **backend を書く** = `Hgg.Plot.Render` の
  `renderToPrimitives :: Resolver -> Layout -> VisualSpec -> [Primitive]` で spec を幾何プリミティブ列
  (`PLine` / `PRect` / `PCircle` / `PPath` / `PText` / `PClipPush`/`PClipPop` / `PTransformPush`/`PTransformPop`)
  に落とし、 それを対象フォーマットへ**畳む**だけ。 **雛形 = `hgg-svg` の `Backend/SVG.hs`**
  (`renderToPrimitives` → `primsToSvg` で文字列化し header/footer で包む)。 Primitive 列 → SVG の既製
  helper `renderPrimitivesSVG` / `savePrimitivesSVG` も export 済。
- **`Primitive` を直接組む** = backend に無い特殊描画をするとき手で構築する。
- 抽象の全体像は [`docs/modules.md`](../modules.md) (`Hgg.Plot.Render` の行) を参照。

## D: 網羅 API 早見表 (本文の topic ページに未掲載の export) {#appendix-coverage}

本文 ([01](01-quickstart.md)〜[07](07-3d.md)) は代表 API を例つきで示すが、 公開 export には
本文で扱いきれない追加設定・builder・型がある。 ここに **本文未登場の export を機械監査
(`scripts/api-coverage-audit.py`) で洗い出し**、 型シグネチャ主体で網羅する (シグネチャは
ソースから抽出した実値)。 設定の最終的な真実は各モジュールのソース。

### 追加 mark / 集計 (`Layer`)

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `geomQQ` | `ColRef -> Layer` | QQ プロット |
| `bin2dCount` | `ColRef -> ColRef -> Layer` | (x,y) を 2D bin して個数で塗る |
| `binCount` | `Int -> Layer` | ヒストグラム/bin2d の bin 数 |
| `contourFilled` | `ColRef -> ColRef -> ColRef -> Layer` | 塗り等高線 |
| `contourLevels` / `contourBreaks` | `Int -> Layer` / `[Double] -> Layer` | 等値線の本数 / 明示水準 |
| `countXY` | `ColRef -> ColRef -> Layer` | (x,y) 出現数の集計 |
| `forestNull` | `Double -> Layer` | forest の帰無線位置 |
| `autocorrMaxLag` | `Int -> Layer` | autocorr の最大ラグ |

DAG ([02 mark 一覧](02-layers.md#marks) の `dag`/`dagFromLists`) の低レベル構築子:
`dagNode :: Text -> Text -> DAGNodeKind -> Double -> Double -> DAGNode` (id・label・種別・x・y)、
`dagNodeDist` (分布名つき)、 `dagEdge :: Text -> Text -> DAGEdge`。

### mark encoding / 分布オプション (`Layer`)

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `densityFill` | `Bool -> Layer` | density を塗りつぶす |
| `histBorder` | `Bool -> Layer` | ヒストグラム枠線 on/off |
| `histogramDensity` | `Bool -> Layer` | 度数→密度スケール |
| `jitterX` / `jitterY` | `Double -> Layer` | 軸方向ジッタ幅 |
| `connectColor` | `Text -> Layer` | 接続線の色 |
| `connectGroup` | `ColRef -> Layer` | 接続線の群 |
| `connectWidth` | `Double -> Layer` | 接続線の幅 |
| `shapeMapEntry` | `Text -> MarkShape -> Layer` | カテゴリ→形の対応 1 件 |
| `edge` | `Text -> Layer` | DAG エッジ指定 |

### scale / palette

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `okabeIto` / `tolBright` / `brewerDark2` / `brewerSet2` | `[Text]` | 既製カラーパレット (色 hex 列) |
| `viridisStops3D` | `[Text]` | viridis colormap の色停止列 (3D colormap 既定) |
| `orderedCats` | `[Text] -> [Text]` | カテゴリ順の正規化 |
| `themeSeriesPalette` | `ThemeName -> [Text]` | テーマ既定の系列色 |

### 軸・座標 (`AxisSpec` builder / 第 2 軸)

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `axisBreak` | `Double -> Double -> AxisSpec` | 軸の break 区間 |
| `axisBreaksLabeled` | `[(Double, Text)] -> AxisSpec` | 目盛位置 + ラベル |
| `axisTickLabels` | `[Text] -> AxisSpec` | tick ラベル明示 |
| `axisFormat` | `AxisFormat -> AxisSpec` | 数値フォーマット |
| `axisRotate` | `Double -> AxisSpec` | tick ラベル回転角 |
| `linearAxis` | `AxisSpec` | 線形軸 (既定) |
| `coordCartesianX` / `coordCartesianY` | `Double -> Double -> VisualSpec` | 軸方向のズーム範囲 (データは捨てない) |
| `reverseY` | `VisualSpec` | y 軸反転 (`reverseX` の y 版) |
| `yAxisRight` | `AxisSpec -> VisualSpec` | 第 2 (右) y 軸 |
| `toLeftY` / `toRightY` | `Layer` | layer を左/右 y 軸へ割当 |

### 凡例

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `legendNcol` / `legendNrow` | `Int -> VisualSpec` | 凡例の列数 / 行数 |
| `legendReverse` | `VisualSpec` | 凡例の並び逆転 |
| `guideColorNone` | `VisualSpec` | 色凡例を隠す |

### theme 要素

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `themeAxisLine` | `Bool -> VisualSpec` | 軸線 (下/左) on/off |
| `themeStrip` / `stripFill` | `Bool -> VisualSpec` / `Text -> VisualSpec` | facet strip 表示 / 背景色 |
| `panelBorder` | `Bool -> VisualSpec` | panel.border on/off |

> font setter (`titleFont` 系) とその `ThemeOverride` 版 (`themeTitleFont`/`themeAxisLabelFont`/
> `themeTickFont`/`themeLegendFont`)・空の `emptyFontSpec` は [03 theme](03-decoration.md#theme) を参照。

### 注釈 / inset / marginal

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `annotText` | `Double -> Double -> Text -> VisualSpec` | テキスト注釈 (`annotate` の構築子) |
| `annotArrow` / `annotLine` | `Double -> Double -> Double -> Double -> VisualSpec` | 矢印 / 線注釈 |
| `annotRect` | `Double -> Double -> Double -> Double -> Text -> VisualSpec` | 矩形注釈 |
| `insetAt` / `insetElement` | `Double -> Double -> Double -> Double -> VisualSpec -> VisualSpec` | 図中図 (位置 + 中身) |
| `marginal` | `VisualSpec` | 周辺分布 (`marginalX`/`marginalY` の基底) |

### facet / subplot helper

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `freeScaleX` / `freeScaleY` | `FacetScales -> Bool` | scale 自由化の述語 |
| `freeSpaceX` / `freeSpaceY` | `FacetSpace -> Bool` | space 自由化の述語 |
| `selectedSubplots` | `VisualSpec -> [VisualSpec]` | `selectPanels` 適用後の panel 取り出し |
| `bakeSpec` | `Resolver -> VisualSpec -> VisualSpec` | spec に Resolver を焼き込む (subplot/HBM で使用) |
| `applyDiscreteLimits` | `Resolver -> VisualSpec -> VisualSpec` | 離散 limits を解決 |

### backend (低レベル save / render)

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `renderSVGInteractive` | `Resolver -> VisualSpec -> Text` | `saveSVGInteractive` の Text 版 |
| `savePrimitivesPDF` | `FilePath -> Int -> Int -> [Primitive] -> IO ()` | Primitive 列 → PDF (`savePrimitivesSVG` の PDF 版) |
| `savePrimitivesPNG` | `PNGConfig -> FilePath -> Int -> Int -> [Primitive] -> IO ()` | Primitive 列 → PNG |
| `plotDF` | `FilePath -> DataFrame -> VisualSpec -> IO ()` | DataFrame + spec を直接保存 (Hackage `dataframe`) |

### 3D 追加 (`Layer3D` / `VisualSpec3D`・[07 3d](07-3d.md))

| 関数 | シグネチャ | 補足 |
|---|---|---|
| `line3D` / `line3DPoints` | `ColRef×3 -> Layer3D` / `[Point3] -> Layer3D` | 3D 折れ線 |
| `wireframe3D` | `[Point3] -> [(Int,Int)] -> Layer3D` | ワイヤフレーム |
| `bar3DPoints` | `[Point3] -> Layer3D` | 点列から 3D 棒 |
| `shaded3D` | `Bool -> Layer3D` | 面のシェーディング |
| `edgeOn` / `edgeColor3D` / `edgeWidth` | `Layer3D` / `Text→` / `Double→` | 面のエッジ表示・色・幅 |
| `width3D` / `colormapWith3D` / `floorContourLevels3D` | `Double→` / `[Text]→` / `Int→` Layer3D | 線幅 / colormap 明示 / 床 contour 本数 |
| `axes3D` / `autoAxes3D` / `projection` | `Axes3D→VisualSpec3D` / `[Layer3D]→Axes3D` / `Projection3D→VisualSpec3D` | 軸・投影の指定 |
| `width3DV` / `height3DV` | `Int -> VisualSpec3D` | 出力 px サイズ |
| `cameraFront` / `cameraSide` | `Double -> Camera3D` | 視点 preset (`cameraIso`/`cameraTop` と同系) |
| `defaultCameraZUp` / `defaultCameraYUp` / `defaultPerspective` | `Double -> Camera3D` / `Projection3D` | 既定カメラ・投影 |
| `unBound3D` | `BoundPlot3D -> (Resolver, VisualSpec3D)` | 3D 束ねの分解 |
| `yUp` / `zUp` | `Vec3` | 上方向ベクトル (既定 z-up) |

### 型 (内部表現・公開構築子/setter は上表/本文)

`AnnotCoord` / `Annotation` / `AxisBreak` / `AxisFormat` / `AxisKind` / `AxisSpec` / `ColorEnc` /
`ConnectSpec` / `DAGNode` / `DAGEdge` / `DAGPlate` / `DAGSpec` / `DAGNodeKind` / `DAGLayoutAlgorithm` /
`Inset` / `Labs` / `LegendSpec` / `MarginalKind` / `MarginalSpec` / `MarkShape` / `ShapeMapEntry` /
`ThemeOverride` / `YAxisSide` / `BindableSpec` / `Categorical` / `BarStyle3D` ─ これらは spec の
内部表現型。 利用時は対応する setter/builder (本文 + 上表) を使う。 `Vec3` / `Mat4` / `Camera3D` /
`Projection3D` / `PNGConfig` / `PNGFonts` は 3D / PNG backend の構成型。

> **監査の自動化**: `scripts/api-coverage-audit.py` が公開モジュールの export を抽出し、
> 本ガイド全文に各シンボルが登場するか照合する。 上表の追加で **user-facing シンボルの
> 漏れはゼロ** (内部の accessor `*Of` / 数学ヘルパ `*V3` / `resolve*` / `layerTo*` /
> render 内部関数は user-facing 対象外として除外)。

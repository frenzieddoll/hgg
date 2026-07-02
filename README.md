# hgg — a grammar of graphics for Haskell

Haskell ネイティブな宣言型の作図ライブラリ。 ggplot2 / Vega-Lite と同じ
**grammar of graphics** の発想で、 図を `purePlot <> layer (mark …) <> 設定 …` の
**モノイド合成**で組み立てます。 統計ライブラリ
[**hanalyze**](https://hackage.haskell.org/package/hanalyze) と対 (hanalyze = 解析 /
hgg = 可視化) で、 回帰・GLM・GP・生存・時系列・ベイズ (HBM) など fit 済みモデルを
そのまま図に重ねることを目的としている。

> **状態: ドキュメント先行公開 (コードは順次公開予定)**。 本リポジトリは現在 **API リファレンス + チュートリアル等のドキュメント**が主体で、 ライブラリ本体 (Haskell パッケージ) は追って公開します。 以下のインストール/ビルド手順は公開後の想定です。

<img src="docs/tutorials/01-visualize/09-final.svg" width="560" alt="penguins の体重とフリッパー長を種で色分け・形分けし回帰直線を重ねた完成図">

## ギャラリー

<table>
  <tr>
    <td><img src="docs/images/readme/line.svg" width="210" alt="関数の線"></td>
    <td><img src="docs/images/readme/scatter.svg" width="210" alt="散布図"></td>
    <td><img src="docs/images/readme/histogram.svg" width="210" alt="ヒストグラム"></td>
    <td><img src="docs/images/readme/density.svg" width="210" alt="密度"></td>
  </tr>
  <tr>
    <td><img src="docs/images/readme/boxplot.svg" width="210" alt="箱ひげ"></td>
    <td><img src="docs/images/readme/violin.svg" width="210" alt="バイオリン"></td>
    <td><img src="docs/images/readme/contour.svg" width="210" alt="等高線"></td>
    <td><img src="docs/images/readme/heatmap.svg" width="210" alt="ヒートマップ"></td>
  </tr>
  <tr>
    <td><img src="docs/images/readme/hexbin.svg" width="210" alt="六角ビニング"></td>
    <td><img src="docs/images/readme/quiver.svg" width="210" alt="ベクトル場"></td>
    <td><img src="docs/images/readme/bar.svg" width="210" alt="積み上げ棒"></td>
    <td><img src="docs/images/readme/pie.svg" width="210" alt="円グラフ"></td>
  </tr>
  <tr>
    <td><a href="docs/api-guide/04-decoration.md#facet"><img src="docs/tutorials/01-visualize/24-facet-island.svg" width="210" alt="ファセット"></a></td>
    <td><img src="docs/images/readme/distcols.svg" width="210" alt="distCols 併置"></td>
    <td><img src="docs/images/readme/subplots.svg" width="210" alt="subplots 貼り合わせ"></td>
    <td><img src="docs/images/readme/surface3d.svg" width="210" alt="3D 応答曲面"></td>
  </tr>
  <tr>
    <td><img src="docs/images/readme/hbm-hier-dag.svg" width="210" alt="階層ベイズモデルの DAG"></td>
  </tr>
</table>

上は主要な geom の一覧です (ファセット図のみ [R4DS チュートリアル](docs/tutorials/01-visualize/README.md)由来)。 API の正式リファレンスは [api-guide](docs/api-guide/02-layers.md) に。
penguins の全 24 図と再現コードは [R for Data Science 第 1 章](docs/tutorials/01-visualize/README.md) に。

## インストール

cabal の `build-depends` に backend パッケージを足します (core は依存で入ります)。

```
build-depends: hgg-svg          -- core + SVG backend
             , hgg-dataframe     -- (任意) DataFrame 連携
```

PDF は `hgg-pdf`、 PNG (日本語フォント可) は `hgg-rasterific`、
Jupyter inline は `hgg-ihaskell`。

## クイックスタート

最短は 1 行 (`Hgg.Plot.Quick`・データ以外は何も決めずに 1 枚)。

```haskell
import Hgg.Plot.Quick

main :: IO ()
main = quickScatter "scatter.svg" [1,2,3,4,5] [1,4,9,16,25]
```

飾りを足すなら `Hgg.Plot.Easy` (値直渡し + `overlay`)。

```haskell
import Hgg.Plot.Easy
import Hgg.Plot.Backend.SVG (saveSVG)
import Hgg.Plot.Unit (px, (*~))

main :: IO ()
main = saveSVG "easy.svg" $
     overlay [ points [1,2,3,4,5] [1,4,9,16,25] ]
  <> title "y = x²" <> xLabel "x" <> yLabel "y"
  <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
```

データを **列名**で扱うなら `|>>` でデータ源を束ねます (これが本命の書き方)。 列をもつ値
(下は inline の `[(列名, ColData)]`・`hgg-dataframe` なら `DataFrame` をそのまま渡せる)
を `|>>` の左に置き、 右の spec では `scatter "x" "y"` のように **列名**で参照します。
`|>>` は `<>` より結合が弱いので、 複数 `layer` を重ねても外側の括弧は要りません。 実データ
(palmerpenguins の CSV) を `|>>` で扱う例は次節。

```haskell
import Hgg.Plot.Spec
import Hgg.Plot.Frame       ((|>>))
-- import Hgg.Plot.DataFrame       ((|>>))
import Hgg.Plot.Backend.SVG (saveSVGBound)
import qualified Data.Vector as V
import Data.Text (Text)

main :: IO ()
main = saveSVGBound "bound.svg" $
     cols |>> layer (scatter "x" "y")
  <> title "y = x²" <> xLabel "x" <> yLabel "y"
  where
    cols = [ ("x", NumData (V.fromList [1,2,3,4,5]))
           , ("y", NumData (V.fromList [1,4,9,16,25])) ] :: [(Text, ColData)]
```
<img src="docs/images/readme/quickstart.svg" width="420" alt="y = x² の散布図">


## 文法のさわり

図は空の `purePlot` に `layer (mark …)` を `<>` で重ねて作ります。 データは `|>>` で束ね、
色・形は mark の中で `colorBy`/`shapeBy` で与えます (以下 `raw` は palmerpenguins。
`raw <- DF.readCsv "penguins.csv"`)。

**1. 散布図** — 列名を与えた `scatter` mark が軸と点を生みます。

```haskell
saveSVGBound "04-scatter.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

<img src="docs/tutorials/01-visualize/04-scatter.svg" width="420" alt="散布図">

**2. 種で色分け** — mark に `colorBy "species"` を足す。

```haskell
saveSVGBound "05-color.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

<img src="docs/tutorials/01-visualize/05-color.svg" width="420" alt="色分け散布図">

**3. 回帰直線とラベルを重ねて完成** — レイヤや装飾を `<>` で足していく。

```haskell
saveSVGBoundStats "09-final.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> palette okabeIto
      <> title "Body mass and flipper length"
      <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
      <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
      <> legendTitle "Species"
      <> theme ThemeGrey
```

<img src="docs/tutorials/01-visualize/09-final.svg" width="420" alt="完成図">

ステップごとの全解説は [R for Data Science 第 1 章](docs/tutorials/01-visualize/README.md)。

## できること

- **layer / mark の宣言型 API** — 散布・線・棒・ヒスト・箱ひげ・violin・density・band・
  forest・heatmap・contour・vector field・DAG・MCMC 診断 など
- **DataFrame 連携** — `df |>> layer (scatter "x" "y")` と列名で書ける (NA は自動除外 = `na.rm`)
- **backend** — SVG / PDF / PNG (日本語フォント可) / Jupyter (iHaskell) inline
- **3D** — 応答曲面 (RSM)・汎用 3D プロット (CPU 投影)
- **統計連携** — `toPlot` / `statLm` / HBM 抽出子で hanalyze の fit 済みモデルをそのまま描画
- **装飾一式** — theme / scale / facet / subplot / 座標系・参照線・凡例 (ggplot 同型)

## 設計原則

- **backend 非依存の core** — `hgg-core` は base / vector / text / containers のみ依存。
  描画先 (SVG/PDF/PNG) は別 package。
- **宣言型・純関数** — 図は純粋値 `VisualSpec`。 副作用は最後の `saveSVG` だけ。
  部分 spec を値として再利用できる。
- **2 階層 Monoid API** — `Layer` (mark・見た目) + `VisualSpec` (タイトル・テーマ・facet)。
  ggplot 同型の `<>` 合成で、 型を見れば部品の置き場所が決まる。

## ドキュメント

- 📚 **[API リファレンス](docs/api-guide/README.md)** — topic 別 (quickstart / layers / decoration /
  backends / dataframe / analyze / 3d / appendix)
- 📗 **[チュートリアル: R for Data Science 第 1 章](docs/tutorials/01-visualize/README.md)** —
  解説 → コード → 図 の walkthrough (全 24 図)

## パッケージ

| Package | 役割 |
|---|---|
| `hgg-core` | Spec / Layout / Render / Palette |
| `hgg-svg` | SVG backend |
| `hgg-pdf` | PDF backend |
| `hgg-rasterific` | PNG backend (日本語フォント可) |
| `hgg-frame` | `class PlotData` + `df \|>> spec` バインド |
| `hgg-dataframe` | dataframe → Resolver bridge |
| `hgg-3d` | 3D plot (CPU 投影) |
| `hgg-ihaskell` | iHaskell (Jupyter) inline display |

## ビルド (ソースから)

```bash
cabal build all
cabal test all
```

GHC 9.6.7 で開発、 GHC 9.4 / 9.6 / 9.8 を目標。

## ライセンス

[BSD-3-Clause](LICENSE)。

-- | チュートリアル 09: 層 (R4DS 2e Ch9 "Layers")
--   https://r4ds.hadley.nz/layers
--
--   R4DS 第 9 章 "Layers" が **表示する図を、 順番どおり・全数** 再現する。 本章は
--   Visualize パートの中核で、 layered grammar of graphics を深掘りする
--   (aesthetic mappings / geometric objects / facets / statistical transformations /
--    position adjustments / coordinate systems)。 データは主に **mpg** (234 台)、
--   §9.5/§9.7 は **diamonds** (53,940 個)。
--
--   ・忠実性メモ (= 実測で確認した R4DS との差異。 近似/置換せず honest 記録):
--     - geom_smooth: R 既定は loess (n<1000)。 hgg の statSmooth は **B-spline**
--       平滑 (knot 数 6)。 曲線形状はおおむね一致するが loess とビット一致はしない。
--     - stat の群分割は **color aesthetic のみ**で駆動 (Bridge.Stat.groupColumn が
--       ColorByCol のみ判定)。 R の「linetype=drv で 3 本」「group=drv で灰色 3 本」は
--       未対応 → grouped smooth は color 版で代表させ、 README に honest 記録。
--     - 26 種の R pch shape 参照図 (fig-shapes) は R 内部仕様。 hgg の MarkShape は
--       8 種なので、 使える 8 種の一覧図に置換し honest 記録。
--     - §9.7 の地図 (map_data("nz") + geom_polygon + coord_quickmap) は未実装
--       (R4DS 自身「本書では地図を扱わない」)。 coord_quickmap は概念のみ、 Coxcomb は
--       既存 coordPolar で再現。
--     - alpha を変数にマップする aesthetic は未対応 (R も「discrete に alpha は非推奨」)。
--     - bar の color(枠) と fill(面) は hgg では分離せず color=面 → §9.6 の
--       「color vs fill」 は 1 図 + honest 記録。
--
--   DataFrame 変換は dataframe の `|>` 前方パイプ。 集計済の件数を bar に渡す
--   (geom_bar の stat_count 相当は groupBy+countAll で先に行う。 値は不変)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Data.List                (sort)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified DataFrame.IO.CSV                     as DF
import qualified DataFrame.Internal.DataFrame         as DF
import qualified DataFrame.Operations.Aggregation     as DF
import qualified DataFrame.Operations.Core            as DF
import qualified DataFrame.Operations.Subset          as DF
import qualified DataFrame.Operations.Transformations as DF
import qualified DataFrame.Functions      as F
import           DataFrame.Operators      ((|>))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)
import           Hgg.Plot.DataFrame   ()

-- ggplot geom_smooth の既定線色 (単一回帰線のとき)。
smoothBlue :: Color
smoothBlue = fromHex "#3366FF"

-- diamonds の factor 水準順 (R の ordered factor)。 既定アルファベット順だと崩れるので
-- scaleXDiscreteLimits / colorCats で明示する。
cutOrder :: [Text]
cutOrder = ["Fair", "Good", "Very Good", "Premium", "Ideal"]

clarityOrder :: [Text]
clarityOrder = ["I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"]

-- 中央値 (ソート後の中央要素 / 偶数なら 2 要素平均)。 R の median と同義。
median :: [Double] -> Double
median [] = 0
median xs =
  let s = sort xs; n = length s
  in if odd n then s !! (n `div` 2)
     else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2

main :: IO ()
main = do
  mpg0     <- DF.readCsv "../_data/mpg.csv"
  diamonds <- DF.readCsv "../_data/_raw/diamonds.csv"

  -- facet 用に cyl (Int) を Text 列 cyl_f にする (R は cyl を離散ラベル "4".."8" で表示。
  -- NumData を facet すると show が "4.0" になるため Text 化で R 同等のラベルにする)。
  let mpg = mpg0 |> DF.derive "cyl_f"
                      (F.lift (T.pack . show :: Int -> Text) (F.col @Int "cyl"))

  -- =========================================================================
  -- §9.2 Aesthetic mappings (mpg: displ vs hwy)
  -- =========================================================================

  -- R4DS L71: aes(color=class) = class で色分け (ggplot 既定 hue)。
  saveSVGBound "01-aes-color.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> colorBy "class" <> alpha 0.9)
          <> xLabel "displ" <> yLabel "hwy" <> legendTitle "class"

  -- R4DS L75: aes(shape=class) = class で形状分け。 R は shape を最大 6 種に制限し
  --   7 番目 (suv) を描かない (警告)。 hgg の MarkShape は 8 種なので 7 class
  --   すべてに形状が付く (= ここは R より多く描ける。 honest 記録)。
  saveSVGBound "02-aes-shape.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> shapeBy "class" <> alpha 0.9)
          <> xLabel "displ" <> yLabel "hwy" <> legendTitle "class"

  -- R4DS L104: aes(size=class) = class を点サイズにマップ (順序を含意するので非推奨)。
  saveSVGBound "03-aes-size.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> sizeBy "class" <> alpha 0.6)
          <> xLabel "displ" <> yLabel "hwy" <> legendTitle "class"

  -- R4DS L108: aes(alpha=class)。 alpha を変数にマップする aesthetic は hgg 未対応
  --   (R も discrete への alpha は非推奨)。 → honest 記録 (README §9.2)。 図は省略しない
  --   代わりに size 版で「順序 aesthetic にカテゴリをマップする」 例示を担う。

  -- R4DS L130: geom_point(color="blue") = aes 外で色を固定 (全点青)。
  saveSVGBound "04-aes-blue.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> color (fromHex "#0000ff") <> alpha 0.9)
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS fig-shapes (L165): R の 26 種 pch 参照図。 hgg の MarkShape は 8 種
  --   (circle/square/triangle/cross/spade/heart/club/diamond)。 使える 8 種の一覧図に
  --   置換し honest 記録。 トランプのスーツ (spade/heart/club/diamond) は hgg 独自
  --   拡張で、 形は htdebeer/SVG-cards (public domain) 由来。 diamond は通常の菱形を
  --   廃しトランプ型のみ、 heart は上下反転 (ユーザ要望)。
  let shapeNames = ["circle","square","triangle","cross","spade","heart","club","diamond"] :: [Text]
      shapeXs    = [0 .. 7] :: [Double]
      shapeYs    = replicate 8 (1.0 :: Double)
      -- shapeBy は内部でカテゴリをアルファベット順に形状へ割り当てるため、 ラベルと形状が
      -- ずれる。 shapeMapEntry で各名前を対応する MarkShape に固定する。
      shapePins  = mconcat (zipWith shapeMapEntry shapeNames
                     [MShCircle, MShSquare, MShTriangle, MShCross
                     , MShSpade, MShHeart, MShClub, MShDiamond])
  saveSVGBound "05-shapes.svg" $
    DF.empty |>>
        theme ThemeGrey
     <> layer (scatter (inline shapeXs) (inline shapeYs)
                 <> shapeBy (inlineCat shapeNames) <> shapePins
                 <> color (fromHex "#ff0000") <> size 12)
     <> layer (text (inline shapeXs) (inline (map (subtract 0.18) shapeYs))
                 (inlineCat shapeNames))
     <> yAxis hideTicks <> xAxis hideTicks
     <> xLabel "" <> yLabel ""
     <> coordCartesianX (-0.7) 7.7   -- 両端ラベルがはみ出ないよう表示域を広げる
     <> widthMm 220                  -- 8 ラベルが重ならないよう図幅を拡張 (既定 165mm)
     <> legendOff   -- 形ギャラリー図ゆえ shape 凡例は不要 (Phase 35 shape-only 凡例を抑制)
     <> title "hgg の 8 shapes (R の pch 26 種に対する honest 版)"

  -- =========================================================================
  -- §9.3 Geometric objects
  -- =========================================================================

  -- R4DS L225/243: geom_point。
  saveSVGBound "06-geom-point.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS L228/248: geom_smooth = 平滑曲線 + 信頼帯。 R は loess、 ここは B-spline (honest)。
  saveSVGBoundStats "07-geom-smooth.svg" $
    mpg |>> theme ThemeGrey <> layer (statSmoothCI "displ" "hwy" 6 <> color smoothBlue)
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS L327: geom_smooth(aes(color=drv)) = drv で 3 本に分かれた平滑曲線。
  --   stat の群分割は color aesthetic で駆動 (= R の color=drv 版に対応)。
  saveSVGBoundStats "08-smooth-color-drv.svg" $
    mpg |>> theme ThemeGrey <> layer (statSmoothCI "displ" "hwy" 6 <> colorBy "drv")
          <> xLabel "displ" <> yLabel "hwy" <> legendTitle "drv"

  -- R4DS L291: 2 つの geom を重畳 + 局所マッピング。 R は point(color=drv) + smooth(linetype=drv)。
  --   ここは smooth も color=drv で群分割 (linetype 群分割は未対応 → 3 本を色で区別、 honest)。
  saveSVGBoundStats "09-point-smooth-drv.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> colorBy "drv" <> alpha 0.9)
          <> layer (statSmoothCI "displ" "hwy" 6 <> colorBy "drv")
          <> xLabel "displ" <> yLabel "hwy" <> legendTitle "drv"

  -- R4DS L343: 局所マッピングの典型。 geom_point(aes(color=class)) + 全体 1 本の geom_smooth。
  saveSVGBoundStats "10-point-class-smooth.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> colorBy "class" <> alpha 0.9)
          <> layer (statSmoothCI "displ" "hwy" 6 <> color smoothBlue)
          <> xLabel "displ" <> yLabel "hwy" <> legendTitle "class"

  -- R4DS L357: 局所 data。 全点 + 2seater を赤点と赤い中抜き円で強調。
  --   Phase 34 で `hollow` (= ggplot shape="circle open") を実装し、 3 層目を
  --   塗り透明 + 赤 stroke の輪 (size=外径・stroke=線幅) にして R4DS 同型に。
  let twoSeater = mpg |> DF.filterBy (== ("2seater" :: Text)) (F.col @Text "class")
  saveSVGBound "11-2seater.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
          <> layer (scatter (inline (DF.columnAsList (F.col @Double "displ") twoSeater))
                            (inline (map fromIntegral (DF.columnAsList (F.col @Int "hwy") twoSeater)))
                     <> color (fromHex "#ff0000"))
          <> layer (scatter (inline (DF.columnAsList (F.col @Double "displ") twoSeater))
                            (inline (map fromIntegral (DF.columnAsList (F.col @Int "hwy") twoSeater)))
                     <> color (fromHex "#ff0000") <> hollow <> size 9 <> stroke 1.2)
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS L380/388: 1 変数 hwy の分布を geom 違いで見る (histogram / density / boxplot)。
  saveSVGBound "12-histogram.svg" $
    mpg |>> theme ThemeGrey <> layer (histogram "hwy" <> binWidth 2)
          <> xLabel "hwy" <> yLabel "count"
  saveSVGBound "13-density.svg" $
    mpg |>> theme ThemeGrey <> layer (density "hwy")
          <> xLabel "hwy" <> yLabel "density"
  saveSVGBound "14-boxplot.svg" $
    mpg |>> theme ThemeGrey <> layer (boxplot "hwy")
          <> xLabel "" <> yLabel "hwy"

  -- R4DS L407: ggridges geom_density_ridges(drv)。 drv ごとの density を縦に積む。
  saveSVGBound "15-ridges.svg" $
    mpg |>> theme ThemeGrey <> layer (ridge "hwy" <> colorBy "drv" <> alpha 0.5)
          <> xLabel "hwy" <> yLabel "drv" <> legendOff

  -- =========================================================================
  -- §9.4 Facets (mpg)
  -- =========================================================================

  -- R4DS L487: facet_wrap(~cyl)。
  saveSVGBound "16-facet-wrap-cyl.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
          <> facetWrap "cyl_f" 2
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS L502: facet_grid(drv ~ cyl)。 行=drv・列=cyl の 2 次元 grid。
  saveSVGBound "17-facet-grid-drv-cyl.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
          <> facetGrid "drv" "cyl_f"
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS L519: facet_grid(drv ~ cyl, scales="free")。 行で y・列で x のスケールを自由化。
  saveSVGBound "18-facet-grid-free.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> alpha 0.9)
          <> facetGrid "drv" "cyl_f" <> facetScales FacetFree
          <> xLabel "displ" <> yLabel "hwy"

  -- =========================================================================
  -- §9.5 Statistical transformations (diamonds)
  -- =========================================================================

  -- geom_bar の stat_count 相当 = groupBy + countAll で件数を先に集計 (値は不変)。
  let byCut = diamonds |> DF.groupBy ["cut"]
                       |> DF.aggregate [ F.countAll `F.as` "n" ]

  -- R4DS L608: geom_bar(aes(x=cut)) = cut ごとの件数 (Fair 1610 .. Ideal 21551)。
  saveSVGBound "19-bar-cut.svg" $
    byCut |>> theme ThemeGrey <> layer (bar "cut" "n")
            <> scaleXDiscreteLimits cutOrder
            <> xLabel "cut" <> yLabel "count"

  -- R4DS L663: count(cut) |> geom_bar(stat="identity") = 既に集計済の n を棒高に直接。
  --   hgg の bar は元々「集計済 y を高さに」 取るので、 19 と同じ集計を y=n で描く。
  saveSVGBound "20-bar-identity.svg" $
    byCut |>> theme ThemeGrey <> layer (bar "cut" "n")
            <> scaleXDiscreteLimits cutOrder
            <> xLabel "cut" <> yLabel "n"

  -- R4DS L677: after_stat(prop) = 件数でなく割合を棒高に。 prop = n / 総数。
  let total  = fromIntegral (fst (DF.dimensions diamonds)) :: Double
      byProp = byCut |> DF.derive "prop"
                          (F.lift (\k -> fromIntegral k / total :: Double) (F.col @Int "n"))
  saveSVGBound "21-bar-prop.svg" $
    byProp |>> theme ThemeGrey <> layer (bar "cut" "prop")
             <> scaleXDiscreteLimits cutOrder
             <> xLabel "cut" <> yLabel "prop"

  -- R4DS L693: stat_summary(depth; min/max/median)。 cut ごとに depth の最小〜最大の縦線 +
  --   中央値の点。 median は dataframe に集約が無いので cut ごとに抽出して Haskell で厳密計算。
  let depthStats =
        [ let sub = diamonds |> DF.filterBy (== c) (F.col @Text "cut")
              ds  = DF.columnAsList (F.col @Double "depth") sub
          in (minimum ds, median ds, maximum ds)
        | c <- cutOrder ]
      mids   = [ (lo + hi) / 2 | (lo, _, hi) <- depthStats ]
      halves = [ (hi - lo) / 2 | (lo, _, hi) <- depthStats ]
      meds   = [ md | (_, md, _) <- depthStats ]
      cutXs  = [0 .. fromIntegral (length cutOrder - 1)] :: [Double]
  -- lineRange / scatter は連続 x を取るので、 cut を 0..4 の数値位置にして
  -- x 軸目盛ラベルを cut 名に差し替える (categorical x は range geom 非対応のため)。
  saveSVGBound "22-stat-summary.svg" $
    DF.empty |>>
        theme ThemeGrey
     <> layer (lineRange (inline cutXs) (inline mids) (inline halves) <> stroke 1.5)
     <> layer (scatter (inline cutXs) (inline meds) <> size 7)
     <> xAxis (axisBreaksLabeled (zip cutXs cutOrder))
     <> xLabel "cut" <> yLabel "depth"

  -- =========================================================================
  -- §9.6 Position adjustments
  -- =========================================================================

  let byDrv      = mpg |> DF.groupBy ["drv"]
                       |> DF.aggregate [ F.countAll `F.as` "n" ]
      byDrvClass = mpg |> DF.groupBy ["drv", "class"]
                       |> DF.aggregate [ F.countAll `F.as` "n" ]

  -- R4DS L746/750: geom_bar(color=drv) と geom_bar(fill=drv)。 hgg は bar の
  --   color(枠) と fill(面) を分離せず color=面色 → 両者は同一図になる (honest 記録)。
  --   ここは fill 相当の 1 図を出す。
  saveSVGBound "23-bar-fill-drv.svg" $
    byDrv |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "drv")
            <> xLabel "drv" <> yLabel "count" <> legendOff

  -- R4DS L764: geom_bar(x=drv, fill=class) = 既定 (stack) で class を積み上げ。
  saveSVGBound "24-bar-stack-class.svg" $
    byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosStack)
                 <> xLabel "drv" <> yLabel "count" <> legendTitle "class"

  -- R4DS L787: position="identity" + alpha = 重なりを半透明で見せる。
  saveSVGBound "25-bar-identity.svg" $
    byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosIdentity <> alpha 0.2)
                 <> xLabel "drv" <> yLabel "count" <> legendTitle "class"

  -- R4DS L818: position="fill" = 各 drv の合計を 1 に揃え割合比較。
  saveSVGBound "26-bar-fill.svg" $
    byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosFill)
                 <> xLabel "drv" <> yLabel "count" <> legendTitle "class"

  -- R4DS L822: position="dodge" = 横に並べて個別値を比較。
  saveSVGBound "27-bar-dodge.svg" $
    byDrvClass |>> theme ThemeGrey <> layer (bar "drv" "n" <> colorBy "class" <> position PosDodge)
                 <> xLabel "drv" <> yLabel "count" <> legendTitle "class"

  -- R4DS L835: 素の散布図 (overplotting: 234 点中 126 点しか見えない)。
  saveSVGBound "28-scatter-overplot.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy")
          <> xLabel "displ" <> yLabel "hwy"

  -- R4DS L852: position="jitter" = 微小ノイズで重なりを散らす。
  saveSVGBound "29-jitter.svg" $
    mpg |>> theme ThemeGrey <> layer (scatter "displ" "hwy" <> jitterX 0.02 <> jitterY 0.02)
          <> xLabel "displ" <> yLabel "hwy"

  -- =========================================================================
  -- §9.7 Coordinate systems (diamonds clarity)
  -- =========================================================================

  let byClarity = diamonds |> DF.groupBy ["clarity"]
                           |> DF.aggregate [ F.countAll `F.as` "n" ]

  -- R4DS L927: 素の bar (clarity)。 Coxcomb の元になる棒グラフ。
  saveSVGBound "30-bar-clarity.svg" $
    byClarity |>> theme ThemeGrey <> layer (bar "clarity" "n" <> colorBy "clarity" <> colorCats clarityOrder)
                <> scaleXDiscreteLimits clarityOrder
                <> xLabel "clarity" <> yLabel "count" <> legendOff

  -- R4DS L935: bar + coord_flip() = 横棒。
  saveSVGBound "31-coord-flip.svg" $
    byClarity |>> theme ThemeGrey <> layer (bar "clarity" "n" <> colorBy "clarity" <> colorCats clarityOrder)
                <> scaleXDiscreteLimits clarityOrder
                <> coordFlip
                <> xLabel "clarity" <> yLabel "count" <> legendOff

  -- R4DS L936: bar + coord_polar() = Coxcomb chart (棒グラフを極座標へ)。
  saveSVGBound "32-coord-polar.svg" $
    byClarity |>> theme ThemeGrey <> layer (bar "clarity" "n" <> colorBy "clarity" <> colorCats clarityOrder)
                <> scaleXDiscreteLimits clarityOrder
                <> coordPolar
                <> xLabel "clarity" <> yLabel "count" <> legendOff

  putStrLn "wrote 01-aes-color .. 32-coord-polar (32 SVG)"

-- | チュートリアル 05: レイヤ — グラフィックスの文法 (R4DS 2e Ch9 "Layers")
--   https://r4ds.hadley.nz/layers
--
--   mpg で「あらゆるプロット = データ + geom + aes + stat + position + 座標系 +
--   facet + theme」 という文法の各要素を一通り描く。 R4DS Ch9 の主要図を再現。
--   変換は dataframe の `|>` 前方パイプ。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Data.Text                (Text)
import qualified DataFrame                as DF
import qualified DataFrame.Functions      as F
import           DataFrame.Operators      ((|>))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)
import           Hgg.Plot.DataFrame   ()

main :: IO ()
main = do
  mpg <- DF.readCsv "mpg.csv"

  -- === 1. 美的マッピング (aes): class で色分け / 形分け ===
  saveSVGBound "01-aes-color.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "class" <> size 5 <> alpha 0.8)
        <> palette okabeIto
        <> title "aes(color = class)" <> xLabel "displ" <> yLabel "hwy"

  saveSVGBound "02-aes-shape.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> shapeBy "drv" <> color "drv" <> size 6 <> alpha 0.85)
        <> palette okabeIto
        <> title "aes(shape = drv, color = drv)" <> xLabel "displ" <> yLabel "hwy"

  -- === 1b. aes の外で色を固定 (= geom_point(color="blue")) ===
  saveSVGBound "03-manual-blue.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> colorStatic "#2c7fb8" <> size 5 <> alpha 0.8)
        <> title "geom_point(color = \"blue\")" <> xLabel "displ" <> yLabel "hwy"

  -- === 2. geom: 点 + 平滑、 drv で色/線種を分ける ===
  saveSVGBoundStats "04-point-smooth.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "drv" <> size 4 <> alpha 0.7)
        <> layer (statSmooth "displ" "hwy" 6 <> linetypeBy "drv" <> colorStatic "#444444" <> stroke 2)
        <> palette okabeIto
        <> title "geom_point() + geom_smooth(aes(linetype = drv))"
        <> xLabel "displ" <> yLabel "hwy"

  -- === 2b. 分布の geom: ヒストグラム / 箱ひげ ===
  saveSVGBound "05-histogram.svg" $
    mpg |>> layer (histogram "hwy" <> binCount 18 <> alpha 0.85)
        <> title "geom_histogram(hwy)" <> xLabel "hwy" <> yLabel "count"

  saveSVGBound "06-boxplot.svg" $
    mpg |>> layer (boxplotBy "drv" "hwy")
        <> title "geom_boxplot(drv, hwy)" <> xLabel "drv" <> yLabel "hwy"

  -- === 3. facet: wrap と grid ===
  saveSVGBound "07-facet-wrap.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> size 4 <> alpha 0.7)
        <> facetWrap "cyl" 2
        <> title "facet_wrap(~cyl)" <> xLabel "displ" <> yLabel "hwy"

  saveSVGBound "08-facet-grid.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> size 3 <> alpha 0.7)
        <> facetGrid "drv" "cyl"
        <> title "facet_grid(drv ~ cyl)" <> xLabel "displ" <> yLabel "hwy"

  -- === 4+5. stat (count) + position 調整: drv を class で積む ===
  -- bar は x,y を要求するので件数を先に集計 (= stat_count を明示)。
  let byDrvClass = mpg |> DF.groupBy ["drv","class"]
                       |> DF.aggregate [ F.count (F.col @Text "class") `F.as` "n" ]
  saveSVGBound "09-bar-stack.svg" $
    byDrvClass |>> layer (bar "drv" "n" <> color "class" <> position PosStack)
               <> palette okabeIto
               <> title "position = stack" <> xLabel "drv" <> yLabel "count"

  saveSVGBound "10-bar-dodge.svg" $
    byDrvClass |>> layer (bar "drv" "n" <> color "class" <> position PosDodge)
               <> palette okabeIto
               <> title "position = dodge" <> xLabel "drv" <> yLabel "count"

  saveSVGBound "11-bar-fill.svg" $
    byDrvClass |>> layer (bar "drv" "n" <> color "class" <> position PosFill)
               <> palette okabeIto
               <> title "position = fill" <> xLabel "drv" <> yLabel "proportion"

  -- === 5b. position = jitter (重なり回避) ===
  saveSVGBound "12-jitter.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> jitterX 0.02 <> jitterY 0.02
                     <> size 4 <> alpha 0.6)
        <> title "geom_jitter()" <> xLabel "displ" <> yLabel "hwy"

  -- === 6. 座標系: coord_flip と coord_polar ===
  saveSVGBound "13-coord-flip.svg" $
    mpg |>> layer (boxplotBy "class" "hwy")
        <> coordFlip
        <> title "boxplot + coord_flip()" <> xLabel "class" <> yLabel "hwy"

  let byClass = mpg |> DF.groupBy ["class"]
                    |> DF.aggregate [ F.count (F.col @Text "class") `F.as` "n" ]
  saveSVGBound "14-coord-polar.svg" $
    byClass |>> layer (bar "class" "n" <> color "class")
            <> coordPolar
            <> palette okabeIto
            <> title "geom_bar + coord_polar() (Coxcomb)"

  putStrLn "wrote 01 .. 14 (14 SVG)"

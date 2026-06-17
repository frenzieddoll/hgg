-- | チュートリアル 07: 伝わる図にする (R4DS 2e Ch11 "Communication")
--   https://r4ds.hadley.nz/communication
--
--   探索用の図を「人に見せる図」 に仕上げる: ラベル・注釈・凡例位置・配色・テーマ・
--   ズーム・軸目盛り。 mpg を使う。 変換は dataframe の `|>` 前方パイプ。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import qualified DataFrame                as DF
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)
import           Hgg.Plot.DataFrame   ()

main :: IO ()
main = do
  mpg <- DF.readCsv "mpg.csv"

  -- === 1. ラベル: title/subtitle/caption/軸/凡例名 (= labs(...)) ===
  saveSVGBoundStats "01-labels.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "class" <> size 4 <> alpha 0.85)
        <> layer (statSmooth "displ" "hwy" 6 <> colorStatic "#444444" <> stroke 2)
        <> palette okabeIto
        <> title "排気量が大きいほど燃費は下がる傾向"
        <> subtitle "2 シーター (スポーツカー) は軽量ゆえ例外"
        <> caption "Data from fueleconomy.gov"
        <> xLabel "Engine displacement (L)" <> yLabel "Highway fuel economy (mpg)"
        <> legendTitle "Car type"

  -- === 2. 注釈: テキスト + 矢印 (= annotate(geom="label"/"segment")) ===
  -- annotText の既定色 ("") は薄いので、 AnnText を直接構築して色 (赤) と大きさを指定。
  saveSVGBound "02-annotate.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> size 4 <> alpha 0.5)
        <> annotate (AnnText { anX = 4.7, anY = 38
                             , anText = "排気量が大きいほど燃費は下がる"
                             , anCoord = AnnotData, anColor = "#cc3333", anSize = 15 })
        <> annotArrow 3.0 35 5.2 22
        <> title "注釈で要点を添える"
        <> xLabel "displ" <> yLabel "hwy"

  -- === 3. 凡例の位置 (= theme(legend.position = "bottom")) ===
  saveSVGBound "03-legend-bottom.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "class" <> size 4 <> alpha 0.85)
        <> palette okabeIto
        <> legendPos LegendBottom
        <> title "凡例を下に" <> xLabel "displ" <> yLabel "hwy"

  -- === 4. 配色 + 形の冗長符号化 (= scale_color_brewer + shape、 色覚配慮) ===
  saveSVGBound "04-palette-shape.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "drv" <> shapeBy "drv" <> size 5 <> alpha 0.85)
        <> palette tolBright
        <> title "色 + 形で冗長に符号化 (色覚配慮)" <> xLabel "displ" <> yLabel "hwy"
        <> legendTitle "drive"

  -- === 5/6. テーマ (= theme_bw() / theme_minimal()) ===
  saveSVGBound "05-theme-bw.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "class" <> size 4 <> alpha 0.85)
        <> palette okabeIto <> theme ThemeBW
        <> title "theme_bw 相当" <> xLabel "displ" <> yLabel "hwy"

  saveSVGBound "06-theme-minimal.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "class" <> size 4 <> alpha 0.85)
        <> palette okabeIto <> theme ThemeMinimal
        <> title "theme_minimal 相当" <> xLabel "displ" <> yLabel "hwy"

  -- === 7. ズーム: データを捨てずに表示範囲だけ絞る (= coord_cartesian) ===
  --   coord_cartesian は平滑線を全データから計算したまま範囲だけ拡大する
  --   (filter で絞ると平滑線自体が変わってしまう)。
  saveSVGBoundStats "07-zoom.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "drv" <> size 4 <> alpha 0.8)
        <> layer (statSmooth "displ" "hwy" 6 <> colorStatic "#444444" <> stroke 2)
        <> palette okabeIto
        <> coordCartesianX 5 7 <> coordCartesianY 10 25
        <> title "coord_cartesian で拡大 (平滑線は全データのまま)"
        <> xLabel "displ" <> yLabel "hwy"

  -- === 8. 軸目盛りの指定 (= scale_y_continuous(breaks = seq(15,40,5))) ===
  saveSVGBound "08-axis-breaks.svg" $
    mpg |>> layer (scatter "displ" "hwy" <> color "drv" <> size 4 <> alpha 0.85)
        <> palette okabeIto
        <> yAxis (axisBreaksAt [15,20,25,30,35,40])
        <> title "y 軸目盛りを 5 刻みに" <> xLabel "displ" <> yLabel "hwy"

  putStrLn "wrote 01 .. 08 (8 SVG)"

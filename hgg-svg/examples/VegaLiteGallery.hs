-- | Vega-Lite examples gallery (https://vega.github.io/vega-lite/examples/) の
--   再現デモ。 各 Vega-Lite セクションの代表例を hgg で描き直す。
--
--   @cabal run vega-lite-gallery@ → @design/vega-lite-gallery/*.svg@ を生成。
--
--   ★描けない例 (地理 / 対話 / image = 仕様外、 concat/repeat 等 = 今後の課題) は
--     本デモには含めない (docs/comparison-vega-lite.md の分類を参照)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Hgg.Plot.Backend.SVG (saveSVG, saveSVGWith)
import           Hgg.Plot.Easy
import qualified Data.Vector              as V
import           System.Directory         (createDirectoryIfMissing)

out :: FilePath -> VisualSpec -> IO ()
out name = saveSVG ("design/vega-lite-gallery/" <> name <> ".svg")

main :: IO ()
main = do
  createDirectoryIfMissing True "design/vega-lite-gallery"

  -- == Bar Charts ==========================================================
  -- Simple Bar Chart
  out "01-bar-simple" $ purePlot
    <> layer (bar (inlineCat (["A","B","C","D","E"] :: [String])) (inline [28,55,43,91,81]))
    <> title "Simple Bar Chart" <> xLabel "category" <> yLabel "value"

  -- Grouped Bar Chart (dodge)
  let gcat = inlineCat (concatMap (replicate 3) (["A","B","C"] :: [String]))
      ggrp = inlineCat (take 9 (cycle (["x","y","z"] :: [String])))
      gval = inline [3,5,2, 4,1,6, 2,3,4]
  out "02-bar-grouped" $ purePlot
    <> layer (bar gcat gval <> colorBy ggrp <> position PosDodge)
    <> title "Grouped Bar Chart (dodge)" <> xLabel "category" <> yLabel "value"

  -- Stacked Bar Chart
  out "03-bar-stacked" $ purePlot
    <> layer (bar gcat gval <> colorBy ggrp <> position PosStack)
    <> title "Stacked Bar Chart" <> xLabel "category" <> yLabel "value"

  -- Normalized (Percentage) Stacked Bar Chart
  out "04-bar-normalized" $ purePlot
    <> layer (bar gcat gval <> colorBy ggrp <> position PosFill)
    <> title "Normalized (Percentage) Stacked Bar Chart"

  -- == Histograms / Density / Cumulative ===================================
  let vals = [3.0,4.0,4.5,5.0,5.0,5.2,5.5,6.0,6.5,7.0,8.0,12.0
             ,4.8,5.1,5.3,5.9,6.2,6.8,7.5,4.2,5.6,6.1]
  out "05-histogram" $ purePlot
    <> layer (histogram (inline vals))
    <> title "Histogram" <> xLabel "x" <> yLabel "count"

  out "06-density" $ purePlot
    <> layer (density (inline vals))
    <> title "Density Plot" <> xLabel "x" <> yLabel "density"

  -- Cumulative Frequency Distribution
  out "07-ecdf" $ purePlot
    <> layer (ecdf (inline vals))
    <> title "Cumulative Frequency Distribution (ECDF)" <> xLabel "x" <> yLabel "F(x)"

  -- == Scatter & Strip Plots ===============================================
  let sx = [1.0,2,3,4,5,6,7,8,9,10]
      sy = [2.1,3.9,6.0,7.7,10.2,11.8,14.1,15.9,18.2,20.0]
  out "08-scatter" $ purePlot
    <> layer (scatter (inline sx) (inline sy) <> size 6)
    <> title "Scatterplot" <> xLabel "x" <> yLabel "y"

  -- Bubble Plot (color + size)
  let bgrp = inlineCat (take 10 (cycle (["A","B"] :: [String])))
      bsz  = inline [2,8,3,9,4,7,5,6,3,8]
  out "09-bubble" $ purePlot
    <> layer (scatter (inline sx) (inline sy) <> colorBy bgrp <> sizeBy bsz <> alpha 0.8)
    <> title "Bubble Plot" <> xLabel "x" <> yLabel "y"

  -- 1D Strip Plot
  out "10-strip" $ purePlot
    <> layer (strip (inline vals) <> groupBy (inlineCat (replicate (length vals) ("v" :: String))))
    <> title "Strip Plot" <> yLabel "value"

  -- == Line Charts =========================================================
  let lx = [0.0,1,2,3,4,5,6,7,8,9]
  out "11-line" $ purePlot
    <> layer (line (inline lx) (inline (map (\x -> sin (x/2)*5+10) lx)) <> stroke 2)
    <> title "Line Chart" <> xLabel "t" <> yLabel "f(t)"

  -- Multi Series Line Chart
  out "12-line-multi" $ purePlot
    <> layer (line (inline lx) (inline (map (\x -> sin (x/2)*5+10) lx)) <> color (fromHex "#1f77b4") <> stroke 2)
    <> layer (line (inline lx) (inline (map (\x -> cos (x/2)*5+10) lx)) <> color (fromHex "#d62728") <> stroke 2)
    <> title "Multi Series Line Chart" <> xLabel "t" <> yLabel "f(t)"

  -- Step Chart
  out "13-step" $ purePlot
    <> layer (step (inline lx) (inline [0,0,1,1,3,3,2,2,4,4]) <> stroke 2)
    <> title "Step Chart" <> xLabel "t" <> yLabel "v"

  -- == Area Charts =========================================================
  -- Area Chart = band を 0..y で塗る
  let ax = [0.0,1,2,3,4,5,6,7,8,9]
      ay = [1.0,3,2,5,4,6,5,7,6,8]
  out "14-area" $ purePlot
    <> layer (band (inline ax) (inline (replicate 10 0.0)) (inline ay) <> alpha 0.5)
    <> layer (line (inline ax) (inline ay) <> stroke 2)
    <> title "Area Chart" <> xLabel "t" <> yLabel "v"

  -- == Table-based Plots ===================================================
  -- Table Heatmap
  let hx = inlineCat [ c | c <- ["A","B","C"] :: [String], _ <- [1::Int ..3] ]
      hy = inlineCat (take 9 (cycle (["P","Q","R"] :: [String])))
      hv = inline [1,5,9, 3,7,2, 8,4,6]
  out "15-heatmap" $ purePlot
    <> layer (heatmap hx hy hv)
    <> title "Table Heatmap" <> xLabel "col" <> yLabel "row"

  -- == Circular Plots ======================================================
  let pc = inlineCat (["A","B","C","D"] :: [String])
      pv = inline [30,25,25,20]
  out "16-pie" $ purePlot
    <> layer (pie pc pv)
    <> title "Pie Chart"

  -- Radial Plot (polar bar)
  out "17-radial" $ purePlot
    <> layer (bar pc pv <> colorBy pc)
    <> coordPolar
    <> title "Radial Plot (polar bar)"

  -- == Advanced Calculations ===============================================
  -- Linear Regression: plot 側では fit しないので OLS 当てはめ値 yhat を作り line で重ねる
  -- (データから自動 fit するなら analyze の statLm)。
  out "18-regression" $
    let n    = fromIntegral (length sx)
        a    = (n * sum (zipWith (*) sx sy) - sum sx * sum sy)
                 / (n * sum (map (^ (2 :: Int)) sx) - sum sx ^ (2 :: Int))
        b    = (sum sy - a * sum sx) / n
        yhat = [ a * x + b | x <- sx ]
    in purePlot
    <> layer (scatter (inline sx) (inline sy) <> size 6)
    <> layer (line (inline sx) (inline yhat) <> color (fromHex "#d62728") <> stroke 2)
    <> title "Linear Regression" <> xLabel "x" <> yLabel "y"

  -- Quantile-Quantile Plot (QQ Plot)
  out "19-qq" $ purePlot
    <> layer (qq (inline vals) <> size 6)
    <> title "Quantile-Quantile Plot (QQ Plot)" <> xLabel "theoretical" <> yLabel "sample"

  -- Parallel Coordinate Plot
  out "20-parallel" $ purePlot
    <> layer (parallelCoords [ inline [1,2,3,4], inline [4,3,2,1], inline [2,4,1,3] ])
    <> title "Parallel Coordinate Plot"

  -- Waterfall Chart of Monthly Profit and Loss
  out "21-waterfall" $ purePlot
    <> layer (waterfall (inlineCat (["Begin","Q1","Q2","Q3","Q4","End"] :: [String]))
                        (inline [1000, 300, -200, 400, -100, 1400]))
    <> title "Waterfall Chart"

  -- == Error Bars & Error Bands ============================================
  -- Error Bars Showing Confidence Interval (pointRange = x, y, err)
  out "22-errorbar" $ purePlot
    <> layer (pointRange (inline [1.0,2,3,4,5]) (inline [2.0,3.5,3.0,4.2,5.1])
                         (inline [0.5,0.8,0.4,0.6,0.7]))
    <> title "Error Bars Showing Confidence Interval" <> xLabel "x" <> yLabel "mean ± CI"

  -- == Box Plots ===========================================================
  out "23-boxplot" $ purePlot
    <> layer (boxplot (inline vals))
    <> title "Box Plot (Tukey 1.5 IQR)" <> yLabel "value"

  -- == Distributions =======================================================
  let dcat = inlineCat (concatMap (replicate 8) (["A","B","C"] :: [String]))
      dval = inline ([3,4,4.5,5,5.2,5.5,6,7] ++ [5,5.5,6,6.2,6.8,7,7.5,8] ++ [2,2.5,3,3.2,3.8,4,4.5,5])
  out "24-violin"   $ purePlot <> layer (violin dval <> groupBy dcat)   <> title "Violin Plot"   <> yLabel "value"
  out "25-swarm"    $ purePlot <> layer (swarm dval <> groupBy dcat)    <> title "Swarm Plot"    <> yLabel "value"
  out "26-raincloud"$ purePlot <> layer (raincloud dval <> groupBy dcat)<> title "Raincloud Plot"<> yLabel "value"
  out "27-ridge"    $ purePlot <> layer (ridge dval <> groupBy dcat)    <> title "Ridgeline Plot"<> yLabel "group"

  -- == Faceting (Trellis) ==================================================
  let r facetN = case facetN of
        "x" -> Just (NumData (V.fromList [1,2,3,4, 1,2,3,4, 1,2,3,4]))
        "y" -> Just (NumData (V.fromList [1,4,9,16, 2,5,8,12, 3,6,9,15]))
        "g" -> Just (TxtData (V.fromList ["A","A","A","A","B","B","B","B","C","C","C","C"]))
        _   -> Nothing
  saveSVGWith "design/vega-lite-gallery/28-trellis-scatter.svg" r $ purePlot
    <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
    <> facet "g"
    <> title "Trellis Scatter Plot (facet)" <> xLabel "x" <> yLabel "y"

  putStrLn "wrote design/vega-lite-gallery/*.svg (28 examples)"

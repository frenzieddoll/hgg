-- | Phase 26 §A-2 新 API デモ: 全 helper を `<>` で paren 無し合成。
--
-- @
-- cabal run scatter-demo
-- @
-- → カレントディレクトリに @scatter-demo.svg@ が生成される。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hgg.Plot.Backend.SVG (saveSVG, saveSVGWith, saveSVGInteractive)
import           Hgg.Plot.Unit         (px, (*~))
import qualified Hgg.Plot.DAG
import           Hgg.Plot.DAG         ((~>))
import           Hgg.Plot.Easy
import qualified Hgg.Plot.Spec
import           Data.Text                (Text)
import qualified Data.Vector              as V

main :: IO ()
main = do
  -- y = x² + regression line を Layer monoid で構築
  let xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
      ys = map (\x -> x * x) xs
      fitY = map (\x -> 8 * x - 12) xs   -- 直線当てはめ (= demo 用)
      spec
        =  purePlot
        <> layer (scatter (inline xs) (inline ys) <> alpha 0.85 <> size 5)
        <> layer (line    (inline xs) (inline fitY) <> color (fromHex "#d62728") <> stroke 2)
        <> title  "y = x²  (with linear fit)"
        <> xLabel "x"
        <> yLabel "y"
        <> widthUnit (800 *~ px)
        <> heightUnit (600 *~ px)
  saveSVG "scatter-demo.svg"      spec
  saveSVG "scatter-demo-dark.svg" (spec <> theme ThemeDark)
  -- categorical 色分け demo: 30 点を 3 group に
  let xs2 = [0, 1 .. 29] :: [Double]
      ys2 = [ sin (x / 3) * 5 + 10 | x <- xs2 ]
      grp = take 30 (cycle (["A", "B", "C"] :: [String]))
      spec2
        =  purePlot
        <> layer (scatter (inline xs2) (inline ys2)
                   <> colorBy (inlineCat grp)
                   <> size 6
                   <> alpha 0.85)
        <> title  "Scatter by group (categorical color)"
        <> xLabel "t" <> yLabel "f(t)"
  saveSVG "scatter-categorical.svg" spec2
  -- Interactive (= hover tooltip + drag pan + wheel zoom、 ブラウザで開いて確認)
  saveSVGInteractive "scatter-interactive.svg" emptyResolver spec2
  -- Box + Density 統計 chart demo (= Phase 26 §E-2)
  let vals = [3.0, 4.0, 4.5, 5.0, 5.0, 5.2, 5.5, 6.0, 6.5, 7.0, 8.0, 12.0]
      boxSpec   = purePlot <> layer (boxplot (inline vals))
                          <> title "Boxplot demo" <> widthUnit (400 *~ px) <> heightUnit (600 *~ px)
      densSpec  = purePlot <> layer (density (inline vals))
                          <> title "Density (KDE) demo"
                          <> xLabel "value" <> yLabel "density"
  saveSVG "box-demo.svg"     boxSpec
  saveSVG "density-demo.svg" densSpec
  -- LogScale demo (= Phase 26 §C-2 #1)
  let xsLog = [1, 10, 100, 1000, 10000] :: [Double]
      ysLog = [2, 30, 500, 8000, 120000] :: [Double]
      logSpec = purePlot
        <> layer (scatter (inline xsLog) (inline ysLog) <> size 6)
        <> xAxis logAxis
        <> yAxis logAxis
        <> title  "Log-Log demo (= xLog + yLog)"
        <> xLabel "x (log)" <> yLabel "y (log)"
  saveSVG "loglog-demo.svg" logSpec
  -- AxisFormat demo (= Phase 26 §C-2 #2)
  let fmtSpec = purePlot
        <> layer (scatter (inline [0.0, 1.0, 2.0]) (inline [0.001234, 0.05678, 1.234]))
        <> yAxis (axisFormat (AxisExponentFmt 2))
        <> xAxis (axisFormat AxisIntegerFmt)
        <> title  "Axis format: Y=exp(2digit), X=integer"
        <> xLabel "x" <> yLabel "y"
  saveSVG "axisformat-demo.svg" fmtSpec
  -- Reference line demo (= Phase 26 §C-2 #3)
  let xsR = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]
      ysR = [0.1, 1.2, 1.8, 3.3, 4.1, 5.2] :: [Double]
      refSpec = purePlot
        <> layer (scatter (inline xsR) (inline ysR) <> size 6)
        <> refIdentity                   -- y = x (Actual vs Predicted の対角線)
        <> refHorizontal 2.5             -- y = 2.5
        <> refVertical 3.0               -- x = 3.0
        <> title  "Reference lines (y=x / y=2.5 / x=3)"
        <> xLabel "predicted" <> yLabel "actual"
  saveSVG "refline-demo.svg" refSpec
  -- Trellis (facet) demo (= Phase 26 §C-2 #12)
  let r facetN = case facetN of
        "x" -> Just (NumData (V.fromList [1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]))
        "y" -> Just (NumData (V.fromList [1, 4, 9, 16, 2, 5, 8, 12, 3, 6, 9, 15]))
        "g" -> Just (TxtData (V.fromList ["A","A","A","A","B","B","B","B","C","C","C","C"]))
        _   -> Nothing
      facetSpec = purePlot
        <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
        <> facet "g"
        <> title  "Facet by group (3 panel)"
        <> xLabel "x" <> yLabel "y"
  saveSVGWith "facet-demo.svg" r facetSpec
  -- DAG demo (= §E-6: HBM ModelGraph、 algebraic-graphs 流 API)
  let alphaN = ("alpha" :: Text, "α"     :: Text, NodeLatent)
      betaN  = ("beta"  :: Text, "β"     :: Text, NodeLatent)
      sigmaN = ("sigma" :: Text, "σ"     :: Text, NodeLatent)
      yN     = ("y"     :: Text, "y obs" :: Text, NodeObserved)
      hbmGraph =  alphaN ~> sigmaN
               <> betaN  ~> sigmaN
               <> sigmaN ~> yN
               <> alphaN ~> yN
               <> betaN  ~> yN
      dagSpec = purePlot
        <> layer (Hgg.Plot.DAG.dagPlot hbmGraph <> size 22)
        <> title  "HBM ModelGraph (§E-6 DAG, hierarchical layout)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  saveSVG "dag-demo.svg" dagSpec
  -- ユーザ PyMC モデル再現 (= s_C/s_P/b_C/b_P/b1〜b4 priors + Weather/A/Score MutableData
  --   + x_J/x_C/x_P/x deterministic + Y Bernoulli observed、 record/course/person plates)
  let pymcLikeSpec = purePlot
        <> layer (Hgg.Plot.Spec.dagFromListsWithPlates pymcNodes pymcEdges
                    Hgg.Plot.Spec.LayoutManual pymcPlates
                  <> size 24)
        <> title "PyMC モデル再現 (自作 hgg)"
        <> theme  ThemeLight
        <> widthUnit (1000 *~ px) <> heightUnit (720 *~ px)
  saveSVG "pymc-like-demo.svg" pymcLikeSpec
  -- TODO-3 (2026-05-29) sub-features demo: jitter + shapeBy + sizeBy + colorCats
  let xs3 = [1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 5.0, 5.0] :: [Double]
      ys3 = [1.5, 1.8, 2.2, 2.6, 3.1, 3.5, 4.0, 4.4, 4.7, 5.2] :: [Double]
      g3  = ["A","B","A","B","A","B","A","B","A","B"] :: [Text]
      s3  = [1.0, 5.0, 2.0, 4.0, 3.0, 6.0, 4.5, 2.5, 5.5, 3.5] :: [Double]
      todo3Spec = purePlot
        <> layer (scatter (inline xs3) (inline ys3)
                   <> jitterX 0.03
                   <> jitterY 0.03
                   <> colorBy (inlineCat g3)
                   <> colorCats ["A", "B"]
                   <> shapeBy (inlineCat g3)
                   <> shapeMapEntry "A" MShTriangle
                   <> shapeMapEntry "B" MShSquare
                   <> sizeBy (inline s3)
                   <> alpha 0.9)
        <> title  "TODO-3 demo (jitter + shapeBy + sizeBy + colorCats)"
        <> xLabel "x" <> yLabel "y"
        <> widthUnit (700 *~ px) <> heightUnit (500 *~ px)
        <> xAxis (axisRotate 30)
  saveSVG "todo3-demo.svg" todo3Spec
  -- TODO-10 (2026-05-29) font sweep demo: titleFont / tickFont / axisLabelFont をカスタム
  let todo10Spec = purePlot
        <> layer (scatter (inline [1.0, 2.0, 3.0, 4.0, 5.0])
                          (inline [1.0, 3.0, 2.0, 4.0, 3.5]) <> size 6)
        <> title  "TODO-10 font sweep demo (bold italic title + monospace tick)"
        <> xLabel "x axis" <> yLabel "y axis"
        <> titleFont     (fontSize 20 <> fontWeight "bold" <> fontItalic True
                                       <> fontColor "#c0392b")
        <> axisLabelFont (fontSize 14 <> fontFamily "Georgia"
                                       <> fontColor "#2c3e50")
        <> tickFont      (fontSize 10 <> fontFamily "monospace"
                                       <> fontColor "#7f8c8d")
        <> widthUnit (700 *~ px) <> heightUnit (500 *~ px)
  saveSVG "todo10-demo.svg" todo10Spec
  putStrLn "wrote scatter + dark + categorical + interactive + box + density + loglog + axisformat + refline + facet + dag + pymc-like + todo3 + todo10"
  where
    -- PyMC モデル node 定義 (= LayoutManual で位置手動指定、 ユーザ画像と同じ配置)
    -- 画像から読み取り: 4 階層 (= prior super → prior → deterministic → observed)
    -- 上から: s_C/s_P → b_C/b_P/b1〜b4 + Weather/A/Score → x_J/x_C/x_P → x → Y
    pymcNodes :: [Hgg.Plot.Spec.DAGNode]
    pymcNodes =
      [ -- 階層 0 (= 超事前、 y=0.05): HalfCauchy hyper-priors (s_C / s_P)
        Hgg.Plot.Spec.dagNodeDist "sC" "s_C" Hgg.Plot.Spec.NodeLatent "HalfCauchy" 0.30 0.05
      , Hgg.Plot.Spec.dagNodeDist "sP" "s_P" Hgg.Plot.Spec.NodeLatent "HalfCauchy" 0.55 0.05
        -- 階層 1 (= prior + data、 y=0.30)
      , Hgg.Plot.Spec.dagNodeDist "weather" "Weather" Hgg.Plot.Spec.NodeData    "MutableData" 0.05 0.30
      , Hgg.Plot.Spec.dagNodeDist "b4"      "b4"      Hgg.Plot.Spec.NodeLatent  "Flat" 0.18 0.30
      , Hgg.Plot.Spec.dagNodeDist "bC"      "b_C"     Hgg.Plot.Spec.NodeLatent  "Normal" 0.30 0.30
      , Hgg.Plot.Spec.dagNodeDist "b2"      "b2"      Hgg.Plot.Spec.NodeLatent  "Flat" 0.45 0.30
      , Hgg.Plot.Spec.dagNodeDist "bP"      "b_P"     Hgg.Plot.Spec.NodeLatent  "Normal" 0.55 0.30
      , Hgg.Plot.Spec.dagNodeDist "A"       "A"       Hgg.Plot.Spec.NodeData    "MutableData" 0.66 0.30
      , Hgg.Plot.Spec.dagNodeDist "score"   "Score"   Hgg.Plot.Spec.NodeData    "MutableData" 0.78 0.30
      , Hgg.Plot.Spec.dagNodeDist "b3"      "b3"      Hgg.Plot.Spec.NodeLatent  "Flat" 0.92 0.30
        -- 階層 2 (= deterministic、 y=0.55)
      , Hgg.Plot.Spec.dagNodeDist "xJ" "x_J" Hgg.Plot.Spec.NodeOther "Deterministic" 0.05 0.55
      , Hgg.Plot.Spec.dagNodeDist "b1" "b1"  Hgg.Plot.Spec.NodeLatent "Flat" 0.20 0.55
      , Hgg.Plot.Spec.dagNodeDist "xC" "x_C" Hgg.Plot.Spec.NodeOther  "Deterministic" 0.30 0.55
      , Hgg.Plot.Spec.dagNodeDist "xP" "x_P" Hgg.Plot.Spec.NodeOther  "Deterministic" 0.66 0.55
        -- 階層 3 (= 合算 deterministic、 y=0.78)
      , Hgg.Plot.Spec.dagNodeDist "x"  "x"   Hgg.Plot.Spec.NodeOther  "Deterministic" 0.10 0.78
        -- 階層 4 (= observed、 y=0.96)
      , Hgg.Plot.Spec.dagNodeDist "Y"  "Y"   Hgg.Plot.Spec.NodeObserved "Bernoulli" 0.10 0.96
      ]
    pymcEdges :: [Hgg.Plot.Spec.DAGEdge]
    pymcEdges =
      [ Hgg.Plot.Spec.dagEdge "sC" "bC"
      , Hgg.Plot.Spec.dagEdge "sP" "bP"
      , Hgg.Plot.Spec.dagEdge "weather" "xJ"
      , Hgg.Plot.Spec.dagEdge "b4" "xJ"
      , Hgg.Plot.Spec.dagEdge "bC" "xC"
      , Hgg.Plot.Spec.dagEdge "b2" "xP"
      , Hgg.Plot.Spec.dagEdge "bP" "xP"
      , Hgg.Plot.Spec.dagEdge "A" "xP"
      , Hgg.Plot.Spec.dagEdge "score" "xP"
      , Hgg.Plot.Spec.dagEdge "b3" "x"
      , Hgg.Plot.Spec.dagEdge "xJ" "x"
      , Hgg.Plot.Spec.dagEdge "b1" "x"
      , Hgg.Plot.Spec.dagEdge "xC" "x"
      , Hgg.Plot.Spec.dagEdge "xP" "x"
      , Hgg.Plot.Spec.dagEdge "x"  "Y"
      ]
    pymcPlates :: [Hgg.Plot.Spec.DAGPlate]
    pymcPlates =
      [ Hgg.Plot.Spec.DAGPlate "record (2396)"  ["xJ", "x", "Y"]
      , Hgg.Plot.Spec.DAGPlate "course (10)"    ["bC", "xC"]
      , Hgg.Plot.Spec.DAGPlate "person (50)"    ["bP", "A", "score", "xP"]
      ]

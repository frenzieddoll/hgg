-- | 02-layers.md の図 (mark 索引 + 定型エントリ用サムネイル)。
--   基本 / 分布 / 区間・エラー / 集計・統計 / 2D 場・行列 / ベクトル場 /
--   MCMC・ベイズ診断 / DAG の各 mark を 1 枚ずつ。
{-# LANGUAGE OverloadedStrings #-}
module DocFig.Layers (figures) where

import           Data.Text     (Text)
import qualified Data.Vector  as V
import           DocFig.Common

figures :: [Figure]
figures =
  -- 3a geom: bar (棒グラフ)
  [ fig "s3a-geom.svg" $
         purePlot <> layer (bar cats vals)
      <> title "3a. geom: bar" <> xLabel "群" <> yLabel "値"

    -- 3b layer 見た目: position dodge で群分け bar
  , fig "s3b-aes.svg" $
         purePlot
      <> layer (bar bx bv <> colorBy bg <> position PosDodge)
      <> legend
      <> title "3b. position PosDodge + color" <> xLabel "群" <> yLabel "値"

    -- 02 encoding: jitterX (整数 x に重なる点を散らす) + densityFill
  , figW "s2-jitter-density.svg" 960 380 $
         subplots [ layer (scatter jxs jys <> jitterX 0.25 <> alpha 0.6 <> size 5) <> title "jitterX (重なり点を散らす)"
                  , layer (density densVals <> densityFill True <> alpha 0.4) <> title "densityFill" ]
      <> subplotCols 2

    -- 基本: scatter
  , fig "scatter.svg" $
         purePlot <> layer (scatter (inline [1,2,3,4,5]) (inline [2,4,3,5,7]))
      <> title "scatter" <> xLabel "x" <> yLabel "y"

    -- 基本: scatterPoints / linePoints
  , fig "scatterpoints.svg" $
         purePlot <> layer (scatterPoints [Point2 1 2, Point2 2 3.5, Point2 3 3, Point2 4 4.2, Point2 5 4])
      <> title "scatterPoints ([Point2])" <> xLabel "x" <> yLabel "y"

    -- 基本: text / label (ラベルは点の少し上に置いて重なりを避ける・label は枠付き)
  , fig "text.svg" $
         purePlot <> layer (scatter txX txY <> size 7) <> layer (label txX txYlab txN)
      <> title "text / label (点に注釈)" <> xLabel "x" <> yLabel "y"

    -- 基本: stem (lollipop)。 ★ stem は数値 x を取る (categorical 非対応)。
  , fig "stem.svg" $
         purePlot <> layer (stem (inline [1,2,3,4,5,6]) (inline [3,7,5,6,4,8]))
      <> title "stem (lollipop)" <> xLabel "x" <> yLabel "値"

    -- 基本: ecdf
  , fig "ecdf.svg" $
         purePlot <> layer (ecdf (inline [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]))
      <> title "ecdf (経験累積分布)" <> xLabel "x" <> yLabel "F(x)"

    -- 分布: histogram
  , fig "histogram.svg" $
         purePlot <> layer (histogram (inline [1,2,2,3,3,3,4,4,5,2,3,4,3,5,4,3,2,4,3,5]) <> binCount 8)
      <> title "histogram (binCount 8)" <> xLabel "x" <> yLabel "count"

    -- 分布: boxplot (群色)
  , fig "boxplot.svg" $
         purePlot <> layer (boxplot dV <> colorBy dG) <> legend
      <> title "boxplot (群ごと colorBy)" <> yLabel "value"

    -- 分布: violin / strip / swarm
  , figW "violin.svg" 1280 420 $
         subplots [ layer (violin dV <> colorBy dG) <> title "violin"
                  , layer (strip  dV <> colorBy dG <> jitterX 0.15) <> title "strip"
                  , layer (swarm  dV <> colorBy dG) <> title "swarm" ]
      <> subplotCols 3 <> legend
      <> title "violin / strip / swarm"

    -- 分布: raincloud
  , fig "raincloud.svg" $
         purePlot <> layer (raincloud dV <> colorBy dG) <> legend
      <> title "raincloud (violin + box + strip)" <> yLabel "value"

    -- 分布: ridge (joyplot)
  , fig "ridge.svg" $
         purePlot <> layer (ridge rV <> colorBy rG) <> legend
      <> title "ridge (joyplot)" <> xLabel "value"

    -- 分布: qq
  , fig "qq.svg" $
         purePlot <> layer (qq (inline [-1.2,-0.3,0.1,0.5,-0.8,1.4,0.2,-0.1,0.9,-0.5,0.3,-0.6,1.1,-0.2,0.7]))
      <> title "qq (正規 QQ)" <> xLabel "理論分位点" <> yLabel "標本分位点"

    -- 区間: band + line
  , fig "band.svg" $
         purePlot <> layer (band bX bLo bHi <> alpha 0.3) <> layer (line bX bY)
      <> title "band (x, lo, hi) + line" <> xLabel "x" <> yLabel "y"

    -- 区間: lineRange / pointRange / crossbar (= (x, y, err) 対称)
  , figW "range.svg" 1280 420 $
         subplots [ layer (lineRange  rgC rgY rgE) <> title "lineRange"
                  , layer (pointRange rgC rgY rgE) <> title "pointRange"
                  , layer (crossbar   rgC rgY rgE) <> title "crossbar" ]
      <> subplotCols 3
      <> title "lineRange / pointRange / crossbar (y ± err)"

    -- 区間: forest (= (ラベル, 推定, err) 対称)
  , fig "forest.svg" $
         purePlot <> layer (forest (inlineCat (["b0","b1","b2","b3"] :: [Text]))
                                   (inline [0.2,-0.1,0.4,0.05]) (inline [0.15,0.2,0.1,0.12])
                            <> forestNull 0)
      <> title "forest (推定 ± err, null=0)" <> xLabel "効果量"

    -- 区間: funnel (= (effect, SE))
  , fig "funnel.svg" $
         purePlot <> layer (funnel (inline [0.1,0.2,-0.1,0.15,0.05,0.12,-0.05,0.18])
                                   (inline [0.05,0.1,0.08,0.12,0.2,0.06,0.15,0.09]))
      <> title "funnel (effect vs SE)" <> xLabel "効果量" <> yLabel "SE"

    -- 集計: statFunction
  , fig "statfunction.svg" $
         purePlot <> layer (scatter (inline [1,3,5,7,9]) (inline [3,7,10,16,18]))
                  <> layer (statFunction (\x -> 2 * x + 1) 0 10 100)
      <> title "statFunction (2x+1)" <> xLabel "x" <> yLabel "y"

    -- 集計: statMean (参照線)
  , fig "statmean.svg" $
         purePlot <> layer (histogram smX) <> layer (statMean smX <> linetype LtDashed)
      <> title "statMean (参照線)" <> xLabel "x" <> yLabel "count"

    -- 集計: countXY
  , fig "countxy.svg" $
         purePlot <> layer (countXY (inlineCat (["A","A","B","A","B","B","A","B","B"] :: [Text]))
                                    (inlineCat (["x","y","x","x","y","y","y","x","y"] :: [Text])))
      <> title "countXY (頻度)" <> xLabel "x" <> yLabel "y"

    -- 集計: histogramWide
  , fig "histogramwide.svg" $
         purePlot <> histogramWide [ inline [1,2,2,3,3,2,3], inline [2,3,3,4,4,3,4], inline [3,4,4,5,5,4,5] ]
      <> legend <> title "histogramWide (複数列重ね)" <> xLabel "x" <> yLabel "count"

    -- 2D 場: heatmap
  , fig "heatmap.svg" $
         purePlot <> layer (heatmap (inlineCat (map fst hmPts)) (inlineCat (map snd hmPts))
                                    (inline [1,0.3,0.1, 0.3,1,0.5, 0.1,0.5,1]))
      <> title "heatmap (カテゴリ grid)" <> xLabel "x" <> yLabel "y"

    -- 2D 場: pie
  , fig "pie.svg" $
         purePlot <> layer (pie (inlineCat (["A","B","C"] :: [Text])) (inline [30,50,20])) <> legend
      <> title "pie (円グラフ)"

    -- 2D 場: waterfall
  , fig "waterfall.svg" $
         purePlot <> layer (waterfall (inlineCat (["start","Q1","Q2","Q3"] :: [Text])) (inline [100,30,-20,15]))
      <> title "waterfall (累積寄与)" <> xLabel "段" <> yLabel "累積"

    -- 2D 場: parallelCoords (inline 列・軸名は無し)
  , fig "parallelcoords.svg" $
         purePlot <> layer (parallelCoords [ inline [1,2,3], inline [4,5,4], inline [2,1,3], inline [5,4,5] ]
                                           <> colorBy (inlineCat (["a","b","a"] :: [Text]))) <> legend
      <> title "parallelCoords (平行座標)"

    -- 2D 場: pairs (SPLOM、 inline 列・軸名は無し)
  , fig "pairs.svg" $
         purePlot <> pairs [ inline [1,2,3,4,5], inline [2,1,4,3,5], inline [1,3,2,5,4] ]
      <> title "pairs (散布図行列)"

    -- 集計2: contour (= 等高線図、 marching squares)
  , fig "contour.svg" $
         purePlot
      <> layer (contour ctX ctY ctZ)
      <> title "contour (等高線、 2 つの山)" <> xLabel "x" <> yLabel "y"

    -- 2D 場: bin2d (= binned heatmap、 contour の塗り版。 同データ)
  , fig "bin2d.svg" $
         purePlot
      <> layer (bin2d ctX ctY ctZ)
      <> title "bin2d (binned heatmap、 contour の塗り版)" <> xLabel "x" <> yLabel "y"

    -- 2D 場: hexbin (= 六角ビニング。 散布過密を六角セル件数の連続色で。 geom_hex)
  , fig "hexbin.svg" $
         purePlot
      <> layer (hexbin hxX hxY <> hexbinBins 12)
      <> title "hexbin (六角ビニング、 件数→連続色)" <> xLabel "x" <> yLabel "y"

    -- ベクトル場: quiver (= vector field、 Phase 26 A2)
  , fig "quiver.svg" $
         purePlot
      <> layer (quiver qX qY qU qV <> arrowColorByMagnitude)
      <> title "quiver (vector field、 magnitude 連続色)" <> xLabel "x" <> yLabel "y"

    -- MCMC: trace / traceLines (自己完結・reader が再現できる式で生成)
  , figW "trace.svg" 1080 420 $
         subplots [ layer (trace (inline (map fromIntegral tcIs)) (inline v1)) <> title "trace (1 chain)"
                  , layer (traceLines tcIter tcVal tcCh) <> title "traceLines (chain 別)" ]
      <> subplotCols 2 <> legend
      <> title "trace / traceLines"

    -- MCMC: ess
  , fig "ess.svg" $
         purePlot <> layer (ess (inline [100,200,300,400,500,600])
                                (inline [80,150,210,260,300,330]))
      <> title "ess (有効サンプルサイズ)" <> xLabel "iter" <> yLabel "ESS"

    -- MCMC: autocorr (AR(1) 風の減衰系列・自己完結式)
  , fig "autocorr.svg" $
         purePlot <> layer (autocorr (inline acSeries) <> autocorrMaxLag 40)
      <> title "autocorr (自己相関)" <> xLabel "lag" <> yLabel "ACF"

    -- 積層: stream (streamgraph)
  , fig "stream.svg" $
         purePlot <> layer (stream stX stY <> colorBy stS) <> legend
      <> title "stream (streamgraph)" <> xLabel "x" <> yLabel "y"

    -- DAG: y ~ Normal(a + b·x, s) の構造を手書き
  , fig "dag-manual.svg" $
         purePlot <> layer (dag dagNodes dagEdges <> size 22)
      <> title "手書き DAG (a,b→mu→y, s→y)"

    -- DAG: plate で観測ループ (mu/y) を囲む
  , fig "dag-plate.svg" $
         purePlot
      <> layer (dagFromListsWithPlates dagNodes dagEdges LayoutManual
                 [ DAGPlate { dpLabel = "obs (N)", dpNodeIds = ["mu", "y"] } ] <> size 22)
      <> title "plate つき DAG (obs を N 個の枠で囲む)"

    -- distCols: 別列を別 mark で 1 パネルに併置 (= <+> の list 版・列名スロット)
  , figR "distcols.svg" rDist $
         distCols [ boxplot "a", violin "c", boxplot "d" ]
      <> title "distCols [box a, violin c, box d]" <> yLabel "value"

    -- distCols × colorBy: レーン内で群ごとに dodge 分割
  , figR "distcols-colorby.svg" rDist $
         distCols [ boxplot "a" <> colorBy "g", boxplot "c" ]
      <> legend
      <> title "distCols [box a <> colorBy g, box c]" <> yLabel "value"
  ]
  where
    cats = inlineCat (["A","B","C"] :: [Text])
    vals = inline [3.0, 7.0, 5.0]
    -- 群分け bar 用 (x category × group)
    bx = inlineCat (concatMap (replicate 2) (["A","B","C"] :: [Text]))
    bg = inlineCat (concat (replicate 3 (["g1","g2"] :: [Text])))
    bv = inline [3.0, 2.0, 5.0, 4.0, 4.0, 6.0]

    jxs = inline ([1,1,1,1,2,2,2,2,3,3,3,3,1,1,2,2,3,3,1,2,3,1,2,3] :: [Double])
    jys = inline ([2.0,2.4,2.1,1.8,2.6,2.2,2.5,2.0,3.1,3.4,2.9,3.3
                  ,2.2,2.7,2.4,2.1,3.0,3.2,1.9,2.5,3.1,2.3,2.6,3.4] :: [Double])
    densVals = inline ([ 4.6,4.9,5.0,5.1,5.4,5.0,4.4,4.9,5.4,4.8,4.8,4.3,5.8,5.7,5.4
                       , 5.1,5.7,5.1,5.4,5.1,4.6,5.1,4.8,5.0,5.0,5.2,5.2,4.7,4.8,5.4 ] :: [Double])

    txX = inline [1,2,3]; txY = inline [2,3,2.5]
    txYlab = inline [2.18,3.18,2.68]; txN = inlineCat (["P","Q","R"] :: [Text])

    dV = inline [4,5,6,5,7, 8,9,7,10,9, 5,6,7,6,8]
    dG = inlineCat (concatMap (replicate 5) (["a","b","c"] :: [Text]))

    rV = inline [1,2,2,3, 3,4,4,5, 5,6,6,7]
    rG = inlineCat (concatMap (replicate 4) (["a","b","c"] :: [Text]))

    bX = inline [1,2,3,4,5]; bY = inline [2,3,2.5,4,3.5]
    bLo = inline [1.5,2.4,2.0,3.4,3.0]; bHi = inline [2.5,3.6,3.0,4.6,4.0]

    rgC = inlineCat (["A","B","C"] :: [Text]); rgY = inline [2,3,2.5]; rgE = inline [0.4,0.6,0.3]

    smX = inline [1,2,2,3,3,3,4,4,5]

    hmCs = ["A","B","C"] :: [Text]
    hmPts = [ (a, b) | a <- hmCs, b <- hmCs ]

    ctGrid = [ (xi, yi) | xi <- [0.0, 0.4 .. 6.0], yi <- [0.0, 0.4 .. 6.0] ]
    ctX = inline (map fst ctGrid)
    ctY = inline (map snd ctGrid)
    ctZ = inline [ exp (-(((x - 3) ** 2) + ((y - 3) ** 2)) / 4)
                 + 0.4 * exp (-(((x - 1.2) ** 2) + ((y - 4.5) ** 2)) / 1.5)
                 | (x, y) <- ctGrid ]

    -- hexbin 用の相関した点群 (決定的・対角バンド + 正弦ばらつきで件数差を出す)
    hxPts = [ (x, y)
            | i <- [0 .. 299 :: Int]
            , let fi = fromIntegral i
                  x  = fromIntegral (i `mod` 20) * 0.3 + sin fi * 0.18
                  y  = fromIntegral (i `mod` 20) * 0.25 + cos (fi * 1.3) * 0.6
                       + fromIntegral (i `div` 20) * 0.05 ]
    hxX = inline (map fst hxPts)
    hxY = inline (map snd hxPts)

    qGrid = [ (gx, gy) | gx <- [-3, -2 .. 3], gy <- [-3, -2 .. 3 :: Double] ]
    qX = inline (map fst qGrid)
    qY = inline (map snd qGrid)
    qU = inline [ -y / 3 - x / 6 | (x, y) <- qGrid ]
    qV = inline [  x / 3 - y / 6 | (x, y) <- qGrid ]

    tcN  = 120 :: Int
    tcIs = [1 .. tcN]
    v1   = [ 1.0 + 0.3 * sin (fromIntegral i / 9) + 0.1 * sin (fromIntegral i * 7.1) | i <- tcIs ]
    v2   = [ 1.3 + 0.3 * sin (fromIntegral i / 9) + 0.1 * sin (fromIntegral i * 3.3) | i <- tcIs ]
    tcIter = inline (map fromIntegral (tcIs ++ tcIs))
    tcVal  = inline (v1 ++ v2)
    tcCh   = inlineCat (replicate tcN "1" ++ replicate tcN "2" :: [Text])

    rnd i  = let h = sin (fromIntegral i * 12.9898) * 43758.5453 in h - fromIntegral (floor h :: Int)
    acSeries = scanl (\prev i -> 0.85 * prev + (rnd i - 0.5)) 0 [1 .. 300 :: Int]

    stX = inline [1,2,3, 1,2,3, 1,2,3]
    stY = inline [2,3,4, 1,2,1, 3,2,3]
    stS = inlineCat (["a","a","a","b","b","b","c","c","c"] :: [Text])

    dagNodes = [ dagNodeDist "a"  "a"  NodeLatent        "Normal(0,10)"  0.15 0.0
               , dagNodeDist "b"  "b"  NodeLatent        "Normal(0,10)"  0.45 0.0
               , dagNodeDist "s"  "s"  NodeLatent        "HalfNormal(1)" 0.85 0.0
               , dagNode     "mu" "mu" NodeDeterministic                 0.30 0.5
               , dagNodeDist "y"  "y"  NodeObserved      "Normal(mu,s)"  0.45 1.0 ]
    dagEdges = [ dagEdge "a" "mu", dagEdge "b" "mu", dagEdge "mu" "y", dagEdge "s" "y" ]

    rDist :: Resolver
    rDist "a" = Just (NumData (V.fromList [3,4,4.5,5,5,5.2,5.5,6,6.5,7,8,4.8]))
    rDist "c" = Just (NumData (V.fromList [6,6.5,7,7.2,7.5,8,8.3,9,9.5,7.8,6.9,8.1]))
    rDist "d" = Just (NumData (V.fromList [2,2.5,3,3.2,3.5,4,4.3,5,2.8,3.1,3.9,2.2]))
    rDist "g" = Just (TxtData (V.fromList (concatMap (replicate 6) ["x","y"])))
    rDist _   = Nothing

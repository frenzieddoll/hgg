-- | README 用の図を生成する (quickstart + matplotlib(Haskell) Hackage gallery の hgg 版)。
--   合成データは決定的 (固定 seed の LCG) なので再現可能。 出力先は CWD 直下の images/。
--   実行: リポジトリ root で `cabal run -v0 readme-images` (= scripts/gen-readme-images.sh)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Hgg.Plot.Spec        -- grammar API (purePlot / layer / 各 mark / inline / …)
import           Hgg.Plot.Backend.SVG (saveSVG, saveSVGWith, saveSVGBound)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Unit        (px, (*~))
import qualified Hgg.Plot.ThreeD.Spec  as P3
import qualified Hgg.Plot.ThreeD.Easy  as P3E
import           Hgg.Plot.ThreeD.Types (cameraIso)
import qualified Data.Vector              as V
import qualified Data.Text                as T
import           Control.Monad            (forM, forM_)
import           System.Directory         (createDirectoryIfMissing)

-- HBM DAG (= 「ならでは」図)。 analyze の本物の階層ベイズモデルから 'dagOf' で構造抽出。
import           Hanalyze.Plot     (hbmModel, defaultHBM, HBMConfig (..), dagOf, toPlot)
import           Hanalyze.Model.HBM
                   ( Distribution (Normal, HalfNormal)
                   , ModelP, sample, deterministic, observe, plate )

-- ── 決定的な合成データ ───────────────────────────────────────────────
linspace :: Double -> Double -> Int -> [Double]
linspace lo hi n =
  [ lo + (hi - lo) * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] ]

-- LCG 一様乱数 (0,1)。 seed 固定で再現可能。
unif :: Int -> [Double]
unif seed = map (\x -> fromIntegral x / fromIntegral m) (tail (iterate step seed))
  where m = 2147483648 :: Int
        step x = (1103515245 * x + 12345) `mod` m

-- 近似 N(0,1) (12 個の一様和 - 6 / 中心極限)。
gauss :: Int -> [Double]
gauss seed = go (unif seed)
  where go xs = let (a, b) = splitAt 12 xs in (sum a - 6) : go b

-- 出力先 = docs/images/readme/ (.gitignore が docs/ 配下の svg のみ track するため)。
imgDir :: FilePath
imgDir = "docs/images/readme"

out :: FilePath -> FilePath
out name = imgDir ++ "/" ++ name

-- ── HBM DAG 用モデル (= ReadmeDagDemo 同型・群レベル予測子つき 変量切片+傾き) ──
-- ★ plate を 2 つに分ける (PyMC 同型): group(J)=係数 a_j,b_j / obs(N)=観測 mu_i,y_i。
--   こうしないと観測の繰り返し (obs) が group plate に潰れる。 DAG は構造由来なので
--   サンプル値には依存しない (seed 不問・ビット決定的)。
dagNGrp :: Int
dagNGrp = 3

dagXg :: Int -> Double
dagXg j = [0.0, 1.0, 2.0] !! j

dagGroupObs :: Int -> [Double]
dagGroupObs j =
  let base = [1.0, 4.0, 8.0] !! j + [0.5, 1.0, 0.3] !! j * dagXg j
  in map (base +) [0.10, -0.08, 0.05, -0.12, 0.09]

dagModel :: ModelP ()
dagModel = do
  muA  <- sample "mu_a"  (Normal 0 10)
  tauA <- sample "tau_a" (HalfNormal 5)
  muB  <- sample "mu_b"  (Normal 0 10)
  tauB <- sample "tau_b" (HalfNormal 5)
  s    <- sample "s"     (HalfNormal 1)
  coefs <- plate "group" dagNGrp $ forM [0 .. dagNGrp - 1] $ \j -> do
             aj <- sample ("a_" <> T.pack (show j)) (Normal muA tauA)
             bj <- sample ("b_" <> T.pack (show j)) (Normal muB tauB)
             pure (aj, bj)
  let rows = [ (j, yi) | j <- [0 .. dagNGrp - 1], yi <- dagGroupObs j ]
  _ <- plate "obs" (length rows) $ forM_ (zip [0 :: Int ..] rows) $ \(i, (j, yi)) -> do
         let (aj, bj) = coefs !! j
         mu <- deterministic ("mu_" <> T.pack (show i)) (aj + bj * realToFrac (dagXg j))
         observe ("y_" <> T.pack (show i)) (Normal mu s) [yi]
  pure ()

-- ── 図 ───────────────────────────────────────────────────────────────
main :: IO ()
main = do
  createDirectoryIfMissing True imgDir

  -- 0. README quickstart (y = x²)
  saveSVG (out "quickstart.svg") $
    layer (scatter (inline [1,2,3,4,5]) (inline [1,4,9,16,25]))
    <> title "y = x²" <> xLabel "x" <> yLabel "y"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)

  -- 1. 関数の線 (減衰サイン)
  let lx = linspace 0 12 200
  saveSVG (out "line.svg") $
    layer (line (inline lx) (inline [ sin x * exp (negate x / 6) | x <- lx ]))
    <> title "Damped sine" <> xLabel "x" <> yLabel "y" <> theme ThemeGrey

  -- 2. 散布図 (線形トレンド + ノイズ)
  let sn  = 120
      sxs = take sn (linspace 0 10 sn)
      sys = zipWith (\x e -> 1.5 * x + 2 + 2.5 * e) sxs (gauss 7)
  saveSVG (out "scatter.svg") $
    layer (scatter (inline sxs) (inline sys) <> alpha 0.7)
    <> title "Scatter" <> xLabel "x" <> yLabel "y" <> theme ThemeGrey

  -- 3. ヒストグラム
  saveSVG (out "histogram.svg") $
    layer (histogram (inline (take 500 (gauss 11))))
    <> title "Histogram" <> xLabel "value" <> yLabel "count" <> theme ThemeGrey

  -- 4. 密度 (KDE)
  saveSVG (out "density.svg") $
    layer (density (inline (take 500 (gauss 13))) <> densityFill True <> alpha 0.5)
    <> title "Density" <> xLabel "value" <> theme ThemeGrey

  -- 群別データ (A/B/C で平均をずらす)
  let gA = map (\e -> 3.0 + 0.9 * e) (take 80 (gauss 1))
      gB = map (\e -> 5.0 + 1.3 * e) (take 80 (gauss 2))
      gC = map (\e -> 4.0 + 0.7 * e) (take 80 (gauss 3))
      gvals   = gA ++ gB ++ gC
      ggroups = concatMap (replicate 80) (["A","B","C"] :: [String])

  -- 5. 群別箱ひげ (★ Phase 36: boxplot v <> groupBy g <> colorBy g)
  saveSVG (out "boxplot.svg") $
    layer (boxplot (inline gvals) <> groupBy (inlineCat ggroups) <> colorBy (inlineCat ggroups))
    <> title "Boxplot" <> xLabel "group" <> yLabel "value" <> theme ThemeGrey

  -- 6. 群別バイオリン
  saveSVG (out "violin.svg") $
    layer (violin (inline gvals) <> groupBy (inlineCat ggroups) <> colorBy (inlineCat ggroups))
    <> title "Violin" <> xLabel "group" <> yLabel "value" <> theme ThemeGrey

  -- 6b. distCols: 別列を別 mark で 1 パネルに併置 (★ Phase 36・列名スロット・y 共有)
  let rdc :: Resolver
      rdc "A" = Just (NumData (V.fromList gA))
      rdc "B" = Just (NumData (V.fromList gB))
      rdc "C" = Just (NumData (V.fromList gC))
      rdc _   = Nothing
  saveSVGWith (out "distcols.svg") rdc $
       distCols [ boxplot "A", violin "B", swarm "C" ]
    <> title "distCols (mark 混在併置)" <> xLabel "column" <> yLabel "value" <> theme ThemeGrey

  -- 2 次元場の grid (z = sin x · cos y)
  let gn    = 40
      grid  = [ (x, y) | x <- linspace (-3) 3 gn, y <- linspace (-3) 3 gn ]
      gx    = inline (map fst grid)
      gy    = inline (map snd grid)
      gz    = inline [ sin x * cos y | (x, y) <- grid ]

  -- 7. 等高線 (contour)
  saveSVG (out "contour.svg") $
    layer (contour gx gy gz)
    <> title "Contour" <> xLabel "x" <> yLabel "y" <> theme ThemeGrey

  -- 8. ヒートマップ (bin2d・連続色塗り = pcolor 相当)
  saveSVG (out "heatmap.svg") $
    layer (bin2d gx gy gz)
    <> title "Heatmap" <> xLabel "x" <> yLabel "y" <> theme ThemeGrey

  -- 9. ベクトル場 (quiver・回転場 u=-y, v=x)
  let qn    = 18
      qgrid = [ (x, y) | x <- linspace (-2) 2 qn, y <- linspace (-2) 2 qn ]
      qx    = inline (map fst qgrid)
      qy    = inline (map snd qgrid)
      qu    = inline [ negate y | (_, y) <- qgrid ]
      qv    = inline [ x        | (x, _) <- qgrid ]
  saveSVG (out "quiver.svg") $
    layer (quiver qx qy qu qv)
    <> title "Vector field" <> xLabel "x" <> yLabel "y" <> theme ThemeGrey

  -- 10. 積み上げ棒
  let cats   = ["Q1","Q2","Q3","Q4"] :: [String]
      series = ["A","B","C"] :: [String]
      bx = inlineCat (concatMap (replicate 3) cats)
      bg = inlineCat (concat (replicate 4 series))
      bv = inline [ 3,5,2, 4,3,6, 5,4,3, 6,2,4 ]
  saveSVG (out "bar.svg") $
    layer (bar bx bv <> colorBy bg <> position PosStack)
    <> title "Stacked bar" <> xLabel "quarter" <> yLabel "value" <> theme ThemeGrey

  -- 11. 円グラフ
  saveSVG (out "pie.svg") $
    layer (pie (inlineCat (["A","B","C","D"] :: [String])) (inline [35,25,22,18]))
    <> title "Pie" <> theme ThemeGrey

  -- 12. subplots (= 独立図の貼り合わせ・4 枚ダッシュボード)。
  --     各パネルも  layer (mark …) の文法合成で統一する。
  saveSVG (out "subplots.svg") $
       subplots
         [ layer (scatter (inline sxs) (inline sys) <> alpha 0.6) <> title "scatter"
         , layer (histogram (inline (take 500 (gauss 11)))) <> title "histogram"
         , layer (density (inline (take 500 (gauss 13))) <> densityFill True <> alpha 0.5) <> title "density"
         , layer (boxplot (inline gvals) <> groupBy (inlineCat ggroups) <> colorBy (inlineCat ggroups)) <> title "boxplot"
         ]
    <> subplotCols 2
    <> title "Subplots (4-panel dashboard)" <> theme ThemeGrey
    <> widthUnit (900 *~ px) <> heightUnit (660 *~ px)

  -- 13. 3D 応答曲面 (= z = sin(3r)/r · ½、 viridis colormap + 床 contour)
  let surfN  = 40
      surfAx = linspace (-3) 3 surfN
      sgrid  = [ [ let r = sqrt (x*x + y*y) + 1e-6 in sin (r * 3) / r * 0.5
                 | x <- surfAx ] | y <- surfAx ]
  P3E.saveSVG3D (out "surface3d.svg") $
    P3.layer3D ( P3.surface3DGrid sgrid
                <> P3.xRange3D (-3, 3) <> P3.yRange3D (-3, 3)
                <> P3.colormap3D
                <> P3.contourZ 8 )
   <> P3.camera (cameraIso 5)
   <> P3.title3D "3D surface (z = sin 3r / r)"

  -- 14. hexbin (= 六角ビニング・散布過密の密度を六角セル件数の連続色で。 geom_hex)
  let hxs = take 2000 (map (\e -> 5 + 2 * e) (gauss 21))
      hys = take 2000 (zipWith (\x e -> 0.5 * x + 1.5 * e + 1) hxs (gauss 22))
  saveSVG (out "hexbin.svg") $
       layer (hexbin (inline hxs) (inline hys) <> hexbinBins 25)
    <> title "Hexbin" <> xLabel "x" <> yLabel "y" <> theme ThemeGrey

  -- 15. HBM 階層モデルの DAG (= 「ならでは」図・analyze の本物の HBM を 'dagOf' で構造抽出)。
  --     DAG は構造由来なので少 draws で十分。 実行可能性チェックも兼ねて実際にサンプリングする。
  let dagCfg = defaultHBM { hbmChains = 2, hbmSamples = 300, hbmWarmup = 300
                          , hbmSeed = Just 20260623 }
  putStrLn "sampling hierarchical HBM for DAG (実行可能性チェック)…"
  dagFit <- hbmModel dagCfg dagModel []
  let noDf = [] :: [(T.Text, ColData)]
  saveSVGBound (out "hbm-hier-dag.svg") $
       noDf |>> toPlot (dagOf dagFit)
    <> title "階層ベイズ回帰の DAG (群レベル予測子つき 変量切片+傾き)"
    <> width 820 <> height 660

  putStrLn ("wrote " ++ imgDir ++ "/{quickstart,line,scatter,histogram,density,boxplot,violin,distcols,contour,heatmap,quiver,bar,pie,subplots,surface3d,hexbin,hbm-hier-dag}.svg")

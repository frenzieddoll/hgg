{-# LANGUAGE OverloadedStrings #-}
-- | hgg-semi のテスト。 §A WaferMap の yield/zone 算出と
-- SVG backend 経由の出力を検証する。
module Main where

import qualified Data.Text             as T
import           Test.Hspec

import           Hgg.Plot.Semi.WaferMap
import           Hgg.Plot.Semi.ControlChart
import           Hgg.Plot.Semi.ProbabilityPlot
import           Hgg.Plot.Semi.ParetoChart
import           Hgg.Plot.Semi.BoxCoxPlot
import           Hgg.Plot.Layout.RangeOf (invNormCdf)
import           Hgg.Plot.Math.Special   (betaQuantile)
import           Hgg.Plot.Backend.SVG (renderPrimitivesSVG)

-- | 手組みの ControlChart (center=0, σ=1, 3σ 限界)。 ルール検出単体テスト用。
mkCC :: [Double] -> ControlChart
mkCC ps = ControlChart "t" ps 0 3 (-3) 1 []

-- | 21x21 グリッドの中央 5x5 (col/row 8..12) に die を置く。 全 die が
-- エッジ除外内 (on-wafer)。 bin = 20 Pass / 3 Fail / 2 Skip。
sampleDies :: [Die]
sampleDies =
  [ Die c r (binFor i)
  | (i, (c, r)) <- zip [0 :: Int ..] [ (c', r') | r' <- [8 .. 12], c' <- [8 .. 12] ] ]
  where
    binFor i
      | i < 20    = BinPass
      | i < 23    = BinFail "scratch"
      | otherwise = BinSkip

sampleSpec :: WaferMapSpec
sampleSpec = defaultWaferMapSpec 21 21 sampleDies

-- | 部分文字列の非重複出現回数。
countOf :: T.Text -> T.Text -> Int
countOf = T.count

main :: IO ()
main = hspec $ do

  describe "geometry / on-wafer" $ do
    it "中央 5x5 の die は全て on-wafer" $
      all (onWafer sampleSpec) sampleDies `shouldBe` True
    it "角 (0,0) の die は edge 除外で off-wafer" $
      onWafer sampleSpec (Die 0 0 BinPass) `shouldBe` False
    it "中心 die (10,10) は ZoneCenter" $
      zoneOf sampleSpec (Die 10 10 BinPass) `shouldBe` ZoneCenter

  describe "computeYield" $ do
    let ys = computeYield sampleSpec
    it "Skip を分母から除外して tested=23" $
      ysTotal ys `shouldBe` 23
    it "Pass=20 / Fail=3" $
      (ysPass ys, ysFail ys) `shouldBe` (20, 3)
    it "yield = 20/23 ≈ 86.96%" $
      abs (ysYield ys - (20 / 23 * 100)) < 1e-9 `shouldBe` True
    it "zone 内訳が 3 zone そろう" $
      length (ysByZone ys) `shouldBe` 3
    it "zone 別 tested 合計 = 全 tested" $
      sum [ p + f | (_, p, f) <- ysByZone ys ] `shouldBe` ysTotal ys

  describe "waferMapPrimitives" $ do
    let prims = waferMapPrimitives sampleSpec
    it "primitive 列が空でない" $
      null prims `shouldBe` False

  describe "SVG 出力 (renderPrimitivesSVG 経由)" $ do
    let (w, h) = waferMapViewport sampleSpec
        svg    = renderPrimitivesSVG w h "" (waferMapPrimitives sampleSpec)
    it "<svg> ルートで始まる" $
      T.isPrefixOf "<svg" svg `shouldBe` True
    it "ウェハ円が 1 個 (<circle>)" $
      countOf "<circle" svg `shouldBe` 1
    it "<rect> = 背景 1 + on-wafer die 25 = 26" $
      countOf "<rect" svg `shouldBe` 26
    it "notch マーカーが 1 個 (<path>)" $
      countOf "<path" svg `shouldBe` 1
    it "yield サマリ text を含む" $
      T.isInfixOf "Yield:" svg `shouldBe` True

  describe "ControlChart: X̄-R" $ do
    let (xbar, rch) = xbarRChart [[2,4],[3,5],[2,2],[4,6]]
    it "X̄ 中心 = 総平均 3.5" $
      abs (ccCenter xbar - 3.5) < 1e-9 `shouldBe` True
    it "X̄ UCL = X̿ + A2·R̄ = 3.5 + 1.880·1.5" $
      abs (ccUCL xbar - (3.5 + 1.880 * 1.5)) < 1e-9 `shouldBe` True
    it "R 中心 = R̄ = 1.5" $
      abs (ccCenter rch - 1.5) < 1e-9 `shouldBe` True
    it "R UCL = D4·R̄ = 3.267·1.5" $
      abs (ccUCL rch - 3.267 * 1.5) < 1e-9 `shouldBe` True

  describe "ControlChart: I-MR" $ do
    let (ich, _) = imrChart [10,12,11,13]
    it "I 中心 = 11.5" $
      abs (ccCenter ich - 11.5) < 1e-9 `shouldBe` True
    it "I UCL = x̄ + 3·(MR̄/d2)" $
      abs (ccUCL ich - (11.5 + 3 * ((5/3) / 1.128))) < 1e-9 `shouldBe` True

  describe "ControlChart: CUSUM" $ do
    let (up, _) = cusumChart (defaultCusumParams 0 1) [0,0,0,5]
    it "C+ 累積で最終点 = 4.5" $
      abs (last (ccPoints up) - 4.5) < 1e-9 `shouldBe` True
    it "UCL = H = 4σ = 4" $
      abs (ccUCL up - 4) < 1e-9 `shouldBe` True

  describe "ControlChart: EWMA" $ do
    let ew = ewmaChart (defaultEwmaParams 0 1) [1,1,1]
    it "z 系列 = [0.2, 0.36, 0.488]" $
      and (zipWith (\a b -> abs (a - b) < 1e-9) (ccPoints ew) [0.2, 0.36, 0.488])
        `shouldBe` True
    it "定常 UCL = L·σ·sqrt(λ/(2-λ)) = 3·sqrt(0.2/1.8)" $
      abs (ccUCL ew - 3 * sqrt (0.2 / 1.8)) < 1e-9 `shouldBe` True

  describe "ControlChart: ルール検出" $ do
    it "WE1: 3σ 超の点を index 2 で検出" $
      lookup 2 (westernElectric (mkCC [0,0,4,0])) `shouldSatisfy`
        maybe False (WE 1 `elem`)
    it "Nelson2: 9 連続同側を index 8 で検出" $
      lookup 8 (nelson (mkCC (replicate 9 0.5))) `shouldSatisfy`
        maybe False (Nelson 2 `elem`)
    it "管理内データは違反ゼロ" $
      westernElectric (mkCC [0.1,-0.2,0.0,0.3,-0.1]) `shouldBe` []

  describe "ControlChart: SVG 出力" $ do
    let ich   = attachViolations westernElectric (fst (imrChart [10,12,11,13]))
        (w,h) = controlChartViewport ich
        svg   = renderPrimitivesSVG w h "" (controlChartPrimitives ich)
    it "点 4 個 (<circle>)" $
      countOf "<circle" svg `shouldBe` 4
    it "<line> = 限界 3 + 結線 3 = 6" $
      countOf "<line" svg `shouldBe` 6
    it "UCL ラベルを含む" $
      T.isInfixOf "UCL" svg `shouldBe` True

  describe "ProbabilityPlot: Normal" $ do
    -- データ = Φ⁻¹((i-0.5)/n) なら点は y=x 上 → 傾き1・切片0
    let nN   = 20 :: Int
        dat  = [ invNormCdf ((fromIntegral i - 0.5) / fromIntegral nN) | i <- [1 .. nN] ]
        spec = (defaultProbabilityPlotSpec DistNormal dat) { ppCI = Nothing }
        fl   = fitProbabilityLine (probabilityPlotPoints spec)
    it "傾き ≈ 1" $ abs (flSlope fl - 1) < 1e-9 `shouldBe` True
    it "切片 ≈ 0" $ abs (flIntercept fl) < 1e-9 `shouldBe` True
    it "CI なしなら ptYLo = Nothing" $
      all (\p -> ptYLo p == Nothing) (probabilityPlotPoints spec) `shouldBe` True

  describe "ProbabilityPlot: Weibull" $ do
    -- β=2, η=10 から生成 → Weibull プロットで傾き=β, η 復元
    let nN   = 30 :: Int
        beta = 2.0; eta = 10.0
        pp i = (fromIntegral i - 0.5) / fromIntegral nN
        dat  = [ eta * (negate (log (1 - pp i))) ** (1 / beta) | i <- [1 .. nN] ]
        spec = defaultProbabilityPlotSpec DistWeibull dat
        fl   = fitProbabilityLine (probabilityPlotPoints spec)
        (b', eta') = weibullParams fl
    it "傾き ≈ β = 2" $ abs (b' - beta) < 1e-6 `shouldBe` True
    it "尺度 ≈ η = 10" $ abs (eta' - eta) < 1e-6 `shouldBe` True

  describe "ProbabilityPlot: CI / SVG" $ do
    let dat  = [2.1, 3.4, 1.9, 5.2, 4.1, 3.3, 2.8, 4.7, 3.9, 2.5]
        spec = defaultProbabilityPlotSpec DistLogNormal dat   -- ppCI = Just 0.95
        pts  = probabilityPlotPoints spec
    it "全点で ptYLo < ptY < ptYHi" $
      all (\p -> maybe False (< ptY p) (ptYLo p) && maybe False (> ptY p) (ptYHi p)) pts
        `shouldBe` True
    -- 厳密 Beta CI: i=1 の下端 = invNormCdf(Beta 分位点(0.025; 1, n))。
    -- log-normal の y 軸変換は yQuantile = invNormCdf なので Normal と同型。
    it "i=1 下端が exact Beta(1,n) の 2.5% 分位点と一致" $
      let nN = length dat
          want = invNormCdf (betaQuantile 0.025 1 (fromIntegral nN))
      in case ptYLo (head pts) of
           Just lo -> abs (lo - want) < 1e-9 `shouldBe` True
           Nothing -> expectationFailure "ptYLo が Nothing"
    it "正規近似 (対称 SE) では無いこと (非対称帯)" $
      let p0   = head pts
          dHi  = maybe 0 (\h -> h - ptY p0) (ptYHi p0)
          dLo  = maybe 0 (\l -> ptY p0 - l) (ptYLo p0)
      in (abs (dHi - dLo) > 1e-6) `shouldBe` True
    let (w,h) = probabilityPlotViewport spec
        svg   = renderPrimitivesSVG w h "" (probabilityPlotPrimitives spec)
    it "点数 = データ数 (<circle>)" $
      countOf "<circle" svg `shouldBe` length dat
    it "CI 帯が 1 個 (<path>)" $
      countOf "<path" svg `shouldBe` 1
    it "当てはめ線が 1 本 (<line>)" $
      countOf "<line" svg `shouldBe` 1

  describe "ParetoChart" $ do
    let spec = defaultParetoChartSpec [("C",20),("A",50),("B",30)]
        bars = paretoData spec
    it "件数降順 (A,B,C)" $
      map pbLabel bars `shouldBe` ["A","B","C"]
    it "累積% = [50, 80, 100]" $
      and (zipWith (\a b -> abs (a - b) < 1e-9) (map pbCumPct bars) [50,80,100])
        `shouldBe` True
    it "最終累積% = 100" $
      abs (pbCumPct (last bars) - 100) < 1e-9 `shouldBe` True
    let (w,h) = paretoChartViewport spec
        svg   = renderPrimitivesSVG w h "" (paretoChartPrimitives spec)
    it "バー 3 本 + 背景 1 (<rect>)" $
      countOf "<rect" svg `shouldBe` 4   -- renderPrimitivesSVG が背景 rect を前置
    it "累積点 3 個 (<circle>)" $
      countOf "<circle" svg `shouldBe` 3
    it "% ラベルを含む" $
      T.isInfixOf "%" svg `shouldBe` True

  describe "BoxCoxPlot" $ do
    it "変換: bc 1 8 = 7" $ abs (boxCoxTransform 1 8 - 7) < 1e-9 `shouldBe` True
    it "変換: bc 0 e = 1 (log)" $ abs (boxCoxTransform 0 (exp 1) - 1) < 1e-9 `shouldBe` True
    it "変換: bc 2 8 = 31.5" $ abs (boxCoxTransform 2 8 - 31.5) < 1e-9 `shouldBe` True
    -- log-normal データ (= exp(対称)) は λ̂ ≈ 0
    let zs   = [ -2, -1.5 .. 2 ] :: [Double]
        ys   = map exp zs
        res  = boxCoxProfile (defaultBoxCoxSpec ys)
    it "log-normal データで λ̂ ≈ 0" $
      abs (brOptLambda res) < 0.1 `shouldBe` True
    it "curve 点数 = steps (81)" $
      length (brCurve res) `shouldBe` 81
    it "CI が λ̂ を含む" $
      maybe False (\(a,b) -> a <= brOptLambda res && brOptLambda res <= b) (brCI res)
        `shouldBe` True
    let (w,h) = boxCoxViewport (defaultBoxCoxSpec ys)
        svg   = renderPrimitivesSVG w h "" (boxCoxPrimitives (defaultBoxCoxSpec ys))
    it "lambda_hat ラベルを含む" $
      T.isInfixOf "lambda_hat" svg `shouldBe` True

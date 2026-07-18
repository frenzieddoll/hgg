-- |
-- Module      : Graphics.Hgg.Semi.ProbabilityPlot
-- Description : 確率プロット (Normal / LogNormal / Weibull Q-Q + rank-based CI)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 順序統計量を参照分布の理論分位点に対してプロットし、 データが分布に従えば
-- 直線に乗る「確率プロット」 を一般化して実装する。 軸変換:
--
--   * Normal    : x = 値、         y = Φ⁻¹(p)
--   * LogNormal : x = log(値)、    y = Φ⁻¹(p)
--   * Weibull   : x = log(値)、    y = log(-log(1-p))   (β = 直線の傾き)
--
-- plotting position は p_i = (i-0.5)/n (core の 'qqPoints' と同一)。
-- 信頼帯は **厳密な順序統計量 CI**: i 番目順序統計量 (一様標本) の plotting
-- position は U_(i) ~ Beta(i, n-i+1) に従うので、 信頼水準 conf に対し
-- p_lo = Beta 分位点(α/2; i, n-i+1)、 p_hi = Beta 分位点(1-α/2; i, n-i+1)
-- (α = 1-conf) を理論分位点に写す (core 'betaQuantile' を使用)。 小 n でも
-- 正しい非対称帯になる (旧 MVP の正規近似 p ± z·sqrt(p(1-p)/n) を置換)。
--
-- backend には依存せず 'probabilityPlotPrimitives' が 'Primitive' 列を返す。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Semi.ProbabilityPlot
  ( -- * Spec
    ProbDist(..)
  , ProbabilityPlotSpec(..)
  , defaultProbabilityPlotSpec
    -- * 点列 + 直線当てはめ
  , PlotPoint(..)
  , probabilityPlotPoints
  , FittedLine(..)
  , fitProbabilityLine
  , weibullParams
    -- * Render
  , probabilityPlotViewport
  , probabilityPlotPrimitives
  ) where

import           Data.List                  (sort)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Printf                (printf)

import           Graphics.Hgg.Layout        (Rect (..))
import           Graphics.Hgg.Layout.RangeOf (invNormCdf)
import           Graphics.Hgg.Math.Special  (betaQuantile)
import           Graphics.Hgg.Render        (FillStyle (..), LineStyle (..),
                                             PathSegment (..), Point (..),
                                             Primitive (..), StrokeStyle (..),
                                             TextAnchor (..), TextStyle (..))

-- ===========================================================================
-- Spec
-- ===========================================================================

-- | 参照分布。
data ProbDist = DistNormal | DistLogNormal | DistWeibull
  deriving (Show, Eq)

-- | 確率プロットの入力。
data ProbabilityPlotSpec = ProbabilityPlotSpec
  { ppDist  :: !ProbDist
  , ppData  :: ![Double]
  , ppCI    :: !(Maybe Double)   -- ^ 信頼水準 (例 Just 0.95)。 Nothing = 帯なし
  , ppTitle :: !Text
  } deriving (Show, Eq)

defaultProbabilityPlotSpec :: ProbDist -> [Double] -> ProbabilityPlotSpec
defaultProbabilityPlotSpec dist xs = ProbabilityPlotSpec
  { ppDist = dist, ppData = xs, ppCI = Just 0.95
  , ppTitle = distTitle dist }

distTitle :: ProbDist -> Text
distTitle DistNormal    = "Normal probability plot"
distTitle DistLogNormal = "Log-normal probability plot"
distTitle DistWeibull   = "Weibull probability plot"

-- ===========================================================================
-- 軸変換 + 点列
-- ===========================================================================

-- | データ値 → プロット x。 log 系分布は log(値)。
xTransform :: ProbDist -> Double -> Double
xTransform DistNormal v = v
xTransform _          v = log v

-- | plotting position p → プロット y (理論分位点)。
yQuantile :: ProbDist -> Double -> Double
yQuantile DistWeibull p = log (negate (log (1 - p)))
yQuantile _           p = invNormCdf p

-- | プロット 1 点。 'ptYLo' / 'ptYHi' は CI 帯の下端 / 上端 (帯なしは Nothing)。
data PlotPoint = PlotPoint
  { ptX   :: !Double
  , ptY   :: !Double
  , ptOrig :: !Double          -- ^ 元のデータ値 (hover 表示用)
  , ptYLo :: !(Maybe Double)
  , ptYHi :: !(Maybe Double)
  } deriving (Show, Eq)

-- | 確率プロットの点列。 log 系分布では正値のみ採用。
probabilityPlotPoints :: ProbabilityPlotSpec -> [PlotPoint]
probabilityPlotPoints spec =
  let dist   = ppDist spec
      usable = case dist of
        DistNormal -> ppData spec
        _          -> filter (> 0) (ppData spec)
      ys = sort usable
      n  = length ys
      mk (i, v) =
        let p  = (fromIntegral i - 0.5) / fromIntegral n
            -- 厳密 rank-based CI: U_(i) ~ Beta(i, n-i+1) の α/2・1-α/2 分位点。
            ci = case ppCI spec of
                   Nothing   -> (Nothing, Nothing)
                   Just conf ->
                     let alpha   = 1 - conf
                         a       = fromIntegral i
                         b       = fromIntegral (n - i + 1)
                         eps     = 1e-6
                         clamp t = max eps (min (1 - eps) t)
                         pLo     = betaQuantile (alpha / 2)       a b
                         pHi     = betaQuantile (1 - alpha / 2)   a b
                     in ( Just (yQuantile dist (clamp pLo))
                        , Just (yQuantile dist (clamp pHi)) )
        in PlotPoint (xTransform dist v) (yQuantile dist p) v (fst ci) (snd ci)
  in if n == 0 then [] else map mk (zip [(1 :: Int) ..] ys)

-- ===========================================================================
-- 直線当てはめ
-- ===========================================================================

-- | 最小二乗で当てはめた参照直線 (y = slope·x + intercept)。
data FittedLine = FittedLine
  { flSlope     :: !Double
  , flIntercept :: !Double
  } deriving (Show, Eq)

-- | (ptX, ptY) への最小二乗当てはめ。
fitProbabilityLine :: [PlotPoint] -> FittedLine
fitProbabilityLine pts
  | null pts  = FittedLine 0 0
  | otherwise =
      let xs  = map ptX pts
          ysv = map ptY pts
          nn  = fromIntegral (length pts)
          mx  = sum xs / nn
          my  = sum ysv / nn
          sxx = sum [ (x - mx) ** 2 | x <- xs ]
          sxy = sum [ (x - mx) * (y - my) | (x, y) <- zip xs ysv ]
          slope = if sxx == 0 then 0 else sxy / sxx
      in FittedLine slope (my - slope * mx)

-- | Weibull プロットの傾き / 切片から (形状 β, 尺度 η) を復元。
-- y = β·log x - β·log η より β = slope, η = exp(-intercept/slope)。
weibullParams :: FittedLine -> (Double, Double)
weibullParams (FittedLine slope icpt) =
  (slope, if slope == 0 then 0 else exp (negate icpt / slope))

-- ===========================================================================
-- Render
-- ===========================================================================

marginL, marginR, marginT, marginB, plotW, plotH :: Double
marginL = 52
marginR = 24
marginT = 30
marginB = 40
plotW   = 320
plotH   = 240

-- | SVG / PNG 出力に渡す viewport 寸法。
probabilityPlotViewport :: ProbabilityPlotSpec -> (Int, Int)
probabilityPlotViewport _ =
  (ceiling (marginL + plotW + marginR), ceiling (marginT + plotH + marginB))

-- | 確率プロットの backend 非依存 'Primitive' 列。
probabilityPlotPrimitives :: ProbabilityPlotSpec -> [Primitive]
probabilityPlotPrimitives spec =
  let pts = probabilityPlotPoints spec
  in if null pts then [frame] else
     let fl    = fitProbabilityLine pts
         xs    = map ptX pts
         hiY p = maybe (ptY p) id (ptYHi p)
         loY p = maybe (ptY p) id (ptYLo p)
         yAll  = concat [ [ptY p, hiY p, loY p] | p <- pts ]
         (xlo, xhi) = padRange (minimum xs) (maximum xs)
         (ylo, yhi) = padRange (minimum yAll) (maximum yAll)
         xAt x = marginL + (x - xlo) / (xhi - xlo) * plotW
         yAt y = marginT + plotH - (y - ylo) / (yhi - ylo) * plotH

         band
           | ppCI spec == Nothing = []
           | otherwise =
               let ups = [ Point (xAt (ptX p)) (yAt (hiY p)) | p <- pts ]
                   los = [ Point (xAt (ptX p)) (yAt (loY p)) | p <- reverse pts ]
                   ring = ups ++ los
               in case ring of
                    []      -> []
                    (q:qs)  -> [ PPath (MoveTo q : map LineTo qs ++ [ClosePath])
                                       (FillStyle "#3b82f6" 0.15) Nothing ]

         fitLine =
           let a = flIntercept fl; b = flSlope fl
               y0 = a + b * xlo; y1 = a + b * xhi
           in PLine (Point (xAt xlo) (yAt y0)) (Point (xAt xhi) (yAt y1))
                    (LineStyle "#dc2626" 1.5 [])

         dot p =
           PCircle (Point (xAt (ptX p)) (yAt (ptY p))) 3 (FillStyle "#2563eb" 1.0)
                   (Just (StrokeStyle "#ffffff" 0.7))
                   (Just (T.pack (printf "value=%.4g" (ptOrig p))))
         dots = map dot pts

         title = PText (Point marginL (marginT - 12)) (ppTitle spec)
                   (TextStyle "#0f172a" 14 "sans-serif" AnchorStart 0 "bold" False)
         xlab = PText (Point (marginL + plotW / 2) (marginT + plotH + 28))
                   (xAxisLabel (ppDist spec))
                   (TextStyle "#334155" 11 "sans-serif" AnchorMiddle 0 "normal" False)
         annot = case ppDist spec of
           DistWeibull ->
             let (beta, eta) = weibullParams fl
             in [ PText (Point (marginL + 8) (marginT + 14))
                    (T.pack (printf "beta=%.3g  eta=%.4g" beta eta))
                    (TextStyle "#7c3aed" 11 "sans-serif" AnchorStart 0 "bold" False) ]
           _ -> []
     in concat [ [frame], band, [fitLine], dots, [title, xlab], annot ]

-- | プロット領域枠。
frame :: Primitive
frame = PRect (Rect marginL marginT plotW plotH)
              (FillStyle "#ffffff" 1.0)
              (Just (StrokeStyle "#cbd5e1" 1.0))

xAxisLabel :: ProbDist -> Text
xAxisLabel DistNormal = "value"
xAxisLabel _          = "log(value)"

-- | 範囲に 6% パディング。 退化時は ±0.5。
padRange :: Double -> Double -> (Double, Double)
padRange lo hi =
  let d = hi - lo
  in if d <= 0 then (lo - 0.5, hi + 0.5)
     else (lo - d * 0.06, hi + d * 0.06)

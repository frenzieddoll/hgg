-- |
-- Module      : Graphics.Hgg.Semi.BoxCoxPlot
-- Description : Box-Cox 変換のプロファイル対数尤度プロット (λ vs logLik + 最適 λ)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Box-Cox 変換 y(λ) = (y^λ-1)/λ (λ≠0) / log y (λ=0) のパラメータ λ を、
-- プロファイル対数尤度
--
--   L(λ) = -(n/2)·ln( (1/n)Σ(z_i - z̄)² ) + (λ-1)Σ ln y_i
--
-- (z_i = boxcox(y_i,λ)) で評価し、 λ̂ = argmax L を求める。 尤度比による
-- 95% CI (= L が L_max - 1.92 を上回る λ 区間) も算出する。
--
-- backend 非依存 'Primitive' 列を返す。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Semi.BoxCoxPlot
  ( BoxCoxSpec(..)
  , defaultBoxCoxSpec
  , boxCoxTransform
  , profileLogLik
  , BoxCoxResult(..)
  , boxCoxProfile
  , boxCoxViewport
  , boxCoxPrimitives
  ) where

import           Data.List           (maximumBy)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Printf         (printf)

import           Graphics.Hgg.Layout (Rect (..))
import           Graphics.Hgg.Render (FillStyle (..), LineStyle (..),
                                      PathSegment (..), Point (..),
                                      Primitive (..), StrokeStyle (..),
                                      TextAnchor (..), TextStyle (..))

-- ===========================================================================
-- Spec / 計算
-- ===========================================================================

data BoxCoxSpec = BoxCoxSpec
  { bcData        :: ![Double]            -- ^ 正値データ (非正は除外)
  , bcLambdaRange :: !(Double, Double)    -- ^ λ グリッド範囲
  , bcSteps       :: !Int                 -- ^ グリッド分割点数
  , bcTitle       :: !Text
  } deriving (Show, Eq)

defaultBoxCoxSpec :: [Double] -> BoxCoxSpec
defaultBoxCoxSpec xs = BoxCoxSpec xs (-2, 2) 81 "Box-Cox plot"

-- | Box-Cox 変換。 λ=0 (|λ|<1e-8) は log。
boxCoxTransform :: Double -> Double -> Double
boxCoxTransform lam y
  | abs lam < 1e-8 = log y
  | otherwise      = (y ** lam - 1) / lam

-- | プロファイル対数尤度 L(λ)。
profileLogLik :: [Double] -> Double -> Double
profileLogLik ys lam =
  let n    = fromIntegral (length ys)
      z    = map (boxCoxTransform lam) ys
      zbar = sum z / n
      s2   = sum [ (zi - zbar) ** 2 | zi <- z ] / n
      jac  = (lam - 1) * sum (map log ys)
  in if s2 <= 0 then -1 / 0 else negate (n / 2) * log s2 + jac

-- | Box-Cox プロファイルの結果。
data BoxCoxResult = BoxCoxResult
  { brCurve     :: ![(Double, Double)]      -- ^ (λ, logLik) グリッド
  , brOptLambda :: !Double
  , brOptLL     :: !Double
  , brCI        :: !(Maybe (Double, Double)) -- ^ 95% CI (λ_lo, λ_hi)
  } deriving (Show, Eq)

-- | グリッド上でプロファイル尤度を評価し λ̂ と CI を求める。
boxCoxProfile :: BoxCoxSpec -> BoxCoxResult
boxCoxProfile spec =
  let ys      = filter (> 0) (bcData spec)
      (lo, hi) = bcLambdaRange spec
      steps   = max 2 (bcSteps spec)
      lams    = [ lo + (hi - lo) * fromIntegral i / fromIntegral (steps - 1)
                | i <- [0 .. steps - 1] ]
      curve   = [ (lam, profileLogLik ys lam) | lam <- lams ]
      (optL, optLL) = maximumBy (comparing snd) curve
      thr     = optLL - 1.92
      ci      = ciFromCurve curve thr
  in BoxCoxResult curve optL optLL ci

-- | 尤度比閾値 thr を超える λ 区間を、 交差点の線形補間で求める。
ciFromCurve :: [(Double, Double)] -> Double -> Maybe (Double, Double)
ciFromCurve curve thr =
  let segs = zip curve (drop 1 curve)
      cross ((x1, y1), (x2, y2)) =
        if (y1 - thr) == 0 then [x1]
        else if (y1 < thr) /= (y2 < thr)
               then [ x1 + (x2 - x1) * (thr - y1) / (y2 - y1) ]
               else []
      crossings = concatMap cross segs
      inside    = [ x | (x, y) <- curve, y >= thr ]
      candidates = crossings ++ inside
  in if null candidates then Nothing else Just (minimum candidates, maximum candidates)

-- ===========================================================================
-- Render
-- ===========================================================================

marginL, marginR, marginT, marginB, plotW, plotH :: Double
marginL = 52
marginR = 22
marginT = 30
marginB = 40
plotW   = 320
plotH   = 220

-- | SVG / PNG 出力に渡す viewport 寸法。
boxCoxViewport :: BoxCoxSpec -> (Int, Int)
boxCoxViewport _ =
  (ceiling (marginL + plotW + marginR), ceiling (marginT + plotH + marginB))

-- | Box-Cox プロットの backend 非依存 'Primitive' 列。
boxCoxPrimitives :: BoxCoxSpec -> [Primitive]
boxCoxPrimitives spec =
  let res   = boxCoxProfile spec
      curve = brCurve res
  in if null curve then [frame] else
     let lams = map fst curve
         lls  = map snd curve
         (lamLo, lamHi) = (minimum lams, maximum lams)
         (llLo0, llHi0) = (minimum lls, maximum lls)
         (llLo, llHi)   = padRange llLo0 llHi0
         xAt l  = marginL + (l - lamLo) / (lamHi - lamLo) * plotW
         yAt v  = marginT + plotH - (v - llLo) / (llHi - llLo) * plotH
         top    = marginT
         bottom = marginT + plotH

         pts = [ Point (xAt l) (yAt v) | (l, v) <- curve ]
         curveP =
           [ PLine a b (LineStyle "#2563eb" 1.6 []) | (a, b) <- zip pts (drop 1 pts) ]

         -- 95% CI 帯
         ciP = case brCI res of
           Just (clo, chi) ->
             [ PRect (Rect (xAt clo) top (xAt chi - xAt clo) plotH)
                     (FillStyle "#60a5fa" 0.12) Nothing ]
           Nothing -> []

         -- 最適 λ の縦線 + ラベル
         optX = xAt (brOptLambda res)
         optP =
           [ PLine (Point optX top) (Point optX bottom) (LineStyle "#dc2626" 1.3 [5, 3])
           , PText (Point (optX + 4) (top + 14))
                   (T.pack (printf "lambda_hat=%.3g" (brOptLambda res)))
                   (TextStyle "#dc2626" 11 "sans-serif" AnchorStart 0 "bold" False) ]

         -- 尤度比閾値の横破線
         thr  = brOptLL res - 1.92
         thrP = [ PLine (Point marginL (yAt thr)) (Point (marginL + plotW) (yAt thr))
                        (LineStyle "#16a34a" 1.0 [4, 3]) ]

         xlab = PText (Point (marginL + plotW / 2) (bottom + 28)) "lambda"
                  (TextStyle "#334155" 11 "sans-serif" AnchorMiddle 0 "normal" False)
         title = PText (Point marginL (marginT - 12)) (bcTitle spec)
                   (TextStyle "#0f172a" 14 "sans-serif" AnchorStart 0 "bold" False)
     in concat [ [frame], ciP, thrP, curveP, optP, [xlab, title] ]

frame :: Primitive
frame = PRect (Rect marginL marginT plotW plotH)
              (FillStyle "#ffffff" 1.0)
              (Just (StrokeStyle "#cbd5e1" 1.0))

padRange :: Double -> Double -> (Double, Double)
padRange lo hi =
  let d = hi - lo
  in if d <= 0 then (lo - 0.5, hi + 0.5) else (lo - d * 0.08, hi + d * 0.08)

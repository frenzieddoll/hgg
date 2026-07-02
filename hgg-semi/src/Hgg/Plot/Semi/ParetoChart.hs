-- |
-- Module      : Hgg.Plot.Semi.ParetoChart
-- Description : パレート図 (件数バー + 累積% 線、 dual-Y)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- カテゴリを件数の降順に並べ、 左 Y 軸に件数バー、 右 Y 軸 (0-100%) に
-- 累積百分率の折れ線を重ねる。 左軸の上端 = 総件数 にすることで「累積% が
-- 100% = バー総和」 が画素上で一致する (古典的パレート図の整列)。
--
-- backend 非依存 'Primitive' 列を返す。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Semi.ParetoChart
  ( ParetoChartSpec(..)
  , defaultParetoChartSpec
  , ParetoBar(..)
  , paretoData
  , paretoChartViewport
  , paretoChartPrimitives
  ) where

import           Data.List           (sortBy)
import           Data.Ord            (Down (..), comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Printf         (printf)

import           Hgg.Plot.Layout (Rect (..))
import           Hgg.Plot.Render (FillStyle (..), LineStyle (..),
                                      Point (..), Primitive (..),
                                      StrokeStyle (..), TextAnchor (..),
                                      TextStyle (..))

-- ===========================================================================
-- Spec
-- ===========================================================================

data ParetoChartSpec = ParetoChartSpec
  { pcCategories :: ![(Text, Double)]   -- ^ (ラベル, 件数)。 内部で降順ソート
  , pcThreshold  :: !(Maybe Double)     -- ^ 累積% 参照線 (例 Just 80)。 Nothing = なし
  , pcTitle      :: !Text
  } deriving (Show, Eq)

defaultParetoChartSpec :: [(Text, Double)] -> ParetoChartSpec
defaultParetoChartSpec cats = ParetoChartSpec cats (Just 80) "Pareto chart"

-- | 降順ソート済の 1 バー (ラベル / 件数 / 累積%)。
data ParetoBar = ParetoBar
  { pbLabel  :: !Text
  , pbCount  :: !Double
  , pbCumPct :: !Double
  } deriving (Show, Eq)

-- | カテゴリを降順ソートし累積% を付与。
paretoData :: ParetoChartSpec -> [ParetoBar]
paretoData spec =
  let sorted = sortBy (comparing (Down . snd)) (pcCategories spec)
      total  = sum (map snd sorted)
      go _ [] = []
      go acc ((lab, c) : rest) =
        let acc' = acc + c
            pct  = if total <= 0 then 0 else acc' / total * 100
        in ParetoBar lab c pct : go acc' rest
  in go 0 sorted

-- ===========================================================================
-- Render
-- ===========================================================================

marginL, marginR, marginT, marginB, plotH, slotW :: Double
marginL = 50
marginR = 52
marginT = 30
marginB = 48
plotH   = 240
slotW   = 46

-- | SVG / PNG 出力に渡す viewport 寸法。
paretoChartViewport :: ParetoChartSpec -> (Int, Int)
paretoChartViewport spec =
  let n = length (pcCategories spec)
      w = marginL + fromIntegral (max 1 n) * slotW + marginR
      h = marginT + plotH + marginB
  in (ceiling w, ceiling h)

-- | パレート図の backend 非依存 'Primitive' 列。
paretoChartPrimitives :: ParetoChartSpec -> [Primitive]
paretoChartPrimitives spec =
  let bars  = paretoData spec
      n     = length bars
  in if n == 0 then [] else
     let total = sum (map pbCount bars)
         lo    = marginL
         bottom = marginT + plotH
         top    = marginT
         leftMax = if total <= 0 then 1 else total
         xCenter i = marginL + (fromIntegral i + 0.5) * slotW
         barW   = slotW * 0.62
         yLeft v  = bottom - v / leftMax * plotH         -- 件数 → px
         yRight p = bottom - p / 100 * plotH             -- 累積% → px

         -- 軸線
         axes =
           [ PLine (Point lo top) (Point lo bottom) (LineStyle "#94a3b8" 1.0 [])
           , PLine (Point (lo + fromIntegral n * slotW) top)
                   (Point (lo + fromIntegral n * slotW) bottom) (LineStyle "#94a3b8" 1.0 [])
           , PLine (Point lo bottom) (Point (lo + fromIntegral n * slotW) bottom)
                   (LineStyle "#94a3b8" 1.0 []) ]

         -- 件数バー
         barOf i b =
           let h = bottom - yLeft (pbCount b)
           in PRect (Rect (xCenter i - barW / 2) (yLeft (pbCount b)) barW h)
                    (FillStyle "#60a5fa" 0.9) (Just (StrokeStyle "#2563eb" 0.8))
         barsP = zipWith barOf [0 :: Int ..] bars

         -- 累積% 折れ線 + 点
         cumPts = [ Point (xCenter i) (yRight (pbCumPct b)) | (i, b) <- zip [0 :: Int ..] bars ]
         connectors =
           [ PLine a b (LineStyle "#dc2626" 1.5 [])
           | (a, b) <- zip cumPts (drop 1 cumPts) ]
         cumDots =
           [ PCircle p 3 (FillStyle "#dc2626" 1.0) (Just (StrokeStyle "#ffffff" 0.7))
                     (Just (T.pack (printf "%.1f%%" (pbCumPct b))))
           | (p, b) <- zip cumPts bars ]

         -- 閾値線
         thresholdP = case pcThreshold spec of
           Just t ->
             [ PLine (Point lo (yRight t)) (Point (lo + fromIntegral n * slotW) (yRight t))
                     (LineStyle "#16a34a" 1.0 [4, 3])
             , PText (Point (lo + fromIntegral n * slotW + 4) (yRight t + 4))
                     (T.pack (printf "%.0f%%" t))
                     (TextStyle "#16a34a" 11 "sans-serif" AnchorStart 0 "normal" False) ]
           Nothing -> []

         -- カテゴリラベル
         catLabels =
           [ PText (Point (xCenter i) (bottom + 16)) (pbLabel b)
                   (TextStyle "#334155" 10 "sans-serif" AnchorMiddle 0 "normal" False)
           | (i, b) <- zip [0 :: Int ..] bars ]

         -- 軸端ラベル
         axisLabels =
           [ PText (Point (lo - 6) (top + 4)) (fmtCount leftMax)
               (TextStyle "#2563eb" 10 "sans-serif" AnchorEnd 0 "normal" False)
           , PText (Point (lo - 6) (bottom + 4)) "0"
               (TextStyle "#2563eb" 10 "sans-serif" AnchorEnd 0 "normal" False)
           , PText (Point (lo + fromIntegral n * slotW + 6) (top + 4)) "100%"
               (TextStyle "#dc2626" 10 "sans-serif" AnchorStart 0 "normal" False) ]

         title = PText (Point marginL (marginT - 12)) (pcTitle spec)
                   (TextStyle "#0f172a" 14 "sans-serif" AnchorStart 0 "bold" False)
     in concat [ axes, barsP, connectors, thresholdP, cumDots, catLabels, axisLabels, [title] ]

fmtCount :: Double -> Text
fmtCount x = T.pack (printf "%.0f" x)

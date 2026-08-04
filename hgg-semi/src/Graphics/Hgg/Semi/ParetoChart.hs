-- |
-- Module      : Graphics.Hgg.Semi.ParetoChart
-- Description : Pareto chart (count bars plus cumulative-% line, dual-Y)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: カテゴリを件数の降順に並べ、 左 Y 軸に件数バー、 右 Y 軸 (0-100%) に
-- 累積百分率の折れ線を重ねる。 左軸の上端 = 総件数 にすることで「累積% が
-- 100% = バー総和」 が画素上で一致する (古典的パレート図の整列)。
--
-- backend 非依存 'Primitive' 列を返す。
--
-- [English]: Sorts categories in descending order of count, overlaying a bar
-- of counts on the left Y axis with a cumulative-percentage line on the
-- right Y axis (0-100%). Setting the top of the left axis to the grand
-- total makes "cumulative % = 100%" align pixel-for-pixel with "bar sum"
-- (the classic Pareto-chart alignment).
--
-- Returns a backend-agnostic list of 'Primitive's.
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Semi.ParetoChart
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

import           Graphics.Hgg.Layout (Rect (..))
import           Graphics.Hgg.Render (FillStyle (..), LineStyle (..),
                                      Point (..), Primitive (..),
                                      StrokeStyle (..), TextAnchor (..),
                                      TextStyle (..))

-- ===========================================================================
-- Spec
-- ===========================================================================

data ParetoChartSpec = ParetoChartSpec
  { pcCategories :: ![(Text, Double)]   -- ^ [日本語]: (ラベル, 件数)。 内部で降順ソート。
                                         --   [English]: (label, count) pairs. Sorted descending internally.
  , pcThreshold  :: !(Maybe Double)     -- ^ [日本語]: 累積% 参照線 (例 Just 80)。 Nothing = なし。
                                         --   [English]: The cumulative-% reference line (e.g. @Just 80@). @Nothing@ = none.
  , pcTitle      :: !Text
  } deriving (Show, Eq)

defaultParetoChartSpec :: [(Text, Double)] -> ParetoChartSpec
defaultParetoChartSpec cats = ParetoChartSpec cats (Just 80) "Pareto chart"

-- | [日本語]: 降順ソート済の 1 バー (ラベル / 件数 / 累積%)。
--   [English]: One bar after descending sort (label / count / cumulative %).
data ParetoBar = ParetoBar
  { pbLabel  :: !Text
  , pbCount  :: !Double
  , pbCumPct :: !Double
  } deriving (Show, Eq)

-- | [日本語]: カテゴリを降順ソートし累積% を付与。
--   [English]: Sorts categories in descending order and attaches cumulative %.
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

-- | [日本語]: SVG / PNG 出力に渡す viewport 寸法。
--   [English]: The viewport dimensions passed to SVG / PNG output.
paretoChartViewport :: ParetoChartSpec -> (Int, Int)
paretoChartViewport spec =
  let n = length (pcCategories spec)
      w = marginL + fromIntegral (max 1 n) * slotW + marginR
      h = marginT + plotH + marginB
  in (ceiling w, ceiling h)

-- | [日本語]: パレート図の backend 非依存 'Primitive' 列。
--   [English]: The backend-agnostic 'Primitive' list for the Pareto chart.
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

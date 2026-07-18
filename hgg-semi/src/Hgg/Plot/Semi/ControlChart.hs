-- |
-- Module      : Hgg.Plot.Semi.ControlChart
-- Description : 統計的工程管理図 (X̄-R / I-MR / CUSUM / EWMA + WE/Nelson ルール)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 1 つの管理図を「プロット系列 + 中心線 (CL) + 管理限界 (UCL/LCL) +
-- プロット統計量の 1σ + ルール違反点」 という共通表現 'ControlChart' に
-- まとめ、 各種チャート (X̄-R / I-MR / CUSUM / EWMA) の constructor と
-- Western Electric / Nelson ルール検出、 backend 非依存 'Primitive' 出力を
-- 提供する。
--
-- ルールは「位置チャート (X̄ / I)」 に適用するのが標準。 σ は
-- @(UCL - CL) / 3@ で推定する (3σ 管理限界前提)。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Semi.ControlChart
  ( -- * 共通表現
    ControlChart(..)
  , Rule(..)
    -- * Chart constructors
  , xbarRChart
  , imrChart
  , CusumParams(..)
  , defaultCusumParams
  , cusumChart
  , EwmaParams(..)
  , defaultEwmaParams
  , ewmaChart
    -- * ルール検出
  , westernElectric
  , nelson
  , attachViolations
    -- * Render
  , controlChartViewport
  , controlChartPrimitives
  ) where

import           Data.List           (sort)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Printf         (printf)

import           Hgg.Plot.Layout (Rect (..))
import           Hgg.Plot.Render (FillStyle (..), LineStyle (..),
                                      Point (..), Primitive (..),
                                      StrokeStyle (..), TextAnchor (..),
                                      TextStyle (..))

-- ===========================================================================
-- 共通表現
-- ===========================================================================

-- | 1 つの管理図。 'ccSigma' はプロット統計量の 1σ (= @(UCL-CL)/3@)。
data ControlChart = ControlChart
  { ccTitle      :: !Text
  , ccPoints     :: ![Double]
  , ccCenter     :: !Double
  , ccUCL        :: !Double
  , ccLCL        :: !Double
  , ccSigma      :: !Double
  , ccViolations :: ![(Int, [Rule])]   -- ^ 点 index → 発火ルール (index 昇順)
  } deriving (Show, Eq)

-- | ルール識別 (Western Electric 1-4 / Nelson 1-8)。
data Rule = WE !Int | Nelson !Int
  deriving (Show, Eq, Ord)

-- ===========================================================================
-- 小ヘルパ
-- ===========================================================================

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

rangeOf :: [Double] -> Double
rangeOf [] = 0
rangeOf xs = maximum xs - minimum xs

-- | X̄-R 用の管理図定数 (A2, D3, D4)。 部分群サイズ 2..10 を table 参照、
-- 範囲外は近端にクランプ。
xbarConstants :: Int -> (Double, Double, Double)
xbarConstants n =
  let tbl = [ (2,  (1.880, 0.000, 3.267))
            , (3,  (1.023, 0.000, 2.574))
            , (4,  (0.729, 0.000, 2.282))
            , (5,  (0.577, 0.000, 2.114))
            , (6,  (0.483, 0.000, 2.004))
            , (7,  (0.419, 0.076, 1.924))
            , (8,  (0.373, 0.136, 1.864))
            , (9,  (0.337, 0.184, 1.816))
            , (10, (0.308, 0.223, 1.777)) ]
      nn = max 2 (min 10 n)
  in maybe (0.577, 0.000, 2.114) id (lookup nn tbl)

-- | d2(n=2) = 移動範囲 (n=2) の bias 補正係数。
d2_2 :: Double
d2_2 = 1.128

-- | (UCL-CL)/3 を 1σ とみなす。
sigmaFromLimits :: Double -> Double -> Double
sigmaFromLimits center ucl = (ucl - center) / 3

-- ===========================================================================
-- Chart constructors
-- ===========================================================================

-- | X̄-R 管理図。 部分群リストから (X̄ チャート, R チャート) を返す。
-- 部分群サイズは先頭群の長さを使う。
xbarRChart :: [[Double]] -> (ControlChart, ControlChart)
xbarRChart subs =
  let n      = case subs of { (g:_) -> length g; [] -> 2 }
      (a2, d3, d4) = xbarConstants n
      means  = map mean subs
      ranges = map rangeOf subs
      xbb    = mean means
      rbar   = mean ranges
      xUCL   = xbb + a2 * rbar
      xLCL   = xbb - a2 * rbar
      xbar   = ControlChart
                 { ccTitle = "X-bar chart", ccPoints = means
                 , ccCenter = xbb, ccUCL = xUCL, ccLCL = xLCL
                 , ccSigma = sigmaFromLimits xbb xUCL, ccViolations = [] }
      rUCL   = d4 * rbar
      rLCL   = d3 * rbar
      rchart = ControlChart
                 { ccTitle = "R chart", ccPoints = ranges
                 , ccCenter = rbar, ccUCL = rUCL, ccLCL = rLCL
                 , ccSigma = sigmaFromLimits rbar rUCL, ccViolations = [] }
  in (xbar, rchart)

-- | I-MR 管理図。 個別値から (I チャート, MR チャート) を返す。
imrChart :: [Double] -> (ControlChart, ControlChart)
imrChart xs =
  let mrs   = zipWith (\a b -> abs (b - a)) xs (drop 1 xs)
      mrbar = mean mrs
      xbar  = mean xs
      sigmaI = mrbar / d2_2
      iUCL  = xbar + 3 * sigmaI
      iLCL  = xbar - 3 * sigmaI
      ichart = ControlChart
                 { ccTitle = "Individuals chart", ccPoints = xs
                 , ccCenter = xbar, ccUCL = iUCL, ccLCL = iLCL
                 , ccSigma = sigmaI, ccViolations = [] }
      mrUCL = 3.267 * mrbar     -- D4 (n=2)
      mrchart = ControlChart
                 { ccTitle = "Moving range chart", ccPoints = mrs
                 , ccCenter = mrbar, ccUCL = mrUCL, ccLCL = 0
                 , ccSigma = sigmaFromLimits mrbar mrUCL, ccViolations = [] }
  in (ichart, mrchart)

-- | CUSUM パラメータ (K / H は σ 単位)。 既定 K=0.5σ / H=4σ。
data CusumParams = CusumParams
  { cpTarget :: !Double
  , cpSigma  :: !Double
  , cpK      :: !Double   -- ^ slack (σ 単位)
  , cpH      :: !Double   -- ^ decision interval (σ 単位)
  } deriving (Show, Eq)

defaultCusumParams :: Double -> Double -> CusumParams
defaultCusumParams target sigma = CusumParams target sigma 0.5 4

-- | 表形式 CUSUM。 (上側 C+, 下側 C-) を返す。 両者とも 0 から累積し、
-- 限界は H (= cpH·σ)。
cusumChart :: CusumParams -> [Double] -> (ControlChart, ControlChart)
cusumChart (CusumParams target sigma k h) xs =
  let kk = k * sigma
      hh = h * sigma
      stepUp prev x = max 0 (prev + (x - (target + kk)))
      stepDn prev x = max 0 (prev + ((target - kk) - x))
      cplus  = drop 1 (scanl stepUp 0 xs)
      cminus = drop 1 (scanl stepDn 0 xs)
      mk ttl pts = ControlChart
        { ccTitle = ttl, ccPoints = pts, ccCenter = 0
        , ccUCL = hh, ccLCL = 0, ccSigma = sigmaFromLimits 0 hh
        , ccViolations = [] }
  in (mk "CUSUM (upper C+)" cplus, mk "CUSUM (lower C-)" cminus)

-- | EWMA パラメータ。 既定 λ=0.2 / L=3。
data EwmaParams = EwmaParams
  { ewTarget :: !Double
  , ewSigma  :: !Double
  , ewLambda :: !Double
  , ewL      :: !Double
  } deriving (Show, Eq)

defaultEwmaParams :: Double -> Double -> EwmaParams
defaultEwmaParams target sigma = EwmaParams target sigma 0.2 3

-- | EWMA 管理図。 限界は定常状態 (asymptotic) の値を用いる:
-- @target ± L·σ·sqrt(λ/(2-λ))@。
ewmaChart :: EwmaParams -> [Double] -> ControlChart
ewmaChart (EwmaParams target sigma lam l) xs =
  let step prev x = lam * x + (1 - lam) * prev
      zs    = drop 1 (scanl step target xs)
      sEw   = sigma * sqrt (lam / (2 - lam))
      ucl   = target + l * sEw
      lcl   = target - l * sEw
  in ControlChart
       { ccTitle = "EWMA chart", ccPoints = zs, ccCenter = target
       , ccUCL = ucl, ccLCL = lcl, ccSigma = sigmaFromLimits target ucl
       , ccViolations = [] }

-- ===========================================================================
-- ルール検出
-- ===========================================================================

-- | サイズ m の終端 index 付き窓 (endIndex, window)。
windows :: Int -> [a] -> [(Int, [a])]
windows m xs
  | m <= 0    = []
  | otherwise = [ (i + m - 1, take m (drop i xs)) | i <- [0 .. length xs - m] ]

-- | (rule, 発火 index 群) のリストを (index, [rule]) に反転 (index 昇順、
-- rule もソート)。
collectRules :: [(Rule, [Int])] -> [(Int, [Rule])]
collectRules hits =
  let m = Map.fromListWith (++) [ (i, [r]) | (r, is) <- hits, i <- is ]
  in [ (i, sort rs) | (i, rs) <- Map.toAscList m ]

-- | 点 v の中心からの符号付きサイド (上=GT / 下=LT / 中心=EQ)。
side :: Double -> Double -> Ordering
side center v = compare v center

-- | 中心からの距離 (σ 単位)。
zoneSigma :: Double -> Double -> Double -> Double
zoneSigma center s v = if s <= 0 then 0 else abs (v - center) / s

-- | 「m 窓のうち k 点が同じ側で t·σ 超」 が成立する終端 index 群。
kOfMBeyond :: ControlChart -> Int -> Int -> Double -> [Int]
kOfMBeyond cc k m t =
  let c = ccCenter cc; s = ccSigma cc
      cntSide o w = length [ () | v <- w, side c v == o, zoneSigma c s v > t ]
  in [ ei | (ei, w) <- windows m (ccPoints cc)
          , cntSide GT w >= k || cntSide LT w >= k ]

-- | 「r 点連続で中心の同じ側」 の終端 index 群。
runSameSide :: ControlChart -> Int -> [Int]
runSameSide cc r =
  let c = ccCenter cc
  in [ ei | (ei, w) <- windows r (ccPoints cc), all (> c) w || all (< c) w ]

-- | 単調増加 / 減少 (狭義)。
monotonic :: [Double] -> Bool
monotonic w = and (zipWith (<) w (drop 1 w)) || and (zipWith (>) w (drop 1 w))

-- | 上下交互。
alternating :: [Double] -> Bool
alternating w =
  let ds = zipWith compare (drop 1 w) w
  in EQ `notElem` ds && and (zipWith (/=) ds (drop 1 ds))

-- | Western Electric ルール (1-4)。
westernElectric :: ControlChart -> [(Int, [Rule])]
westernElectric cc = collectRules
  [ (WE 1, [ ei | (ei, w) <- windows 1 (ccPoints cc)
                , let c = ccCenter cc; s = ccSigma cc
                , any (\v -> zoneSigma c s v > 3) w ])
  , (WE 2, kOfMBeyond cc 2 3 2)
  , (WE 3, kOfMBeyond cc 4 5 1)
  , (WE 4, runSameSide cc 8)
  ]

-- | Nelson ルール (1-8)。
nelson :: ControlChart -> [(Int, [Rule])]
nelson cc =
  let c = ccCenter cc; s = ccSigma cc
      within1 v  = zoneSigma c s v < 1
      beyond1 v  = zoneSigma c s v > 1
  in collectRules
    [ (Nelson 1, [ ei | (ei, w) <- windows 1 (ccPoints cc), any (\v -> zoneSigma c s v > 3) w ])
    , (Nelson 2, runSameSide cc 9)
    , (Nelson 3, [ ei | (ei, w) <- windows 6 (ccPoints cc), monotonic w ])
    , (Nelson 4, [ ei | (ei, w) <- windows 14 (ccPoints cc), alternating w ])
    , (Nelson 5, kOfMBeyond cc 2 3 2)
    , (Nelson 6, kOfMBeyond cc 4 5 1)
    , (Nelson 7, [ ei | (ei, w) <- windows 15 (ccPoints cc), all within1 w ])
    , (Nelson 8, [ ei | (ei, w) <- windows 8 (ccPoints cc), all beyond1 w ])
    ]

-- | ルール検出結果を chart に取り付ける。
attachViolations :: (ControlChart -> [(Int, [Rule])]) -> ControlChart -> ControlChart
attachViolations f cc = cc { ccViolations = f cc }

-- ===========================================================================
-- Render
-- ===========================================================================

marginL, marginR, marginT, marginB, plotH, dx :: Double
marginL = 54
marginR = 56
marginT = 30
marginB = 28
plotH   = 200
dx      = 30

-- | SVG / PNG 出力に渡す viewport 寸法。
controlChartViewport :: ControlChart -> (Int, Int)
controlChartViewport cc =
  let n = length (ccPoints cc)
      w = marginL + fromIntegral (max 1 (n - 1)) * dx + marginR
      h = marginT + plotH + marginB
  in (ceiling w, ceiling h)

-- | 管理図の backend 非依存 'Primitive' 列。
controlChartPrimitives :: ControlChart -> [Primitive]
controlChartPrimitives cc =
  let pts   = ccPoints cc
      n     = length pts
      lo0   = minimum (ccLCL cc : pts)
      hi0   = maximum (ccUCL cc : pts)
      pad   = let d = hi0 - lo0 in if d <= 0 then 1 else d * 0.08
      lo    = lo0 - pad
      hi    = hi0 + pad
      top   = marginT
      bot   = marginT + plotH
      xAt i = marginL + fromIntegral i * dx
      yAt v = bot - (v - lo) / (hi - lo) * (bot - top)
      x0    = marginL
      x1    = marginL + fromIntegral (max 1 (n - 1)) * dx
      vmap  = Map.fromList (ccViolations cc)

      -- 限界線 + ラベル
      limLine v st = PLine (Point x0 (yAt v)) (Point x1 (yAt v)) st
      dashed col = LineStyle col 1.0 [4, 3]
      solidL col = LineStyle col 1.0 []
      label v txt col =
        PText (Point (x1 + 6) (yAt v + 4)) txt
          (TextStyle col 11 "sans-serif" AnchorStart 0 "normal" False)
      limits =
        [ limLine (ccUCL cc)    (dashed "#dc2626")
        , limLine (ccCenter cc) (solidL "#16a34a")
        , limLine (ccLCL cc)    (dashed "#dc2626")
        , label (ccUCL cc)    (T.append "UCL " (fmt (ccUCL cc)))    "#dc2626"
        , label (ccCenter cc) (T.append "CL "  (fmt (ccCenter cc))) "#16a34a"
        , label (ccLCL cc)    (T.append "LCL " (fmt (ccLCL cc)))    "#dc2626"
        ]

      -- 系列を結ぶ線
      connectors =
        [ PLine (Point (xAt i) (yAt (pts !! i))) (Point (xAt (i + 1)) (yAt (pts !! (i + 1))))
                (solidL "#475569")
        | i <- [0 .. n - 2] ]

      -- 点 (違反は赤・大・hover title)
      dot i =
        let v     = pts !! i
            rules = Map.findWithDefault [] i vmap
            isV   = not (null rules)
            r     = if isV then 5 else 3
            fill  = if isV then "#dc2626" else "#2563eb"
            ttl   = T.concat [ "i=", T.pack (show i), " v=", fmt v
                             , if isV then T.append " " (rulesText rules) else "" ]
        in PCircle (Point (xAt i) (yAt v)) r (FillStyle fill 1.0)
                   (Just (StrokeStyle "#ffffff" 0.8)) (Just ttl)
      dots = [ dot i | i <- [0 .. n - 1] ]

      title = PText (Point marginL (marginT - 12)) (ccTitle cc)
                (TextStyle "#0f172a" 14 "sans-serif" AnchorStart 0 "bold" False)
  in concat [ [title], limits, connectors, dots ]

fmt :: Double -> Text
fmt x = T.pack (printf "%.3g" x)

rulesText :: [Rule] -> Text
rulesText rs = T.concat ["[", T.intercalate "," (map ruleAbbr rs), "]"]

ruleAbbr :: Rule -> Text
ruleAbbr (WE k)     = T.append "WE" (T.pack (show k))
ruleAbbr (Nelson k) = T.append "N"  (T.pack (show k))

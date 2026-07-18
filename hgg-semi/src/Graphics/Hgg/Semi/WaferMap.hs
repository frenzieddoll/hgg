-- |
-- Module      : Graphics.Hgg.Semi.WaferMap
-- Description : 半導体 wafer map (die grid + bin 色塗り + edge 除外 + reticle + notch + yield/zone)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- ウェハ上の die を 2D グリッドで可視化する。 1 die = 1 セルで、 bin
-- (良品 / 不良カテゴリ / 測定対象外) ごとに色を塗る。 ウェハ外周は円で表し、
-- エッジ除外幅の内側だけを「on-wafer」 として扱う。 reticle (露光ショット)
-- 境界を太線で、 notch / flat 方向をマーカーで示し、 yield と zone
-- (center / mid / edge) サマリを算出する。
--
-- backend には依存せず、 'waferMapPrimitives' が hgg-core の
-- backend 非依存 'Primitive' 列を返す。 出力は SVG / PDF / PNG / Canvas
-- backend がそのまま consume する。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Semi.WaferMap
  ( -- * Spec
    DieBin(..)
  , Die(..)
  , Notch(..)
  , WaferMapSpec(..)
  , defaultWaferMapSpec
    -- * Yield / zone サマリ
  , Zone(..)
  , YieldSummary(..)
  , onWafer
  , zoneOf
  , computeYield
    -- * Render
  , waferMapViewport
  , waferMapPrimitives
  ) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Printf         (printf)

import           Graphics.Hgg.Layout (Rect (..))
import           Graphics.Hgg.Render (FillStyle (..), LineStyle (..),
                                      PathSegment (..), Point (..),
                                      Primitive (..), StrokeStyle (..),
                                      TextAnchor (..), TextStyle (..))

-- ===========================================================================
-- Spec
-- ===========================================================================

-- | 1 つの die の bin (= 検査カテゴリ)。
--
--   * 'BinPass' = 良品
--   * 'BinFail' = 不良 (fail カテゴリ名を保持。 ビン別の色分けに使う)
--   * 'BinSkip' = placed だが測定対象外 (yield 分母に含めない)
data DieBin
  = BinPass
  | BinFail !Text
  | BinSkip
  deriving (Show, Eq, Ord)

-- | グリッド上の 1 die (0-based の列 / 行と bin)。
data Die = Die
  { dieCol :: !Int
  , dieRow :: !Int
  , dieBin :: !DieBin
  } deriving (Show, Eq)

-- | notch / flat の向き (ウェハ方位の基準点)。
data Notch = NotchN | NotchE | NotchS | NotchW
  deriving (Show, Eq)

-- | WaferMap の入力一式。 px 寸法はすべて 'wmCellSize' / 'wmMargin' から導く。
data WaferMapSpec = WaferMapSpec
  { wmCols          :: !Int               -- ^ グリッド列数
  , wmRows          :: !Int               -- ^ グリッド行数
  , wmDies          :: ![Die]             -- ^ placed die (位置 + bin)
  , wmEdgeExclusion :: !Double            -- ^ エッジ除外幅 (die 単位)。 半径から内側に控える量
  , wmReticleCols   :: !(Maybe Int)       -- ^ reticle 境界 (n 列ごとに太線)。 Nothing = 描かない
  , wmReticleRows   :: !(Maybe Int)       -- ^ reticle 境界 (n 行ごとに太線)
  , wmNotch         :: !Notch             -- ^ notch / flat 方向
  , wmBinColors     :: ![(DieBin, Text)]  -- ^ bin → 色の上書き (無ければ 'defaultBinColor')
  , wmCellSize      :: !Double            -- ^ die 1 個の px サイズ
  , wmMargin        :: !Double            -- ^ 余白 px
  } deriving (Show, Eq)

-- | 典型値で 'WaferMapSpec' を作る。 列数 / 行数 / die 列を渡すだけ。
defaultWaferMapSpec :: Int -> Int -> [Die] -> WaferMapSpec
defaultWaferMapSpec cols rows dies = WaferMapSpec
  { wmCols          = cols
  , wmRows          = rows
  , wmDies          = dies
  , wmEdgeExclusion = 1.0
  , wmReticleCols   = Nothing
  , wmReticleRows   = Nothing
  , wmNotch         = NotchS
  , wmBinColors     = []
  , wmCellSize      = 16
  , wmMargin        = 24
  }

-- ===========================================================================
-- 幾何 (内部)
-- ===========================================================================

-- | グリッド原点 (左上の die の左上角) の px 座標。
gridOrigin :: WaferMapSpec -> Point
gridOrigin s = Point (wmMargin s) (wmMargin s)

-- | グリッド全体の px 幅・高さ。
gridSize :: WaferMapSpec -> (Double, Double)
gridSize s =
  ( fromIntegral (wmCols s) * wmCellSize s
  , fromIntegral (wmRows s) * wmCellSize s )

-- | ウェハ円の中心と半径 (px)。 半径はグリッド短辺に内接。
waferGeometry :: WaferMapSpec -> (Point, Double)
waferGeometry s =
  let Point ox oy = gridOrigin s
      (gw, gh)    = gridSize s
      center      = Point (ox + gw / 2) (oy + gh / 2)
      radius      = min gw gh / 2
  in (center, radius)

-- | die セルの px 矩形。
dieRect :: WaferMapSpec -> Die -> Rect
dieRect s d =
  let Point ox oy = gridOrigin s
      cs          = wmCellSize s
  in Rect (ox + fromIntegral (dieCol d) * cs)
          (oy + fromIntegral (dieRow d) * cs)
          cs cs

-- | die セル中心の px 座標。
dieCenter :: WaferMapSpec -> Die -> Point
dieCenter s d =
  let Rect rx ry rw rh = dieRect s d
  in Point (rx + rw / 2) (ry + rh / 2)

-- | die 中心からウェハ中心までの距離 (px)。
distFromCenter :: WaferMapSpec -> Die -> Double
distFromCenter s d =
  let (Point cx cy, _) = waferGeometry s
      Point dx dy      = dieCenter s d
  in sqrt ((dx - cx) ** 2 + (dy - cy) ** 2)

-- | エッジ除外を効かせた有効半径 (px)。
effectiveRadius :: WaferMapSpec -> Double
effectiveRadius s =
  let (_, r) = waferGeometry s
  in r - wmEdgeExclusion s * wmCellSize s

-- | die がエッジ除外内 (= 測定対象) か。 中心が有効半径内なら on-wafer。
onWafer :: WaferMapSpec -> Die -> Bool
onWafer s d = distFromCenter s d <= effectiveRadius s

-- ===========================================================================
-- Yield / zone
-- ===========================================================================

-- | ウェハ径方向の領域区分 (中心 / 中間 / 外周)。 有効半径を 3 等分。
data Zone = ZoneCenter | ZoneMid | ZoneEdge
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | die が属する zone。 有効半径に対する正規化距離で 1/3 ・ 2/3 で区切る。
zoneOf :: WaferMapSpec -> Die -> Zone
zoneOf s d =
  let r = effectiveRadius s
      t = if r <= 0 then 1 else distFromCenter s d / r
  in if t < 1 / 3 then ZoneCenter
     else if t < 2 / 3 then ZoneMid
     else ZoneEdge

-- | yield と zone 別内訳。
data YieldSummary = YieldSummary
  { ysTotal  :: !Int                  -- ^ on-wafer かつ測定済 (Pass + Fail)
  , ysPass   :: !Int
  , ysFail   :: !Int
  , ysYield  :: !Double               -- ^ Pass / (Pass + Fail) [%]、 分母 0 なら 0
  , ysByZone :: ![(Zone, Int, Int)]   -- ^ (zone, pass, fail)
  } deriving (Show, Eq)

-- | bin が「測定済」 (Pass / Fail) かどうか。 'BinSkip' は分母に入れない。
isTested :: DieBin -> Bool
isTested BinPass     = True
isTested (BinFail _) = True
isTested BinSkip     = False

isPass :: DieBin -> Bool
isPass BinPass = True
isPass _       = False

-- | on-wafer die から yield と zone サマリを算出。
computeYield :: WaferMapSpec -> YieldSummary
computeYield s =
  let tested = [ d | d <- wmDies s, onWafer s d, isTested (dieBin d) ]
      nPass  = length (filter (isPass . dieBin) tested)
      nFail  = length tested - nPass
      yield  = if null tested then 0
               else fromIntegral nPass / fromIntegral (length tested) * 100
      zoneRow z =
        let zs = filter ((== z) . zoneOf s) tested
            p  = length (filter (isPass . dieBin) zs)
        in (z, p, length zs - p)
  in YieldSummary
       { ysTotal  = length tested
       , ysPass   = nPass
       , ysFail   = nFail
       , ysYield  = yield
       , ysByZone = map zoneRow [minBound .. maxBound]
       }

-- ===========================================================================
-- 色
-- ===========================================================================

-- | bin の既定色。 Pass = 緑、 Fail = 赤、 Skip = 薄灰。
defaultBinColor :: DieBin -> Text
defaultBinColor BinPass     = "#22c55e"
defaultBinColor (BinFail _) = "#ef4444"
defaultBinColor BinSkip     = "#e5e7eb"

-- | 上書きマップを優先した bin 色解決。
binColor :: WaferMapSpec -> DieBin -> Text
binColor s b = maybe (defaultBinColor b) id (lookup b (wmBinColors s))

-- ===========================================================================
-- Render
-- ===========================================================================

-- | サマリ表示の高さ (px、 2 行分)。
summaryHeight :: Double
summaryHeight = 44

-- | SVG / PNG 出力に渡す viewport 寸法 (幅, 高さ)。
waferMapViewport :: WaferMapSpec -> (Int, Int)
waferMapViewport s =
  let (gw, gh) = gridSize s
      w = gw + 2 * wmMargin s
      h = gh + 2 * wmMargin s + summaryHeight
  in (ceiling w, ceiling h)

-- | wafer map の backend 非依存 'Primitive' 列。
--
-- 描画順: ウェハ円 → on-wafer die セル → reticle 境界 → notch マーカー →
-- yield / zone サマリ text。
waferMapPrimitives :: WaferMapSpec -> [Primitive]
waferMapPrimitives s =
  concat
    [ [waferOutline s]
    , dieCells s
    , reticleLines s
    , [notchMarker s]
    , summaryText s
    ]

-- | ウェハ外周の円 (薄塗り + 細枠)。
waferOutline :: WaferMapSpec -> Primitive
waferOutline s =
  let (center, radius) = waferGeometry s
  in PCircle center radius
       (FillStyle "#f8fafc" 1.0)
       (Just (StrokeStyle "#94a3b8" 1.5))
       Nothing

-- | on-wafer die のセル矩形 (bin 色 + 白い細い区切り枠)。
dieCells :: WaferMapSpec -> [Primitive]
dieCells s =
  [ PRect (dieRect s d)
          (FillStyle (binColor s (dieBin d)) 1.0)
          (Just (StrokeStyle "#ffffff" 0.5))
  | d <- wmDies s, onWafer s d ]

-- | reticle (露光ショット) 境界線。 'wmReticleCols' / 'wmReticleRows' が
-- 指定されていれば、 その倍数の格子位置にグリッド全幅 / 全高の太線を引く。
reticleLines :: WaferMapSpec -> [Primitive]
reticleLines s =
  let Point ox oy = gridOrigin s
      (gw, gh)    = gridSize s
      cs          = wmCellSize s
      style       = LineStyle "#64748b" 1.2 []
      vline c = PLine (Point (ox + fromIntegral c * cs) oy)
                      (Point (ox + fromIntegral c * cs) (oy + gh)) style
      hline r = PLine (Point ox        (oy + fromIntegral r * cs))
                      (Point (ox + gw) (oy + fromIntegral r * cs)) style
      vs = case wmReticleCols s of
             Just n | n > 0 -> [ vline c | c <- [n, 2 * n .. wmCols s - 1] ]
             _              -> []
      hs = case wmReticleRows s of
             Just n | n > 0 -> [ hline r | r <- [n, 2 * n .. wmRows s - 1] ]
             _              -> []
  in vs ++ hs

-- | notch / flat マーカー。 ウェハ外周の該当方位に内向きの小三角形を置く。
notchMarker :: WaferMapSpec -> Primitive
notchMarker s =
  let (Point cx cy, r) = waferGeometry s
      sz               = wmCellSize s
      (p1, p2, tip)    = case wmNotch s of
        NotchS -> ( Point (cx - sz / 2) (cy + r), Point (cx + sz / 2) (cy + r), Point cx (cy + r - sz) )
        NotchN -> ( Point (cx - sz / 2) (cy - r), Point (cx + sz / 2) (cy - r), Point cx (cy - r + sz) )
        NotchE -> ( Point (cx + r) (cy - sz / 2), Point (cx + r) (cy + sz / 2), Point (cx + r - sz) cy )
        NotchW -> ( Point (cx - r) (cy - sz / 2), Point (cx - r) (cy + sz / 2), Point (cx - r + sz) cy )
  in PPath [MoveTo p1, LineTo p2, LineTo tip, ClosePath]
           (FillStyle "#1e293b" 1.0)
           Nothing

-- | yield / zone サマリの text 2 行 (グリッド下)。
summaryText :: WaferMapSpec -> [Primitive]
summaryText s =
  let ys          = computeYield s
      Point ox oy = gridOrigin s
      (_, gh)     = gridSize s
      baseY       = oy + gh + 18
      style w =
        TextStyle { tsColor = "#0f172a", tsSize = 13, tsFamily = "sans-serif"
                  , tsAnchor = AnchorStart, tsRotate = 0, tsWeight = w
                  , tsItalic = False }
      line1 = T.pack (printf "Yield: %.1f%%  (Pass %d / Fail %d, n=%d)"
                             (ysYield ys) (ysPass ys) (ysFail ys) (ysTotal ys))
      zoneLab (z, p, f) = T.concat [zoneName z, " ", T.pack (show p), "/", T.pack (show (p + f))]
      line2 = T.append "Zone  " (T.intercalate "   " (map zoneLab (ysByZone ys)))
  in [ PText (Point ox baseY)        line1 (style "bold")
     , PText (Point ox (baseY + 18)) line2 (style "normal") ]

zoneName :: Zone -> Text
zoneName ZoneCenter = "C:"
zoneName ZoneMid    = "M:"
zoneName ZoneEdge   = "E:"

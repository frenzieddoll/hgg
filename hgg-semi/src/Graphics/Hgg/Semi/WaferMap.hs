-- |
-- Module      : Graphics.Hgg.Semi.WaferMap
-- Description : 半導体 wafer map (die grid + bin 色塗り + edge 除外 + reticle + notch + yield/zone)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: ウェハ上の die を 2D グリッドで可視化する。 1 die = 1 セルで、 bin
-- (良品 / 不良カテゴリ / 測定対象外) ごとに色を塗る。 ウェハ外周は円で表し、
-- エッジ除外幅の内側だけを「on-wafer」 として扱う。 reticle (露光ショット)
-- 境界を太線で、 notch / flat 方向をマーカーで示し、 yield と zone
-- (center / mid / edge) サマリを算出する。
--
-- backend には依存せず、 'waferMapPrimitives' が hgg-core の
-- backend 非依存 'Primitive' 列を返す。 出力は SVG / PDF / PNG / Canvas
-- backend がそのまま consume する。
--
-- [English]: Visualizes the dies on a wafer as a 2D grid. Each die is one
-- cell, colored by its bin (pass / a fail category / untested). The wafer
-- outline is drawn as a circle, and only the area inside the edge-exclusion
-- width is treated as "on-wafer". Reticle (exposure-shot) boundaries are
-- drawn as thick lines, the notch / flat direction is shown with a marker,
-- and a yield and zone (center / mid / edge) summary is computed.
--
-- Independent of any backend; 'waferMapPrimitives' returns
-- hgg-core's backend-agnostic 'Primitive' list. The output is
-- consumed as-is by the SVG / PDF / PNG / Canvas backends.
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

-- | [日本語]: 1 つの die の bin (= 検査カテゴリ)。
--
--   * 'BinPass' = 良品
--   * 'BinFail' = 不良 (fail カテゴリ名を保持。 ビン別の色分けに使う)
--   * 'BinSkip' = placed だが測定対象外 (yield 分母に含めない)
--   [English]: The bin (inspection category) of a single die.
--
--   * 'BinPass' = pass
--   * 'BinFail' = fail (holds the fail category name, used for per-bin coloring)
--   * 'BinSkip' = placed but untested (excluded from the yield denominator)
data DieBin
  = BinPass
  | BinFail !Text
  | BinSkip
  deriving (Show, Eq, Ord)

-- | [日本語]: グリッド上の 1 die (0-based の列 / 行と bin)。
--   [English]: A single die on the grid (0-based column / row and its bin).
data Die = Die
  { dieCol :: !Int
  , dieRow :: !Int
  , dieBin :: !DieBin
  } deriving (Show, Eq)

-- | [日本語]: notch / flat の向き (ウェハ方位の基準点)。
--   [English]: The notch / flat direction (the wafer's orientation reference point).
data Notch = NotchN | NotchE | NotchS | NotchW
  deriving (Show, Eq)

-- | [日本語]: WaferMap の入力一式。 px 寸法はすべて 'wmCellSize' / 'wmMargin' から導く。
--   [English]: The full set of inputs to a WaferMap. All px dimensions are
--   derived from 'wmCellSize' / 'wmMargin'.
data WaferMapSpec = WaferMapSpec
  { wmCols          :: !Int               -- ^ [日本語]: グリッド列数。
                                           --   [English]: Number of grid columns.
  , wmRows          :: !Int               -- ^ [日本語]: グリッド行数。
                                           --   [English]: Number of grid rows.
  , wmDies          :: ![Die]             -- ^ [日本語]: placed die (位置 + bin)。
                                           --   [English]: The placed dies (position + bin).
  , wmEdgeExclusion :: !Double            -- ^ [日本語]: エッジ除外幅 (die 単位)。 半径から内側に控える量。
                                           --   [English]: The edge-exclusion width, in die units — how far to pull in from the radius.
  , wmReticleCols   :: !(Maybe Int)       -- ^ [日本語]: reticle 境界 (n 列ごとに太線)。 Nothing = 描かない。
                                           --   [English]: The reticle boundary (a thick line every n columns). @Nothing@ = not drawn.
  , wmReticleRows   :: !(Maybe Int)       -- ^ [日本語]: reticle 境界 (n 行ごとに太線)。
                                           --   [English]: The reticle boundary (a thick line every n rows).
  , wmNotch         :: !Notch             -- ^ [日本語]: notch / flat 方向。
                                           --   [English]: The notch / flat direction.
  , wmBinColors     :: ![(DieBin, Text)]  -- ^ [日本語]: bin → 色の上書き (無ければ 'defaultBinColor')。
                                           --   [English]: Bin to color overrides (falls back to 'defaultBinColor' when absent).
  , wmCellSize      :: !Double            -- ^ [日本語]: die 1 個の px サイズ。
                                           --   [English]: The px size of a single die.
  , wmMargin        :: !Double            -- ^ [日本語]: 余白 px。
                                           --   [English]: The margin, in px.
  } deriving (Show, Eq)

-- | [日本語]: 典型値で 'WaferMapSpec' を作る。 列数 / 行数 / die 列を渡すだけ。
--   [English]: Builds a 'WaferMapSpec' with typical values. Just pass the
--   column count / row count / die list.
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

-- | [日本語]: グリッド原点 (左上の die の左上角) の px 座標。
--   [English]: The grid origin (the top-left corner of the top-left die), in px.
gridOrigin :: WaferMapSpec -> Point
gridOrigin s = Point (wmMargin s) (wmMargin s)

-- | [日本語]: グリッド全体の px 幅・高さ。
--   [English]: The overall grid width / height, in px.
gridSize :: WaferMapSpec -> (Double, Double)
gridSize s =
  ( fromIntegral (wmCols s) * wmCellSize s
  , fromIntegral (wmRows s) * wmCellSize s )

-- | [日本語]: ウェハ円の中心と半径 (px)。 半径はグリッド短辺に内接。
--   [English]: The wafer circle's center and radius, in px. The radius is
--   inscribed within the grid's shorter side.
waferGeometry :: WaferMapSpec -> (Point, Double)
waferGeometry s =
  let Point ox oy = gridOrigin s
      (gw, gh)    = gridSize s
      center      = Point (ox + gw / 2) (oy + gh / 2)
      radius      = min gw gh / 2
  in (center, radius)

-- | [日本語]: die セルの px 矩形。
--   [English]: The die cell's px rectangle.
dieRect :: WaferMapSpec -> Die -> Rect
dieRect s d =
  let Point ox oy = gridOrigin s
      cs          = wmCellSize s
  in Rect (ox + fromIntegral (dieCol d) * cs)
          (oy + fromIntegral (dieRow d) * cs)
          cs cs

-- | [日本語]: die セル中心の px 座標。
--   [English]: The die cell's center, in px.
dieCenter :: WaferMapSpec -> Die -> Point
dieCenter s d =
  let Rect rx ry rw rh = dieRect s d
  in Point (rx + rw / 2) (ry + rh / 2)

-- | [日本語]: die 中心からウェハ中心までの距離 (px)。
--   [English]: The distance from the die's center to the wafer's center, in px.
distFromCenter :: WaferMapSpec -> Die -> Double
distFromCenter s d =
  let (Point cx cy, _) = waferGeometry s
      Point dx dy      = dieCenter s d
  in sqrt ((dx - cx) ** 2 + (dy - cy) ** 2)

-- | [日本語]: エッジ除外を効かせた有効半径 (px)。
--   [English]: The effective radius after applying edge exclusion, in px.
effectiveRadius :: WaferMapSpec -> Double
effectiveRadius s =
  let (_, r) = waferGeometry s
  in r - wmEdgeExclusion s * wmCellSize s

-- | [日本語]: die がエッジ除外内 (= 測定対象) か。 中心が有効半径内なら on-wafer。
--   [English]: Whether the die is inside the edge exclusion (= tested). A
--   die is on-wafer when its center is within the effective radius.
onWafer :: WaferMapSpec -> Die -> Bool
onWafer s d = distFromCenter s d <= effectiveRadius s

-- ===========================================================================
-- Yield / zone
-- ===========================================================================

-- | [日本語]: ウェハ径方向の領域区分 (中心 / 中間 / 外周)。 有効半径を 3 等分。
--   [English]: The wafer's radial zone classification (center / mid /
--   edge). Divides the effective radius into three equal parts.
data Zone = ZoneCenter | ZoneMid | ZoneEdge
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | [日本語]: die が属する zone。 有効半径に対する正規化距離で 1/3 ・ 2/3 で区切る。
--   [English]: The zone a die belongs to, split at 1/3 and 2/3 of the
--   distance normalized to the effective radius.
zoneOf :: WaferMapSpec -> Die -> Zone
zoneOf s d =
  let r = effectiveRadius s
      t = if r <= 0 then 1 else distFromCenter s d / r
  in if t < 1 / 3 then ZoneCenter
     else if t < 2 / 3 then ZoneMid
     else ZoneEdge

-- | [日本語]: yield と zone 別内訳。
--   [English]: The yield and its per-zone breakdown.
data YieldSummary = YieldSummary
  { ysTotal  :: !Int                  -- ^ [日本語]: on-wafer かつ測定済 (Pass + Fail)。
                                       --   [English]: On-wafer and tested (Pass + Fail).
  , ysPass   :: !Int
  , ysFail   :: !Int
  , ysYield  :: !Double               -- ^ [日本語]: Pass / (Pass + Fail) [%]、 分母 0 なら 0。
                                       --   [English]: Pass / (Pass + Fail) [%]; 0 when the denominator is 0.
  , ysByZone :: ![(Zone, Int, Int)]   -- ^ (zone, pass, fail)
  } deriving (Show, Eq)

-- | [日本語]: bin が「測定済」 (Pass / Fail) かどうか。 'BinSkip' は分母に入れない。
--   [English]: Whether the bin is "tested" (Pass / Fail). 'BinSkip' is
--   excluded from the denominator.
isTested :: DieBin -> Bool
isTested BinPass     = True
isTested (BinFail _) = True
isTested BinSkip     = False

isPass :: DieBin -> Bool
isPass BinPass = True
isPass _       = False

-- | [日本語]: on-wafer die から yield と zone サマリを算出。
--   [English]: Computes the yield and zone summary from the on-wafer dies.
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

-- | [日本語]: bin の既定色。 Pass = 緑、 Fail = 赤、 Skip = 薄灰。
--   [English]: The bin's default color. Pass = green, Fail = red, Skip = light gray.
defaultBinColor :: DieBin -> Text
defaultBinColor BinPass     = "#22c55e"
defaultBinColor (BinFail _) = "#ef4444"
defaultBinColor BinSkip     = "#e5e7eb"

-- | [日本語]: 上書きマップを優先した bin 色解決。
--   [English]: Resolves the bin color, preferring the override map.
binColor :: WaferMapSpec -> DieBin -> Text
binColor s b = maybe (defaultBinColor b) id (lookup b (wmBinColors s))

-- ===========================================================================
-- Render
-- ===========================================================================

-- | [日本語]: サマリ表示の高さ (px、 2 行分)。
--   [English]: The summary display's height, in px (for 2 lines).
summaryHeight :: Double
summaryHeight = 44

-- | [日本語]: SVG / PNG 出力に渡す viewport 寸法 (幅, 高さ)。
--   [English]: The viewport dimensions (width, height) passed to SVG / PNG output.
waferMapViewport :: WaferMapSpec -> (Int, Int)
waferMapViewport s =
  let (gw, gh) = gridSize s
      w = gw + 2 * wmMargin s
      h = gh + 2 * wmMargin s + summaryHeight
  in (ceiling w, ceiling h)

-- | [日本語]: wafer map の backend 非依存 'Primitive' 列。
--
-- 描画順: ウェハ円 → on-wafer die セル → reticle 境界 → notch マーカー →
-- yield / zone サマリ text。
--   [English]: The backend-agnostic 'Primitive' list for the wafer map.
--
-- Draw order: wafer circle to on-wafer die cells to reticle boundary to
-- notch marker to yield / zone summary text.
waferMapPrimitives :: WaferMapSpec -> [Primitive]
waferMapPrimitives s =
  concat
    [ [waferOutline s]
    , dieCells s
    , reticleLines s
    , [notchMarker s]
    , summaryText s
    ]

-- | [日本語]: ウェハ外周の円 (薄塗り + 細枠)。
--   [English]: The wafer's outer circle (light fill + thin stroke).
waferOutline :: WaferMapSpec -> Primitive
waferOutline s =
  let (center, radius) = waferGeometry s
  in PCircle center radius
       (FillStyle "#f8fafc" 1.0)
       (Just (StrokeStyle "#94a3b8" 1.5))
       Nothing

-- | [日本語]: on-wafer die のセル矩形 (bin 色 + 白い細い区切り枠)。
--   [English]: The cell rectangles of the on-wafer dies (bin color + thin
--   white divider stroke).
dieCells :: WaferMapSpec -> [Primitive]
dieCells s =
  [ PRect (dieRect s d)
          (FillStyle (binColor s (dieBin d)) 1.0)
          (Just (StrokeStyle "#ffffff" 0.5))
  | d <- wmDies s, onWafer s d ]

-- | [日本語]: reticle (露光ショット) 境界線。 'wmReticleCols' / 'wmReticleRows' が
-- 指定されていれば、 その倍数の格子位置にグリッド全幅 / 全高の太線を引く。
--   [English]: The reticle (exposure-shot) boundary lines. When
--   'wmReticleCols' / 'wmReticleRows' are specified, draws a thick line
--   spanning the grid's full width / height at each multiple of the grid position.
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

-- | [日本語]: notch / flat マーカー。 ウェハ外周の該当方位に内向きの小三角形を置く。
--   [English]: The notch / flat marker. Places a small inward-pointing
--   triangle at the corresponding direction on the wafer's outer edge.
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

-- | [日本語]: yield / zone サマリの text 2 行 (グリッド下)。
--   [English]: The yield / zone summary's two text lines (below the grid).
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

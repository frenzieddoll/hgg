-- |
-- Module      : Graphics.Hgg.Primitive
-- Description : Backend-agnostic drawing primitives, geometry, and style leaf types
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 描画 primitive (Point/Rect/style/PathSegment/Transform/Primitive) を
--   Spec/Layout/Render に依存しない __leaf module__ へ集約。 これらは元々
--   'Graphics.Hgg.Render.Common' (Spec/Layout を import する上位) に置かれていたため、
--   「'Spec.Layer' が draw closure (@RenderCtx -> [Primitive]@) を保持する」 拡張 (custom
--   mark) が __module 循環__で不能だった。 primitive は概念的に幾何 + Text のみに依存する
--   基盤型ゆえ、 正しい層 (= 最下層 leaf) へ戻す。 挙動・出力は完全に不変 (純粋な型移動)。
--   'Graphics.Hgg.Render.Common' / 'Graphics.Hgg.Render' が本 module を re-export するので
--   既存の import 経路は不変。
--   [English]: Consolidates the drawing primitives (Point/Rect/style/PathSegment/
--   Transform/Primitive) into a __leaf module__ with no dependency on Spec/Layout/
--   Render. These originally lived in 'Graphics.Hgg.Render.Common' (an upper layer
--   that imports Spec/Layout), which made it impossible to extend 'Spec.Layer' to
--   hold a draw closure (@RenderCtx -> [Primitive]@) for custom marks, due to a
--   __module cycle__. Since primitives conceptually depend only on geometry and
--   Text, they belong in the correct (lowest, leaf) layer. Behaviour and output
--   are completely unchanged (a pure type relocation). 'Graphics.Hgg.Render.Common'
--   and 'Graphics.Hgg.Render' re-export this module, so existing import paths are
--   unaffected.
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Primitive
  ( -- * 幾何
    Point(..)
  , Rect(..)
    -- * スタイル
  , LineStyle(..)
  , solid
  , FillStyle(..)
  , StrokeStyle(..)
  , TextStyle(..)
  , TextAnchor(..)
  , Transform(..)
  , PathSegment(..)
    -- * Primitive
  , Primitive(..)
    -- * pt→device scale (backend の唯一の dpi 適用点)
  , scalePrimitives
  ) where

import           Data.Aeson  (FromJSON, ToJSON)
import           Data.Text   (Text)
import           GHC.Generics (Generic)

-- ===========================================================================
-- 幾何
-- ===========================================================================

data Point = Point !Double !Double deriving (Show, Eq)

-- | [日本語]: plot 領域や clip 矩形。 (x,y) 左上 + 幅高 (pt 空間)。
--   [English]: A plot area or clip rectangle: (x,y) top-left plus width and
--   height, in pt space.
data Rect = Rect { rX :: !Double, rY :: !Double, rW :: !Double, rH :: !Double }
  deriving (Show, Eq, Generic)

instance ToJSON   Rect
instance FromJSON Rect

-- ===========================================================================
-- スタイル
-- ===========================================================================

-- | [日本語]: 線スタイル。 'lsDash' = SVG stroke-dasharray / Canvas setLineDash 用 px 配列。
--   既定 (= 実線) は空配列 []。 'solid' ヘルパで作ると常に実線。
--   [English]: A line style. 'lsDash' is the px array used by SVG
--   stroke-dasharray / Canvas setLineDash. The default (solid) is the empty
--   array []. Building it via the 'solid' helper always yields a solid line.
data LineStyle   = LineStyle   { lsColor :: !Text, lsWidth :: !Double, lsDash :: ![Double] } deriving (Show, Eq)

-- | [日本語]: 実線 'LineStyle' の簡易構築 (= 旧 2 引数 LineStyle と同一)。
--   dash を持たない既存呼出は全てこれに置換 (出力完全不変)。
--   [English]: A convenience constructor for a solid 'LineStyle' (equivalent
--   to the old 2-argument LineStyle). All existing call sites without a dash
--   are replaced by this (output is completely unchanged).
solid :: Text -> Double -> LineStyle
solid c w = LineStyle c w []

data FillStyle   = FillStyle   { fsColor :: !Text, fsOpacity :: !Double } deriving (Show, Eq)
data StrokeStyle = StrokeStyle { ssColor :: !Text, ssWidth :: !Double } deriving (Show, Eq)
data TextStyle = TextStyle
  { tsColor  :: !Text
  , tsSize   :: !Double
  , tsFamily :: !Text
  , tsAnchor :: !TextAnchor
  , tsRotate :: !Double         -- degrees **CCW** (canonical・R/ggplot 準拠)、 0 = 水平。
                                --   device (SVG/canvas/rasterific=CW) への符号変換は各 backend emit で 1 回 (PDF=y-up ゆえ恒等)。
  , tsWeight :: !Text           -- ★ TODO-10 (2026-05-29): "normal" / "bold" 等
  , tsItalic :: !Bool           -- ★ TODO-10: italic on/off
  } deriving (Show, Eq)

data TextAnchor = AnchorStart | AnchorMiddle | AnchorEnd
  deriving (Show, Eq)

data Transform = TranslateT !Double !Double | ScaleT !Double !Double
  deriving (Show, Eq)

data PathSegment
  = MoveTo  !Point
  | LineTo  !Point
  | CurveTo !Point !Point !Point
  | ClosePath
  deriving (Show, Eq)

-- ===========================================================================
-- Primitive
-- ===========================================================================

-- | [日本語]: backend 非依存の描画 primitive。 各 backend は drawPrimitives で
--   これを順に解釈するだけ。
--   [English]: A backend-agnostic drawing primitive. Each backend simply
--   interprets these in sequence via drawPrimitives.
data Primitive
  = PLine          !Point !Point !LineStyle
  | PRect          !Rect !FillStyle (Maybe StrokeStyle)
  -- | [日本語]: 'PCircle' は最終フィールドに optional hover label。 SVG backend は
  --   <title> 要素として埋め込み、 ブラウザ native の hover tooltip に。
  --   JS 不要。
  --   [English]: 'PCircle' carries an optional hover label as its final
  --   field. The SVG backend embeds it as a @\<title\>@ element, giving a
  --   browser-native hover tooltip with no JS required.
  | PCircle        !Point !Double !FillStyle (Maybe StrokeStyle) (Maybe Text)
  | PPath          ![PathSegment] !FillStyle (Maybe StrokeStyle)
  | PText          !Point !Text !TextStyle
  | PClipPush      !Rect
  -- | [日本語]: 多角形 clip (Phase 64 §2)。 頂点列は 'PRect' と同じ左上原点 y-down
  --   空間で、 **最後の頂点から最初の頂点へ暗黙に閉じる** (明示 close 不要)。
  --   矩形 clip は高速経路として 'PClipPush' を使い続ける (本 primitive は
  --   polar の外周・ternary の三角形など矩形で表せない panel 用)。
  --   ★ 頂点が 3 点未満の退化列は **clip 無し (素通し)** として扱う — 全 backend で
  --   統一。 「全消し」 にすると図が黙って白紙になるので fail-open を採る。
  --   [English]: Polygon clip (Phase 64 §2). The vertex list lives in the
  --   same top-left-origin, y-down space as 'PRect', and is **implicitly
  --   closed** from the last vertex back to the first. Rectangular clips
  --   keep using 'PClipPush' as the fast path; this primitive is for panels
  --   that a rectangle cannot express (a polar boundary, a ternary
  --   triangle, ...). A degenerate list of fewer than 3 vertices is treated
  --   as __no clip at all__ (pass-through) in every backend: failing open
  --   avoids silently blanking a figure.
  | PClipPath      ![Point]
  | PClipPop
  | PTransformPush !Transform
  | PTransformPop
  deriving (Show, Eq)

-- | [日本語]: pt 空間の primitive を device 単位へ一括 scale (k = dpi/72)。
--   ★ raster/vector backend で __唯一の dpi 適用点__。Layout/Render は
--   純 pt を出力し、ここで一度だけ k を掛ける。PDF は k=1 (pt 直結・恒等) を渡す。
--   座標・サイズ・線幅・font size・dash 配列を全て k 倍する。'ScaleT' は比率ゆえ不変。
--   [English]: Bulk-scales primitives from pt space to device units
--   (k = dpi/72). This is the __sole point where dpi is applied__ across
--   the raster/vector backends: Layout/Render emit pure pt values, and k is
--   applied exactly once here. PDF passes k=1 (a pt-direct identity).
--   Coordinates, sizes, line widths, font sizes, and dash arrays are all
--   scaled by k; 'ScaleT' is unaffected since it is a ratio.
scalePrimitives :: Double -> [Primitive] -> [Primitive]
scalePrimitives k
  | k == 1    = id
  | otherwise = map go
  where
    sp (Point x y)        = Point (x * k) (y * k)
    sr (Rect x y w h)     = Rect (x * k) (y * k) (w * k) (h * k)
    sl (LineStyle c w d)  = LineStyle c (w * k) (map (* k) d)
    sst (StrokeStyle c w) = StrokeStyle c (w * k)
    sts ts                = ts { tsSize = tsSize ts * k }
    sseg seg = case seg of
      MoveTo p        -> MoveTo (sp p)
      LineTo p        -> LineTo (sp p)
      CurveTo a b c   -> CurveTo (sp a) (sp b) (sp c)
      ClosePath       -> ClosePath
    str (TranslateT dx dy) = TranslateT (dx * k) (dy * k)
    str t@(ScaleT _ _)     = t
    go p = case p of
      PLine a b ls           -> PLine (sp a) (sp b) (sl ls)
      PRect r fs mss         -> PRect (sr r) fs (fmap sst mss)
      PCircle c rad fs mss t -> PCircle (sp c) (rad * k) fs (fmap sst mss) t
      PPath segs fs mss      -> PPath (map sseg segs) fs (fmap sst mss)
      PText pt txt ts        -> PText (sp pt) txt (sts ts)
      PClipPush r            -> PClipPush (sr r)
      PClipPath ps           -> PClipPath (map sp ps)
      PClipPop               -> PClipPop
      PTransformPush tr      -> PTransformPush (str tr)
      PTransformPop          -> PTransformPop

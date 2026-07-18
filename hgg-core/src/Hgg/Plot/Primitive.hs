-- |
-- Module      : Hgg.Plot.Primitive
-- Description : backend 非依存の描画 primitive・幾何・スタイルの基盤型 (leaf)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 51: 描画 primitive (Point/Rect/style/PathSegment/Transform/Primitive) を
-- Spec/Layout/Render に依存しない **leaf module** へ集約。 これらは元々
-- 'Hgg.Plot.Render.Common' (Spec/Layout を import する上位) に置かれていたため、
-- 「'Spec.Layer' が draw closure (@RenderCtx -> [Primitive]@) を保持する」 拡張 (custom
-- mark) が **module 循環**で不能だった。 primitive は概念的に幾何 + Text のみに依存する
-- 基盤型ゆえ、 正しい層 (= 最下層 leaf) へ戻す。 挙動・出力は完全に不変 (純粋な型移動)。
-- 'Hgg.Plot.Render.Common' / 'Hgg.Plot.Render' が本 module を re-export するので
-- 既存の import 経路は不変。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Primitive
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

-- | plot 領域や clip 矩形。 (x,y) 左上 + 幅高 (pt 空間)。
data Rect = Rect { rX :: !Double, rY :: !Double, rW :: !Double, rH :: !Double }
  deriving (Show, Eq, Generic)

instance ToJSON   Rect
instance FromJSON Rect

-- ===========================================================================
-- スタイル
-- ===========================================================================

-- | 線スタイル。 'lsDash' = SVG stroke-dasharray / Canvas setLineDash 用 px 配列。
-- 既定 (= 実線) は空配列 []。 'solid' ヘルパで作ると常に実線 (Phase 11 A4-b 以前と同一)。
data LineStyle   = LineStyle   { lsColor :: !Text, lsWidth :: !Double, lsDash :: ![Double] } deriving (Show, Eq)

-- | Phase 11 A4-b: 実線 'LineStyle' の簡易構築 (= 旧 2 引数 LineStyle と同一)。
-- dash を持たない既存呼出は全てこれに置換 (出力完全不変)。
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

-- | backend 非依存の描画 primitive。 各 backend は drawPrimitives で
-- これを順に解釈するだけ。
data Primitive
  = PLine          !Point !Point !LineStyle
  | PRect          !Rect !FillStyle (Maybe StrokeStyle)
  -- | 'PCircle' は最終フィールドに optional hover label。 SVG backend は
  -- <title> 要素として埋め込み、 ブラウザ native の hover tooltip に。
  -- JS 不要。
  | PCircle        !Point !Double !FillStyle (Maybe StrokeStyle) (Maybe Text)
  | PPath          ![PathSegment] !FillStyle (Maybe StrokeStyle)
  | PText          !Point !Text !TextStyle
  | PClipPush      !Rect
  | PClipPop
  | PTransformPush !Transform
  | PTransformPop
  deriving (Show, Eq)

-- | Phase 33 B5: pt 空間の primitive を device 単位へ一括 scale (k = dpi/72)。
-- ★ raster/vector backend で **唯一の dpi 適用点**。Layout/Render は
-- 純 pt を出力し、ここで一度だけ k を掛ける。PDF は k=1 (pt 直結・恒等) を渡す。
-- 座標・サイズ・線幅・font size・dash 配列を全て k 倍する。'ScaleT' は比率ゆえ不変。
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
      PClipPop               -> PClipPop
      PTransformPush tr      -> PTransformPush (str tr)
      PTransformPop          -> PTransformPop

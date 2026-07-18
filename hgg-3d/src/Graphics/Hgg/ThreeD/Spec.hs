-- |
-- Module      : Graphics.Hgg.ThreeD.Spec
-- Description : 3D VisualSpec / Layer + Monoid (Phase 5 A3)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 2D 同型 (= Graphics.Hgg.Spec の 'VisualSpec' / 'Layer' / Monoid 構成) を踏襲した
-- 3D 用 spec API。 ユーザ視点で 2D と同じ構文で組める:
--
-- @
-- -- 2D
-- purePlot <> layer (scatter x y <> color (fromHex "#ff0000") <> alpha 0.7) <> title "..."
--
-- -- 3D (= 構文 100% 同型)
-- purePlot3D <> layer3D (scatter3D pts <> color3D (fromHex "#ff0000") <> alpha3D 0.7)
--            <> camera (defaultCameraZUp 3) <> axes3D defaultAxes3D <> title3D "..."
-- @
--
-- 値型は分離 (= 'Layer' と 'Layer3D' は別)。 理由は phase-5 計画 md §2.2。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.ThreeD.Spec
  ( -- * MarkKind3D
    Mark3DKind (..)
    -- * Bar スタイル (Phase 25 A5・Bar module 再 export)
  , BarStyle3D (..)
    -- * Layer3D (= 1 layer の per-field Monoid)
  , Layer3D (..)
    -- * VisualSpec3D
  , VisualSpec3D (..)
    -- * 純粋起点
  , purePlot3D
    -- * Layer 起点 (= 2D 'layer' 同型)
  , layer3D
    -- * Mark コンストラクタ (= Layer3D 返却)
  , scatter3D
  , scatter3DPoints
  , line3D
  , line3DPoints
  , wireframe3D
  , surface3D
  , surface3DGrid
  , bar3D
  , bar3DPoints
  , stem3D
  , stem3DPoints
  , stemBaseZ
  , quiver3D
  , vecScale3D
  , trisurf
  , text3D
  , text3DPoints
  , annotate3D
    -- * per-layer 属性 (= Layer3D 返却)
  , color3D
  , colorRGBA3D
  , colorRGBA3DMaybe
  , colorBy3D
  , colorContinuousBy3D
  , sizeBy3D
  , sizeRange3D
  , barStyle3D
  , barWidth3D
  , errorBar3D
  , edgeColor3D
  , size3D
  , alpha3D
  , width3D
  , shaded3D
  , colormap3D
  , colormapWith3D
  , contourX
  , contourY
  , contourZ
  , ContourDir (..)
  , surfaceWire
  , viridisStops3D
  , xRange3D
  , yRange3D
    -- * VisualSpec3D 属性
  , camera
  , projection
  , axes3D
  , title3D
  , axisTitles3D
  , zAspect3D
  , pane3D
  , xAspect3D
  , yAspect3D
  , logScale3D
  , width3DV
  , height3DV
    -- * 変換 (= Layer3D を既存 Scatter3D/... に展開、 render 経路で使う)
  , layerToScatter
  , layerToLine
  , layerToWireframe
  , layerToSurface
  , layerToBar
  , layerToQuiver
    -- * 列参照の解決 (Phase 24 A6)
  , resolveLayer3D
  , resolveSpec3D
    -- * Helper: layers から bounding box 自動算出 (= Axes 未指定時の default)
  , autoAxes3D
  , layerPoints
  ) where

import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    (First (..), Last (..))
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

import           Graphics.Hgg.ThreeD.Axes       (Axes3D (..), defaultAxes3D)
import           Graphics.Hgg.ThreeD.Line       (Line3D (..), Wireframe3D (..),
                                                 defaultLine3D, defaultWireframe3D,
                                                 Quiver3D (..), defaultQuiver3D)
import           Graphics.Hgg.ThreeD.Scatter    (Scatter3D (..), defaultScatter3D)
import           Graphics.Hgg.ThreeD.Surface    (Surface3D (..), defaultSurface3D)
import           Graphics.Hgg.ThreeD.Bar        (Bar3D (..), BarStyle3D (..),
                                                 defaultBar3D)
import           Graphics.Hgg.ThreeD.Delaunay   (delaunay2D)
import           Graphics.Hgg.Spec              (ColRef, Resolver, resolveNum, resolveCol,
                                                 ColData (..))
import           Graphics.Hgg.Color             (Color, toCss, fromHexA, fromHexAMaybe)
import           Graphics.Hgg.Palette           (ggplotHue)
import           Graphics.Hgg.Render.Common     (continuousColor)
import           Data.List                      (nub)
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Graphics.Hgg.ThreeD.Types

-- ===========================================================================
-- MarkKind3D
-- ===========================================================================

-- | 3D layer の幾何種別 (= 2D 'MarkKind' の 3D 版)。
data Mark3DKind
  = M3Scatter
  | M3Line
  | M3Wireframe
  | M3Surface
  | M3Bar          -- Phase 25 A5: 3D 棒 (直方体 / stick)
  | M3Text         -- Phase 25 A7: 3D テキスト注釈 (任意点にラベル)
  | M3Stem         -- Phase 26 A4: 3D stem (底面 z0 への垂線 + 先端マーカー・3D lollipop)
  | M3Quiver       -- Phase 26 A3: 3D vector field (各点に成分ベクトルの矢印)
  | M3Trisurf      -- Phase 26 A5: 不規則点群を Delaunay 三角分割して曲面化
  deriving (Show, Eq, Generic)
instance ToJSON   Mark3DKind
instance FromJSON Mark3DKind

-- ===========================================================================
-- Layer3D (= 2D 'Layer' 同型、 per-field First/Last Monoid)
-- ===========================================================================

-- | 3D 1 layer の rolled-up 表現。 全 field が 'First' or 'Last' なので per-field
--   '(<>)' で自然に合成可能 (= 2D 'Layer' と同じ規律)。
--
--   * 'lyr3Kind' は 'First' (= 最初に設定した kind が勝つ、 2D 'lyKind' と同じ)
--   * その他属性は 'Last' (= 後勝ち、 2D 'lyColor' 等と同じ)
data Layer3D = Layer3D
  { lyr3Kind      :: !(First Mark3DKind)
  , lyr3Points    :: !(Last [Point3])
  , lyr3Color     :: !(Last Text)
  , lyr3Size      :: !(Last Double)
  , lyr3Alpha     :: !(Last Double)
  , lyr3Width     :: !(Last Double)
  , lyr3Edges     :: !(Last [(Int, Int)])    -- Wireframe 用
  , lyr3Grid      :: !(Last [[Double]])      -- Surface 用
  , lyr3XRange    :: !(Last (Double, Double))
  , lyr3YRange    :: !(Last (Double, Double))
  , lyr3Shaded    :: !(Last Bool)            -- Surface 用
  , lyr3EdgeColor :: !(Last Text)            -- Surface 用
  , lyr3Colormap  :: !(Last [Text])          -- Surface 用 (Phase 24 A2: z 連続色 stops)
  , lyr3Contours  :: ![(ContourDir, Int)]    -- Surface 用: 投影 contour 群 (各 (軸, 本数))。 X/Y 壁=断面・
                                             --   Z 床=等値面。 同一 surface に複数合成可・投影壁はカメラから遠い面に自動固定
  , lyr3SurfaceWire :: !(Last Bool)          -- Surface 用: 面を塗らず格子線メッシュで描く (matplotlib plot_wireframe 相当)
  , lyr3EncX      :: !(Last ColRef)          -- Phase 24 A6: 列参照 (x)
  , lyr3EncY      :: !(Last ColRef)          -- Phase 24 A6: 列参照 (y)
  , lyr3EncZ      :: !(Last ColRef)          -- Phase 24 A6: 列参照 (z)
  , lyr3ColorBy   :: !(Last ColRef)          -- Phase 25 A2: 群色分けのカテゴリ列
  , lyr3PtColors  :: !(Last [Text])          -- Phase 25 A2/A3: 解決後の点ごと色 (resolve 産物)
  , lyr3Legend    :: !(Last [(Text, Text)])  -- Phase 25 A2: 離散凡例 (カテゴリ, 色)
  , lyr3ColorByV  :: !(Last ColRef)          -- Phase 25 A3: 連続色マップの数値列
  , lyr3SizeBy    :: !(Last ColRef)          -- Phase 25 A3: size マップ (bubble) の数値列
  , lyr3SizeRange :: !(Last (Double, Double))-- Phase 25 A3: size マップの px 範囲 (既定 (4,18))
  , lyr3PtSizes   :: !(Last [Double])        -- Phase 25 A3: 解決後の点ごと基本 size (resolve 産物)
  , lyr3Colorbar  :: !(Last ([Text], Double, Double))
                                             -- Phase 25 A3: 連続色 colorbar (stops, min, max・resolve 産物)
  , lyr3BarStyle  :: !(Last BarStyle3D)      -- Phase 25 A5: bar スタイル (既定 BarCuboid)
  , lyr3BarWidth  :: !(Last Double)          -- Phase 25 A5: bar footprint 半幅 (軸 span 比・既定 0.04)
  , lyr3BarBaseZ  :: !(Last Double)          -- Phase 25 A5: bar 底面 z (正規化値・normLayer3D 産物)
  , lyr3ErrBy     :: !(Last ColRef)          -- Phase 25 A5: 誤差棒の err 数値列
  , lyr3PtErrs    :: !(Last [Double])        -- Phase 25 A5: 解決後の点ごと err (data→正規化は normLayer3D)
  , lyr3Labels    :: !(Last [Text])          -- Phase 25 A7: テキスト注釈ラベル (lyr3Points と整列)
  , lyr3StemBaseZ :: !(Last Double)          -- Phase 26 A4: stem の底面 z (data 空間・既定 0。 normLayer3D が lyr3BarBaseZ へ正規化)
  , lyr3Vectors   :: !(Last [Vec3])          -- Phase 26 A3: quiver3D の各点ベクトル (data 空間→normLayer3D で per-axis 正規化)
  , lyr3VecScale  :: !(Last Double)          -- Phase 26 A3: quiver3D 矢印長の倍率 (autoscale × この値・既定 1)
  , lyr3Faces     :: !(Last [(Int, Int, Int)])
                                             -- Phase 27 A2: trisurf の face index (resolve 産物・座標非依存)
                                             --   WebGL は z-buffer ゆえ CPU の depth ソート不要。 face は (x,y) Delaunay で前計算。
  , lyr3TextBy    :: !(Last ColRef)          -- Phase 30 A6: 列駆動 'text3D' の label 列 (resolveLayer3D で lyr3Labels へ)
  , lyr3Annots    :: ![(Point3, Text)]       -- Phase 30 A6: inline 注釈 ('text3DPoints'/'annotate3D')。
                                             --   plain list = concat Monoid ゆえ 'annotate3D' を <> で畳める (lyr3Points/Labels は Last で後勝ち・畳めない)。
                                             --   normLayer3D の M3Text で lyr3Points/lyr3Labels に materialize → renderer 無改修。
  } deriving (Show, Eq, Generic)
instance ToJSON   Layer3D
instance FromJSON Layer3D

-- 25 field と多いので positional でなく record 構文で per-field '(<>)' する
-- (= field 追加時の取り違え防止。 Phase 25 A3 で 20→25 field に拡張した際に変更)。
instance Semigroup Layer3D where
  x <> y = Layer3D
    { lyr3Kind      = lyr3Kind      x <> lyr3Kind      y
    , lyr3Points    = lyr3Points    x <> lyr3Points    y
    , lyr3Color     = lyr3Color     x <> lyr3Color     y
    , lyr3Size      = lyr3Size      x <> lyr3Size      y
    , lyr3Alpha     = lyr3Alpha     x <> lyr3Alpha     y
    , lyr3Width     = lyr3Width     x <> lyr3Width     y
    , lyr3Edges     = lyr3Edges     x <> lyr3Edges     y
    , lyr3Grid      = lyr3Grid      x <> lyr3Grid      y
    , lyr3XRange    = lyr3XRange    x <> lyr3XRange    y
    , lyr3YRange    = lyr3YRange    x <> lyr3YRange    y
    , lyr3Shaded    = lyr3Shaded    x <> lyr3Shaded    y
    , lyr3EdgeColor = lyr3EdgeColor x <> lyr3EdgeColor y
    , lyr3Colormap  = lyr3Colormap  x <> lyr3Colormap  y
    , lyr3Contours  = lyr3Contours  x <> lyr3Contours  y
    , lyr3SurfaceWire = lyr3SurfaceWire x <> lyr3SurfaceWire y
    , lyr3EncX      = lyr3EncX      x <> lyr3EncX      y
    , lyr3EncY      = lyr3EncY      x <> lyr3EncY      y
    , lyr3EncZ      = lyr3EncZ      x <> lyr3EncZ      y
    , lyr3ColorBy   = lyr3ColorBy   x <> lyr3ColorBy   y
    , lyr3PtColors  = lyr3PtColors  x <> lyr3PtColors  y
    , lyr3Legend    = lyr3Legend    x <> lyr3Legend    y
    , lyr3ColorByV  = lyr3ColorByV  x <> lyr3ColorByV  y
    , lyr3SizeBy    = lyr3SizeBy    x <> lyr3SizeBy    y
    , lyr3SizeRange = lyr3SizeRange x <> lyr3SizeRange y
    , lyr3PtSizes   = lyr3PtSizes   x <> lyr3PtSizes   y
    , lyr3Colorbar  = lyr3Colorbar  x <> lyr3Colorbar  y
    , lyr3BarStyle  = lyr3BarStyle  x <> lyr3BarStyle  y
    , lyr3BarWidth  = lyr3BarWidth  x <> lyr3BarWidth  y
    , lyr3BarBaseZ  = lyr3BarBaseZ  x <> lyr3BarBaseZ  y
    , lyr3ErrBy     = lyr3ErrBy     x <> lyr3ErrBy     y
    , lyr3PtErrs    = lyr3PtErrs    x <> lyr3PtErrs    y
    , lyr3Labels    = lyr3Labels    x <> lyr3Labels    y
    , lyr3StemBaseZ = lyr3StemBaseZ x <> lyr3StemBaseZ y
    , lyr3Vectors   = lyr3Vectors   x <> lyr3Vectors   y
    , lyr3VecScale  = lyr3VecScale  x <> lyr3VecScale  y
    , lyr3Faces     = lyr3Faces     x <> lyr3Faces     y
    , lyr3TextBy    = lyr3TextBy    x <> lyr3TextBy    y
    , lyr3Annots    = lyr3Annots    x <> lyr3Annots    y
    }

instance Monoid Layer3D where
  mempty = Layer3D mempty mempty mempty mempty mempty mempty
                   mempty mempty mempty mempty mempty mempty mempty
                   mempty mempty mempty mempty mempty mempty mempty
                   mempty mempty mempty mempty mempty
                   mempty mempty mempty mempty mempty mempty mempty
                   mempty mempty mempty mempty mempty mempty

-- ===========================================================================
-- VisualSpec3D (= 2D 'VisualSpec' 同型、 layers ++ + per-field Last)
-- ===========================================================================

-- | 3D 図全体の spec。 'layers' は append、 その他は 'Last' (= 後勝ち)。
data VisualSpec3D = VisualSpec3D
  { vs3Layers     :: ![Layer3D]
  , vs3Title      :: !(Last Text)
  , vs3Camera     :: !(Last Camera3D)
  , vs3Proj       :: !(Last Projection3D)
  , vs3Axes       :: !(Last Axes3D)
  , vs3Width      :: !(Last Int)
  , vs3Height     :: !(Last Int)
  , vs3AxisTitles :: !(Last (Text, Text, Text))  -- Phase 24 A8: 軸名 (既定 "x"/"y"/"z")
  , vs3ZAspect    :: !(Last Double)               -- Phase 24 A8: z 軸の box 縦横比 (既定 1)
  , vs3Pane       :: !(Last Bool)                 -- Phase 25 A6: 壁面 pane + gridline (既定 ON)
  , vs3XAspect    :: !(Last Double)               -- Phase 25 A8: x 軸の box 縦横比 (既定 1)
  , vs3YAspect    :: !(Last Double)               -- Phase 25 A8: y 軸の box 縦横比 (既定 1)
  , vs3Log        :: !(Last (Bool, Bool, Bool))   -- Phase 25 A8: 軸 log scale (x,y,z・既定 全 False)
  } deriving (Show, Eq, Generic)
instance ToJSON   VisualSpec3D
instance FromJSON VisualSpec3D

instance Semigroup VisualSpec3D where
  VisualSpec3D l1 t1 c1 p1 a1 w1 h1 at1 za1 pn1 xa1 ya1 lg1
    <> VisualSpec3D l2 t2 c2 p2 a2 w2 h2 at2 za2 pn2 xa2 ya2 lg2 =
      VisualSpec3D (l1 <> l2) (t1 <> t2) (c1 <> c2)
                   (p1 <> p2) (a1 <> a2) (w1 <> w2) (h1 <> h2)
                   (at1 <> at2) (za1 <> za2) (pn1 <> pn2)
                   (xa1 <> xa2) (ya1 <> ya2) (lg1 <> lg2)

instance Monoid VisualSpec3D where
  mempty = VisualSpec3D [] mempty mempty mempty mempty mempty mempty mempty mempty mempty
                        mempty mempty mempty

-- ===========================================================================
-- 純粋起点 + lift helpers (= 2D 'purePlot' / 'layer' 同型)
-- ===========================================================================

-- | 'VisualSpec3D' 起点 (= 2D 'purePlot' 同型、 'mempty' alias)。
purePlot3D :: VisualSpec3D
purePlot3D = mempty

-- | 'Layer3D' を 'VisualSpec3D' に lift (= 2D 'layer' 同型)。
layer3D :: Layer3D -> VisualSpec3D
layer3D l = mempty { vs3Layers = [l] }

-- ===========================================================================
-- Mark コンストラクタ (= Layer3D を返却、 後で `<> color3D ...` で属性 append)
-- ===========================================================================

-- | 3D scatter (= M3Scatter)。 Phase 24 A6: 2D 'Graphics.Hgg.Spec.scatter' と
-- 対称の **ColRef 3 つ** (x, y, z)。 列名 (OverloadedStrings) か 'inline' の
-- 生値を渡す。 旧 [Point3] 直入れは 'scatter3DPoints'。
scatter3D :: ColRef -> ColRef -> ColRef -> Layer3D
scatter3D x y z = mempty
  { lyr3Kind = First (Just M3Scatter)
  , lyr3EncX = Last (Just x), lyr3EncY = Last (Just y), lyr3EncZ = Last (Just z)
  }

-- | 3D scatter ([Point3] 直入れ・旧 'scatter3D')。
scatter3DPoints :: [Point3] -> Layer3D
scatter3DPoints pts = mempty
  { lyr3Kind   = First (Just M3Scatter)
  , lyr3Points = Last  (Just pts)
  }

-- | 3D line (= MLine3D、 連続折れ線)。
line3D :: ColRef -> ColRef -> ColRef -> Layer3D
line3D x y z = mempty
  { lyr3Kind = First (Just M3Line)
  , lyr3EncX = Last (Just x), lyr3EncY = Last (Just y), lyr3EncZ = Last (Just z)
  }

-- | 3D line ([Point3] 直入れ・旧 'line3D')。
line3DPoints :: [Point3] -> Layer3D
line3DPoints pts = mempty
  { lyr3Kind   = First (Just M3Line)
  , lyr3Points = Last  (Just pts)
  }

-- | 3D wireframe (= MWireframe3D、 任意 edge 群)。
wireframe3D :: [Point3] -> [(Int, Int)] -> Layer3D
wireframe3D pts es = mempty
  { lyr3Kind   = First (Just M3Wireframe)
  , lyr3Points = Last  (Just pts)
  , lyr3Edges  = Last  (Just es)
  }

-- | Phase 30 A6: 列駆動 3D surface (案C・= 2D 系と対称の ColRef 3 つ x/y/z)。 df の
--   long 形 (各行 = 格子点 (x,y,z)) を受け、 'resolveLayer3D' で **内部 pivot** して
--   grid mesh ('lyr3Grid' + xRange/yRange) に落とす。 grid は x/y が規則格子である前提
--   (= 全 (x_i,y_j) 組が揃う・欠損は NaN 穴)。 既に行列を持っている場合は 'surface3DGrid'。
--
--   @df |>> layer3D (surface3D \"x\" \"y\" \"z\" <> colormap3D)@
surface3D :: ColRef -> ColRef -> ColRef -> Layer3D
surface3D x y z = mempty
  { lyr3Kind = First (Just M3Surface)
  , lyr3EncX = Last (Just x), lyr3EncY = Last (Just y), lyr3EncZ = Last (Just z)
  }

-- | 3D surface (= MSurface3D、 grid mesh・行列直入れ・旧 'surface3D')。 z 値の
--   2 次元配列を受け、 x/y 範囲は 'xRange3D'/'yRange3D' (既定 (-1,1)) で与える。
surface3DGrid :: [[Double]] -> Layer3D
surface3DGrid grid = mempty
  { lyr3Kind = First (Just M3Surface)
  , lyr3Grid = Last  (Just grid)
  }

-- | Phase 25 A5: 3D bar (= M3Bar)。 'scatter3D' と対称の ColRef 3 つ (x, y,
--   高さ z)。 各 (x,y) に底面 (data z=0) から高さ z までの棒を立てる。 既定は
--   直方体 ('barStyle3D' で stick へ)。 [Point3] 直入れは 'bar3DPoints'。
bar3D :: ColRef -> ColRef -> ColRef -> Layer3D
bar3D x y z = mempty
  { lyr3Kind = First (Just M3Bar)
  , lyr3EncX = Last (Just x), lyr3EncY = Last (Just y), lyr3EncZ = Last (Just z)
  }

-- | Phase 25 A5: 3D bar ([Point3] 直入れ・各点の z = 棒の高さ)。
bar3DPoints :: [Point3] -> Layer3D
bar3DPoints tops = mempty
  { lyr3Kind   = First (Just M3Bar)
  , lyr3Points = Last  (Just tops)
  }

-- | Phase 26 A4: 3D stem (= M3Stem)。 'bar3D' と対称の ColRef 3 つ (x, y, z)。
--   各 (x,y) に底面 ('stemBaseZ'・既定 0) から z までの細い垂線 + 先端マーカーを
--   描く (3D lollipop)。 [Point3] 直入れは 'stem3DPoints'。 底面は 'stemBaseZ' で変更。
--
--   @stem3D \"x\" \"y\" \"z\" <> color3D (fromHex \"#d62728\") <> stemBaseZ 0@
stem3D :: ColRef -> ColRef -> ColRef -> Layer3D
stem3D x y z = mempty
  { lyr3Kind = First (Just M3Stem)
  , lyr3EncX = Last (Just x), lyr3EncY = Last (Just y), lyr3EncZ = Last (Just z)
  }

-- | Phase 26 A4: 3D stem ([Point3] 直入れ・各点の z = stem の先端高さ)。
stem3DPoints :: [Point3] -> Layer3D
stem3DPoints tops = mempty
  { lyr3Kind   = First (Just M3Stem)
  , lyr3Points = Last  (Just tops)
  }

-- | Phase 26 A4: stem の底面 z (data 空間・既定 0)。 mplot3d @stem(..., bottom=)@ 相当。
--   'normLayer3D' が axes に基づき正規化して 'lyr3BarBaseZ' へ落とす。
stemBaseZ :: Double -> Layer3D
stemBaseZ z = mempty { lyr3StemBaseZ = Last (Just z) }

-- | Phase 26 A3: 3D vector field (= M3Quiver)。 各 @(位置, ベクトル)@ に矢印を描く
--   (= mplot3d @quiver@)。 矢印長は autoscale (= 最長矢印が cube の ~35%) に
--   'vecScale3D' 倍を掛けた長さ。 描画は投影後の 2D 矢印 (本線 + 矢じり)。
--
--   @quiver3D [(Point3 0 0 0, Vec3 1 0 0), (Point3 1 1 1, Vec3 0 0 1)]@
quiver3D :: [(Point3, Vec3)] -> Layer3D
quiver3D items = mempty
  { lyr3Kind    = First (Just M3Quiver)
  , lyr3Points  = Last  (Just (map fst items))
  , lyr3Vectors = Last  (Just (map snd items))
  }

-- | Phase 26 A3: quiver3D 矢印長の倍率 (autoscale × この値・既定 1)。
vecScale3D :: Double -> Layer3D
vecScale3D s = mempty { lyr3VecScale = Last (Just s) }

-- | Phase 26 A5: trisurf (= M3Trisurf)。 不規則 (非 grid) な 3D 点群を (x,y) 平面で
--   Delaunay 三角分割して曲面化する (= mplot3d @plot_trisurf@)。 散らばった観測点・
--   GP 事後点など、 規則 grid でない曲面を描ける。 z 連続色は 'colormap3D' で。
--
--   @trisurf pts <> colormap3D viridisStops3D@
trisurf :: [Point3] -> Layer3D
trisurf pts = mempty
  { lyr3Kind   = First (Just M3Trisurf)
  , lyr3Points = Last  (Just pts)
  }

-- | Phase 30 A6: 列駆動の 3D テキスト注釈 (= 2D 'Graphics.Hgg.Spec.text' と対称・
--   ColRef 4 つ x/y/z + **label 列**)。 df の各行を投影し、 label 列の文字を PText で
--   置く。 解決は 'resolveLayer3D' (x/y/z → 'lyr3Points'・label → 'lyr3Labels')。
--   inline 生値版は 'text3DPoints'。 文字色は 'color3D' (既定 @#333333@)、 サイズは
--   'size3D' (既定 11)。
--
--   @text3D \"x\" \"y\" \"z\" \"name\"@
text3D :: ColRef -> ColRef -> ColRef -> ColRef -> Layer3D
text3D x y z lab = mempty
  { lyr3Kind   = First (Just M3Text)
  , lyr3EncX = Last (Just x), lyr3EncY = Last (Just y), lyr3EncZ = Last (Just z)
  , lyr3TextBy = Last (Just lab)
  }

-- | Phase 30 A6: 3D テキスト注釈 (inline・[(点, 文字列)] 直入れ・旧 'text3D')。 各
--   @(点, 文字列)@ を投影して PText を出す (depth 統合外の前面 overlay)。 位置は他
--   mark と同じ正規化 / z-aspect pipeline を通る。 'lyr3Annots' (concat Monoid) に
--   積むので 'annotate3D' と @<>@ で畳める。
--
--   @text3DPoints [(Point3 0 0 1, \"peak\"), (Point3 1 1 0, \"corner\")]@
text3DPoints :: [(Point3, Text)] -> Layer3D
text3DPoints items = mempty
  { lyr3Kind   = First (Just M3Text)
  , lyr3Annots = items
  }

-- | Phase 25 A7 / 30 A6: 単一ラベルの注釈。 'lyr3Annots' に 1 件積むので
--   @annotate3D a \"A\" <> annotate3D b \"B\"@ のように **畳める** (= 複数注釈が累積。
--   旧実装は 'lyr3Labels' が Last で後勝ち → 畳めなかった)。
--
--   @annotate3D (Point3 0 0 1) \"max\" <> color3D (fromHex \"#d62728\") <> size3D 13@
annotate3D :: Point3 -> Text -> Layer3D
annotate3D p t = text3DPoints [(p, t)]

-- ===========================================================================
-- per-layer 属性 (= 2D 'color' / 'size' / 'alpha' 同型、 Layer3D 返却)
-- ===========================================================================

-- | Phase 30 A5: 固定色 (= 2D 'Graphics.Hgg.Spec.color' 同型・型安全な 'Color')。
--   @scatter3D pts <> color3D (fromHex "#56B4E9")@。 ワイヤは従来通り Text なので
--   入口で 'toCss' 変換して格納する (Render / PS / JSON は無改修)。
color3D :: Color -> Layer3D
color3D c = mempty { lyr3Color = Last (Just (toCss c)) }

-- | 便利関数 (2D 'Graphics.Hgg.Spec.colorRGBA' の 3D 双子): 8 桁 RGBA hex
--   (@"#rrggbbaa"@ / 4 桁 @"#rgba"@) を @color3D (fromHex …) <> alpha3D …@ に展開。
--   不正入力は 'error' (total 版は 'colorRGBA3DMaybe')。
colorRGBA3D :: Text -> Layer3D
colorRGBA3D t = let (c, a) = fromHexA t in color3D c <> alpha3D a

-- | 'colorRGBA3D' の total 版。 不正な hex は 'Nothing'。
colorRGBA3DMaybe :: Text -> Maybe Layer3D
colorRGBA3DMaybe t = (\(c, a) -> color3D c <> alpha3D a) <$> fromHexAMaybe t

-- | Phase 25 A2: scatter/line を**カテゴリ列**で群色分けする (+ 離散凡例)。
--   @scatter3D "x" "y" "z" <> colorBy3D "group"@。 色は 2D 同型の ggplot 既定
--   palette ('ggplotHue')。 解決は 'resolveLayer3D' で点ごと色 + 凡例 mapping に
--   落とす (カテゴリは初出順)。
colorBy3D :: ColRef -> Layer3D
colorBy3D c = mempty { lyr3ColorBy = Last (Just c) }

-- | Phase 25 A3: scatter を**数値列**の連続色 (viridis) でマップする (+ colorbar)。
--   @scatter3D "x" "y" "z" <> colorContinuousBy3D "temp"@。 A2 'colorBy3D' (カテゴリ
--   →離散凡例) と対。 解決は 'resolveLayer3D' で点ごと色 + colorbar 情報
--   (stops, min, max) に落とす。 stops は surface と共有の 'viridisStops3D'。
--   (Phase 30 A5: 2D 'colorContinuousBy' と命名対称化。 旧名 @colorByValue3D@。)
colorContinuousBy3D :: ColRef -> Layer3D
colorContinuousBy3D c = mempty { lyr3ColorByV = Last (Just c) }

-- | Phase 25 A3: scatter の点サイズ (= 基本半径 px) を**数値列**でマップする
--   (bubble chart)。 @scatter3D "x" "y" "z" <> sizeBy3D "mass"@。 px 範囲は
--   既定 @(4, 18)@、 変えたい時は 'sizeRange3D'。 値→size は線形 (min→範囲下端、
--   max→範囲上端)。 depth cue は基本 size に乗算される。
sizeBy3D :: ColRef -> Layer3D
sizeBy3D c = mempty { lyr3SizeBy = Last (Just c) }

-- | Phase 25 A3: 'sizeBy3D' の出力 px 範囲を明示指定 (下端, 上端)。 単独では
--   無効 ('sizeBy3D' と併用)。 未指定時の既定は @(4, 18)@。
sizeRange3D :: (Double, Double) -> Layer3D
sizeRange3D r = mempty { lyr3SizeRange = Last (Just r) }

-- | Phase 25 A5: bar スタイル (直方体 'BarCuboid' / 縦線 'BarStick')。 既定は
--   'BarCuboid'。 @bar3D "x" "y" "z" <> barStyle3D BarStick@。
barStyle3D :: BarStyle3D -> Layer3D
barStyle3D s = mempty { lyr3BarStyle = Last (Just s) }

-- | Phase 25 A5: bar footprint の半幅 (= 軸 span に対する比・既定 0.04)。
--   正規化空間 ([-1,1]) では x/y 共通でこの値がそのまま半幅になる。
barWidth3D :: Double -> Layer3D
barWidth3D w = mempty { lyr3BarWidth = Last (Just w) }

-- | Phase 25 A5: 誤差棒 (z 方向 ±err) を**数値列**で付ける。 bar・scatter
--   どちらの layer にも付けられる (頂点に縦線 + 端キャップ)。
--   @bar3D "x" "y" "z" <> errorBar3D "se"@。
errorBar3D :: ColRef -> Layer3D
errorBar3D c = mempty { lyr3ErrBy = Last (Just c) }

-- | Phase 30 A5: surface 等のエッジ線色 (固定色・型安全な 'Color')。
--   ワイヤは Text 維持 ('toCss' 変換して格納)。
edgeColor3D :: Color -> Layer3D
edgeColor3D c = mempty { lyr3EdgeColor = Last (Just (toCss c)) }

size3D :: Double -> Layer3D
size3D s = mempty { lyr3Size = Last (Just s) }

alpha3D :: Double -> Layer3D
alpha3D a = mempty { lyr3Alpha = Last (Just a) }

width3D :: Double -> Layer3D
width3D w = mempty { lyr3Width = Last (Just w) }

shaded3D :: Bool -> Layer3D
shaded3D b = mempty { lyr3Shaded = Last (Just b) }

-- | Phase 24 A2: surface 面色を z 値の連続色 (viridis) にする。
--   @layer3D (surface3DGrid grid <> colormap3D)@。 stops を変えたい時は
--   'colormapWith3D'。
colormap3D :: Layer3D
colormap3D = colormapWith3D viridisStops3D

-- | Phase 24 A2: 任意 gradient stops の colormap (hex 色の線形補間)。
colormapWith3D :: [Text] -> Layer3D
colormapWith3D stops = mempty { lyr3Colormap = Last (Just stops) }

-- | surface を**面なしの格子線メッシュ**で描く (matplotlib @plot_wireframe@ 相当)。
--   @surface3D@ に @\<>@ で合成: @layer3D (surface3DGrid grid \<> surfaceWire \<> color3D (fromHex "#2563eb"))@。
--   面を塗らないので 'colormap3D' / 'shaded3D' は無効 (線色は 'color3D')。 任意エッジの
--   'wireframe3D' とは別 (こちらは grid から行/列の線メッシュを自動生成)。
surfaceWire :: Layer3D
surfaceWire = mempty { lyr3SurfaceWire = Last (Just True) }

-- | 投影 contour の軸 (内部表現)。 'contourX' / 'contourY' / 'contourZ' が設定する。
--   matplotlib @contour(..., zdir=)@ 相当 (= dir に垂直な平面で曲面を切り、 壁へ投影)。
data ContourDir = ContourX | ContourY | ContourZ
  deriving (Show, Eq, Generic)
instance ToJSON   ContourDir
instance FromJSON ContourDir

-- | surface の **x 断面** @n@ 本を左右の壁 (yz 平面) へ投影する (matplotlib
--   @contour(..., zdir='x')@ / plotly @contours.x.project@ 相当)。 x 軸を等分した
--   @n@ 位置で曲面を切り、 各断面プロファイル @z = f(x_k, y)@ を壁に描く。 投影壁は
--   **カメラから遠い面に自動固定**。 同じ surface に 'contourY' / 'contourZ' を @\<>@ で
--   合成可。 線色は colormap (既定 viridis) の x 連続色。
--   @surface3DGrid grid \<> colormap3D \<> contourX 8 \<> contourY 8 \<> contourZ 8@。
contourX :: Int -> Layer3D
contourX n = mempty { lyr3Contours = [(ContourX, n)] }

-- | surface の **y 断面** @n@ 本を前後の壁 (xz 平面) へ投影する (matplotlib
--   @contour(..., zdir='y')@ 相当)。 各断面プロファイル @z = f(x, y_k)@ を壁に描く。
--   投影壁はカメラから遠い面に自動固定。 'contourX' / 'contourZ' と合成可。
contourY :: Int -> Layer3D
contourY n = mempty { lyr3Contours = [(ContourY, n)] }

-- | surface の **等高線 (z 等値面)** @n@ 本を床 (xy 平面) へ投影する (matplotlib
--   @contour(..., zdir='z')@ / plotly @contours_z@ 相当)。 z 軸を等分した @n@ 値で
--   level set @{f = z_k}@ を抽出し床へ落とす (topographic map)。 投影面はカメラから
--   遠い面 (通常は床) に自動固定。 'contourX' / 'contourY' と合成可。
contourZ :: Int -> Layer3D
contourZ n = mempty { lyr3Contours = [(ContourZ, n)] }

-- | 2D 'ColorByContinuous' と同じ viridis 5-stop (palette 共有)。
viridisStops3D :: [Text]
viridisStops3D = ["#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725"]

xRange3D :: (Double, Double) -> Layer3D
xRange3D r = mempty { lyr3XRange = Last (Just r) }

yRange3D :: (Double, Double) -> Layer3D
yRange3D r = mempty { lyr3YRange = Last (Just r) }

-- ===========================================================================
-- VisualSpec3D 属性
-- ===========================================================================

-- | camera 設定 (= 後勝ち、 'Last')。
camera :: Camera3D -> VisualSpec3D
camera c = mempty { vs3Camera = Last (Just c) }

-- | projection 設定。
projection :: Projection3D -> VisualSpec3D
projection p = mempty { vs3Proj = Last (Just p) }

-- | axes 設定。 名前は 'axes3D' で 2D 'theme' / 'facet' と同型語感。
axes3D :: Axes3D -> VisualSpec3D
axes3D a = mempty { vs3Axes = Last (Just a) }

-- | title (= 2D 'title' 同型、 ただし `3D` suffix で衝突回避)。
title3D :: Text -> VisualSpec3D
title3D t = mempty { vs3Title = Last (Just t) }

-- | Phase 24 A8: 軸名 (x, y, z) を任意指定 (既定 "x"/"y"/"z")。
axisTitles3D :: Text -> Text -> Text -> VisualSpec3D
axisTitles3D x y z = mempty { vs3AxisTitles = Last (Just (x, y, z)) }

-- | Phase 24 A8: z 軸の box 縦横比 (正規化後の z スケール係数・既定 1)。
--   @< 1@ で扁平、 @> 1@ で縦長。 軸 box・surface・scatter・床面 contour すべてに
--   一貫適用される。
zAspect3D :: Double -> VisualSpec3D
zAspect3D a = mempty { vs3ZAspect = Last (Just a) }

-- | Phase 25 A6: 壁面 pane + gridline の on/off (mplot3d 標準の薄灰 3 壁・既定 ON)。
--   @pane3D False@ で従来の cube wireframe + tick のみに戻す。
pane3D :: Bool -> VisualSpec3D
pane3D b = mempty { vs3Pane = Last (Just b) }

-- | Phase 25 A8: x 軸の box 縦横比 (正規化後 x スケール係数・既定 1。 'zAspect3D' の x 版)。
xAspect3D :: Double -> VisualSpec3D
xAspect3D a = mempty { vs3XAspect = Last (Just a) }

-- | Phase 25 A8: y 軸の box 縦横比 (正規化後 y スケール係数・既定 1。 'zAspect3D' の y 版)。
yAspect3D :: Double -> VisualSpec3D
yAspect3D a = mempty { vs3YAspect = Last (Just a) }

-- | Phase 25 A8: 軸を log scale に (x, y, z の順で flag 指定・既定 全 False)。
--   log 軸はデータが正の前提 (非正は 1e-12 に clamp)。 tick は 10 の冪 (decade)、
--   ラベルは元の値のまま。 surface の log-z は対応、 surface の log-x/y は現状未対応
--   (point 系 mark = scatter/line/bar は全軸 log 可)。
--
--   @logScale3D False False True@  -- z だけ log (片対数)
logScale3D :: Bool -> Bool -> Bool -> VisualSpec3D
logScale3D x y z = mempty { vs3Log = Last (Just (x, y, z)) }

-- | width (= canvas 幅 px、 2D 'vsWidth' 同型)。
width3DV :: Int -> VisualSpec3D
width3DV w = mempty { vs3Width = Last (Just w) }

-- | height (= canvas 高さ px、 2D 'vsHeight' 同型)。
height3DV :: Int -> VisualSpec3D
height3DV h = mempty { vs3Height = Last (Just h) }

-- ===========================================================================
-- Layer3D → 既存 Scatter3D / Line3D / Wireframe3D / Surface3D 変換
-- (= render 経路で使う。 defaults は existing default*3D 関数を経由)
-- ===========================================================================

-- | Layer3D (= rolled-up) を 'Scatter3D' に変換。 'lyr3Kind' が 'M3Scatter' か
--   未指定の時に意味あり。 必須項目 ('lyr3Points') 未指定なら空 points で。
layerToScatter :: Layer3D -> Scatter3D
layerToScatter l =
  let pts = fromMaybe [] (getLast (lyr3Points l))
      base = defaultScatter3D pts
  in base
       { sc3Color  = fromMaybe (sc3Color base) (getLast (lyr3Color l))
       , sc3Size   = fromMaybe (sc3Size  base) (getLast (lyr3Size  l))
       , sc3Alpha  = fromMaybe (sc3Alpha base) (getLast (lyr3Alpha l))
       , sc3Colors = getLast (lyr3PtColors l)   -- Phase 25 A2/A3: per-point 色
       , sc3Sizes  = getLast (lyr3PtSizes  l)   -- Phase 25 A3: per-point size (bubble)
       }

layerToLine :: Layer3D -> Line3D
layerToLine l =
  let pts = fromMaybe [] (getLast (lyr3Points l))
      base = defaultLine3D pts
  in base
       { Graphics.Hgg.ThreeD.Line.l3Color = fromMaybe (Graphics.Hgg.ThreeD.Line.l3Color base) (getLast (lyr3Color l))
       , Graphics.Hgg.ThreeD.Line.l3Width = fromMaybe (Graphics.Hgg.ThreeD.Line.l3Width base) (getLast (lyr3Width l))
       }

layerToWireframe :: Layer3D -> Wireframe3D
layerToWireframe l =
  let pts = fromMaybe [] (getLast (lyr3Points l))
      es  = fromMaybe [] (getLast (lyr3Edges  l))
      base = defaultWireframe3D pts es
  in base
       { wfColor = fromMaybe (wfColor base) (getLast (lyr3Color l))
       , wfWidth = fromMaybe (wfWidth base) (getLast (lyr3Width l))
       }

layerToSurface :: Layer3D -> Surface3D
layerToSurface l =
  let grid = fromMaybe [] (getLast (lyr3Grid l))
      base = defaultSurface3D grid
  in base
       { sf3Color     = fromMaybe (sf3Color     base) (getLast (lyr3Color     l))
       , sf3EdgeColor = fromMaybe (sf3EdgeColor base) (getLast (lyr3EdgeColor l))
       , sf3Shaded    = fromMaybe (sf3Shaded    base) (getLast (lyr3Shaded    l))
       , sf3XRange    = fromMaybe (sf3XRange    base) (getLast (lyr3XRange    l))
       , sf3YRange    = fromMaybe (sf3YRange    base) (getLast (lyr3YRange    l))
       , sf3Colormap  = getLast (lyr3Colormap l)
       , sf3Alpha     = fromMaybe (sf3Alpha     base) (getLast (lyr3Alpha     l))  -- Phase 25 A4
       , sf3Wire      = fromMaybe (sf3Wire      base) (getLast (lyr3SurfaceWire l))
       }

-- | Phase 25 A5: Layer3D を 'Bar3D' に変換 (render 経路で使う)。 点・base・half-width
--   は正規化済前提 ('normLayer3D' / 'scaleZLayer' が事前に処理)。 base は
--   'lyr3BarBaseZ' (未設定なら 0)、 半幅は 'lyr3BarWidth' (既定 0.04)。
layerToBar :: Layer3D -> Bar3D
layerToBar l =
  let tops = fromMaybe [] (getLast (lyr3Points l))
      base = defaultBar3D tops
  in base
       { br3BaseZ = fromMaybe (br3BaseZ base) (getLast (lyr3BarBaseZ l))
       , br3HalfW = fromMaybe (br3HalfW base) (getLast (lyr3BarWidth l))
       , br3Style = fromMaybe (br3Style base) (getLast (lyr3BarStyle l))
       , br3Color = fromMaybe (br3Color base) (getLast (lyr3Color    l))
       , br3Alpha = fromMaybe (br3Alpha base) (getLast (lyr3Alpha    l))
       , br3Width = fromMaybe (br3Width base) (getLast (lyr3Width    l))
       }

-- | Phase 26 A3: Layer3D を 'Quiver3D' に変換 (render 経路で使う)。 点・ベクトルは
--   正規化済前提 ('normLayer3D' / 'scaleAspectLayer' が事前に処理)。 autoscale =
--   最長矢印が cube の 35% になるよう正規化ベクトル長で割り、 'lyr3VecScale' を掛ける。
--   終点 = 始点 + scale × vec。
layerToQuiver :: Layer3D -> Quiver3D
layerToQuiver l =
  let starts = fromMaybe [] (getLast (lyr3Points  l))
      vecs   = fromMaybe [] (getLast (lyr3Vectors l))
      userS  = fromMaybe 1  (getLast (lyr3VecScale l))
      lens   = [ sqrt (vx*vx + vy*vy + vz*vz) | Vec3 vx vy vz <- vecs ]
      maxLen = if null lens then 0 else maximum lens
      autoS  = if maxLen <= 0 then 0 else 0.35 / maxLen
      s      = autoS * userS
      ends   = [ Point3 (px + s*vx) (py + s*vy) (pz + s*vz)
               | (Point3 px py pz, Vec3 vx vy vz) <- zip starts vecs ]
      base = defaultQuiver3D starts ends
  in base
       { q3Color = fromMaybe (q3Color base) (getLast (lyr3Color l))
       , q3Width = fromMaybe (q3Width base) (getLast (lyr3Width l))
       }

-- ===========================================================================
-- autoAxes3D: layers から bounding box を自動算出 (= Axes 未指定時の default)
-- ===========================================================================

-- | 全 layer の Point3 集合から min/max を取って 'Axes3D' を生成。
--   surface3D は xRange/yRange + grid 高さで Point3 集合を構成。
--   layer が無い / 全 Point3 が空なら 'defaultAxes3D' (= 単位 cube)。
-- Phase 24 A6: 列参照 (Enc) の解決 — Resolver で [Point3] に落とす

-- | 層の列参照 (x,y,z) を 'Resolver' で解決して 'lyr3Points' に格納する。
--   3 列が全部解決できた時だけ上書き ('inline' 生値は resolver 不要で解決)。
--   Enc が無い層 (旧 [Point3] 直入れ・surface 等) は素通し。
resolveLayer3D :: Resolver -> Layer3D -> Layer3D
resolveLayer3D r l0 =
  let -- まず x/y/z 列参照 → 点列 (Phase 24 A6)
      l1 = case (getLast (lyr3EncX l0), getLast (lyr3EncY l0), getLast (lyr3EncZ l0)) of
        (Just cx, Just cy, Just cz)
          | Just xs <- resolveNum r cx
          , Just ys <- resolveNum r cy
          , Just zs <- resolveNum r cz ->
              let n   = minimum [V.length xs, V.length ys, V.length zs]
              -- Phase 30 A6: surface (案C) は long 形 (x,y,z) を pivot して grid へ。
              --   それ以外 (scatter/line/bar/stem/text…) は従来通り点列に落とす。
              in case getFirst (lyr3Kind l0) of
                   Just M3Surface ->
                     let (grid, xr, yr) = pivotSurface3D
                           (take n (V.toList xs)) (take n (V.toList ys)) (take n (V.toList zs))
                     in l0 { lyr3Grid   = Last (Just grid)
                           , lyr3XRange = Last (Just xr)
                           , lyr3YRange = Last (Just yr) }
                   _ ->
                     let pts = [ Point3 (xs V.! i) (ys V.! i) (zs V.! i) | i <- [0 .. n - 1] ]
                     in l0 { lyr3Points = Last (Just pts) }
        _ -> l0
      -- Phase 27 A2: trisurf の face index を前計算 (= (x,y) Delaunay)。
      --   座標非依存ゆえ WebGL/CPU 両経路で同一。 WebGL は z-buffer なので depth ソート不要。
      l2 = case getFirst (lyr3Kind l1) of
        Just M3Trisurf ->
          let pts = fromMaybe [] (getLast (lyr3Points l1))
          in if null pts then l1
             else l1 { lyr3Faces = Last (Just (delaunay2D [ (x, y) | Point3 x y _ <- pts ])) }
        _ -> l1
  in resolveTextBy3D r (resolveErrBy3D r (resolveSizeBy3D r (resolveColorByValue3D r (resolveColorBy3D r l2))))

-- | Phase 30 A6: 列駆動 'text3D' の label 列 ('lyr3TextBy') を解決して 'lyr3Labels' に
--   落とす (x/y/z は 'resolveLayer3D' 先頭で既に 'lyr3Points' へ解決済)。 数値列は
--   'numLabel' で文字化。 未指定なら素通し。
resolveTextBy3D :: Resolver -> Layer3D -> Layer3D
resolveTextBy3D r l = case getLast (lyr3TextBy l) of
  Nothing -> l
  Just c  ->
    let labels = case resolveCol r c of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> map numLabel (V.toList v)
          Nothing          -> []
    in if null labels then l else l { lyr3Labels = Last (Just labels) }

-- | Phase 30 A6: surface 案C の pivot。 long 形の (xs, ys, zs) を規則格子の z 行列に
--   畳む。 列 = x の昇順 unique・行 = y の昇順 unique。 grid[i][j] = (uxs[j], uys[i]) の z。
--   欠損 (組が無い) セルは NaN (= 描画穴)。 重複 (同一 (x,y)) は後勝ち。 xRange/yRange は
--   実データの min/max (空なら (-1,1))。
pivotSurface3D :: [Double] -> [Double] -> [Double]
               -> ([[Double]], (Double, Double), (Double, Double))
pivotSurface3D xs ys zs =
  let m    = M.fromList (zip (zip xs ys) zs)       -- (x,y) → z (後勝ち)
      uxs  = M.keys (M.fromList [ (x, ()) | x <- xs ])  -- 昇順 unique x (列)
      uys  = M.keys (M.fromList [ (y, ()) | y <- ys ])  -- 昇順 unique y (行)
      nan  = 0 / 0 :: Double
      grid = [ [ M.findWithDefault nan (x, y) m | x <- uxs ] | y <- uys ]
      rng vs = if null vs then (-1, 1) else (minimum vs, maximum vs)
  in (grid, rng xs, rng ys)

-- | Phase 25 A2: カテゴリ列 ('lyr3ColorBy') を解決して点ごと色 + 凡例 mapping に
--   落とす。 カテゴリは初出順、 色は 'ggplotHue'。 未指定なら素通し。
resolveColorBy3D :: Resolver -> Layer3D -> Layer3D
resolveColorBy3D r l = case getLast (lyr3ColorBy l) of
  Nothing -> l
  Just c  ->
    let labels = case resolveCol r c of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> map numLabel (V.toList v)
          Nothing          -> []
        cats     = nub labels                       -- 初出順
        palette  = ggplotHue (length cats)
        catColor = zip cats (palette ++ repeat "#333333")
        colOf lbl = maybe "#333333" id (lookup lbl catColor)
    in if null labels then l
       else l { lyr3PtColors = Last (Just (map colOf labels))
              , lyr3Legend   = Last (Just catColor) }

-- | Phase 25 A3: 数値列 ('lyr3ColorByV') を解決して点ごと連続色 (viridis) +
--   colorbar 情報 (stops, min, max) に落とす。 値→色は線形正規化 + 'continuousColor'
--   (= 2D gradient 凡例 / surface colormap と同じ補間)。 未指定 / 非数値なら素通し。
--   全値同一なら中央色 (t=0.5)。
resolveColorByValue3D :: Resolver -> Layer3D -> Layer3D
resolveColorByValue3D r l = case getLast (lyr3ColorByV l) of
  Nothing -> l
  Just c  -> case resolveNum r c of
    Nothing -> l
    Just v  ->
      let vals = V.toList v
      in if null vals then l else
        let vMin = minimum vals
            vMax = maximum vals
            stops = viridisStops3D
            tOf x = if vMax <= vMin then 0.5 else (x - vMin) / (vMax - vMin)
            cols  = map (continuousColor stops . tOf) vals
        in l { lyr3PtColors = Last (Just cols)
             , lyr3Colorbar = Last (Just (stops, vMin, vMax)) }

-- | Phase 25 A3: 数値列 ('lyr3SizeBy') を解決して点ごと基本 size (px) に落とす
--   (bubble)。 値→size は線形 (min→範囲下端、 max→範囲上端)。 範囲は
--   'lyr3SizeRange' (既定 (4, 18))。 未指定 / 非数値なら素通し。 全値同一なら
--   範囲の中点。
resolveSizeBy3D :: Resolver -> Layer3D -> Layer3D
resolveSizeBy3D r l = case getLast (lyr3SizeBy l) of
  Nothing -> l
  Just c  -> case resolveNum r c of
    Nothing -> l
    Just v  ->
      let vals = V.toList v
      in if null vals then l else
        let (sLo, sHi) = fromMaybe (4, 18) (getLast (lyr3SizeRange l))
            vMin = minimum vals
            vMax = maximum vals
            sizeOf x = if vMax <= vMin then (sLo + sHi) / 2
                       else sLo + (sHi - sLo) * (x - vMin) / (vMax - vMin)
        in l { lyr3PtSizes = Last (Just (map sizeOf vals)) }

-- | Phase 25 A5: 誤差棒の err 列 ('lyr3ErrBy') を解決して点ごと err (data 単位)
--   に落とす。 正規化 (data z → [-1,1]) は 'Easy.normLayer3D' が行う。 未指定 /
--   非数値なら素通し。
resolveErrBy3D :: Resolver -> Layer3D -> Layer3D
resolveErrBy3D r l = case getLast (lyr3ErrBy l) of
  Nothing -> l
  Just c  -> case resolveNum r c of
    Nothing -> l
    Just v  -> let vals = V.toList v
               in if null vals then l
                  else l { lyr3PtErrs = Last (Just vals) }

-- | 数値カテゴリのラベル整形 (整数なら末尾 .0 を落とす)。
numLabel :: Double -> Text
numLabel x =
  let r = fromIntegral (round x :: Int) :: Double
  in if r == x then T.pack (show (round x :: Int)) else T.pack (show x)

-- | spec 内の全層の列参照を解決する ('saveSVG3DBound' / bind 経路の正本)。
resolveSpec3D :: Resolver -> VisualSpec3D -> VisualSpec3D
resolveSpec3D r spec = spec { vs3Layers = map (resolveLayer3D r) (vs3Layers spec) }

autoAxes3D :: [Layer3D] -> Axes3D
autoAxes3D ls =
  let pts = concatMap layerPoints ls
  in if null pts
       then defaultAxes3D
       else
         let xs = [x | Point3 x _ _ <- pts]
             ys = [y | Point3 _ y _ <- pts]
             zs = [z | Point3 _ _ z <- pts]
         in Axes3D
              { axesXMin  = minimum xs
              , axesXMax  = maximum xs
              , axesYMin  = minimum ys
              , axesYMax  = maximum ys
              , axesZMin  = minimum zs
              , axesZMax  = maximum zs
              , axesNTicks = 5
              , axesXLog = False, axesYLog = False, axesZLog = False
              }

-- | Layer から Point3 集合を抽出 (= Surface は grid を Point3 に展開)。
-- Phase 25 A5: bar は底面 (z=0) を、 誤差棒のある層は z±err 端を含めて axis box が
-- 棒の根本 / whisker を覆うようにする。
layerPoints :: Layer3D -> [Point3]
layerPoints l = case getFirst (lyr3Kind l) of
  Just M3Surface ->
    let grid = fromMaybe [] (getLast (lyr3Grid l))
        (xMin, xMax) = fromMaybe (-1, 1) (getLast (lyr3XRange l))
        (yMin, yMax) = fromMaybe (-1, 1) (getLast (lyr3YRange l))
        ny = length grid
        nx = case grid of
          row:_ -> length row
          []    -> 0
        xAt j = if nx <= 1 then xMin
                else xMin + (xMax - xMin) * fromIntegral j / fromIntegral (nx - 1)
        yAt i = if ny <= 1 then yMin
                else yMin + (yMax - yMin) * fromIntegral i / fromIntegral (ny - 1)
    in [ Point3 (xAt j) (yAt i) z
       | (i, row) <- zip [0 :: Int ..] grid
       , (j, z)   <- zip [0 :: Int ..] row
       ]
  Just M3Bar ->
    let tops  = fromMaybe [] (getLast (lyr3Points l))
        bases = [ Point3 x y 0 | Point3 x y _ <- tops ]
    in tops ++ bases ++ errPoints l tops
  -- Phase 26 A4: stem は底面 (stemBaseZ・data 空間) を axis box に含める
  Just M3Stem ->
    let tops  = fromMaybe [] (getLast (lyr3Points l))
        bz    = fromMaybe 0 (getLast (lyr3StemBaseZ l))
        bases = [ Point3 x y bz | Point3 x y _ <- tops ]
    in tops ++ bases ++ errPoints l tops
  -- Phase 26 A3: quiver は始点 (位置) のみで axis box (矢印は autoscale で cube 内)
  Just M3Quiver -> fromMaybe [] (getLast (lyr3Points l))
  -- Phase 30 A6: text は inline 注釈 ('lyr3Annots') の点も axis box に含める
  --   (= 列駆動は lyr3Points・inline は lyr3Annots。 normLayer3D 前なので両方見る)。
  Just M3Text ->
    fromMaybe [] (getLast (lyr3Points l)) ++ map fst (lyr3Annots l)
  _ -> let pts = fromMaybe [] (getLast (lyr3Points l))
       in pts ++ errPoints l pts

-- | Phase 25 A5: 誤差棒のある層で、 各点の z±err 端点を返す (axis box 拡張用)。
errPoints :: Layer3D -> [Point3] -> [Point3]
errPoints l pts = case getLast (lyr3PtErrs l) of
  Nothing -> []
  Just es -> concat [ [Point3 x y (z + e), Point3 x y (z - e)]
                    | (Point3 x y z, e) <- zip pts es ]

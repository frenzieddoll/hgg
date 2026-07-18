-- |
-- Module      : Hgg.Plot.ThreeD.Line
-- Description : 3D 線 / wireframe (Phase 3 A6)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.ThreeD.Line
  ( Line3D (..)
  , defaultLine3D
  , renderLine3D
  , Wireframe3D (..)
  , defaultWireframe3D
  , renderWireframe3D
  , Quiver3D (..)
  , defaultQuiver3D
  , renderQuiver3D
  ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.List                       (sortOn)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Hgg.Plot.Render             (LineStyle (..), Point (..),
                                                  Primitive (..))
import           Hgg.Plot.ThreeD.Projection  (Projected (..), Viewport,
                                                  project3D)
import           Hgg.Plot.ThreeD.Types

-- 3D 連続線
data Line3D = Line3D
  { l3Points :: ![Point3]
  , l3Color  :: !Text
  , l3Width  :: !Double
  } deriving (Show, Eq, Generic)
instance ToJSON   Line3D
instance FromJSON Line3D

defaultLine3D :: [Point3] -> Line3D
defaultLine3D ps = Line3D ps "#1f77b4" 1.5

renderLine3D :: Camera3D -> Projection3D -> Viewport -> Line3D -> [Primitive]
renderLine3D cam proj vp ln =
  let project = project3D cam proj vp
      projected = [ project p | p <- l3Points ln ]
      pairs = zip projected (drop 1 projected)
      style = LineStyle (l3Color ln) (l3Width ln) []
      mid (Projected _ _ d1) (Projected _ _ d2) = (d1 + d2) / 2
      sorted = sortOn (\(p1, p2) -> negate (mid p1 p2)) pairs
      mkLine (Projected x1 y1 _, Projected x2 y2 _) =
        PLine (Point x1 y1) (Point x2 y2) style
  in map mkLine sorted

-- wireframe (= 任意 segment 群)
data Wireframe3D = Wireframe3D
  { wfPoints :: ![Point3]
  , wfEdges  :: ![(Int, Int)]
  , wfColor  :: !Text
  , wfWidth  :: !Double
  } deriving (Show, Eq, Generic)
instance ToJSON   Wireframe3D
instance FromJSON Wireframe3D

defaultWireframe3D :: [Point3] -> [(Int, Int)] -> Wireframe3D
defaultWireframe3D pts es = Wireframe3D pts es "#2ca02c" 1.0

renderWireframe3D :: Camera3D -> Projection3D -> Viewport -> Wireframe3D -> [Primitive]
renderWireframe3D cam proj vp wf =
  let project = project3D cam proj vp
      pts = wfPoints wf
      projAll = [ project p | p <- pts ]
      style = LineStyle (wfColor wf) (wfWidth wf) []
      validEdges = [ (i, j) | (i, j) <- wfEdges wf
                            , i >= 0, j >= 0
                            , i < length pts, j < length pts ]
      mkSeg (i, j) =
        let p1 = projAll !! i
            p2 = projAll !! j
            d = (projDepth p1 + projDepth p2) / 2
        in (d, p1, p2)
      segs = map mkSeg validEdges
      sorted = sortOn (\(d, _, _) -> negate d) segs
      mkLine (_, Projected x1 y1 _, Projected x2 y2 _) =
        PLine (Point x1 y1) (Point x2 y2) style
  in map mkLine sorted

-- ===========================================================================
-- Phase 26 A3: 3D vector field (quiver3D)
-- ===========================================================================

-- | 3D vector field。 各矢印は始点 'q3Starts' → 終点 'q3Ends' (= 正規化済・
-- autoscale + aspect 適用後の点列)。 描画は両端を投影して 2D 矢印 (本線 + 矢じり)
-- にする (= mplot3d も実質 2D 矢じり)。 depth 統合外の前面 overlay。
data Quiver3D = Quiver3D
  { q3Starts :: ![Point3]   -- ^ 矢印の始点 (正規化済)
  , q3Ends   :: ![Point3]   -- ^ 矢印の終点 (正規化済・始点 + scaled vec)
  , q3Color  :: !Text
  , q3Width  :: !Double
  } deriving (Show, Eq, Generic)
instance ToJSON   Quiver3D
instance FromJSON Quiver3D

defaultQuiver3D :: [Point3] -> [Point3] -> Quiver3D
defaultQuiver3D ss es = Quiver3D ss es "#1f77b4" 1.5

renderQuiver3D :: Camera3D -> Projection3D -> Viewport -> Quiver3D -> [Primitive]
renderQuiver3D cam proj vp q =
  concatMap arrow (zip (q3Starts q) (q3Ends q))
  where
    project = project3D cam proj vp
    style = LineStyle (q3Color q) (q3Width q) []
    arrow (s, e) =
      let Projected sx sy _ = project s
          Projected ex ey _ = project e
      in arrowHead2D sx sy ex ey style

-- | 投影後の 2D 矢印 (本線 + 矢じり 2 本)。 矢じり形状は 2D quiver
-- (core Render.Basic drawArrow2D) と同じ (長さ 9px・開き比 0.5)。
arrowHead2D :: Double -> Double -> Double -> Double -> LineStyle -> [Primitive]
arrowHead2D px1 py1 px2 py2 ls =
  let dx = px2 - px1; dy = py2 - py1
      len = sqrt (dx * dx + dy * dy)
  in if len < 1e-9 then []
     else
       let ux = dx / len; uy = dy / len
           ah = 9; aw = 0.5
           bx = px2 - ux * ah; by = py2 - uy * ah
           lx = bx - uy * ah * aw; ly = by + ux * ah * aw
           rx = bx + uy * ah * aw; ry = by - ux * ah * aw
       in [ PLine (Point px1 py1) (Point px2 py2) ls
          , PLine (Point px2 py2) (Point lx ly) ls
          , PLine (Point px2 py2) (Point rx ry) ls ]

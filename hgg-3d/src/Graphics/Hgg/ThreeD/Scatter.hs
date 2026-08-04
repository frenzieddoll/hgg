-- |
-- Module      : Graphics.Hgg.ThreeD.Scatter
-- Description : 3D scatter plot
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 3D 点群を camera + projection で 2D に変換し、 painter's algorithm
--   (= z で sort、 奥から手前へ) で描画。 depth cue として size と alpha を z で
--   線形に減衰させる。
-- [English]: Converts a 3D point cloud into 2D via the camera and
--   projection, then renders it with the painter's algorithm (sorted by z,
--   drawn from back to front). Size and alpha fade linearly with z as a
--   depth cue.
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.ThreeD.Scatter
  ( Scatter3D (..)
  , defaultScatter3D
  , renderScatter3D
  , scatterPointsDepth
  ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.List                       (sortOn)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Graphics.Hgg.Render             (FillStyle (..),
                                                  Point (..), Primitive (..),
                                                  StrokeStyle (..))
import           Graphics.Hgg.ThreeD.Projection  (Projected (..), Viewport,
                                                  project3D)
import           Graphics.Hgg.ThreeD.Types

-- | [日本語]: 3D scatter 1 series 分の設定。
--   [English]: The configuration for one series of a 3D scatter plot.
data Scatter3D = Scatter3D
  { sc3Points :: ![Point3]   -- ^ [日本語]: 3D 点群
                              --   [English]: The 3D point cloud.
  , sc3Color  :: !Text       -- ^ [日本語]: 色 (= hex 例 "#1f77b4")
                              --   [English]: The color (hex, e.g. "#1f77b4").
  , sc3Size   :: !Double     -- ^ [日本語]: 基本 size (= 近側の半径 px、 奥は depth cue で小さく)
                              --   [English]: The base size (the near-side radius in
                              --   px; farther points shrink via the depth cue).
  , sc3Alpha  :: !Double     -- ^ [日本語]: 基本 alpha (= 近側の値、 奥は depth cue で薄く)
                              --   [English]: The base alpha (the near-side value;
                              --   farther points fade via the depth cue).
  , sc3Colors :: !(Maybe [Text])
    -- ^ [日本語]: 点ごとの色 (= 群色分け / 連続色マップ)。 @Just cs@ で
    --   @cs !! i@ を点 i に使う (長さが点数未満なら超過点は 'sc3Color')。
    --   @Nothing@ = 全点 'sc3Color' (後方互換)。
    --   [English]: A per-point color (for group coloring or a continuous
    --   colormap). With @Just cs@, point i uses @cs !! i@ (points beyond the
    --   list's length fall back to 'sc3Color'). @Nothing@ means every point
    --   uses 'sc3Color' (backward compatible).
  , sc3Sizes  :: !(Maybe [Double])
    -- ^ [日本語]: 点ごとの基本 size (= 値マップ bubble)。 @Just ss@ で
    --   @ss !! i@ を点 i の基本半径に使う (長さが点数未満なら超過点は 'sc3Size')。
    --   @Nothing@ = 全点 'sc3Size' (後方互換)。 depth cue は基本 size に乗算。
    --   [English]: A per-point base size (a value-mapped bubble). With
    --   @Just ss@, point i's base radius is @ss !! i@ (points beyond the
    --   list's length fall back to 'sc3Size'). @Nothing@ means every point
    --   uses 'sc3Size' (backward compatible). The depth cue multiplies the
    --   base size.
  } deriving (Show, Eq, Generic)
instance ToJSON   Scatter3D
instance FromJSON Scatter3D

-- | [日本語]: default: 青、 size 5、 alpha 0.85、 単色・単 size (per-point 無し)。
--   [English]: The default: blue, size 5, alpha 0.85, a single color and
--   size (no per-point overrides).
defaultScatter3D :: [Point3] -> Scatter3D
defaultScatter3D ps = Scatter3D ps "#1f77b4" 5 0.85 Nothing Nothing

-- | [日本語]: 3D scatter を 2D Primitive (= PCircle) 列に変換。
--   z で sort して painter's (= 奥から描画)、 size / alpha は depth cue で減衰。
--   [English]: Converts a 3D scatter into a list of 2D primitives (@PCircle@).
--   Sorted by z for the painter's algorithm (drawn back-to-front); size and
--   alpha fade via the depth cue.
renderScatter3D
  :: Camera3D -> Projection3D -> Viewport
  -> Scatter3D -> [Primitive]
renderScatter3D cam proj vp sc =
  map snd (sortOn (negate . fst) (scatterPointsDepth cam proj vp sc))

-- | [日本語]: (depth 統合): 各点を @(投影 depth, PCircle)@ として__未ソート__で
--   返す (= 'renderScatter3D' は @sortOn (negate.fst)@ → @map snd@・ビット同一)。
--   surface 面との層横断 depth 統合に使う (scatter 点が膜を透ける問題の解消)。
--   [English]: (depth integration): Returns each point as
--   @(projected depth, PCircle)@, __unsorted__ ('renderScatter3D' is
--   bit-identical to @sortOn (negate . fst)@ followed by @map snd@ of this).
--   Used to integrate depth across layers together with surface faces
--   (fixing scatter points showing through a surface).
scatterPointsDepth
  :: Camera3D -> Projection3D -> Viewport
  -> Scatter3D -> [(Double, Primitive)]
scatterPointsDepth cam proj vp sc =
  let project = project3D cam proj vp
      stroke = Just (StrokeStyle "#333333" 0.5)
      -- Phase 25 A2: 点ごとの色 (無ければ単色)。 index で引く。
      colorAt i = case sc3Colors sc of
        Just cs | i < length cs -> cs !! i
        _                       -> sc3Color sc
      -- Phase 25 A3: 点ごとの基本 size (無ければ単 size)。 index で引く。
      sizeAt i = case sc3Sizes sc of
        Just ss | i < length ss -> ss !! i
        _                       -> sc3Size sc
      mkCircle baseR col (Projected sx sy d) =
        let depthCue = max 0 (min 1 ((1 - d) / 2))
            r = baseR * (0.55 + 0.45 * depthCue)
            a = sc3Alpha sc * (0.4 + 0.6 * depthCue)
        in PCircle (Point sx sy) r
                   (FillStyle col a)
                   stroke
                   Nothing
  in [ (d, mkCircle (sizeAt i) (colorAt i) pr)
     | (i, p) <- zip [0 ..] (sc3Points sc)
     , let pr@(Projected _ _ d) = project p ]

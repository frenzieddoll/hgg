-- |
-- Module      : Graphics.Hgg.ThreeD
-- Description : Public entry point of hgg-3d (re-exports the main API)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 3D plot library (CPU projection + painter's algorithm)。 既存の
--   hgg 2D backend (SVG / PDF / Rasterific) でそのまま使える。
--
--   主要型と関数を本 module で再エクスポート。 細部は子 module を参照:
--
--   * "Graphics.Hgg.ThreeD.Types" — Point3 / Vec3 / Mat4 / Camera3D / Projection3D
--   * "Graphics.Hgg.ThreeD.Projection" — project3D / lookAt 等の純粋関数
--   * "Graphics.Hgg.ThreeD.Axes" — Axes3D (= 立方体 wireframe + 3 軸 tick)
--   * "Graphics.Hgg.ThreeD.Scatter" — MScatter3D
--   * "Graphics.Hgg.ThreeD.Line" — MLine3D / MWireframe3D
--   * "Graphics.Hgg.ThreeD.Surface" — MSurface3D
--   * "Graphics.Hgg.ThreeD.Easy" — matplotlib mplot3d 風 helper
-- [English]: A 3D plot library (CPU projection + the painter's algorithm).
--   Works directly with the existing hgg 2D backends (SVG / PDF /
--   Rasterific).
--
--   This module re-exports the main types and functions; see the child
--   modules for the details:
--
--   * "Graphics.Hgg.ThreeD.Types" — Point3 / Vec3 / Mat4 / Camera3D / Projection3D
--   * "Graphics.Hgg.ThreeD.Projection" — pure functions such as project3D / lookAt
--   * "Graphics.Hgg.ThreeD.Axes" — Axes3D (a cube wireframe plus tick marks on the 3 axes)
--   * "Graphics.Hgg.ThreeD.Scatter" — MScatter3D
--   * "Graphics.Hgg.ThreeD.Line" — MLine3D / MWireframe3D
--   * "Graphics.Hgg.ThreeD.Surface" — MSurface3D
--   * "Graphics.Hgg.ThreeD.Easy" — matplotlib mplot3d-style helpers
module Graphics.Hgg.ThreeD
  ( -- * 中核型
    module Graphics.Hgg.ThreeD.Types
    -- * Projection
  , module Graphics.Hgg.ThreeD.Projection
    -- * Axes3D
    -- * Scatter / Line / Wireframe / Surface
  ) where

import Graphics.Hgg.ThreeD.Types
import Graphics.Hgg.ThreeD.Projection

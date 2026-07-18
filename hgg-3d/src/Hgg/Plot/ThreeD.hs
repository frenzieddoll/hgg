-- |
-- Module      : Hgg.Plot.ThreeD
-- Description : hgg-3d 公開エントリ (= 主要 API の再エクスポート)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 3D plot library (CPU projection + painter's algorithm)。 既存の hgg
-- 2D backend (SVG / PDF / Rasterific) でそのまま使える。
--
-- 主要型と関数を本 module で再エクスポート。 細部は子 module を参照:
--
-- * "Hgg.Plot.ThreeD.Types" — Point3 / Vec3 / Mat4 / Camera3D / Projection3D
-- * "Hgg.Plot.ThreeD.Projection" — project3D / lookAt 等の純粋関数
-- * "Hgg.Plot.ThreeD.Axes" — Axes3D (= 立方体 wireframe + 3 軸 tick)
-- * "Hgg.Plot.ThreeD.Scatter" — MScatter3D
-- * "Hgg.Plot.ThreeD.Line" — MLine3D / MWireframe3D
-- * "Hgg.Plot.ThreeD.Surface" — MSurface3D
-- * "Hgg.Plot.ThreeD.Easy" — matplotlib mplot3d 風 helper
module Hgg.Plot.ThreeD
  ( -- * 中核型 (= A3 で実装)
    module Hgg.Plot.ThreeD.Types
    -- * Projection (= A3 で実装)
  , module Hgg.Plot.ThreeD.Projection
    -- * Axes3D (= A4 で実装)
    -- * Scatter / Line / Wireframe / Surface (= A5-A7 で実装)
  ) where

import Hgg.Plot.ThreeD.Types
import Hgg.Plot.ThreeD.Projection

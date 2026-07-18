-- |
-- Module      : Graphics.Hgg.ThreeD.Projection
-- Description : 3D → 2D 投影 (Phase 3 A3)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 標準 3D グラフィックス pipeline:
--
--   world space → (view 行列、 lookAt) → camera space
--               → (projection 行列、 ortho or persp) → clip space
--               → (perspective divide) → NDC (= [-1, 1]^3)
--               → (viewport 変換) → 2D screen 座標
--
-- 'project3D' が全 step をまとめて (Point3 world → Point2D screen + zClip) を返す。
-- z は depth cue (= painter's algorithm の sort key) に使う。
{-# LANGUAGE BangPatterns #-}
module Graphics.Hgg.ThreeD.Projection
  ( -- * Matrix 演算
    identityM
  , multM
  , transformPoint
    -- * View / Projection 行列
  , viewMatrix
  , projectionMatrix
    -- * Projection (= 1 ショット)
  , Projected (..)
  , project3D
    -- * Viewport (= NDC → screen pixel)
  , Viewport (..)
  , viewportTransform
  ) where

import           Graphics.Hgg.ThreeD.Types

-- ===========================================================================
-- Matrix 基本演算 (= 4x4)
-- ===========================================================================

-- | 単位行列。
identityM :: Mat4
identityM = Mat4 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1

-- | 行列積 (= row-major、 自分 × 引数)。
multM :: Mat4 -> Mat4 -> Mat4
multM
  (Mat4 a11 a12 a13 a14
        a21 a22 a23 a24
        a31 a32 a33 a34
        a41 a42 a43 a44)
  (Mat4 b11 b12 b13 b14
        b21 b22 b23 b24
        b31 b32 b33 b34
        b41 b42 b43 b44) =
  Mat4
    (a11*b11 + a12*b21 + a13*b31 + a14*b41)
    (a11*b12 + a12*b22 + a13*b32 + a14*b42)
    (a11*b13 + a12*b23 + a13*b33 + a14*b43)
    (a11*b14 + a12*b24 + a13*b34 + a14*b44)

    (a21*b11 + a22*b21 + a23*b31 + a24*b41)
    (a21*b12 + a22*b22 + a23*b32 + a24*b42)
    (a21*b13 + a22*b23 + a23*b33 + a24*b43)
    (a21*b14 + a22*b24 + a23*b34 + a24*b44)

    (a31*b11 + a32*b21 + a33*b31 + a34*b41)
    (a31*b12 + a32*b22 + a33*b32 + a34*b42)
    (a31*b13 + a32*b23 + a33*b33 + a34*b43)
    (a31*b14 + a32*b24 + a33*b34 + a34*b44)

    (a41*b11 + a42*b21 + a43*b31 + a44*b41)
    (a41*b12 + a42*b22 + a43*b32 + a44*b42)
    (a41*b13 + a42*b23 + a43*b33 + a44*b43)
    (a41*b14 + a42*b24 + a43*b34 + a44*b44)

-- | Point3 を 4x4 行列で変換 (= 同次座標で w=1 として扱い、 結果も Point3 返す)。
-- w 成分は別途扱うので 'project3D' を経由するのが通常。
transformPoint :: Mat4 -> Point3 -> Point3
transformPoint
  (Mat4 m11 m12 m13 m14
        m21 m22 m23 m24
        m31 m32 m33 m34
        _ _ _ _)
  (Point3 x y z) =
  Point3
    (m11 * x + m12 * y + m13 * z + m14)
    (m21 * x + m22 * y + m23 * z + m24)
    (m31 * x + m32 * y + m33 * z + m34)

-- ===========================================================================
-- View 行列 (= lookAt) / Projection 行列
-- ===========================================================================

-- | lookAt: camera の view 行列を生成。 右手系、 OpenGL 流。
--
-- 結果: world space の点 P に対し viewMatrix * P が camera space の P' を返す。
-- camera space では camera が原点、 -z 方向を向く (= 右手系 OpenGL 慣例)。
viewMatrix :: Camera3D -> Mat4
viewMatrix (Camera3D (Point3 ex ey ez) (Point3 tx ty tz) up) =
  let -- forward = normalize(target - eye)、 ただし camera は -z を向くので f は実は反転して使う
      f@(Vec3 fx fy fz) = normalizeV3 (Vec3 (tx - ex) (ty - ey) (tz - ez))
      -- right = normalize(forward × up_raw)
      r@(Vec3 rx ry rz) = normalizeV3 (crossV3 f up)
      -- up = right × forward (= 直交化)
      Vec3 ux uy uz = crossV3 r f
      -- camera space: x → right、 y → up、 z → -forward
      tx' = - (rx * ex + ry * ey + rz * ez)
      ty' = - (ux * ex + uy * ey + uz * ez)
      tz' =   (fx * ex + fy * ey + fz * ez)  -- 負号は -forward 経由で打消し
  in Mat4
       rx       ry       rz       tx'
       ux       uy       uz       ty'
       (-fx)    (-fy)    (-fz)    tz'
       0        0        0        1

-- | Projection 行列を生成。 'Orthographic' / 'Perspective' どちらも対応。
--
-- 出力: camera space の点 P → clip space の P' (= w 成分も含む)。
-- 'project3D' で perspective divide (= x,y,z を w で割る) を続けて NDC へ。
projectionMatrix :: Projection3D -> Mat4
projectionMatrix (Orthographic xH yH n f) =
  let !sx = 1 / xH
      !sy = 1 / yH
      !sz = -2 / (f - n)
      !tz = -(f + n) / (f - n)
  in Mat4 sx 0  0  0   0  sy 0  0   0  0  sz tz   0 0 0 1
projectionMatrix (Perspective fov aspect n f) =
  let !ft  = 1 / tan (fov / 2)         -- focal term (= cot(fov/2))
      !sx  = ft / aspect
      !sy  = ft
      !sz  = -(f + n) / (f - n)
      !tz  = -(2 * f * n) / (f - n)
  in Mat4 sx 0  0  0   0  sy 0  0   0  0  sz tz   0 0 (-1) 0

-- ===========================================================================
-- Projection 一発
-- ===========================================================================

-- | Viewport: NDC (= [-1, 1]^2) → screen pixel への変換パラメタ。
-- 通常 Layout 領域に合わせて (xMin, yMin, width, height) で指定。
data Viewport = Viewport
  { vpX :: !Double
  , vpY :: !Double
  , vpW :: !Double
  , vpH :: !Double
  } deriving (Show, Eq)

-- | NDC [-1,1] の (x, y) → screen pixel への変換。 y は反転 (= SVG 慣例で y 下方向)。
viewportTransform :: Viewport -> Double -> Double -> (Double, Double)
viewportTransform (Viewport vx vy vw vh) ndcX ndcY =
  let sx = vx + (ndcX + 1) / 2 * vw
      sy = vy + (1 - (ndcY + 1) / 2) * vh
  in (sx, sy)

-- | 投影結果。 screen 座標 (= viewport 変換後) + z (= depth cue / sort 用、 NDC z)。
data Projected = Projected
  { projX     :: !Double  -- ^ screen pixel x
  , projY     :: !Double  -- ^ screen pixel y
  , projDepth :: !Double  -- ^ NDC z ∈ [-1, 1]、 -1 が手前、 +1 が奥
  } deriving (Show, Eq)

-- | World point → Projected (screen + depth)。
--
-- pipeline: world → view → projection → perspective divide → viewport。
project3D :: Camera3D -> Projection3D -> Viewport -> Point3 -> Projected
project3D cam proj vp p =
  let view = viewMatrix cam
      pj   = projectionMatrix proj
      mvp  = pj `multM` view
      -- 同次座標で 4 成分計算 (= w が必要)
      Mat4 m11 m12 m13 m14
           m21 m22 m23 m24
           m31 m32 m33 m34
           m41 m42 m43 m44 = mvp
      Point3 x y z = p
      cx = m11 * x + m12 * y + m13 * z + m14
      cy = m21 * x + m22 * y + m23 * z + m24
      cz = m31 * x + m32 * y + m33 * z + m34
      cw = m41 * x + m42 * y + m43 * z + m44
      -- perspective divide
      w' = if abs cw < 1e-12 then 1 else cw
      ndcX = cx / w'
      ndcY = cy / w'
      ndcZ = cz / w'
      (sx, sy) = viewportTransform vp ndcX ndcY
  in Projected sx sy ndcZ

-- |
-- Module      : Hgg.Plot.ThreeD.Types
-- Description : 3D の中核型 (Point3 / Vec3 / Mat4 / Camera3D / Projection3D)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- Phase 3 A2 段階: 中核型のみ定義。 関数 (= project3D / lookAt 等) は A3
-- (Hgg.Plot.ThreeD.Projection) で実装。
--
-- 設計判断: linear package 等の外部依存を入れず、 自前 Vec3 / Mat4 を持つ。
-- これで core (= base/vector/text/containers のみ) の依存戦略と整合する。
-- 行列演算は 4x4 で十分 (= camera + projection)、 性能不問の前提で simple list 実装。
{-# LANGUAGE DeriveGeneric #-}
module Hgg.Plot.ThreeD.Types
  ( -- * 幾何プリミティブ
    Point3 (..)
  , Vec3 (..)
    -- * 行列 (= 4x4 同次座標)
  , Mat4 (..)
    -- * Camera + Projection
  , Camera3D (..)
  , Projection3D (..)
    -- * Convention (= z-up / y-up を両対応、 default は z-up)
    -- $convention
  , zUp
  , yUp
  , defaultCameraZUp
  , defaultCameraYUp
    -- * 視点 preset (Phase 24 A8・z-up)
  , cameraIso
  , cameraTop
  , cameraFront
  , cameraSide
  , defaultPerspective
    -- * helper: 基本ベクトル演算
  , addV3
  , subV3
  , scaleV3
  , dotV3
  , crossV3
  , lengthV3
  , normalizeV3
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           GHC.Generics    (Generic)

-- | 3D 点 (= world / camera space どちらでも使う)。
--
-- JSON: positional fields → array `[x, y, z]` (= aeson Generic デフォルト挙動)。
-- PS Argonaut 側も同形式で decode。
data Point3 = Point3 !Double !Double !Double
  deriving (Show, Eq, Generic)
instance ToJSON   Point3
instance FromJSON Point3

-- | 3D ベクトル (= 方向 / 法線)。 JSON は Point3 と同じく array `[x, y, z]`。
data Vec3 = Vec3 !Double !Double !Double
  deriving (Show, Eq, Generic)
instance ToJSON   Vec3
instance FromJSON Vec3

-- $convention
-- hgg-3d は **z-up と y-up の両方をサポート** する。 default は **z-up**
-- (= mplot3d / 工学慣例)。 user は用途に応じて切替可能。
--
-- == 業界の使い分け (= 2026 時点の調査)
--
-- +-------------------+-------------------------------------------------+------------+
-- | 分野              | 主要ライブラリ                                  | up         |
-- +===================+=================================================+============+
-- | __data viz__      | matplotlib mplot3d / plotly / Mathematica /     | __z-up__   |
-- |                   | gnuplot splot / mayavi / R rgl                  |            |
-- +-------------------+-------------------------------------------------+------------+
-- | __工学 / CAD__    | AutoCAD / SolidWorks / Inventor / Unreal Engine | __z-up__   |
-- +-------------------+-------------------------------------------------+------------+
-- | __game / CG__     | Unity / Godot 3D / OpenGL 慣例 / DirectX 慣例   | __y-up__   |
-- +-------------------+-------------------------------------------------+------------+
-- | __3D modeling__   | Blender (= z-up に切替済) / Maya (= y-up)       | 分裂       |
-- +-------------------+-------------------------------------------------+------------+
--
-- == hgg-3d の default = z-up を選んだ理由
--
-- * メインターゲットが data viz (= hanalyze の HBM 等)
-- * mplot3d / plotly の慣例と整合 → 既存 user 知識が活きる
-- * @z = f(x, y)@ の数学慣例と一致 → 'Hgg.Plot.ThreeD.Surface.Surface3D' が直感的
-- * z = 物理量の高さ (= 確率密度 / 計測値 / 標高) の伝統
--
-- == どちらを使うべきか
--
-- * __z-up__ ('zUp' / 'defaultCameraZUp'): data viz、 工学解析、 統計分布、 surface plot。 default 推奨
-- * __y-up__ ('yUp' / 'defaultCameraYUp'): game-like view、 OpenGL 系 sample との整合、 既存 CG 知識の流用
--
-- 両 helper とも up vector を切替えるだけで他は同じ。 任意の up vector が必要なら
-- 'Camera3D' を直接構築する。

-- | z-up convention の up vector (= 'Vec3' 0 0 1)。 data viz / mplot3d 慣例。
zUp :: Vec3
zUp = Vec3 0 0 1

-- | y-up convention の up vector (= 'Vec3' 0 1 0)。 OpenGL / game engine 慣例。
yUp :: Vec3
yUp = Vec3 0 1 0

-- | z-up convention の default camera。 'eye' を data の bounding box 寄りに配置し、
-- 'target' を原点、 up を 'zUp' に。 typical 3/4 view (= 上前方斜め見下ろし)。
--
-- @
-- defaultCameraZUp 5  -- camera at (5, -5, 3) → origin (= 上前方斜め)
-- @
defaultCameraZUp :: Double -> Camera3D
defaultCameraZUp dist = Camera3D
  { cameraEye    = Point3 dist (negate dist) (dist * 0.6)
  , cameraTarget = Point3 0 0 0
  , cameraUp     = zUp
  }

-- | y-up convention の default camera。 OpenGL / Unity 慣例に整合する 3/4 view
-- (= y = 縦軸、 x/z = 床平面、 camera は y > 0 から見下ろし)。 game-like 表現や
-- OpenGL 系 sample との整合に。
--
-- @
-- defaultCameraYUp 5  -- camera at (5, 3, 5) → origin (= 上前方斜め、 y = up)
-- @
defaultCameraYUp :: Double -> Camera3D
defaultCameraYUp dist = Camera3D
  { cameraEye    = Point3 dist (dist * 0.6) dist
  , cameraTarget = Point3 0 0 0
  , cameraUp     = yUp
  }

-- | Phase 24 A8: 視点 preset (z-up・target = 原点)。 'cameraIso' = 'defaultCameraZUp'
-- (上前方斜め見下ろし)。 @dist@ は eye と原点の距離スケール。
cameraIso :: Double -> Camera3D
cameraIso = defaultCameraZUp

-- | Phase 24 A8: 真上から見下ろす視点 (xy 平面を正対・z は奥行き)。 up は y 軸
-- (見下ろし時に zUp は eye 方向と平行になり退化するため)。
cameraTop :: Double -> Camera3D
cameraTop dist = Camera3D
  { cameraEye    = Point3 0 0 dist
  , cameraTarget = Point3 0 0 0
  , cameraUp     = yUp
  }

-- | Phase 24 A8: 正面 (−y 方向から xz 面を正対・z = 縦)。
cameraFront :: Double -> Camera3D
cameraFront dist = Camera3D
  { cameraEye    = Point3 0 (negate dist) 0
  , cameraTarget = Point3 0 0 0
  , cameraUp     = zUp
  }

-- | Phase 24 A8: 真横 (+x 方向から yz 面を正対・z = 縦)。
cameraSide :: Double -> Camera3D
cameraSide dist = Camera3D
  { cameraEye    = Point3 dist 0 0
  , cameraTarget = Point3 0 0 0
  , cameraUp     = zUp
  }

-- | aspect 1:1 / fov 45° / near 0.1 / far 100 の sane default。
defaultPerspective :: Projection3D
defaultPerspective = Perspective (pi / 4) 1.0 0.1 100

-- | 4x4 同次座標行列 (= row-major、 16 Double を直接持つ)。
-- camera transform / perspective projection / model matrix で使う。
data Mat4 = Mat4
  !Double !Double !Double !Double
  !Double !Double !Double !Double
  !Double !Double !Double !Double
  !Double !Double !Double !Double
  deriving (Show, Eq, Generic)
-- JSON: 16 要素 array (= aeson Generic で positional fields は array 化)。
instance ToJSON   Mat4
instance FromJSON Mat4

-- | Camera 設定。 eye (= 視点)、 target (= 注視点)、 up (= 上方向、 通常 (0,1,0))。
-- 'Hgg.Plot.ThreeD.Projection.lookAt' で view 行列を生成する。
data Camera3D = Camera3D
  { cameraEye    :: !Point3
  , cameraTarget :: !Point3
  , cameraUp     :: !Vec3
  } deriving (Show, Eq, Generic)
instance ToJSON   Camera3D
instance FromJSON Camera3D

-- | Projection 設定。 orthographic / perspective の 2 種。
--
--   * 'Orthographic': 平行投影。 box の半幅 (xHalf, yHalf, near, far) で領域指定
--   * 'Perspective': 透視投影。 fov (= 縦方向視野角、 radians) + aspect ratio +
--     near + far クリップ面
data Projection3D
  = Orthographic
      { orthoXHalf :: !Double  -- ^ x 方向半幅
      , orthoYHalf :: !Double  -- ^ y 方向半幅
      , orthoNear  :: !Double
      , orthoFar   :: !Double
      }
  | Perspective
      { perspFov    :: !Double  -- ^ 縦方向 FOV、 radians
      , perspAspect :: !Double  -- ^ aspect ratio (= width / height)
      , perspNear   :: !Double
      , perspFar    :: !Double
      }
  deriving (Show, Eq, Generic)
-- JSON: aeson Generic デフォルト (= `{"tag": "Orthographic", "orthoXHalf": ..., ...}` or
--       `{"tag": "Perspective", "perspFov": ..., ...}`)。 PS Argonaut 側で同形式 decode。
instance ToJSON   Projection3D
instance FromJSON Projection3D

-- ===========================================================================
-- 基本ベクトル演算
-- ===========================================================================

-- | 加算。
addV3 :: Vec3 -> Vec3 -> Vec3
addV3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax + bx) (ay + by) (az + bz)

-- | 減算。
subV3 :: Vec3 -> Vec3 -> Vec3
subV3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax - bx) (ay - by) (az - bz)

-- | スカラー倍。
scaleV3 :: Double -> Vec3 -> Vec3
scaleV3 s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)

-- | 内積。
dotV3 :: Vec3 -> Vec3 -> Double
dotV3 (Vec3 ax ay az) (Vec3 bx by bz) = ax * bx + ay * by + az * bz

-- | 外積 (= 右手系)。
crossV3 :: Vec3 -> Vec3 -> Vec3
crossV3 (Vec3 ax ay az) (Vec3 bx by bz) =
  Vec3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

-- | L2 ノルム。
lengthV3 :: Vec3 -> Double
lengthV3 v = sqrt (dotV3 v v)

-- | 正規化 (= 単位ベクトル化)。 ゼロベクトルは (0,0,0) 返す (= 例外無し)。
normalizeV3 :: Vec3 -> Vec3
normalizeV3 v =
  let l = lengthV3 v
  in if l < 1e-12 then Vec3 0 0 0 else scaleV3 (1 / l) v

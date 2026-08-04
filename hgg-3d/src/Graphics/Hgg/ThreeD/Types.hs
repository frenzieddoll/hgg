-- |
-- Module      : Graphics.Hgg.ThreeD.Types
-- Description : 3D の中核型 (Point3 / Vec3 / Mat4 / Camera3D / Projection3D)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 中核型のみを定義する。 関数 (= project3D / lookAt 等) は
--   Graphics.Hgg.ThreeD.Projection で実装する。
--
--   設計判断: linear package 等の外部依存を入れず、 自前 Vec3 / Mat4 を持つ。
--   これで core (= base/vector/text/containers のみ) の依存戦略と整合する。
--   行列演算は 4x4 で十分 (= camera + projection)、 性能不問の前提で simple
--   list 実装。
-- [English]: Defines only the core types here. Functions (@project3D@,
--   @lookAt@, etc.) are implemented in Graphics.Hgg.ThreeD.Projection.
--
--   Design decision: rather than depending on an external package such as
--   @linear@, this module carries its own 'Vec3' / 'Mat4'. This keeps it
--   consistent with core's dependency strategy (base/vector/text/containers
--   only). 4x4 matrices are enough for camera + projection math, and since
--   performance is not a concern here, they are implemented as a simple
--   list of fields.
{-# LANGUAGE DeriveGeneric #-}
module Graphics.Hgg.ThreeD.Types
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
    -- * 視点 preset (z-up)
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

-- | [日本語]: 3D 点 (= world / camera space どちらでも使う)。
--
--   JSON: positional fields → array `[x, y, z]` (= aeson Generic デフォルト挙動)。
--   PS Argonaut 側も同形式で decode。
--   [English]: A 3D point (used for both world space and camera space).
--
--   JSON: positional fields serialize to the array @[x, y, z]@ (aeson's
--   default Generic behavior). The PureScript Argonaut side decodes the
--   same format.
data Point3 = Point3 !Double !Double !Double
  deriving (Show, Eq, Generic)
instance ToJSON   Point3
instance FromJSON Point3

-- | [日本語]: 3D ベクトル (= 方向 / 法線)。 JSON は Point3 と同じく array `[x, y, z]`。
--   [English]: A 3D vector (a direction or normal). Serializes to JSON as the
--   same array `[x, y, z]` as 'Point3'.
data Vec3 = Vec3 !Double !Double !Double
  deriving (Show, Eq, Generic)
instance ToJSON   Vec3
instance FromJSON Vec3

-- $convention
-- [日本語]: hgg-3d は __z-up と y-up の両方をサポート__ する。 default は
--   __z-up__ (= mplot3d / 工学慣例)。 user は用途に応じて切替可能。
--
--   == 業界の使い分け (= 2026 時点の調査)
--
--   [data viz]: __z-up__ — matplotlib mplot3d、 plotly、 Mathematica、
--     gnuplot splot、 mayavi、 R rgl
--   [工学 / CAD]: __z-up__ — AutoCAD、 SolidWorks、 Inventor、 Unreal Engine
--   [game / CG]: __y-up__ — Unity、 Godot 3D、 OpenGL 慣例、 DirectX 慣例
--   [3D modeling]: 分裂 — Blender (= z-up に切替済) と Maya (= y-up) で割れている
--
--   == hgg-3d の default = z-up を選んだ理由
--
--   * メインターゲットが data viz (= hanalyze の HBM 等)
--   * mplot3d / plotly の慣例と整合 → 既存 user 知識が活きる
--   * @z = f(x, y)@ の数学慣例と一致 → 'Graphics.Hgg.ThreeD.Surface.Surface3D' が直感的
--   * z = 物理量の高さ (= 確率密度 / 計測値 / 標高) の伝統
--
--   == どちらを使うべきか
--
--   * __z-up__ ('zUp' / 'defaultCameraZUp'): data viz、 工学解析、 統計分布、 surface plot。 default 推奨
--   * __y-up__ ('yUp' / 'defaultCameraYUp'): game-like view、 OpenGL 系 sample との整合、 既存 CG 知識の流用
--
--   両 helper とも up vector を切替えるだけで他は同じ。 任意の up vector が必要なら
--   'Camera3D' を直接構築する。
--
-- [English]: hgg-3d supports __both z-up and y-up__ conventions. The
--   default is __z-up__ (following the mplot3d / engineering convention);
--   users may switch depending on the use case.
--
--   == Industry conventions (as surveyed in 2026)
--
--   [data viz]: __z-up__ — matplotlib mplot3d, plotly, Mathematica,
--     gnuplot splot, mayavi, R rgl
--   [engineering / CAD]: __z-up__ — AutoCAD, SolidWorks, Inventor, Unreal Engine
--   [game / CG]: __y-up__ — Unity, Godot 3D, the OpenGL convention, the DirectX convention
--   [3D modeling]: split — Blender (switched to z-up) and Maya (y-up) disagree
--
--   == Why hgg-3d chose z-up as the default
--
--   * The main target is data viz (e.g. HBM plots in hanalyze)
--   * It matches the mplot3d / plotly convention, so existing user knowledge transfers
--   * It matches the mathematical convention @z = f(x, y)@, making 'Graphics.Hgg.ThreeD.Surface.Surface3D' intuitive
--   * z as the height of a physical quantity (probability density, a measurement, elevation) is a long-standing tradition
--
--   == Which one should you use
--
--   * __z-up__ ('zUp' / 'defaultCameraZUp'): data viz, engineering analysis, statistical distributions, surface plots. Recommended default
--   * __y-up__ ('yUp' / 'defaultCameraYUp'): game-like views, alignment with OpenGL-style samples, reuse of existing CG knowledge
--
--   Both helpers only swap the up vector; everything else stays the same. If
--   an arbitrary up vector is needed, construct 'Camera3D' directly.

-- | [日本語]: z-up convention の up vector (= 'Vec3' 0 0 1)。 data viz / mplot3d 慣例。
--   [English]: The up vector for the z-up convention ('Vec3' 0 0 1). Follows
--   the data-viz / mplot3d convention.
zUp :: Vec3
zUp = Vec3 0 0 1

-- | [日本語]: y-up convention の up vector (= 'Vec3' 0 1 0)。 OpenGL / game engine 慣例。
--   [English]: The up vector for the y-up convention ('Vec3' 0 1 0). Follows
--   the OpenGL / game-engine convention.
yUp :: Vec3
yUp = Vec3 0 1 0

-- | [日本語]: z-up convention の default camera。 @eye@ を data の bounding box 寄りに配置し、
--   @target@ を原点、 up を 'zUp' に。 typical 3/4 view (= 上前方斜め見下ろし)。
--   [English]: The default camera for the z-up convention. Places @eye@ near
--   the data's bounding box, sets @target@ to the origin, and up to 'zUp' —
--   a typical 3/4 view (looking down diagonally from the front).
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

-- | [日本語]: y-up convention の default camera。 OpenGL / Unity 慣例に整合する 3/4 view
--   (= y = 縦軸、 x/z = 床平面、 camera は y > 0 から見下ろし)。 game-like 表現や
--   OpenGL 系 sample との整合に。
--   [English]: The default camera for the y-up convention. A 3/4 view
--   consistent with the OpenGL / Unity convention (y is the vertical axis,
--   x/z form the ground plane, and the camera looks down from y > 0). Useful
--   for game-like presentations or alignment with OpenGL-style samples.
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

-- | [日本語]: 視点 preset (z-up・target = 原点)。 'cameraIso' = 'defaultCameraZUp'
--   (上前方斜め見下ろし)。 @dist@ は eye と原点の距離スケール。
--   [English]: A viewpoint preset (z-up, target at the origin). 'cameraIso'
--   is 'defaultCameraZUp' (looking down diagonally from the front). @dist@
--   scales the distance between eye and origin.
cameraIso :: Double -> Camera3D
cameraIso = defaultCameraZUp

-- | [日本語]: 真上から見下ろす視点 (xy 平面を正対・z は奥行き)。 up は y 軸
--   (見下ろし時に zUp は eye 方向と平行になり退化するため)。
--   [English]: A top-down viewpoint (facing the xy plane, with z as depth).
--   Up is the y axis, because looking straight down would make 'zUp'
--   parallel to the eye direction and degenerate.
cameraTop :: Double -> Camera3D
cameraTop dist = Camera3D
  { cameraEye    = Point3 0 0 dist
  , cameraTarget = Point3 0 0 0
  , cameraUp     = yUp
  }

-- | [日本語]: 正面 (−y 方向から xz 面を正対・z = 縦)。
--   [English]: A front view (facing the xz plane from the −y direction, with
--   z as vertical).
cameraFront :: Double -> Camera3D
cameraFront dist = Camera3D
  { cameraEye    = Point3 0 (negate dist) 0
  , cameraTarget = Point3 0 0 0
  , cameraUp     = zUp
  }

-- | [日本語]: 真横 (+x 方向から yz 面を正対・z = 縦)。
--   [English]: A side view (facing the yz plane from the +x direction, with
--   z as vertical).
cameraSide :: Double -> Camera3D
cameraSide dist = Camera3D
  { cameraEye    = Point3 dist 0 0
  , cameraTarget = Point3 0 0 0
  , cameraUp     = zUp
  }

-- | [日本語]: aspect 1:1 / fov 45° / near 0.1 / far 100 の sane default。
--   [English]: A sane default with aspect 1:1, fov 45°, near 0.1, far 100.
defaultPerspective :: Projection3D
defaultPerspective = Perspective (pi / 4) 1.0 0.1 100

-- | [日本語]: 4x4 同次座標行列 (= row-major、 16 Double を直接持つ)。
--   camera transform / perspective projection / model matrix で使う。
--   [English]: A 4x4 homogeneous-coordinate matrix (row-major, storing 16
--   'Double's directly). Used for the camera transform, perspective
--   projection, and model matrix.
data Mat4 = Mat4
  !Double !Double !Double !Double
  !Double !Double !Double !Double
  !Double !Double !Double !Double
  !Double !Double !Double !Double
  deriving (Show, Eq, Generic)
-- JSON: 16 要素 array (= aeson Generic で positional fields は array 化)。
instance ToJSON   Mat4
instance FromJSON Mat4

-- | [日本語]: Camera 設定。 eye (= 視点)、 target (= 注視点)、 up (= 上方向、 通常 (0,1,0))。
--   'Graphics.Hgg.ThreeD.Projection.lookAt' で view 行列を生成する。
--   [English]: Camera settings: eye (viewpoint), target (look-at point), and
--   up (up direction, usually (0,1,0)). 'Graphics.Hgg.ThreeD.Projection.lookAt'
--   builds the view matrix from these.
data Camera3D = Camera3D
  { cameraEye    :: !Point3
  , cameraTarget :: !Point3
  , cameraUp     :: !Vec3
  } deriving (Show, Eq, Generic)
instance ToJSON   Camera3D
instance FromJSON Camera3D

-- | [日本語]: Projection 設定。 orthographic / perspective の 2 種。
--
--     * 'Orthographic': 平行投影。 box の半幅 (xHalf, yHalf, near, far) で領域指定
--     * 'Perspective': 透視投影。 fov (= 縦方向視野角、 radians) + aspect ratio +
--       near + far クリップ面
--   [English]: Projection settings; either orthographic or perspective.
--
--     * 'Orthographic': parallel projection, specifying the region by box
--       half-widths (xHalf, yHalf, near, far)
--     * 'Perspective': perspective projection, specified by fov (vertical
--       field of view, in radians), aspect ratio, and the near/far clip
--       planes
data Projection3D
  = Orthographic
      { orthoXHalf :: !Double  -- ^ [日本語]: x 方向半幅
                                --   [English]: The half-width along x.
      , orthoYHalf :: !Double  -- ^ [日本語]: y 方向半幅
                                --   [English]: The half-width along y.
      , orthoNear  :: !Double
      , orthoFar   :: !Double
      }
  | Perspective
      { perspFov    :: !Double  -- ^ [日本語]: 縦方向 FOV、 radians
                                 --   [English]: The vertical field of view, in radians.
      , perspAspect :: !Double  -- ^ [日本語]: aspect ratio (= width / height)
                                 --   [English]: The aspect ratio (width / height).
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

-- | [日本語]: 加算。
--   [English]: Addition.
addV3 :: Vec3 -> Vec3 -> Vec3
addV3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax + bx) (ay + by) (az + bz)

-- | [日本語]: 減算。
--   [English]: Subtraction.
subV3 :: Vec3 -> Vec3 -> Vec3
subV3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax - bx) (ay - by) (az - bz)

-- | [日本語]: スカラー倍。
--   [English]: Scalar multiplication.
scaleV3 :: Double -> Vec3 -> Vec3
scaleV3 s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)

-- | [日本語]: 内積。
--   [English]: The dot product.
dotV3 :: Vec3 -> Vec3 -> Double
dotV3 (Vec3 ax ay az) (Vec3 bx by bz) = ax * bx + ay * by + az * bz

-- | [日本語]: 外積 (= 右手系)。
--   [English]: The cross product (right-handed).
crossV3 :: Vec3 -> Vec3 -> Vec3
crossV3 (Vec3 ax ay az) (Vec3 bx by bz) =
  Vec3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

-- | [日本語]: L2 ノルム。
--   [English]: The L2 norm.
lengthV3 :: Vec3 -> Double
lengthV3 v = sqrt (dotV3 v v)

-- | [日本語]: 正規化 (= 単位ベクトル化)。 ゼロベクトルは (0,0,0) 返す (= 例外無し)。
--   [English]: Normalization (converts to a unit vector). A zero vector
--   returns (0,0,0) (no exception is thrown).
normalizeV3 :: Vec3 -> Vec3
normalizeV3 v =
  let l = lengthV3 v
  in if l < 1e-12 then Vec3 0 0 0 else scaleV3 (1 / l) v

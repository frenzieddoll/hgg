-- |
-- Module      : Hgg.Plot.ThreeD.Axes
-- Description : 3D 軸 (立方体 wireframe + 3 軸 tick / label) (Phase 3 A4)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- mplot3d 風の axes: data bounding box (= xMin..zMax) を立方体 wireframe で
-- 囲み、 各軸に等間隔の tick を 1 列描画。 ラベルは tick の少し外側へ。
--
-- 設計判断:
--
--   * Phase 3 では「奥側面のみ」 を厳密判定せず、 12 辺全て描画 (= 多少視認性
--     落ちるが mplot3d 流の hairy wireframe より単純)
--   * tick は (xMin..xMax 等) の niceTicks3D で 5 点 default
--   * label: tick 値 + 軸名 (= "x"/"y"/"z")
--   * 出力は '[Primitive]' (= hgg-core の 2D primitive)、 既存 backend で描画
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.ThreeD.Axes
  ( Axes3D (..)
  , defaultAxes3D
  , niceTicks3D
  , logTicks3D
  , axisTicks3D
  , renderAxes3D
  , renderAxes3DWith
  , renderAxes3DWithLabels
    -- * 壁面 pane + gridline (Phase 25 A6)
  , PaneStyle3D (..)
  , defaultPaneStyle3D
  , renderAxes3DPanes
  ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.List                       (minimumBy)
import           Data.Ord                        (comparing)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)

import           Hgg.Plot.Render             (FillStyle (..), LineStyle (..),
                                                  PathSegment (..), Point (..),
                                                  Primitive (..),
                                                  TextAnchor (..),
                                                  TextStyle (..))
import           Hgg.Plot.ThreeD.Projection  (Projected (..), Viewport,
                                                  project3D)
import           Hgg.Plot.ThreeD.Types

-- | 3D 軸の bounding box + tick 数。
data Axes3D = Axes3D
  { axesXMin  :: !Double
  , axesXMax  :: !Double
  , axesYMin  :: !Double
  , axesYMax  :: !Double
  , axesZMin  :: !Double
  , axesZMax  :: !Double
  , axesNTicks :: !Int       -- ^ 軸あたり tick 数 (= default 5)
  , axesXLog  :: !Bool       -- ^ Phase 25 A8: x 軸を log scale に (既定 False)
  , axesYLog  :: !Bool       -- ^ Phase 25 A8: y 軸を log scale に (既定 False)
  , axesZLog  :: !Bool       -- ^ Phase 25 A8: z 軸を log scale に (既定 False)
  } deriving (Show, Eq, Generic)
instance ToJSON   Axes3D
instance FromJSON Axes3D

-- | 単位 cube (= [-1, 1]^3) + tick 5。
defaultAxes3D :: Axes3D
defaultAxes3D = Axes3D
  { axesXMin = -1, axesXMax = 1
  , axesYMin = -1, axesYMax = 1
  , axesZMin = -1, axesZMax = 1
  , axesNTicks = 5
  , axesXLog = False, axesYLog = False, axesZLog = False
  }

-- | 等間隔 tick 位置を 'n' 個生成。 niceNumbers アルゴリズム ではなく素朴な等間隔
-- (= mplot3d 風)。
niceTicks3D :: Int -> Double -> Double -> [Double]
niceTicks3D n lo hi
  | n <= 1 || abs (hi - lo) < 1e-12 = [lo]
  | otherwise =
      let step = (hi - lo) / fromIntegral (n - 1)
      in [ lo + step * fromIntegral i | i <- [0 .. n - 1] ]

-- | Phase 25 A8: log scale 軸の tick 位置 = @[lo, hi]@ 区間内の 10 の冪
-- (decade)。 端 (lo/hi) も含め、 冪が 1 個以下なら端 2 点で補う。 値は正前提
-- (lo<=0 は 1e-12 に clamp)。 軸ラベルは元の値のまま 'formatNum' で出る。
logTicks3D :: Double -> Double -> [Double]
logTicks3D lo0 hi0 =
  let lo = max 1e-12 lo0
      hi = max (lo * 10) hi0
      -- ±1e-9 の余裕で境界の冪 (log10 1000 = 2.9999.. 等の floor 漏れ) を拾う。
      k0 = ceiling (logBase 10 lo - 1e-9) :: Int
      k1 = floor   (logBase 10 hi + 1e-9) :: Int
      -- 10 ^^ k は反復乗算で exact (10 ** 3 = 1000.0000..1 を避ける)。
      decades = [ 10 ^^ k | k <- [k0 .. k1] ]
      inRange = filter (\v -> v >= lo * (1 - 1e-9) && v <= hi * (1 + 1e-9)) decades
  in case inRange of
       (_:_:_) -> inRange
       _       -> [lo, hi]   -- 冪が足りなければ端 2 点

-- | Phase 25 A8: log フラグで tick 生成を切替える (log = 'logTicks3D'・線形 =
-- 'niceTicks3D')。 両 axes renderer が共有。
axisTicks3D :: Bool -> Int -> Double -> Double -> [Double]
axisTicks3D isLog n lo hi
  | isLog     = logTicks3D lo hi
  | otherwise = niceTicks3D n lo hi

-- ===========================================================================
-- 描画
-- ===========================================================================

-- | Axes3D を 2D Primitive 列にレンダリング (= cube wireframe + 3 軸 tick + label)。
renderAxes3D :: Camera3D -> Projection3D -> Viewport -> Axes3D -> [Primitive]
renderAxes3D = renderAxes3DWith id

-- | Phase 24 A3: 投影前に座標変換 @f@ を合成する版。 @f@ に正規化
-- (データ bbox → [-1,1]^3) を渡すと、 **tick ラベルは元のデータ値のまま**
-- 形状だけ view box に収まる (saveSVG3D の正規化 pipeline 用)。
renderAxes3DWith :: (Point3 -> Point3)
                 -> Camera3D -> Projection3D -> Viewport -> Axes3D -> [Primitive]
renderAxes3DWith f = renderAxes3DWithLabels f ("x", "y", "z")

-- | Phase 24 A8: 軸名を任意指定する版 ('renderAxes3DWith' = @("x","y","z")@)。
renderAxes3DWithLabels :: (Point3 -> Point3) -> (Text, Text, Text)
                       -> Camera3D -> Projection3D -> Viewport -> Axes3D -> [Primitive]
renderAxes3DWithLabels f (xName, yName, zName) cam proj vp ax =
  let project = project3D cam proj vp . f
      toScreen p = let Projected sx sy _ = project p in Point sx sy

      xLo = axesXMin ax; xHi = axesXMax ax
      yLo = axesYMin ax; yHi = axesYMax ax
      zLo = axesZMin ax; zHi = axesZMax ax
      corners =
        [ Point3 xLo yLo zLo  -- 0
        , Point3 xHi yLo zLo  -- 1
        , Point3 xHi yHi zLo  -- 2
        , Point3 xLo yHi zLo  -- 3
        , Point3 xLo yLo zHi  -- 4
        , Point3 xHi yLo zHi  -- 5
        , Point3 xHi yHi zHi  -- 6
        , Point3 xLo yHi zHi  -- 7
        ]
      pts2d = map toScreen corners

      edges_ =
        [ (0,1), (1,2), (2,3), (3,0)
        , (4,5), (5,6), (6,7), (7,4)
        , (0,4), (1,5), (2,6), (3,7)
        ]

      cubeStyle = LineStyle "#bbbbbb" 1.0 []
      cubeLines =
        [ PLine (pts2d !! i) (pts2d !! j) cubeStyle
        | (i, j) <- edges_ ]

      xTicks = axisTicks3D (axesXLog ax) (axesNTicks ax) xLo xHi
      yTicks = axisTicks3D (axesYLog ax) (axesNTicks ax) yLo yHi
      zTicks = axisTicks3D (axesZLog ax) (axesNTicks ax) zLo zHi

      tickStyle = LineStyle "#888888" 1.0 []
      -- ★軸ごとのレンジに比例した offset (正規化 pipeline 対応)。 旧実装は
      -- 全軸共通 @0.04 * max(range)@ をデータ空間で使っていたため、 レンジが
      -- 軸間で大きく異なるデータ (例: z=収率 40・x/y=±1.4) で z レンジに
      -- 引っ張られ、 正規化後に tick/軸名が cube から大きく外れていた。
      -- 各 offset を「その方向の軸レンジ」 に比例させると、 正規化後 (各軸
      -- [-1,1]) で一律 0.08 になる。 均一 cube ([-1,1]^3) では旧実装と同値。
      offX = 0.04 * (xHi - xLo)   -- x 方向の offset 量
      offY = 0.04 * (yHi - yLo)   -- y 方向の offset 量

      xMid = (xLo + xHi) / 2; yMid = (yLo + yHi) / 2; zMid = (zLo + zHi) / 2

      -- ★camera-aware なエッジ選択 (Phase 24 A9)。 旧実装は tick/軸名を固定
      -- エッジ (x→yLo・y→xHi・z→(xHi,yLo)) に置いていたが、 視点を回すと
      -- 裏エッジに乗ったり、 z tick が最近接の鉛直エッジ (= 図の中央を縦断) に
      -- 重なって読めなかった。 投影スクリーン座標から「前面下エッジ」 と
      -- 「左 silhouette 鉛直エッジ」 を選び、 回転に追従させる。
      screenXY cx cy cz = let Point sx sy = toScreen (Point3 cx cy cz) in (sx, sy)
      midScrY (a1,b1) (a2,b2) = let (_,y1) = screenXY a1 b1 zLo
                                    (_,y2) = screenXY a2 b2 zLo
                                in (y1 + y2) / 2
      -- x tick を載せる y (= yLo/yHi のうち画面で前 = 下にくる方)
      xTickY = if midScrY (xLo,yLo) (xHi,yLo) >= midScrY (xLo,yHi) (xHi,yHi)
                 then yLo else yHi
      inYx   = if xTickY == yLo then offY else negate offY   -- cube 内向き
      -- y tick を載せる x (= xLo/xHi のうち前)
      yTickX = if midScrY (xLo,yLo) (xLo,yHi) >= midScrY (xHi,yLo) (xHi,yHi)
                 then xLo else xHi
      inXy   = if yTickX == xLo then offX else negate offX
      -- z tick/軸名を載せる鉛直エッジ = 画面で最も左の底コーナー (silhouette・
      -- 右の colorbar と干渉しない)
      (zcx, zcy) = minimumBy (comparing (\(cx,cy) -> fst (screenXY cx cy zLo)))
                     [(xLo,yLo), (xHi,yLo), (xHi,yHi), (xLo,yHi)]
      inXz = if zcx < xMid then offX else negate offX        -- cube 内向き
      inYz = if zcy < yMid then offY else negate offY

      ts = TextStyle "#444444" 9 "sans-serif" AnchorMiddle 0 "normal" False

      xTickPrims = concat
        [ let p1 = toScreen (Point3 xi xTickY zLo)
              p2 = toScreen (Point3 xi (xTickY + inYx) zLo)
              Point lx ly = toScreen (Point3 xi (xTickY - inYx) zLo)
          in [ PLine p1 p2 tickStyle
             , PText (Point lx (ly + 10)) (formatNum xi) ts ]
        | xi <- xTicks ]

      yTickPrims = concat
        [ let p1 = toScreen (Point3 yTickX yi zLo)
              p2 = toScreen (Point3 (yTickX + inXy) yi zLo)
              Point lx ly = toScreen (Point3 (yTickX - inXy) yi zLo)
          in [ PLine p1 p2 tickStyle
             , PText (Point lx (ly + 4)) (formatNum yi) ts ]
        | yi <- yTicks ]

      zTickPrims = concat
        [ let p1 = toScreen (Point3 zcx zcy zi)
              p2 = toScreen (Point3 (zcx + inXz) (zcy + inYz) zi)
              Point lx ly = toScreen (Point3 (zcx - inXz) (zcy - inYz) zi)
          in [ PLine p1 p2 tickStyle
             , PText (Point lx (ly + 4)) (formatNum zi) ts ]
        | zi <- zTicks ]

      tsAxis = TextStyle "#333333" 11 "sans-serif" AnchorMiddle 0 "bold" False
      axisNames =
        [ let Point lx ly = toScreen (Point3 xMid (xTickY - inYx * 3) zLo)
          in PText (Point lx (ly + 14)) xName tsAxis
        , let Point lx ly = toScreen (Point3 (yTickX - inXy * 3) yMid zLo)
          in PText (Point lx ly) yName tsAxis
        -- z 軸名も z tick と同じ左 silhouette 鉛直エッジ沿い (中央高さ・外側)。
        , let Point lx ly = toScreen (Point3 (zcx - inXz * 2) (zcy - inYz * 2) zMid)
          in PText (Point lx ly) zName tsAxis
        ]

  in cubeLines <> xTickPrims <> yTickPrims <> zTickPrims <> axisNames

-- ===========================================================================
-- 壁面 pane + gridline (Phase 25 A6 = G5)
-- ===========================================================================

-- | Phase 25 A6: 壁面 pane (= mplot3d の背面 3 壁) のスタイル。
-- pane = 薄灰の塗り面、 gridline = 壁面に引く tick 格子線 (mplot3d 既定は白)。
data PaneStyle3D = PaneStyle3D
  { paneFill    :: !Text    -- ^ 壁面塗り色
  , paneOpacity :: !Double  -- ^ 塗り不透明度 (0..1)
  , paneGrid    :: !Text    -- ^ 格子線色
  } deriving (Show, Eq, Generic)
instance ToJSON   PaneStyle3D
instance FromJSON PaneStyle3D

-- | mplot3d 風 default (薄灰 pane + 白格子線)。
defaultPaneStyle3D :: PaneStyle3D
defaultPaneStyle3D = PaneStyle3D "#eaeaea" 1.0 "#ffffff"

-- | Phase 25 A6: 3 つの「奥壁」 を薄灰 pane で塗り、 各壁に tick 格子線を引く
-- (mplot3d 標準の axes pane)。 出力は @pane 塗り → gridline@ の順なので、
-- 'renderAxes3DWithLabels' (cube wireframe + tick) の **前** に置けば最背面に
-- なる (= データ・wireframe が pane の手前に来る)。
--
-- 奥壁判定: 各軸の対向 2 面のうち、 面中心の投影 depth ('projDepth'、 +1 が奥)
-- が大きい方を奥壁に採る。 視点回転に追従する。 @f@ は 'renderAxes3DWith' と
-- 同じ正規化変換 (データ bbox → [-1,1]^3)。
renderAxes3DPanes :: (Point3 -> Point3) -> PaneStyle3D
                  -> Camera3D -> Projection3D -> Viewport -> Axes3D -> [Primitive]
renderAxes3DPanes f sty cam proj vp ax =
  let project    = project3D cam proj vp . f
      toScreen p = let Projected sx sy _ = project p in Point sx sy
      depthAt p  = let Projected _ _ sz = project p in sz

      xLo = axesXMin ax; xHi = axesXMax ax
      yLo = axesYMin ax; yHi = axesYMax ax
      zLo = axesZMin ax; zHi = axesZMax ax
      xMid = (xLo + xHi) / 2; yMid = (yLo + yHi) / 2; zMid = (zLo + zHi) / 2

      -- 各軸の奥壁座標 (= 面中心 depth が大きい = 奥の面)
      xWall = if depthAt (Point3 xLo yMid zMid) >= depthAt (Point3 xHi yMid zMid)
                then xLo else xHi
      yWall = if depthAt (Point3 xMid yLo zMid) >= depthAt (Point3 xMid yHi zMid)
                then yLo else yHi
      zWall = if depthAt (Point3 xMid yMid zLo) >= depthAt (Point3 xMid yMid zHi)
                then zLo else zHi

      xTicks = axisTicks3D (axesXLog ax) (axesNTicks ax) xLo xHi
      yTicks = axisTicks3D (axesYLog ax) (axesNTicks ax) yLo yHi
      zTicks = axisTicks3D (axesZLog ax) (axesNTicks ax) zLo zHi

      fillSty = FillStyle (paneFill sty) (paneOpacity sty)
      gridSty = LineStyle (paneGrid sty) 1.0 []

      quad a b c d =
        PPath [ MoveTo (toScreen a), LineTo (toScreen b)
              , LineTo (toScreen c), LineTo (toScreen d), ClosePath ]
              fillSty Nothing
      gline a b = PLine (toScreen a) (toScreen b) gridSty

      -- x 奥壁 (x=xWall の y×z 面)
      xPane = quad (Point3 xWall yLo zLo) (Point3 xWall yHi zLo)
                   (Point3 xWall yHi zHi) (Point3 xWall yLo zHi)
      xGrid = [ gline (Point3 xWall yi zLo) (Point3 xWall yi zHi) | yi <- yTicks ]
           <> [ gline (Point3 xWall yLo zi) (Point3 xWall yHi zi) | zi <- zTicks ]
      -- y 奥壁 (y=yWall の x×z 面)
      yPane = quad (Point3 xLo yWall zLo) (Point3 xHi yWall zLo)
                   (Point3 xHi yWall zHi) (Point3 xLo yWall zHi)
      yGrid = [ gline (Point3 xi yWall zLo) (Point3 xi yWall zHi) | xi <- xTicks ]
           <> [ gline (Point3 xLo yWall zi) (Point3 xHi yWall zi) | zi <- zTicks ]
      -- z 奥壁 (z=zWall の x×y 面 = 床 or 天井)
      zPane = quad (Point3 xLo yLo zWall) (Point3 xHi yLo zWall)
                   (Point3 xHi yHi zWall) (Point3 xLo yHi zWall)
      zGrid = [ gline (Point3 xi yLo zWall) (Point3 xi yHi zWall) | xi <- xTicks ]
           <> [ gline (Point3 xLo yi zWall) (Point3 xHi yi zWall) | yi <- yTicks ]

  in [xPane, yPane, zPane] <> xGrid <> yGrid <> zGrid

-- | 数値を短く整形 (= 小数 1 桁、 末尾 0 と . を除去)。 ★末尾 0 除去は **小数点が
-- ある場合のみ** (整数の末尾 0 を削ると 10→1・100→1 になるバグを Phase 25 A8 で
-- 修正。 log tick の 10/100/1000 で顕在化)。
formatNum :: Double -> Text
formatNum x =
  let s = T.pack (show (fromIntegral (round (x * 10) :: Int) / 10.0 :: Double))
  in if T.any (== '.') s
       then T.dropWhileEnd (== '.') (T.dropWhileEnd (== '0') s)  -- "10.0"→"10."→"10"
       else s

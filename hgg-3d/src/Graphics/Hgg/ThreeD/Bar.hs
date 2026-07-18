-- |
-- Module      : Graphics.Hgg.ThreeD.Bar
-- Description : 3D bar (直方体 / stick) + 誤差棒 (Phase 25 A5)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DoE / 実験データ定番の 3D 棒グラフ。 各 (x, y) 位置に底面 (base z) から高さ
-- (top z) までの棒を立てる。 2 スタイル:
--
--   * 'BarCuboid' — 直方体 (top + 4 側面の 5 quad)。 surface 同様 painter's の
--     depth 統合 ('barFacesDepth') に乗せ、 scatter/surface と層横断ソートされる。
--   * 'BarStick'  — 太い縦線 1 本 (面なし・軽量)。 line/wireframe 同様 depth 統合
--     対象外で前面に描く ('renderBarSticks')。
--
-- 誤差棒 ('renderErrorBars3D') は棒/点の頂点に z 方向 ±err の縦線 + 端キャップ。
-- bar・scatter どちらの layer にも付けられる (層の per-point err 列で駆動)。
--
-- ⚠ 入力 ('Bar3D' の点・base・half-width・err) はすべて**正規化済**座標
-- ([-1,1]^3・z-aspect 適用後) を渡す前提。 正規化は 'Easy.normLayer3D' /
-- 'scaleZLayer' が担い、 本 module は投影 + 幾何のみ。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.ThreeD.Bar
  ( BarStyle3D (..)
  , Bar3D (..)
  , defaultBar3D
  , barFacesDepth
  , renderBarSticks
  , renderStems3D
  , renderErrorBars3D
  ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)

import           Graphics.Hgg.Render             (FillStyle (..),
                                                  PathSegment (..),
                                                  Point (..), Primitive (..),
                                                  StrokeStyle (..), solid)
import           Graphics.Hgg.ThreeD.Projection  (Projected (..), Viewport,
                                                  project3D)
import           Graphics.Hgg.ThreeD.Types

-- | 棒のスタイル。
data BarStyle3D
  = BarCuboid   -- ^ 直方体 (top + 4 側面・depth 統合)
  | BarStick    -- ^ 太い縦線 (面なし・前面描画)
  deriving (Show, Eq, Generic)
instance ToJSON   BarStyle3D
instance FromJSON BarStyle3D

-- | 3D bar 1 series 分の設定 (= 全点共通スタイル・点ごとに棒を立てる)。
data Bar3D = Bar3D
  { br3Tops   :: ![Point3]    -- ^ 各棒の頂点 (x, y, 高さ z)・正規化済
  , br3BaseZ  :: !Double      -- ^ 底面 z (正規化済・通常 data z=0 の正規化値)
  , br3HalfW  :: !Double      -- ^ footprint 半幅 (正規化空間・x/y 共通)
  , br3Style  :: !BarStyle3D
  , br3Color  :: !Text
  , br3Alpha  :: !Double      -- ^ 面/線の不透明度 (0..1)
  , br3Width  :: !Double      -- ^ stick 線幅 px ('BarStick' 時)
  } deriving (Show, Eq, Generic)
instance ToJSON   Bar3D
instance FromJSON Bar3D

-- | default: 青、 base 0、 半幅 0.04、 直方体、 alpha 1、 stick 幅 6。
defaultBar3D :: [Point3] -> Bar3D
defaultBar3D tops = Bar3D
  { br3Tops  = tops
  , br3BaseZ = 0
  , br3HalfW = 0.04
  , br3Style = BarCuboid
  , br3Color = "#5b9bd5"
  , br3Alpha = 1.0
  , br3Width = 6
  }

-- ===========================================================================
-- 直方体 face の depth 列 (= surfaceFacesDepth と同型・大域ソートに混ぜる)
-- ===========================================================================

-- | 各棒を直方体の 5 quad (top + 4 側面・底面は隠れるので省く) に展開し、
-- @(投影 depth, PPath)@ で**未ソート**返す。 'BarStick' 時は @[]@ (= 'renderBarSticks'
-- が描く)。 簡易 Lambert shading (surface と同じ light) で立体感を付ける。
barFacesDepth
  :: Camera3D -> Projection3D -> Viewport
  -> Bar3D -> [(Double, Primitive)]
barFacesDepth cam proj vp br
  | br3Style br /= BarCuboid = []
  | otherwise = concatMap barQuads (br3Tops br)
  where
    project = project3D cam proj vp
    w  = br3HalfW br
    zb = br3BaseZ br
    lightDir = normalizeV3 (Vec3 0.5 0.5 (-1.0))
    barQuads (Point3 cx cy zt) =
      let -- 8 corners (b* = 底面, t* = 上面)
          b0 = Point3 (cx - w) (cy - w) zb
          b1 = Point3 (cx + w) (cy - w) zb
          b2 = Point3 (cx + w) (cy + w) zb
          b3 = Point3 (cx - w) (cy + w) zb
          t0 = Point3 (cx - w) (cy - w) zt
          t1 = Point3 (cx + w) (cy - w) zt
          t2 = Point3 (cx + w) (cy + w) zt
          t3 = Point3 (cx - w) (cy + w) zt
          faces = [ (t0, t1, t2, t3)   -- top
                  , (b0, b1, t1, t0)   -- front (y-)
                  , (b1, b2, t2, t1)   -- right (x+)
                  , (b2, b3, t3, t2)   -- back  (y+)
                  , (b3, b0, t0, t3) ] -- left  (x-)
      in map quadPrim faces
    quadPrim (a, b, c, d) =
      let pa = project a; pb = project b; pc = project c; pd = project d
          depth = (projDepth pa + projDepth pb + projDepth pc + projDepth pd) / 4
          n = normalizeV3 (crossV3 (sub b a) (sub c a))
          ndotl = max 0 (dotV3 n lightDir)
          intensity = 0.55 + 0.45 * ndotl
          fill = mulColor (br3Color br) intensity
          path = [ MoveTo (toPt pa), LineTo (toPt pb)
                 , LineTo (toPt pc), LineTo (toPt pd), ClosePath ]
      in (depth, PPath path (FillStyle fill (br3Alpha br))
                            (Just (StrokeStyle "#33333355" 0.3)))
    toPt (Projected x y _) = Point x y
    sub (Point3 bx by bz) (Point3 ax ay az) = Vec3 (bx - ax) (by - ay) (bz - az)

-- ===========================================================================
-- stick スタイル (太い縦線・前面描画)
-- ===========================================================================

-- | 'BarStick' 時、 各棒を底面→頂点の太い縦線で描く。 'BarCuboid' 時は @[]@。
renderBarSticks
  :: Camera3D -> Projection3D -> Viewport
  -> Bar3D -> [Primitive]
renderBarSticks cam proj vp br
  | br3Style br /= BarStick = []
  | otherwise = map stick (br3Tops br)
  where
    project = project3D cam proj vp
    zb = br3BaseZ br
    stick (Point3 cx cy zt) =
      let pb = project (Point3 cx cy zb)
          pt = project (Point3 cx cy zt)
      in PLine (Point (projX pb) (projY pb)) (Point (projX pt) (projY pt))
               (solid (br3Color br) (br3Width br))

-- ===========================================================================
-- stem (Phase 26 A4・3D lollipop = 細い垂線 + 先端マーカー・前面描画)
-- ===========================================================================

-- | Phase 26 A4: 各点を底面 ('br3BaseZ') → 先端の細い縦線 + 先端の円マーカーで
-- 描く (3D lollipop)。 'renderBarSticks' (太線・マーカー無し) と違い stem は
-- 細線 + マーカー。 線色/幅/alpha は 'Bar3D' から、 マーカー半径は引数 @markerR@
-- (px)。 depth 統合外の前面 overlay。 先端の depth cue で僅かに半径を変える
-- (scatter と同じ・近側が大きい)。
renderStems3D
  :: Camera3D -> Projection3D -> Viewport
  -> Bar3D -> Double          -- ^ マーカー基本半径 px
  -> [Primitive]
renderStems3D cam proj vp br markerR =
  concatMap stem (br3Tops br)
  where
    project = project3D cam proj vp
    zb = br3BaseZ br
    stroke = Just (StrokeStyle "#333333" 0.5)
    stem (Point3 cx cy zt) =
      let pb = project (Point3 cx cy zb)
          pt = project (Point3 cx cy zt)
          line = PLine (Point (projX pb) (projY pb)) (Point (projX pt) (projY pt))
                       (solid (br3Color br) (br3Width br))
          depthCue = max 0 (min 1 ((1 - projDepth pt) / 2))
          r = markerR * (0.55 + 0.45 * depthCue)
          marker = PCircle (Point (projX pt) (projY pt)) r
                           (FillStyle (br3Color br) (br3Alpha br))
                           stroke
                           Nothing
      in [line, marker]

-- ===========================================================================
-- 誤差棒 (z 方向 ±err・縦線 + 端キャップ)
-- ===========================================================================

-- | 各頂点 @(x,y,z)@ に z 方向 ±err の縦線 + 上下端の水平キャップ (画面空間の
-- 短い横線) を描く。 err は正規化済 z 量 (= 'normLayer3D' で換算済)。 bar/scatter
-- どちらの頂点列にも使える。 err <= 0 の点はスキップ。 線色 'col'・幅 'lw'。
renderErrorBars3D
  :: Camera3D -> Projection3D -> Viewport
  -> Text -> Double          -- ^ 色・線幅 px
  -> [(Point3, Double)]      -- ^ (頂点, 正規化 err)
  -> [Primitive]
renderErrorBars3D cam proj vp col lw pes =
  concatMap whisker pes
  where
    project = project3D cam proj vp
    capW = 3.5 :: Double      -- キャップ半幅 px
    whisker (Point3 x y z, e)
      | e <= 0    = []
      | otherwise =
          let pHi = project (Point3 x y (z + e))
              pLo = project (Point3 x y (z - e))
              (hx, hy) = (projX pHi, projY pHi)
              (lx, ly) = (projX pLo, projY pLo)
          in [ PLine (Point lx ly) (Point hx hy) (solid col lw)
             , PLine (Point (hx - capW) hy) (Point (hx + capW) hy) (solid col lw)
             , PLine (Point (lx - capW) ly) (Point (lx + capW) ly) (solid col lw) ]

-- ===========================================================================
-- 簡易色操作 (= Surface.hs と同じ hex #RRGGBB を intensity 倍)
-- ===========================================================================

mulColor :: Text -> Double -> Text
mulColor hex intensity =
  let s = T.unpack hex
      body = case s of ('#':rest) -> rest; other -> other
      r = parseHex2 (take 2 body)
      g = parseHex2 (take 2 (drop 2 body))
      b = parseHex2 (take 2 (drop 4 body))
      clampMul i = max 0 (min 255 (round (fromIntegral i * intensity :: Double) :: Int))
      toHex2 i = pad2 (decToHex (clampMul i))
  in T.pack ("#" <> toHex2 r <> toHex2 g <> toHex2 b)

parseHex2 :: String -> Int
parseHex2 [c1, c2] = hexVal c1 * 16 + hexVal c2
parseHex2 _        = 0

hexVal :: Char -> Int
hexVal c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise            = 0

decToHex :: Int -> String
decToHex n
  | n < 16    = [hexChar n]
  | otherwise = decToHex (n `div` 16) <> [hexChar (n `mod` 16)]

hexChar :: Int -> Char
hexChar v
  | v < 10    = toEnum (fromEnum '0' + v)
  | otherwise = toEnum (fromEnum 'a' + v - 10)

pad2 :: String -> String
pad2 s
  | length s < 2 = '0' : s
  | otherwise    = s

-- |
-- Module      : Hgg.Plot.ThreeD.Surface
-- Description : 3D surface plot (grid mesh + painter's algorithm) (Phase 3 A7)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 規則 grid (= 2D 配列の z 値) から triangle mesh を作り、 painter's algorithm
-- で奥から塗り潰す。 optional Lambert shading (= flat、 1 directional light)。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.ThreeD.Surface
  ( Surface3D (..)
  , defaultSurface3D
  , surfaceFromFunction
  , renderSurface3D
  , surfaceFacesDepth
  , trianglesFacesDepth
  ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.List                       (sortOn)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)

import           Hgg.Plot.Render             (FillStyle (..),
                                                  LineStyle (..),
                                                  PathSegment (..),
                                                  Point (..), Primitive (..),
                                                  StrokeStyle (..))
import           Hgg.Plot.Render.Common      (continuousColor)
import           Hgg.Plot.ThreeD.Projection  (Projected (..), Viewport,
                                                  project3D)
import           Hgg.Plot.ThreeD.Types

data Surface3D = Surface3D
  { sf3Grid       :: ![[Double]]
  , sf3XRange     :: !(Double, Double)
  , sf3YRange     :: !(Double, Double)
  , sf3Color      :: !Text
  , sf3EdgeColor  :: !Text
  , sf3Shaded     :: !Bool
  , sf3Colormap   :: !(Maybe [Text])
    -- ^ Phase 24 A2: @Just stops@ で面色を z 値の連続色 (gradient stops の
    --   線形補間・2D 'ColorByContinuous' と同じ 'continuousColor') にする。
    --   @Nothing@ = 従来の単色 'sf3Color' (後方互換)。
  , sf3Alpha      :: !Double
    -- ^ Phase 25 A4: 面の不透明度 (0..1・既定 1)。 @< 1@ で半透明 surface。
    --   face は depth ソート済 (painter's・A8 で scatter と大域マージ) なので
    --   back-to-front 合成が正しく重なる。
  , sf3Wire       :: !Bool
    -- ^ surfaceWire: 面を塗らず grid の行/列を**格子線メッシュ**で描く
    --   (matplotlib @plot_wireframe@ 相当)。 True で fill 三角形の代わりに
    --   隣接 grid 点を結ぶ line segment を depth 統合付きで返す。 colormap/shaded は無効。
  } deriving (Show, Eq, Generic)
instance ToJSON   Surface3D
instance FromJSON Surface3D

defaultSurface3D :: [[Double]] -> Surface3D
defaultSurface3D grid = Surface3D
  { sf3Grid      = grid
  , sf3XRange    = (-1, 1)
  , sf3YRange    = (-1, 1)
  , sf3Color     = "#5b9bd5"
  , sf3EdgeColor = "#3a73a6"
  , sf3Shaded    = True
  , sf3Colormap  = Nothing
  , sf3Alpha     = 1.0
  , sf3Wire      = False
  }

surfaceFromFunction
  :: Int -> Int
  -> Double -> Double
  -> Double -> Double
  -> (Double -> Double -> Double)
  -> Surface3D
surfaceFromFunction nx ny xMin xMax yMin yMax f =
  let xs = [ xMin + (xMax - xMin) * fromIntegral i / fromIntegral (nx - 1)
           | i <- [0 .. nx - 1] ]
      ys = [ yMin + (yMax - yMin) * fromIntegral j / fromIntegral (ny - 1)
           | j <- [0 .. ny - 1] ]
      grid = [ [ f x y | x <- xs ] | y <- ys ]
  in (defaultSurface3D grid)
       { sf3XRange = (xMin, xMax)
       , sf3YRange = (yMin, yMax)
       }

renderSurface3D
  :: Camera3D -> Projection3D -> Viewport
  -> Surface3D -> [Primitive]
renderSurface3D cam proj vp sf =
  map snd (sortOn (negate . fst) (surfaceFacesDepth cam proj vp sf))

-- | Phase 24 A8 (depth 統合): 各 face を @(投影 depth, poly Primitive)@ として
-- **未ソート**で返す (= 'renderSurface3D' は @sortOn (negate.fst)@ してから
-- @map snd@・ビット同一)。 scatter 点との層横断 depth 統合に使う。
surfaceFacesDepth
  :: Camera3D -> Projection3D -> Viewport
  -> Surface3D -> [(Double, Primitive)]
surfaceFacesDepth cam proj vp sf =
  let project = project3D cam proj vp
      grid = sf3Grid sf
      nRows = length grid
      nCols = if null grid then 0 else length (head grid)
      (xMin, xMax) = sf3XRange sf
      (yMin, yMax) = sf3YRange sf
      xAt c | nCols <= 1 = xMin
            | otherwise  = xMin + (xMax - xMin) * fromIntegral c
                                                    / fromIntegral (nCols - 1)
      yAt r | nRows <= 1 = yMin
            | otherwise  = yMin + (yMax - yMin) * fromIntegral r
                                                    / fromIntegral (nRows - 1)
      zAt r c = (grid !! r) !! c
      pt r c = Point3 (xAt c) (yAt r) (zAt r c)

      cells = [ (r, c) | r <- [0 .. nRows - 2], c <- [0 .. nCols - 2] ]
      mkTriangles (r, c) =
        let p00 = pt r       c
            p01 = pt r       (c + 1)
            p10 = pt (r + 1) c
            p11 = pt (r + 1) (c + 1)
        in [ (p00, p01, p11), (p00, p11, p10) ]
      triangles = concatMap mkTriangles cells

      lightDir = normalizeV3 (Vec3 0.5 0.5 (-1.0))
      -- Phase 24 A2: colormap 用の z 正規化 (grid 全体の z range)
      allZ = concat grid
      zMin = if null allZ then 0 else minimum allZ
      zMax = if null allZ then 1 else maximum allZ
      zT z | zMax <= zMin = 0.5
           | otherwise    = (z - zMin) / (zMax - zMin)
      faceData (a, b, c) =
        let pa = project a
            pb = project b
            pc = project c
            d  = (projDepth pa + projDepth pb + projDepth pc) / 3
            n  = normalizeV3 (crossV3 (subAsVec b a) (subAsVec c a))
            ndotl = max 0 (dotV3 n lightDir)
            -- colormap 時は shading を弱める (0.75-1.0): 低 z 側の dark stop が
            -- 黒潰れせず、 色 = z の読みが主・陰影 = 立体感の補助に留める
            intensity
              | not (sf3Shaded sf)           = 1.0
              | Just _ <- sf3Colormap sf     = 0.75 + 0.25 * ndotl
              | otherwise                    = 0.4 + 0.6 * ndotl
            -- 面の代表 z (3 頂点平均)。 colormap の色決め用
            zAvg = let Point3 _ _ za = a; Point3 _ _ zb = b; Point3 _ _ zc = c
                   in (za + zb + zc) / 3
        in (d, intensity, zAvg, pa, pb, pc)
      facesData = map faceData triangles

      baseColorAt zAvg = case sf3Colormap sf of
        Just stops -> continuousColor stops (zT zAvg)
        Nothing    -> sf3Color sf
      edgeStroke = Just (StrokeStyle (sf3EdgeColor sf) 0.3)
      mkPoly (_, intensity, zAvg, Projected x1 y1 _, Projected x2 y2 _, Projected x3 y3 _) =
        let base = baseColorAt zAvg
            fill = if sf3Shaded sf
                     then mulColor base intensity
                     else base
        in PPath
             [ MoveTo (Point x1 y1)
             , LineTo (Point x2 y2)
             , LineTo (Point x3 y3)
             , ClosePath
             ]
             (FillStyle fill (sf3Alpha sf))
             edgeStroke
      -- surfaceWire: 面を塗らず grid の行/列を線メッシュで描く (plot_wireframe 相当)
      wireStyle = LineStyle (sf3Color sf) 1.0 []
      gridEdges = [ (pt r c, pt r (c + 1)) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 2] ]
               ++ [ (pt r c, pt (r + 1) c) | r <- [0 .. nRows - 2], c <- [0 .. nCols - 1] ]
      mkSeg (a, b) = case (project a, project b) of
        (Projected x1 y1 d1, Projected x2 y2 d2) ->
          ((d1 + d2) / 2, PLine (Point x1 y1) (Point x2 y2) wireStyle)
  in if sf3Wire sf
       then map mkSeg gridEdges
       else [ (d, mkPoly fd) | fd@(d, _, _, _, _, _) <- facesData ]
  where
    subAsVec (Point3 bx by bz) (Point3 ax ay az) =
      Vec3 (bx - ax) (by - ay) (bz - az)

-- | Phase 26 A5 (trisurf): grid 非依存に **任意の三角形 face 列**を depth +
-- Lambert shading + (任意) colormap で @(投影 depth, PPath)@ 化する
-- ('surfaceFacesDepth' の grid 専用ロジックを triangle 列入力へ一般化した版)。
-- 頂点は正規化済前提。 colormap の z range は全 face 頂点 z から算出。
trianglesFacesDepth
  :: Camera3D -> Projection3D -> Viewport
  -> Text          -- ^ 基本色 (colormap 無しの単色)
  -> Text          -- ^ edge 色
  -> Bool          -- ^ shaded (Lambert)
  -> Maybe [Text]  -- ^ colormap stops (Just で z 連続色)
  -> Double        -- ^ alpha
  -> [(Point3, Point3, Point3)]  -- ^ 三角形 (正規化済)
  -> [(Double, Primitive)]
trianglesFacesDepth cam proj vp baseCol edgeCol shaded cmap alpha triangles =
  let project = project3D cam proj vp
      lightDir = normalizeV3 (Vec3 0.5 0.5 (-1.0))
      allZ = concat [ [za, zb, zc]
                    | (Point3 _ _ za, Point3 _ _ zb, Point3 _ _ zc) <- triangles ]
      zMin = if null allZ then 0 else minimum allZ
      zMax = if null allZ then 1 else maximum allZ
      zT z | zMax <= zMin = 0.5
           | otherwise    = (z - zMin) / (zMax - zMin)
      subAsVec (Point3 bx by bz) (Point3 ax ay az) =
        Vec3 (bx - ax) (by - ay) (bz - az)
      edgeStroke = Just (StrokeStyle edgeCol 0.3)
      baseColorAt zAvg = case cmap of
        Just stops -> continuousColor stops (zT zAvg)
        Nothing    -> baseCol
      faceData (a, b, c) =
        let pa = project a; pb = project b; pc = project c
            dd = (projDepth pa + projDepth pb + projDepth pc) / 3
            nrm = normalizeV3 (crossV3 (subAsVec b a) (subAsVec c a))
            ndotl = max 0 (dotV3 nrm lightDir)
            intensity
              | not shaded         = 1.0
              | Just _ <- cmap     = 0.75 + 0.25 * ndotl
              | otherwise          = 0.4 + 0.6 * ndotl
            zAvg = let Point3 _ _ za = a; Point3 _ _ zb = b; Point3 _ _ zc = c
                   in (za + zb + zc) / 3
        in (dd, intensity, zAvg, pa, pb, pc)
      mkPoly (_, intensity, zAvg, Projected x1 y1 _, Projected x2 y2 _, Projected x3 y3 _) =
        let base = baseColorAt zAvg
            fill = if shaded then mulColor base intensity else base
        in PPath [ MoveTo (Point x1 y1), LineTo (Point x2 y2)
                 , LineTo (Point x3 y3), ClosePath ]
                 (FillStyle fill alpha) edgeStroke
  in [ (d, mkPoly fd) | fd@(d, _, _, _, _, _) <- map faceData triangles ]

-- ===========================================================================
-- 簡易色操作 (= hex #RRGGBB を intensity 倍)
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

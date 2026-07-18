-- | Phase 5 A8 showBrowser demo。
--
-- @
-- cabal run browser-3d-demo
-- @
--
-- → tmp HTML 生成 → xdg-open でブラウザ起動 → WebGL 3D が interactive 表示。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.ThreeD          (defaultCameraZUp, defaultPerspective,
                                                Point3 (..))
import           Graphics.Hgg.ThreeD.Axes     (defaultAxes3D)
import           Graphics.Hgg.ThreeD.Browser  (showBrowser)
import           Graphics.Hgg.ThreeD.Spec
import           Graphics.Hgg.Color           (fromHex)

main :: IO ()
main = showBrowser $
     purePlot3D
  <> layer3D (scatter3DPoints helix     <> color3D (fromHex "#56B4E9") <> size3D 6  <> alpha3D 0.9)
  <> layer3D (line3DPoints helixLine <> color3D (fromHex "#009E73") <> width3D 1.5)
  <> camera     (defaultCameraZUp 3)
  <> projection defaultPerspective
  <> axes3D     defaultAxes3D
  <> title3D    "Phase 5 browser display demo"
  where
    -- 200 点 helix scatter
    helix =
      [ Point3 (cos (t * 4 * pi) * 0.8)
               (sin (t * 4 * pi) * 0.8)
               (t * 2 - 1)
      | i <- [0 .. 199 :: Int]
      , let t = fromIntegral i / 199.0
      ]
    -- 100 点 helix line (= 別の螺旋)
    helixLine =
      [ Point3 (cos (t * 6 * pi) * 0.5)
               (sin (t * 6 * pi) * 0.5)
               (t - 0.5)
      | i <- [0 .. 99 :: Int]
      , let t = fromIntegral i / 99.0
      ]

-- | 各 3D 型の JSON 出力を確認する (= Phase 5 A4 PS Argonaut codec 作成前の wire format 確定用)。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy.Char8  as BL
import           Hgg.Plot.ThreeD.Axes
import           Hgg.Plot.ThreeD.Line
import           Hgg.Plot.ThreeD.Scatter
import           Hgg.Plot.ThreeD.Spec
import           Hgg.Plot.Color          (fromHex)
import           Hgg.Plot.ThreeD.Surface
import           Hgg.Plot.ThreeD.Types

main :: IO ()
main = do
  putStrLn "-- Point3 --"
  BL.putStrLn $ encode (Point3 1 2 3)
  putStrLn "-- Vec3 --"
  BL.putStrLn $ encode (Vec3 0 0 1)
  putStrLn "-- Mat4 --"
  BL.putStrLn $ encode (Mat4 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1)
  putStrLn "-- Camera3D --"
  BL.putStrLn $ encode (defaultCameraZUp 3)
  putStrLn "-- Projection3D (Perspective) --"
  BL.putStrLn $ encode defaultPerspective
  putStrLn "-- Projection3D (Orthographic) --"
  BL.putStrLn $ encode (Orthographic 1 1 0.1 100)
  putStrLn "-- Axes3D --"
  BL.putStrLn $ encode defaultAxes3D
  putStrLn "-- Scatter3D --"
  BL.putStrLn $ encode (defaultScatter3D [Point3 0 0 0, Point3 1 1 1])
  putStrLn "-- Line3D --"
  BL.putStrLn $ encode (defaultLine3D [Point3 0 0 0, Point3 1 1 1])
  putStrLn "-- Wireframe3D --"
  BL.putStrLn $ encode (defaultWireframe3D [Point3 0 0 0, Point3 1 0 0] [(0,1)])
  putStrLn "-- Surface3D --"
  BL.putStrLn $ encode (defaultSurface3D [[0,1],[1,0]])
  putStrLn "-- Mark3DKind --"
  BL.putStrLn $ encode M3Scatter
  putStrLn "-- Layer3D (scatter + color + size) --"
  BL.putStrLn $ encode (scatter3DPoints [Point3 0 0 0] <> color3D (fromHex "#56B4E9") <> size3D 6)
  putStrLn "-- VisualSpec3D --"
  BL.putStrLn $ encode $
       purePlot3D
    <> layer3D (scatter3DPoints [Point3 0 0 0, Point3 1 1 1] <> color3D (fromHex "#56B4E9") <> size3D 6)
    <> camera     (defaultCameraZUp 3)
    <> projection defaultPerspective
    <> axes3D     defaultAxes3D
    <> title3D    "demo"

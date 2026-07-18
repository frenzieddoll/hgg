-- | Tutorial 05 ─ テーマと配色。 同じ図を ThemeDefault / ThemeDark で出し分ける。
--   `theme` は VisualSpec を返すだけなので `<>` で足すだけ。
--
-- @
-- cabal run tutorial-05-theme
-- @
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Backend.SVG (saveSVG)
import           Graphics.Hgg.Unit         (px, (*~))
import           Graphics.Hgg.Spec
import           Data.Text                (Text)

main :: IO ()
main = do
  let xs = inline    (concat (replicate 3 ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double])))
      ys = inline    ([ 1, 2, 1.5, 3, 2.5,  2, 3, 2.5, 4, 3.5,  3, 4, 3.5, 5, 4.5 ] :: [Double])
      gs = inlineCat (concatMap (replicate 5) (["x", "y", "z"] :: [Text]))
      base nm = purePlot
             <> layer (scatter xs ys <> colorBy gs <> size 6)
             <> legend
             <> title nm
             <> xLabel "x" <> yLabel "y"
             <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  saveSVG "tutorial-05-light.svg" (base "ThemeDefault" <> theme ThemeDefault)
  saveSVG "tutorial-05-dark.svg"  (base "ThemeDark"    <> theme ThemeDark)
  putStrLn "wrote tutorial-05-light.svg / tutorial-05-dark.svg"

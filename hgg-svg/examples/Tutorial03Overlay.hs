-- | Tutorial 03 ─ 複数 layer の重畳。 散布点の上に折れ線を重ねる。
--   各 `layer` は `<>` で合成され、 後に書いた layer が上に描かれる。
--
-- @
-- cabal run tutorial-03-overlay
-- @
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hgg.Plot.Backend.SVG (saveSVG)
import           Hgg.Plot.Unit         (px, (*~))
import           Hgg.Plot.Spec

main :: IO ()
main = do
  let xs  = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
      ys  = [0.2, 1.1, 3.9, 9.2, 15.8, 25.1, 36.2, 48.9, 64.1, 80.8] :: [Double]
      fit = map (\x -> x * x) xs
  saveSVG "tutorial-03-overlay.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> alpha 0.85 <> size 5)
    <> layer (line    (inline xs) (inline fit) <> color (fromHex "#dc2626") <> stroke 2)
    <> title  "Overlay: observed points + fitted curve"
    <> xLabel "x" <> yLabel "y"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  putStrLn "wrote tutorial-03-overlay.svg"

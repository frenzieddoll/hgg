-- | Tutorial 02 ─ Grammar API (Layer 2、 ggplot 風)。 `inline`/`inlineCat` で
--   channel を作り、 `<>` で aesthetic を合成、 `scale_*` + `legend` で色分け。
--
-- @
-- cabal run tutorial-02-grammar
-- @
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Backend.SVG (saveSVG)
import           Graphics.Hgg.Unit         (px, (*~))
import           Graphics.Hgg.Spec
import           Data.Text                (Text)

main :: IO ()
main = do
  -- long-form データ: 各 row が (x, y, group)。 group で色分けする。
  let xs = inline    (concat (replicate 2 ([1.0, 2.0, 3.0, 4.0] :: [Double])))
      ys = inline    ([2.0, 3.0, 1.0, 4.0,  3.0, 1.0, 4.0, 2.0] :: [Double])
      gs = inlineCat (concatMap (replicate 4) (["alpha", "beta"] :: [Text]))
  -- ggplot で言えば:
  --   ggplot(d, aes(x, y, color=group)) + geom_point(size=6) +
  --     scale_color_manual(values=c(alpha="#1B9E77", beta="#D95F02"))
  saveSVG "tutorial-02-grammar.svg" $
       purePlot
    <> layer (scatter xs ys <> colorBy gs <> size 6)
    <> scaleColorManual [("alpha", "#1B9E77"), ("beta", "#D95F02")]
    <> legend
    <> title  "Grammar API: scale_color_manual"
    <> xLabel "x" <> yLabel "y"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  putStrLn "wrote tutorial-02-grammar.svg"

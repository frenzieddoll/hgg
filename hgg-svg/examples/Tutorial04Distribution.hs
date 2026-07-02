-- | Tutorial 04 ─ 分布の可視化。 群ごとの violin と box を並べる。
--   分布系 mark は (群ラベル channel, 値 channel) を取る。
--
-- @
-- cabal run tutorial-04-distribution
-- @
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hgg.Plot.Backend.SVG (saveSVG)
import           Hgg.Plot.Unit         (px, (*~))
import           Hgg.Plot.Spec
import           Data.Text                (Text)

main :: IO ()
main = do
  -- 各 row が (群, 値)。 ここでは 3 群 × 各数点の小さな例。
  let cats = inlineCat (concatMap (replicate 6)
                         (["control", "low", "high"] :: [Text]))
      vals = inline ([ 4.8, 5.1, 5.3, 4.9, 5.0, 5.2      -- control
                     , 6.0, 6.4, 5.8, 6.2, 6.1, 6.3      -- low
                     , 7.1, 7.5, 6.9, 7.3, 7.0, 7.4      -- high
                     ] :: [Double])
  saveSVG "tutorial-04-violin.svg" $
       purePlot
    <> layer (violin vals <> groupBy cats)
    <> title  "Distribution: violin by group"
    <> xLabel "dose" <> yLabel "response"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  saveSVG "tutorial-04-box.svg" $
       purePlot
    <> layer (boxplot vals <> groupBy cats)
    <> title  "Distribution: box by group"
    <> xLabel "dose" <> yLabel "response"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  putStrLn "wrote tutorial-04-violin.svg / tutorial-04-box.svg"

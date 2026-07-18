-- | 01-quickstart.md の図 (Easy / 文法レイヤ)。
{-# LANGUAGE OverloadedStrings #-}
module DocFig.Quickstart (figures) where

import           Data.Text     (Text)
import           DocFig.Common

figures :: [Figure]
figures =
  [ -- Lesson 1: Easy
    fig "lesson1-easy.svg" $
         overlay [ points [1,2,3,4,5] [1,4,9,16,25] ]
      <> title "Lesson 1: y = x²" <> xLabel "x" <> yLabel "y"

    -- Lesson 2: Grammar (色分け + scale_color_manual + legend)
  , fig "lesson2-grammar.svg" $
         purePlot
      <> layer (scatter xs ys <> colorBy gs <> size 7)
      <> scaleColorManual [("alpha","#1B9E77"), ("beta","#D95F02")]
      <> legend
      <> title "Lesson 2: scale_color_manual" <> xLabel "x" <> yLabel "y"
  ]
  where
    xs = inline    [1,2,3,4, 1,2,3,4]
    ys = inline    [2,3,1,4, 3,1,4,2]
    gs = inlineCat (concatMap (replicate 4) (["alpha","beta"] :: [Text]))

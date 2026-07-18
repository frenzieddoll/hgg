-- | Tutorial 01 ─ Easy API (Layer 1)。 入門者向け、 [Double] を直接渡して 1 枚出す。
--
-- @
-- cabal run tutorial-01-easy
-- @
--
-- → カレントディレクトリに @tutorial-01-easy.svg@ を生成。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Backend.SVG (saveSVG)
import           Graphics.Hgg.Unit         (px, (*~))
import           Graphics.Hgg.Easy

main :: IO ()
main = do
  let xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
      ys = map (\x -> x * x) xs
  -- Easy 層: `points` は値直接受け (= `scatter (inline xs) (inline ys)` の別名)、
  -- 重畳は `overlay` で包む (`scatter <> line` の落とし穴を回避)。
  saveSVG "tutorial-01-easy.svg" $
       overlay [ points xs ys ]
    <> title  "Easy API: y = x\xb2"
    <> xLabel "x" <> yLabel "y"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  putStrLn "wrote tutorial-01-easy.svg"

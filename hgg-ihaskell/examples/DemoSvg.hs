-- | Phase 12 A4: demo.ipynb の各セルが iHaskell 上で出力するのと同じ SVG を
-- ファイルに書き出す (= notebook の出力を環境非依存に確認するための具体物)。
--
-- @cabal run ihaskell-demo-svg@ → design/ihaskell/ に 3 枚の SVG を生成。
-- iHaskell セルでは @display@ がこの SVG を inline 表示する (= 同一描画経路)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.SVG (renderSVG, renderSVGWith)
import           Graphics.Hgg.Easy
import qualified Data.Text.IO             as TIO
import qualified Data.Vector              as V

outDir :: FilePath
outDir = "design/ihaskell/"

-- セル 1: inline 散布図 (Resolver 不要、 IHaskellDisplay VisualSpec が描く図)
cell1 :: VisualSpec
cell1 =
     layer (scatter (inline xs) (inline ys) <> size 5)
  <> title "Cell 1: inline scatter (y = x²)"
  <> xLabel "x" <> yLabel "y"
  where
    xs = [0, 1 .. 9] :: [Double]
    ys = map (\x -> x * x) xs

-- セル 2: Easy 層 + 折れ線の重畳 (overlay)
cell2 :: VisualSpec
cell2 =
     layer (points xs ys <> size 5)
  <> layer (lineXY xs fitY <> color (fromHex "#d62728") <> stroke 2)
  <> title "Cell 2: Easy points + line overlay"
  where
    xs   = [0, 1 .. 9] :: [Double]
    ys   = map (\x -> x * x) xs
    fitY = map (\x -> 8 * x - 12) xs

-- セル 3: ColByName 図 + Resolver (DisplayPlot が描く図)
cell3Spec :: VisualSpec
cell3Spec =
     layer (scatter (ColByName "x") (ColByName "y") <> size 5)
  <> title "Cell 3: ColByName + Resolver"
  <> xLabel "x" <> yLabel "y"

cell3Resolver :: Resolver
cell3Resolver "x" = Just (NumData (V.fromList [0, 1, 2, 3, 4]))
cell3Resolver "y" = Just (NumData (V.fromList [3, 1, 4, 1, 5]))
cell3Resolver _   = Nothing

main :: IO ()
main = do
  TIO.writeFile (outDir ++ "cell1-inline-scatter.svg") (renderSVG cell1)
  TIO.writeFile (outDir ++ "cell2-easy-overlay.svg")    (renderSVG cell2)
  TIO.writeFile (outDir ++ "cell3-colbyname.svg")       (renderSVGWith cell3Resolver cell3Spec)
  putStrLn "wrote 3 demo SVGs to design/ihaskell/"

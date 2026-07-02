{-# LANGUAGE OverloadedStrings #-}
module Main where
import Hgg.Plot.Easy
import           Hgg.Plot.Unit         (px, (*~))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  let s = purePlot
            <> layer (scatter "x" "y" <> alpha 0.7 <> size 5
                       <> colorBy "g")
            <> layer (line (inline [1.0, 2.0, 3.0]) (inline [4.0, 5.0, 6.0])
                       <> color (fromHex "#ff0000"))
            <> title "demo"
            <> theme ThemeDark
            <> facet "region"
            <> widthUnit (800 *~ px) <> heightUnit (600 *~ px)
  BL.putStrLn (encode s)

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hgg.Plot.Backend.SVG (renderBound)
import           Hgg.Plot.DataFrame   (dfResolver)
import           Hgg.Plot.Frame       (PlotData (..), (|>>))
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Vector              as V
import qualified DataFrame                as DF
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "dfResolver" $ do
    it "Double 列を resolve" $ do
      let df = DF.fromNamedColumns
                 [ ("x", DF.fromList ([1.0, 2.0, 3.0] :: [Double])) ]
          r  = dfResolver df
      case r "x" of
        Just (NumData v) -> V.toList v `shouldBe` [1.0, 2.0, 3.0]
        _                -> expectationFailure "expected NumData"
    it "Int 列を resolve (= Double に変換)" $ do
      let df = DF.fromNamedColumns
                 [ ("n", DF.fromList ([10, 20, 30] :: [Int])) ]
          r  = dfResolver df
      case r "n" of
        Just (NumData v) -> V.toList v `shouldBe` [10.0, 20.0, 30.0]
        _                -> expectationFailure "expected NumData"
    it "存在しない列は Nothing" $ do
      let df = DF.fromNamedColumns
                 [ ("x", DF.fromList ([1.0, 2.0] :: [Double])) ]
          r  = dfResolver df
      r "nope" `shouldBe` Nothing
    it "nullable (Maybe Int) 列を resolve (NA → NaN・長さ保持)" $ do
      let df = DF.fromNamedColumns
                 [ ("m", DF.fromList ([Just 1, Nothing, Just 3] :: [Maybe Int])) ]
          r  = dfResolver df
      case r "m" of
        Just (NumData v) -> do
          length (V.toList v) `shouldBe` 3
          (V.toList v !! 0, V.toList v !! 2) `shouldBe` (1.0, 3.0)
          isNaN (V.toList v !! 1) `shouldBe` True   -- NA は NaN
        _ -> expectationFailure "expected NumData for Maybe Int column"
    it "nullable (Maybe Double) 列を resolve" $ do
      let df = DF.fromNamedColumns
                 [ ("d", DF.fromList ([Just 1.5, Nothing] :: [Maybe Double])) ]
          r  = dfResolver df
      case r "d" of
        Just (NumData v) -> do
          V.toList v !! 0 `shouldBe` 1.5
          isNaN (V.toList v !! 1) `shouldBe` True
        _ -> expectationFailure "expected NumData for Maybe Double column"

  describe "PlotData DataFrame instance (Phase 14 A4)" $ do
    let df = DF.fromNamedColumns
               [ ("x", DF.fromList ([1.0, 2.0, 3.0] :: [Double]))
               , ("y", DF.fromList ([4.0, 5.0, 6.0] :: [Double])) ]
    it "columnNames は全列" $
      columnNames df `shouldBe` ["x", "y"]
    it "nrows は行数 (dataframeDimensions の fst)" $
      nrows df `shouldBe` 3
    it "Map と DataFrame で同一 SVG (同データ、 spec-2 §7)" $ do
      let mapDF :: Map Text ColData
          mapDF = M.fromList
            [ ("x", NumData (V.fromList [1.0, 2.0, 3.0]))
            , ("y", NumData (V.fromList [4.0, 5.0, 6.0])) ]
      renderBound (df    |>> layer (scatter "x" "y"))
        `shouldBe`
        renderBound (mapDF |>> layer (scatter "x" "y"))

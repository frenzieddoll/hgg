-- | hgg-svg テスト。 Phase 14 A3 = BoundPlot (df |>> spec) → SVG smoke。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.SVG (renderBound)
import           Graphics.Hgg.Frame       ((|>>))
import           Graphics.Hgg.Spec        (ColData (..), layer, scatter)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Test.Hspec

-- 列順違いで同一データ (Map vs assoc-list)
xyAssoc :: [(Text, ColData)]
xyAssoc =
  [ ("x", NumData (V.fromList [1, 2, 3]))
  , ("y", NumData (V.fromList [4, 5, 6]))
  ]

xyMap :: Map Text ColData
xyMap = M.fromList xyAssoc

main :: IO ()
main = hspec $
  describe "renderBound (df |>> spec)" $ do
    it "df |>> spec が SVG を出す" $ do
      let svg = renderBound (xyMap |>> layer (scatter "x" "y"))
      ("<svg"   `T.isInfixOf` svg) `shouldBe` True
      ("</svg>" `T.isInfixOf` svg) `shouldBe` True
    it "Map と assoc-list で同一 SVG (同データ、 spec-2 §7)" $
      renderBound (xyMap   |>> layer (scatter "x" "y"))
        `shouldBe`
      renderBound (xyAssoc |>> layer (scatter "x" "y"))

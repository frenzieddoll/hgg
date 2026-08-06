-- | hgg-svg テスト。 Phase 14 A3 = BoundPlot (df |>> spec) → SVG smoke。
-- Phase 64 A7 = PClipPath (多角形 clip) の SVG 出力。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.SVG (renderBound, renderPrimitivesSVG)
import           Graphics.Hgg.Frame       ((|>>))
import           Graphics.Hgg.Render      (FillStyle (..), Point (..),
                                           Primitive (..))
import           Graphics.Hgg.Layout      (Rect (..))
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

-- Phase 64 A7: 三角形 clip の中に全面 rect を 1 枚置いた最小列。
triangle :: [Point]
triangle = [Point 100 20, Point 180 180, Point 20 180]

clipped :: [Point] -> [Primitive]
clipped pts =
  [ PClipPath pts
  , PRect (Rect 0 0 200 200) (FillStyle "#ff0000" 1) Nothing
  , PClipPop
  ]

main :: IO ()
main = hspec $ do
  describe "renderBound (df |>> spec)" $ do
    it "df |>> spec が SVG を出す" $ do
      let svg = renderBound (xyMap |>> layer (scatter "x" "y"))
      ("<svg"   `T.isInfixOf` svg) `shouldBe` True
      ("</svg>" `T.isInfixOf` svg) `shouldBe` True
    it "Map と assoc-list で同一 SVG (同データ、 spec-2 §7)" $
      renderBound (xyMap   |>> layer (scatter "x" "y"))
        `shouldBe`
      renderBound (xyAssoc |>> layer (scatter "x" "y"))

  describe "Phase 64 A7: PClipPath (多角形 clip)" $ do
    it "clipPath + polygon 要素を出し、 <g clip-path> で囲う" $ do
      let svg = renderPrimitivesSVG 200 200 "" (clipped triangle)
      svg `shouldSatisfy` T.isInfixOf "<clipPath id=\"clip0\"><polygon points=\""
      svg `shouldSatisfy` T.isInfixOf "100.0,20.0 180.0,180.0 20.0,180.0"
      svg `shouldSatisfy` T.isInfixOf "<g clip-path=\"url(#clip0)\">"
      svg `shouldSatisfy` T.isInfixOf "</g>"

    it "頂点 3 点未満は clip 無し (polygon を出さず素通し)" $ do
      let svg = renderPrimitivesSVG 200 200 ""
                  (clipped [Point 10 10, Point 20 20])
      svg `shouldSatisfy` (not . T.isInfixOf "<polygon")
      svg `shouldSatisfy` (not . T.isInfixOf "clipPath")
      -- 中身は clip されずそのまま描かれる
      svg `shouldSatisfy` T.isInfixOf "#ff0000"

    it "<g> の開閉が釣り合う (退化列でも)" $ do
      let count needle = length . T.breakOnAll needle
          balanced ps  = let svg = renderPrimitivesSVG 200 200 "" (clipped ps)
                         in count "<g" svg `shouldBe` count "</g>" svg
      balanced triangle
      balanced [Point 10 10, Point 20 20]

    it "clip primitive の無い列には clipPath も <g> も出ない (既存図ゼロ diff)" $ do
      let plain = [PRect (Rect 0 0 200 200) (FillStyle "#ff0000" 1) Nothing]
          svg   = renderPrimitivesSVG 200 200 "" plain
      svg `shouldSatisfy` (not . T.isInfixOf "clipPath")
      svg `shouldSatisfy` (not . T.isInfixOf "<g")

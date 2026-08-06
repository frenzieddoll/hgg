-- | hgg-pdf のテスト (Phase 17)。
-- PDF バイト列はメタデータ等で非決定になり得るため golden 比較はせず、
-- 「%PDF- 先頭 + 例外なし + サイズ下限」 を固定する (計画 md のリスク欄)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.PDF (savePDF, savePrimitivesPDF)
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Layout      (Rect (..))
import           Graphics.Hgg.Render      (FillStyle (..), Point (..),
                                           Primitive (..))
import qualified Data.ByteString.Char8    as BS
import           System.Directory         (getTemporaryDirectory, removeFile)
import           System.FilePath          ((</>))
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Phase 17 A1: savePDF (骨格 = line/rect/circle)" $ do
    it "scatter spec が %PDF- 先頭の非自明なファイルを書く" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "hgg-pdf-test-scatter.pdf"
          spec = layer (scatter (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))
      savePDF path spec
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      BS.length bs `shouldSatisfy` (> 500)
      removeFile path

    it "bar spec (rect 経路) も例外なく書ける" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "hgg-pdf-test-bar.pdf"
          spec = layer (bar (inlineCat (["a", "b", "a"] :: [String]))
                            (inline [1, 2, 3]))
      savePDF path spec
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      removeFile path

  describe "Phase 17 A2: PPath + clip 再帰グルーピング" $ do
    it "density (PPath 経路) が例外なく書ける" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "hgg-pdf-test-density.pdf"
          spec = layer (density (inline [1, 2, 2, 3, 3, 3, 4, 5]))
      savePDF path spec
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      removeFile path

    it "coordCartesianX (PClipPush/Pop 経路) が例外なく書ける" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "hgg-pdf-test-clip.pdf"
          spec = layer (line (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))
                 <> coordCartesianX 1.5 3.5
      savePDF path spec
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      removeFile path

  describe "Phase 64 A7: PClipPath (多角形 clip)" $ do
    -- PDF は content stream が圧縮され得るので描画結果の byte 検査はせず、
    -- 「例外なく書ける + 中身が非自明」 を固定する (このファイル冒頭の方針)。
    it "三角形 clip が例外なく書ける" $ do
      tmp <- getTemporaryDirectory
      let path  = tmp </> "hgg-pdf-test-clippath.pdf"
          prims = [ PClipPath [Point 100 20, Point 180 180, Point 20 180]
                  , PRect (Rect 0 0 200 200) (FillStyle "#ff0000" 1) Nothing
                  , PClipPop ]
      savePrimitivesPDF path 200 200 prims
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      BS.length bs `shouldSatisfy` (> 500)
      removeFile path

    it "頂点 3 点未満 (退化) でも例外なく書ける (clip 無しで素通し)" $ do
      tmp <- getTemporaryDirectory
      let path  = tmp </> "hgg-pdf-test-clippath-degenerate.pdf"
          prims = [ PClipPath [Point 10 10, Point 20 20]
                  , PRect (Rect 0 0 200 200) (FillStyle "#ff0000" 1) Nothing
                  , PClipPop ]
      savePrimitivesPDF path 200 200 prims
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      removeFile path

  describe "Phase 17 A3: PText (標準フォント・anchor/rotate)" $ do
    it "title + 軸ラベル (回転 y ラベル含む) が例外なく書ける" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "hgg-pdf-test-text.pdf"
          spec = layer (scatter (inline [1, 2, 3]) (inline [3, 1, 2]))
                 <> title "Latin title"
                 <> xLabel "weight" <> yLabel "mpg"
      savePDF path spec
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      BS.length bs `shouldSatisfy` (> 1000)
      removeFile path

    it "非 Latin-1 ラベルでも crash しない (? 置換 + 警告)" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "hgg-pdf-test-cjk.pdf"
          spec = layer (scatter (inline [1, 2, 3]) (inline [3, 1, 2]))
                 <> title "日本語タイトル"
      savePDF path spec
      bs <- BS.readFile path
      BS.take 5 bs `shouldBe` "%PDF-"
      removeFile path

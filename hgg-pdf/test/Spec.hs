-- | hgg-pdf のテスト (Phase 17)。
-- PDF バイト列はメタデータ等で非決定になり得るため golden 比較はせず、
-- 「%PDF- 先頭 + 例外なし + サイズ下限」 を固定する (計画 md のリスク欄)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.PDF (savePDF)
import           Graphics.Hgg.Easy
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

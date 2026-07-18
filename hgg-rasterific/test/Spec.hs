-- | hgg-rasterific のテスト (Phase 22)。
-- PNG は「magic 先頭 + JuicyPixels decode 成功 + 寸法 > 0」 を固定する
-- (golden バイト比較は SVG golden と役割重複のためしない。 計画 md のリスク欄)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.Rasterific (PNGConfig (..), PNGFonts (..),
                                                  defaultPNGConfig, loadPNGFonts,
                                                  savePNG, savePNGConfigured)
import           Graphics.Hgg.Easy
import           Codec.Picture                   (Image (..), PixelRGBA8,
                                                  convertRGBA8, decodePng)
import qualified Data.ByteString                 as BS
import qualified Graphics.Text.TrueType          as F
import           System.Directory                (getTemporaryDirectory,
                                                  removeFile)
import           System.FilePath                 ((</>))
import           Test.Hspec

-- | savePNG して decode まで通し、 画像を返す共通 helper。
saveAndDecodeImg :: FilePath -> VisualSpec -> IO (Image PixelRGBA8)
saveAndDecodeImg name spec = do
  tmp <- getTemporaryDirectory
  let path = tmp </> name
  savePNG path spec
  bs <- BS.readFile path
  BS.take 8 bs `shouldBe` BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  removeFile path
  case decodePng bs of
    Left err  -> error ("PNG decode 失敗: " ++ err)
    Right dyn -> pure (convertRGBA8 dyn)

-- | 寸法だけ欲しい場合の短縮。
saveAndDecode :: FilePath -> VisualSpec -> IO (Int, Int)
saveAndDecode name spec = do
  img <- saveAndDecodeImg name spec
  pure (imageWidth img, imageHeight img)

main :: IO ()
main = hspec $ do
  describe "Phase 22 A1: savePNG (骨格 = line/rect/circle)" $ do
    it "scatter spec が valid な PNG を書く (decode + 寸法)" $ do
      (w, h) <- saveAndDecode "hgg-png-test-scatter.png" $
        layer (scatter (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))
      w `shouldSatisfy` (> 100)
      h `shouldSatisfy` (> 100)

    it "bar spec (rect 経路) も例外なく書ける" $ do
      (w, _) <- saveAndDecode "hgg-png-test-bar.png" $
        layer (bar (inlineCat (["a", "b", "a"] :: [String]))
                   (inline [1, 2, 3]))
      w `shouldSatisfy` (> 100)

  describe "Phase 22 A2: PPath + clip/transform 再帰グルーピング" $ do
    it "density (PPath 経路) が例外なく書ける" $ do
      (w, _) <- saveAndDecode "hgg-png-test-density.png" $
        layer (density (inline [1, 2, 2, 3, 3, 3, 4, 5]))
      w `shouldSatisfy` (> 100)

    it "coordCartesianX (PClipPush/Pop 経路) が例外なく書ける" $ do
      (w, _) <- saveAndDecode "hgg-png-test-clip.png" $
        layer (line (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))
          <> coordCartesianX 1.5 3.5
      w `shouldSatisfy` (> 100)

    it "facet (複数 panel) が例外なく書ける" $ do
      (w, _) <- saveAndDecode "hgg-png-test-facet.png" $
        layer (scatter (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))
          <> facet (inlineCat (["g1", "g1", "g2", "g2"] :: [String]))
      w `shouldSatisfy` (> 100)

    it "coordPolar が例外なく書ける" $ do
      (w, _) <- saveAndDecode "hgg-png-test-polar.png" $
        layer (line (inline [1, 2, 3, 4, 5, 6]) (inline [2, 4, 1, 3, 2, 5]))
          <> coordPolar
      w `shouldSatisfy` (> 100)

  describe "Phase 22 A3: PText (TrueType・anchor/rotate・日本語)" $ do
    it "title + 軸ラベル (回転 y ラベル含む) が例外なく書ける" $ do
      (w, _) <- saveAndDecode "hgg-png-test-text.png" $
        layer (scatter (inline [1, 2, 3]) (inline [3, 1, 2]))
          <> title "Latin title"
          <> xLabel "weight" <> yLabel "mpg"
      w `shouldSatisfy` (> 100)

    it "既定探索のフォントが日本語 glyph を持つ (isPlaceholder 欠落なし)" $ do
      fonts <- loadPNGFonts defaultPNGConfig
      let jp = "日本語タイトル重さあいう" :: String
      filter (F.isPlaceholder (pfRegular fonts)) jp `shouldBe` []

    it "日本語 title が実際に pixel を描く (空 title との画像差分)" $ do
      let base    = layer (scatter (inline [1, 2, 3]) (inline [3, 1, 2]))
      imgJp <- saveAndDecodeImg "hgg-png-test-jp.png"
                 (base <> title "日本語タイトル")
      imgNo <- saveAndDecodeImg "hgg-png-test-nojp.png" base
      imageData imgJp `shouldNotBe` imageData imgNo

  describe "Phase 22 A4: pngScale (Hi-DPI)" $ do
    it "pngScale 2.0 で寸法が縦横 2 倍になる" $ do
      tmp <- getTemporaryDirectory
      let spec  = layer (scatter (inline [1, 2, 3]) (inline [3, 1, 2]))
          save s name = do
            let path = tmp </> name
            savePNGConfigured defaultPNGConfig { pngScale = s }
                              path emptyResolver spec
            bs <- BS.readFile path
            removeFile path
            case decodePng bs of
              Left err  -> error ("PNG decode 失敗: " ++ err)
              Right dyn -> pure (convertRGBA8 dyn)
      img1 <- save 1.0 "hgg-png-test-1x.png"
      img2 <- save 2.0 "hgg-png-test-2x.png"
      imageWidth img2  `shouldBe` 2 * imageWidth img1
      imageHeight img2 `shouldBe` 2 * imageHeight img1

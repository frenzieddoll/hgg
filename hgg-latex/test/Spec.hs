-- | hgg-latex のテスト (Phase 54)。
-- 生成 .tex は決定論的な純テキストなので text 内容で固定する
-- (LaTeX での組版コンパイルは dev-only 検証 = design/phase54-latex/、
-- test は TeX 環境に依存しない)。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.LaTeX (CJKMode (..), TeXConfig (..),
                                             defaultTeXConfig, luaLaTeXConfig,
                                             renderPrimitivesTeX, renderTeX,
                                             renderTeXConfigured)
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Layout        (Rect (..))
import           Graphics.Hgg.Spec          (emptyResolver)
import           Graphics.Hgg.Render        (FillStyle (..), LineStyle (..),
                                             PathSegment (..), Point (..),
                                             Primitive (..), Transform (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Phase 54 A2: 文書骨格" $ do
    it "standalone class + tikz + DejaVuSans preamble + bounding box" $ do
      let tex = renderTeX scatterSpec
      tex `shouldSatisfy` has "\\documentclass[border=0pt]{standalone}"
      tex `shouldSatisfy` has "\\usepackage{tikz}"
      tex `shouldSatisfy` has "\\usepackage{DejaVuSans}"
      tex `shouldSatisfy` has "\\path[use as bounding box]"
      tex `shouldSatisfy` has "\\begin{tikzpicture}"
      tex `shouldSatisfy` has "\\end{document}"

    it "render は決定論的 (同一 spec → byte 一致)" $
      renderTeX scatterSpec `shouldBe` renderTeX scatterSpec

    it "座標・寸法は明示 bp 単位 + 指数表記なし" $ do
      let tex = renderTeX scatterSpec
      tex `shouldSatisfy` has "bp,"
      -- 1.0e-2 等の指数表記が出たら NG (数字直後の e のみ。 "latex" 等は除外)
      tex `shouldSatisfy` (not . hasSciNotation)

  describe "Phase 54 A2: 基本 primitive" $ do
    it "scatter → circle + 軸線 (\\draw) + definecolor 集約" $ do
      let tex = renderTeX scatterSpec
      tex `shouldSatisfy` has "circle [radius="
      tex `shouldSatisfy` has "\\draw["
      tex `shouldSatisfy` has "\\definecolor{pc"

    it "bar → rectangle" $ do
      let tex = renderTeX
            (layer (bar (inlineCat (["a", "b", "a"] :: [String]))
                        (inline [1, 2, 3])))
      tex `shouldSatisfy` has "rectangle"

    it "軸目盛ラベル → \\node (anchor + \\sffamily + \\fontsize)" $ do
      let tex = renderTeX scatterSpec
      tex `shouldSatisfy` has "\\node[anchor="
      tex `shouldSatisfy` has "\\sffamily"
      tex `shouldSatisfy` has "\\fontsize{"

    it "y 軸ラベルの rotate は CCW 恒等 (rotate=90)" $ do
      let tex = renderTeX (scatterSpec <> yLabel "response")
      tex `shouldSatisfy` has "rotate=90"

  describe "Phase 54 A2: escape" $ do
    it "LaTeX 特殊文字 (% $ _ # &) が escape される" $ do
      let tex = renderTeX (scatterSpec <> title "50% of $x_1 #a & b")
      tex `shouldSatisfy` has "50\\% of \\$x\\_1 \\#a \\& b"

    it "backslash / brace / tilde / caret も escape される" $ do
      let tex = renderTeX (scatterSpec <> title "a\\b{c}~d^e")
      tex `shouldSatisfy` has
        "a\\textbackslash{}b\\{c\\}\\textasciitilde{}d\\textasciicircum{}e"

  describe "Phase 54 A3: PPath + clip/transform scope" $ do
    it "density (PPath 経路) → \\path[...] 折れ線 + 例外なし" $ do
      let tex = renderTeX (layer (density (inline [1, 2, 2, 3, 3, 3, 4, 5])))
      tex `shouldSatisfy` has " -- "

    it "coordCartesianX → \\begin{scope} + \\clip (対応 \\end{scope} 同数)" $ do
      let tex = renderTeX
            (layer (line (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))
             <> coordCartesianX 1.5 3.5)
      tex `shouldSatisfy` has "\\clip "
      T.count "\\begin{scope}" tex `shouldBe` T.count "\\end{scope}" tex
      T.count "\\begin{scope}" tex `shouldSatisfy` (> 0)

    it "CurveTo → .. controls .. 構文 (primitive 直入力)" $ do
      let prims = [ PPath [ MoveTo (Point 10 10)
                          , CurveTo (Point 20 10) (Point 20 30) (Point 30 30)
                          , ClosePath ]
                          (FillStyle "#ff0000" 1) Nothing ]
      renderPrimitivesTeX 100 100 prims
        `shouldSatisfy` has ".. controls (20.000bp,90.000bp) and (20.000bp,70.000bp) .. (30.000bp,70.000bp) -- cycle;"

    it "入れ子 clip は scope が入れ子で対応する" $ do
      let clip r = PClipPush (Rect r r 50 50)
          prims  = [ clip 0, clip 10
                   , PLine (Point 0 0) (Point 9 9)
                       (LineStyle "#000000" 1 [])
                   , PClipPop, PClipPop ]
          tex    = renderPrimitivesTeX 100 100 prims
      T.count "\\begin{scope}" tex `shouldBe` 2
      T.count "\\end{scope}" tex `shouldBe` 2

    it "PTransformPush → cm= 共役行列 (PDF backend の matrixOf と同式)" $ do
      let prims = [ PTransformPush (TranslateT 5 7)
                  , PLine (Point 0 0) (Point 1 1) (LineStyle "#000000" 1 [])
                  , PTransformPop
                  , PTransformPush (ScaleT 2 0.5)
                  , PLine (Point 0 0) (Point 1 1) (LineStyle "#000000" 1 [])
                  , PTransformPop ]
          tex   = renderPrimitivesTeX 100 100 prims
      tex `shouldSatisfy` has "cm={1,0,0,1,(5.000bp,-7.000bp)}"
      tex `shouldSatisfy` has "cm={2.000,0,0,0.500,(0bp,50.000bp)}"

  describe "Phase 54 A4: 数式 passthrough / CJK / config" $ do
    it "全体 $...$ のラベルは escape されず生で出る" $ do
      let tex = renderTeX (scatterSpec <> title "$\\hat{\\beta}_1 \\pm 1.96$")
      tex `shouldSatisfy` has "\\selectfont $\\hat{\\beta}_1 \\pm 1.96$}"

    it "混在 ($a$ and $b$) は数式扱いせず escape" $ do
      let tex = renderTeX (scatterSpec <> title "$a$ and $b$")
      tex `shouldSatisfy` has "\\$a\\$ and \\$b\\$"

    it "日本語ラベル → CJK 環境 wrap + \\usepackage{CJKutf8} 自動付与" $ do
      let tex = renderTeX (scatterSpec <> title "応答曲面")
      tex `shouldSatisfy` has "\\usepackage{CJKutf8}"
      tex `shouldSatisfy` has "\\begin{CJK}{UTF8}{ipxg}応答曲面\\end{CJK}"

    it "Latin のみなら CJKutf8 は出ない" $ do
      renderTeX scatterSpec `shouldSatisfy` (not . has "CJKutf8")

    it "texCJKFamily で明朝 (ipxm) に差し替え可" $ do
      let cfg = defaultTeXConfig { texCJKFamily = "ipxm" }
          tex = renderTeXConfigured cfg emptyResolver
                  (scatterSpec <> title "応答曲面")
      tex `shouldSatisfy` has "{UTF8}{ipxm}"

    it "素片 mode = documentclass 無し + 要 preamble コメント" $ do
      let cfg = defaultTeXConfig { texStandalone = False }
          tex = renderTeXConfigured cfg emptyResolver scatterSpec
      tex `shouldSatisfy` (not . has "\\documentclass")
      tex `shouldSatisfy` (not . has "\\end{document}")
      tex `shouldSatisfy` has "% 要 preamble:"
      tex `shouldSatisfy` has "\\begin{tikzpicture}"

    it "CJKRaw (lualatex/xelatex 向け) = wrap 無し生 UTF-8 + CJKutf8 も出ない" $ do
      let cfg = defaultTeXConfig { texCJKMode = CJKRaw }
          tex = renderTeXConfigured cfg emptyResolver
                  (scatterSpec <> title "応答曲面")
      tex `shouldSatisfy` (not . has "CJKutf8")
      tex `shouldSatisfy` (not . has "\\begin{CJK}")
      tex `shouldSatisfy` has "\\selectfont 応答曲面}"

    it "luaLaTeXConfig = CJKRaw + luatexja preamble (lualatex 実測済 preset)" $ do
      let tex = renderTeXConfigured luaLaTeXConfig emptyResolver
                  (scatterSpec <> title "応答曲面")
      tex `shouldSatisfy` has "\\usepackage{luatexja}"
      tex `shouldSatisfy` (not . has "CJKutf8")
      tex `shouldSatisfy` has "\\selectfont 応答曲面}"

    it "texExtraPreamble が preamble に入る (standalone)" $ do
      let cfg = defaultTeXConfig
                  { texExtraPreamble = ["\\usepackage{mypkg}"] }
          tex = renderTeXConfigured cfg emptyResolver scatterSpec
      tex `shouldSatisfy` has "\\usepackage{mypkg}"

  describe "Phase 54 A5: golden" $
    it "scatter の生成 .tex が golden fixture と全文一致" $ do
      golden <- TIO.readFile "test/golden/scatter-golden.tex"
      renderTeX scatterSpec `shouldBe` golden

scatterSpec :: VisualSpec
scatterSpec =
  layer (scatter (inline [1, 2, 3, 4]) (inline [2, 4, 1, 3]))

has :: Text -> Text -> Bool
has needle hay = needle `T.isInfixOf` hay

-- | "3.0e-2" / "1e5" のような指数表記の数値が含まれるか (数字 + e/E + 符号/数字)。
hasSciNotation :: Text -> Bool
hasSciNotation t = any isSci (zip3' (T.unpack t))
  where
    isSci (a, b, c) = (a >= '0' && a <= '9')
                   && (b == 'e' || b == 'E')
                   && (c == '-' || c == '+' || (c >= '0' && c <= '9'))

-- | 隣接 3 文字組 (Data.Text に無いので List 側で)。
zip3' :: [Char] -> [(Char, Char, Char)]
zip3' cs = zip3 cs (drop 1 cs) (drop 2 cs)

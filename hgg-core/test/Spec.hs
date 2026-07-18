{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Easy
import           Graphics.Hgg.Validate
import           Graphics.Hgg.Layout
import           Graphics.Hgg.Render
import           Graphics.Hgg.Render.Common  (pointShapeAt, alphaVector)
import           Graphics.Hgg.Primitive      (Point (..))
import           Graphics.Hgg.Render.Special (renderDAGStandalone, primsBBoxDAG, dagToScreen)
import           Graphics.Hgg.Layout.RangeOf (invNormCdf, qqPoints, ecdfPoints)
import           Graphics.Hgg.Layout.Grid    (GridCell (..), GridPlacement (..),
                                              flattenSubplots, gridDims, toPTree)
import           Graphics.Hgg.Math.Special   (logGamma, regIncompleteBeta, betaQuantile)
import qualified Graphics.Hgg.Math.Griddata  as Griddata
import qualified Graphics.Hgg.DAG
import           Graphics.Hgg.DAG ((~>))
import qualified Graphics.Hgg.DAG.Internal.Sugiyama as Sugi
import qualified Graphics.Hgg.Render.EdgeRoute as ER
import qualified Data.Map.Strict as Map
import           Data.List (sort)
import qualified Data.List
import qualified Data.Text
import           Data.Monoid         (First (..), Last (..))
import qualified Data.Vector         as V
import           Test.Hspec
-- Phase 7 A7: gallery primitive count 回帰 test 用
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson           (eitherDecode, encode)
import           Graphics.Hgg.Unit    (Length (..), LUnit (..), (*~),
                                       mm, inch, px, mmToPt, toPt, lengthToPt,
                                       Pos (..), resolveLen)
import           System.Directory     (listDirectory, doesDirectoryExist, doesFileExist)
import           System.FilePath      ((</>), takeExtension)

main :: IO ()
main = hspec $ do

  describe "P2a acyclic (Sugiyama.breakCycles)" $ do
    it "acyclic 入力は順序保持で不変 (= 現行図に非破壊)" $ do
      let es = [("a","b"),("b","c"),("a","c")]
      Sugi.breakCycles ["a","b","c"] es `shouldBe` es
    it "back-edge を反転して DAG 化する (a→b→c→a の c→a を反転)" $ do
      Sugi.breakCycles ["a","b","c"] [("a","b"),("b","c"),("c","a")]
        `shouldBe` [("a","b"),("b","c"),("a","c")]
    it "self-loop は rank 制約に寄与しないので除去する" $ do
      Sugi.breakCycles ["a","b"] [("a","b"),("a","a"),("b","b")]
        `shouldBe` [("a","b")]
    it "閉路でも rank が単調になる (従来の 0 仮置きは誤りだった)" $ do
      let lg = Sugi.assignRanks (Sugi.buildLayoutGraph ["a","b","c"]
                 (Sugi.breakCycles ["a","b","c"] [("a","b"),("b","c"),("c","a")]))
          rk = Map.fromList [ (Sugi.lnId n, Sugi.lnRank n) | n <- Sugi.lgNodes lg ]
      -- a<b<c が保たれる (a=0,b=1,c=2)
      (Map.lookup "a" rk, Map.lookup "b" rk, Map.lookup "c" rk)
        `shouldBe` (Just 0, Just 1, Just 2)

  describe "Graphics.Hgg.Layout.Grid (Phase 37 A2 統一グリッド平坦化)" $ do
    -- 各 leaf を title で識別し、 占有セルを title で引く。
    let leaf nm = title (Data.Text.pack nm)
        cellOf nm gp =
          case [ c | (s, c) <- gpPanels gp, getLast (vsTitle s) == Just (Data.Text.pack nm) ] of
            (c:_) -> c
            []    -> error ("panel not found: " ++ nm)
    it "leaf 単体は 1x1" $
      gridDims (toPTree (leaf "a")) `shouldBe` (1, 1)
    it "a <-> b <-> c は 1 行 3 列・各 1x1" $ do
      let gp = flattenSubplots (leaf "a" <-> leaf "b" <-> leaf "c")
      (gpCols gp, gpRows gp) `shouldBe` (3, 1)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "b" gp `shouldBe` GridCell 0 1 1 1
      cellOf "c" gp `shouldBe` GridCell 0 1 2 1
    it "a <:> b <:> c は 3 行 1 列・各 1x1" $ do
      let gp = flattenSubplots (leaf "a" <:> leaf "b" <:> leaf "c")
      (gpCols gp, gpRows gp) `shouldBe` (1, 3)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "b" gp `shouldBe` GridCell 1 1 0 1
      cellOf "c" gp `shouldBe` GridCell 2 1 0 1
    it "(a<->b<->c) <:> d は d が下段全幅 (colSpan=3) で左端整列" $ do
      let gp = flattenSubplots ((leaf "a" <-> leaf "b" <-> leaf "c") <:> leaf "d")
      (gpCols gp, gpRows gp) `shouldBe` (3, 2)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "c" gp `shouldBe` GridCell 0 1 2 1
      cellOf "d" gp `shouldBe` GridCell 1 1 0 3   -- 上段左 a と下段 d の左端が col0 で一致
    it "(a<:>b) <-> c は c が右列全高 (rowSpan=2)" $ do
      let gp = flattenSubplots ((leaf "a" <:> leaf "b") <-> leaf "c")
      (gpCols gp, gpRows gp) `shouldBe` (2, 2)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "b" gp `shouldBe` GridCell 1 1 0 1
      cellOf "c" gp `shouldBe` GridCell 0 2 1 1
    it "Phase 59: a <:> b <-> c (無括弧) は (a<:>b)<->c と同結合 (both infixl 6 = 左結合)" $ do
      -- fixity 回帰: 旧 <:>=infixl 5 では a <:> (b<->c) と別構造にパースされ fail する。
      let gp = flattenSubplots (leaf "a" <:> leaf "b" <-> leaf "c")
      (gpCols gp, gpRows gp) `shouldBe` (2, 2)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "b" gp `shouldBe` GridCell 1 1 0 1
      cellOf "c" gp `shouldBe` GridCell 0 2 1 1
    it "(a<->b) <:> (c<->d) は 2x2 グリッド" $ do
      let gp = flattenSubplots ((leaf "a" <-> leaf "b") <:> (leaf "c" <-> leaf "d"))
      (gpCols gp, gpRows gp) `shouldBe` (2, 2)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "b" gp `shouldBe` GridCell 0 1 1 1
      cellOf "c" gp `shouldBe` GridCell 1 1 0 1
      cellOf "d" gp `shouldBe` GridCell 1 1 1 1
    it "深いネスト (a<->b<->c)<:>(d<->e) も span 整列" $ do
      let gp = flattenSubplots ((leaf "a" <-> leaf "b" <-> leaf "c") <:> (leaf "d" <-> leaf "e"))
      (gpCols gp, gpRows gp) `shouldBe` (3, 2)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "c" gp `shouldBe` GridCell 0 1 2 1
      -- 下段 d<->e は 2 要素を 3 列に詰める (hbox 幅 2 < グループ幅 3)。
      cellOf "d" gp `shouldBe` GridCell 1 1 0 1
      cellOf "e" gp `shouldBe` GridCell 1 1 1 1
    it "subplots 4 枚 + subplotCols 2 は 2x2 wrap grid" $ do
      let gp = flattenSubplots (subplots [leaf "a", leaf "b", leaf "c", leaf "d"]
                                  <> subplotCols 2)
      (gpCols gp, gpRows gp) `shouldBe` (2, 2)
      cellOf "a" gp `shouldBe` GridCell 0 1 0 1
      cellOf "d" gp `shouldBe` GridCell 1 1 1 1

  describe "Phase 38 凡例 content-based 幅" $ do
    it "isWideChar: ASCII は半角・CJK/かな/全角記号は全角" $ do
      map isWideChar "Ab1_-"      `shouldBe` [False, False, False, False, False]
      map isWideChar "あ漢Ａ％"   `shouldBe` [True, True, True, True]
    it "textWidthEm: 字種別 advance (小文字0.58/全角1.0/細字0.30) を加算" $ do
      textWidthEm "ab"   `shouldBe` 1.16         -- 0.58 + 0.58
      textWidthEm "あい" `shouldBe` 2.0          -- 1.0 + 1.0
      textWidthEm "a漢"  `shouldBe` 1.58         -- 0.58 + 1.0
      textWidthEm "il"   `shouldBe` 0.6          -- 0.30 + 0.30 (細字 < 小文字)
      textWidthEm "WM"   `shouldBe` 1.84         -- 0.92 + 0.92 (幅広 > 小文字)
      textWidthEm ""     `shouldBe` 0.0
    it "legendGuideWidth: 最長ラベル(幅基準)で colW を駆動" $ do
      -- colW = legendKeyW + ggHalfLine/2 + fItem*maxEm + ggHalfLine
      let fItem = 8.8; fTitle = 11.0
          w = legendGuideWidth fItem fTitle "" ["aa", "bbbb"]   -- 最長 = "bbbb" (em 4*0.58=2.32)
      w `shouldBe` legendKeyW + ggHalfLine/2 + fItem * 2.32 + ggHalfLine
    it "legendGuideWidth: 全角ラベルは半角同字数より広い" $ do
      let f t = legendGuideWidth 8.8 11.0 "" [t]
      f "東京"  `shouldSatisfy` (> f "ab")        -- 全角2 (2.0em) > 半角2 (1.2em)
    it "legendGuideWidth: タイトルが最長アイテムより広ければタイトル幅" $ do
      -- 短いラベル + 長いタイトル → titleW が勝つ
      let w = legendGuideWidth 8.8 11.0 "verylongtitlexxxx" ["a"]
      w `shouldBe` 11.0 * textWidthEm "verylongtitlexxxx"
    it "legendGuideWidth: ラベル空集合でも key+pad 分の最小幅は確保" $ do
      legendGuideWidth 8.8 11.0 "" [] `shouldBe` legendKeyW + ggHalfLine/2 + ggHalfLine

  describe "Graphics.Hgg.Unit (Phase 33 単位系)" $ do
    it "(*~) はスカラ倍で単位保存" $
      (7 *~ inch) `shouldBe` Length 7 In
    it "lengthToPt: inch は dpi 非依存 (7in = 504pt)" $
      lengthToPt 96 (7 *~ inch) `shouldBe` 504
    it "lengthToPt: mm は mmToPt 係数" $
      abs (lengthToPt 96 (1 *~ mm) - mmToPt) `shouldSatisfy` (< 1e-9)
    it "lengthToPt: px は dpi 依存 (800px@96dpi = 600pt)" $
      lengthToPt 96 (800 *~ px) `shouldBe` 600
    it "px 遅延解決: pt→px 戻しで元の px に一致 (dpi 不問)" $
      let n = 800; dpiV = 137
      in abs (lengthToPt dpiV (n *~ px) * (dpiV/72) - n) `shouldSatisfy` (< 1e-9)
    it "toPt: 物理単位は Just" $
      toPt (7 *~ inch) `shouldBe` Just 504
    it "toPt: px は Nothing (dpi 必須を型で表現)" $
      toPt (800 *~ px) `shouldBe` Nothing
    it "JSON round-trip" $
      eitherDecode (encode (180 *~ mm)) `shouldBe` Right (Length 180 Mm)
    it "JSON は {v,u} 順固定・tag 小文字" $
      encode (180 *~ mm) `shouldBe` "{\"v\":180.0,\"u\":\"mm\"}"

  describe "Graphics.Hgg.Unit Pos + resolver (Phase 33 B3)" $ do
    -- panel rect: x=10,y=20,w=200,h=100。x scale: data 0..10→pt 10..210、
    -- y scale: data 0..5→pt 120(下)..20(上) の反転 (rY=上端 規約と整合)。
    let ctx = UCtx { uDpi = 96
                   , uRect = Rect 10 20 200 100
                   , uXScale = LinearScale 0 10 10 210
                   , uYScale = LinearScale 0 5 120 20 }
    it "resolvePosX PNpc: 0=左端, 1=右端, 0.5=中央" $ do
      resolvePosX ctx (PNpc 0)   `shouldBe` 10
      resolvePosX ctx (PNpc 1)   `shouldBe` 210
      resolvePosX ctx (PNpc 0.5) `shouldBe` 110
    it "resolvePosY PNpc: 1=上端 rY, 0=下端 rY+rH" $ do
      resolvePosY ctx (PNpc 1) `shouldBe` 20
      resolvePosY ctx (PNpc 0) `shouldBe` 120
    it "resolvePosX PNative: scaleApply 経由" $
      resolvePosX ctx (PNative 5) `shouldBe` 110
    it "resolvePosY PNative: 反転 scale が処理" $
      resolvePosY ctx (PNative 0) `shouldBe` 120
    it "resolvePosX PAbs: rX + 物理長 pt (1in=72pt)" $
      resolvePosX ctx (PAbs (1 *~ inch)) `shouldBe` 82
    it "resolveLen = lengthToPt" $
      resolveLen 96 (7 *~ inch) `shouldBe` 504
    it "Pos JSON round-trip (abs/npc/native)" $ do
      eitherDecode (encode (PNative 3.5))         `shouldBe` Right (PNative 3.5)
      eitherDecode (encode (PNpc 0.25))           `shouldBe` Right (PNpc 0.25)
      eitherDecode (encode (PAbs (180 *~ mm)))    `shouldBe` Right (PAbs (180 *~ mm))
    it "Pos JSON tag 形 (byte 安定・PS とミラー)" $ do
      encode (PNpc 0.5)            `shouldBe` "{\"t\":\"npc\",\"p\":0.5}"
      encode (PNative 3.5)         `shouldBe` "{\"t\":\"native\",\"p\":3.5}"
      encode (PAbs (180.5 *~ mm))  `shouldBe` "{\"t\":\"abs\",\"l\":{\"v\":180.5,\"u\":\"mm\"}}"

  describe "scalePrimitives (Phase 33 B5・pt→device)" $ do
    let rct = PRect (Rect 1 2 10 20) (FillStyle "#000" 1.0) (Just (StrokeStyle "#111" 3))
        cir = PCircle (Point 4 6) 5 (FillStyle "#000" 1.0) Nothing Nothing
        txt = PText (Point 2 3) "x" (TextStyle "#000" 11 "sans-serif" AnchorStart 0 "normal" False)
    it "k=1 は恒等" $
      scalePrimitives 1 [rct, cir, txt] `shouldBe` [rct, cir, txt]
    it "k=2: rect 座標+サイズ+stroke 幅を倍化" $
      scalePrimitives 2 [rct] `shouldBe`
        [PRect (Rect 2 4 20 40) (FillStyle "#000" 1.0) (Just (StrokeStyle "#111" 6))]
    it "k=2: circle 中心+半径を倍化" $
      scalePrimitives 2 [cir] `shouldBe`
        [PCircle (Point 8 12) 10 (FillStyle "#000" 1.0) Nothing Nothing]
    it "k=2: text 位置+font size を倍化" $
      scalePrimitives 2 [txt] `shouldBe`
        [PText (Point 4 6) "x" (TextStyle "#000" 22 "sans-serif" AnchorStart 0 "normal" False)]

  describe "Annotation Pos API (Phase 33 B6)" $ do
    it "annotTextP は Pos をそのまま格納" $
      vsAnnotations (annotTextP (PNpc 0.95) (PNative 3) "R")
        `shouldBe` [AnnText (PNpc 0.95) (PNative 3) "R" "" 12]
    it "annotRect (旧 x,y,w,h) は 2 隅 PNative に変換" $
      vsAnnotations (annotRect 2 5 1 3 "grey")
        `shouldBe` [AnnRect (PNative 2) (PNative 5) (PNative 3) (PNative 8)
                            "grey" "" 0 0.2]
    it "Annotation JSON round-trip (native/npc/abs 混在)" $ do
      let a1 = AnnText (PNpc 0.95) (PNative 3) "R" "#000" 12
          a2 = AnnArrow (PNative 1) (PNative 2) (PAbs (5 *~ mm)) (PNpc 0.5) "#444" 1.5
      eitherDecode (encode a1) `shouldBe` Right a1
      eitherDecode (encode a2) `shouldBe` Right a2
    it "PNpc 注釈が panel 相対で解決 (旧 HS の Frac 無視バグ修正)" $
      -- npc(0,1) = panel 左上 = (rX, rY)。旧実装は coord を無視し data 扱いだった。
      let spec = layer (scatter (inline [0.0, 1.0, 2.0]) (inline [0.0, 1.0, 2.0]))
                   <> annotTextP (PNpc 0) (PNpc 1) "tl"
          lay  = computeLayout emptyResolver spec
          a    = lpPlotArea lay
          ps   = renderToPrimitives emptyResolver lay spec
      in [ p | PText p "tl" _ <- ps ] `shouldBe` [Point (rX a) (rY a)]

  describe "ColRef + OverloadedStrings" $ do
    it "\"weight\" :: ColRef を ColByName に" $
      ("weight" :: ColRef) `shouldBe` ColByName "weight"
    it "inline (Vector Double) → ColNum" $
      case inline (V.fromList [1.0, 2.0, 3.0]) of
        ColNum v -> V.length v `shouldBe` 3
        _        -> expectationFailure "wrong tag"
    it "inline [Int] → ColNum (auto-promotion)" $
      case inline [1, 2, 3 :: Int] of
        ColNum v -> V.toList v `shouldBe` [1.0, 2.0, 3.0]
        _        -> expectationFailure "wrong tag"
    it "inlineCat [String] → ColTxt" $
      case inlineCat (["a", "b", "c"] :: [String]) of
        ColTxt v -> V.length v `shouldBe` 3
        _        -> expectationFailure "wrong tag"
    it "resolveNum inline は resolver 不要で解決" $
      resolveNum emptyResolver (inline [10.0, 20.0])
        `shouldBe` Just (V.fromList [10, 20])
    it "resolveNum ColByName は resolver を引く" $
      let r n = if n == "x" then Just (NumData (V.fromList [1, 2])) else Nothing
      in resolveNum r "x" `shouldBe` Just (V.fromList [1, 2])
    it "resolveTxt 文字列列を解決" $
      let r n = if n == "g" then Just (TxtData (V.fromList ["a", "b"])) else Nothing
      in resolveTxt r "g" `shouldBe` Just (V.fromList ["a", "b"])
    it "resolveTxt は 数値 inline では Nothing" $
      resolveTxt emptyResolver (inline [1.0]) `shouldBe` Nothing

  describe "Layer Monoid" $ do
    it "scatter sets kind = MScatter" $
      getFirst (lyKind (scatter "x" "y")) `shouldBe` Just MScatter
    it "alpha 2 回 → 後勝ち (Last)" $
      let l = scatter "x" "y" <> alpha 0.5 <> alpha 0.7
      in getLast (lyAlpha l) `shouldBe` Just 0.7
    it "kind は First (= 先勝ち)、 別 kind を <> しても上書きされない" $
      let l = scatter "x" "y" <> line "x" "z"
      in getFirst (lyKind l) `shouldBe` Just MScatter
    it "mempty <> l == l (Monoid law)" $
      let l = scatter "x" "y" <> alpha 0.5
      in (mempty <> l) `shouldBe` l
    it "結合則 (a <> b) <> c == a <> (b <> c)" $
      let a = scatter "x" "y"
          b = alpha 0.5
          c = size 6
      in ((a <> b) <> c) `shouldBe` (a <> (b <> c))

  describe "Phase 30 A7: Point2 inline 形 (3D scatter3DPoints と対称)" $ do
    it "scatterPoints == scatter (inline xs) (inline ys)" $
      scatterPoints [Point2 1 2, Point2 3 4]
        `shouldBe` scatter (inline [1.0, 3.0]) (inline [2.0, 4.0])
    it "linePoints == line (inline xs) (inline ys)" $
      linePoints [Point2 1 2, Point2 3 4]
        `shouldBe` line (inline [1.0, 3.0]) (inline [2.0, 4.0])
    it "scatterPoints の kind = MScatter" $
      getFirst (lyKind (scatterPoints [Point2 0 0])) `shouldBe` Just MScatter
    it "Point2 JSON = positional array [x, y] (decode 往復)" $
      (eitherDecode "[1.5,2.5]" :: Either String Point2) `shouldBe` Right (Point2 1.5 2.5)

  describe "Phase 30 A8: alphaBy 連続 alpha encoding (= ggplot scale_alpha)" $ do
    it "alphaBy で lyAlphaBy が設定される" $
      getLast (lyAlphaBy (alphaBy "w")) `shouldBe` Just (ColByName "w")
    it "alphaVector: 列値 min..max → alpha [0.1, 1.0] に線形 map" $
      let ly = scatter "x" "y" <> alphaBy (inline [0.0, 5.0, 10.0])
          v  = alphaVector emptyResolver ly 0.85 3
      in (V.toList v) `shouldBe` [0.1, 0.55, 1.0]
    it "alphaVector: lyAlphaBy 無指定なら baseAlpha を全点に" $
      let ly = scatter "x" "y"
          v  = alphaVector emptyResolver ly 0.85 3
      in (V.toList v) `shouldBe` [0.85, 0.85, 0.85]
    it "alphaVector: 定数列 (min==max) は baseAlpha にフォールバック" $
      let ly = scatter "x" "y" <> alphaBy (inline [4.0, 4.0])
          v  = alphaVector emptyResolver ly 0.85 2
      in (V.toList v) `shouldBe` [0.85, 0.85]

  describe "colorRGBA: 8 桁 RGBA hex 便利関数 (= color (fromHex …) <> alpha …)" $ do
    it "colorRGBA \"#00887766\" == color (fromHex \"#008877\") <> alpha (0x66/255)" $
      colorRGBA "#00887766"
        `shouldBe` (color (fromHex "#008877") <> alpha (102/255))
    it "6 桁 (alpha 無し) は alpha=1.0 で不透明" $
      colorRGBA "#008877" `shouldBe` (color (fromHex "#008877") <> alpha 1.0)
    it "4 桁省略形 #rgba を展開 (#0876 → #008877 + alpha 0x66/255)" $
      colorRGBA "#0876" `shouldBe` (color (fromHex "#008877") <> alpha (102/255))
    it "fromHexAMaybe: 不正 hex は Nothing" $
      fromHexAMaybe "#zz" `shouldBe` Nothing
    it "colorRGBAMaybe: 正しい hex は Just" $
      colorRGBAMaybe "#00887766" `shouldBe` Just (color (fromHex "#008877") <> alpha (102/255))

  describe "VisualSpec Monoid" $ do
    it "purePlot == mempty" $
      purePlot `shouldBe` (mempty :: VisualSpec)
    it "title 2 回 → 後勝ち" $
      getLast (vsTitle (title "a" <> title "b")) `shouldBe` Just "b"
    it "layer を 2 つ <> すると vsLayers が 2 要素" $
      length (vsLayers (layer (scatter "x" "y") <> layer (line "x" "z")))
        `shouldBe` 2
    it "結合則 (top-level)" $
      let a = layer (scatter "x" "y")
          b = title "t"
          c = theme ThemeDark
      in ((a <> b) <> c) `shouldBe` (a <> (b <> c))

  describe "Layout" $ do
    it "computeLayout default viewport 468x288pt (= 6.5x4in・Phase 33 B8)" $
      let l = computeLayout emptyResolver mempty
      in (vsW (lpViewport l), vsH (lpViewport l)) `shouldBe` (468, 288)
    it "spec 指定 size は pt 空間で viewport に反映 (px は dpi で pt 化)" $
      -- ★ Phase 33 B4: layout は純 pt。1024px@96dpi = 768pt / 768px = 576pt
      --   (backend が k=dpi/72=4/3 を掛けて device px を復元するのは B5)。
      let l = computeLayout emptyResolver (widthUnit (1024 *~ px) <> heightUnit (768 *~ px))
      in (vsW (lpViewport l), vsH (lpViewport l)) `shouldBe` (768, 576)
    it "niceTicks 5 0 10 == [0,2..10]" $
      niceTicks 5 0 10 `shouldBe` [0, 2, 4, 6, 8, 10]
    -- Phase 8 C (§5 G3): extendedBreaks = R labeling::extended 移植。
    -- 既知の R 出力と照合 (Talbot-Lin-Hanrahan 2010 / ggplot2 既定 breaks)。
    it "G3 extendedBreaks 5 0 10 == [0,2.5,5,7.5,10]" $
      extendedBreaks 5 0 10 `shouldBe` [0, 2.5, 5, 7.5, 10]
    it "G3 extendedBreaks 5 0 100 == [0,25,50,75,100]" $
      extendedBreaks 5 0 100 `shouldBe` [0, 25, 50, 75, 100]
    it "G3 extendedBreaks 5 0 1 == [0,0.25,0.5,0.75,1]" $
      extendedBreaks 5 0 1 `shouldBe` [0, 0.25, 0.5, 0.75, 1]
    it "G3 extendedBreaks 5 1 9 == [0,2.5,5,7.5,10] (censor 前のデータ範囲基準)" $
      extendedBreaks 5 1 9 `shouldBe` [0, 2.5, 5, 7.5, 10]
    it "G3 extendedBreaks 退化域 (lo==hi) は単点" $
      extendedBreaks 5 2 2 `shouldBe` [2]
    -- Phase 8 C (gtable §E-1): solveTracks = Fixed 先取り → 残りを Null 重み比で配分。
    it "A-gtable solveTracks: Fixed 先取り + 単一 Null に残り" $
      solveTracks 0 100 [Fixed 20, Null 1, Fixed 30] `shouldBe` [(0,20),(20,50),(70,30)]
    it "A-gtable solveTracks: Null 重み比 (1:3 = 25:75)" $
      solveTracks 0 100 [Null 1, Null 3] `shouldBe` [(0,25),(25,75)]
    it "A-gtable solveTracks: origin offset 反映" $
      solveTracks 10 100 [Fixed 20, Null 1] `shouldBe` [(10,20),(30,80)]
    it "A-gtable solveTracks: Fixed 超過なら Null=0 (パネル潰れ)" $
      solveTracks 0 30 [Fixed 20, Fixed 20, Null 1] `shouldBe` [(0,20),(20,20),(40,0)]
    -- Phase 8 C G8: insetElement (patchwork 左下原点) = insetAt (左上原点) への変換。
    -- (left,bottom,right,top)=(0.5,0.5,1,1) 右上 → insetAt(x=0.5,y=0,w=0.5,h=0.5)。
    it "G8 insetElement (0.5,0.5,1,1) == insetAt (0.5,0,0.5,0.5)" $
      insetElement 0.5 0.5 1.0 1.0 mempty `shouldBe` insetAt 0.5 0.0 0.5 0.5 mempty
    it "scaleApply Linear 0..1 → 100..200 中点 150" $
      scaleApply (LinearScale 0 1 100 200) 0.5 `shouldBe` 150
    it "Phase 26 §C-2 #1: scaleApply Log 1..1000 → 0..300 中点 (=10) は ≈100" $
      abs (scaleApply (LogScale 1 1000 0 300) 10 - 100.0) `shouldSatisfy` (< 1e-9)
    it "Phase 26 §C-2 #1: niceTicksLog 5 1 10000 = [1,10,100,1000,10000]" $
      niceTicksLog 5 1 10000 `shouldBe` [1, 10, 100, 1000, 10000]
    it "Phase 26 §C-2 #1: xAxis logAxis を spec に与えると LogScale が出る" $
      let spec = layer (scatter (inline [1.0, 10.0, 100.0]) (inline [1.0, 4.0, 9.0]))
                   <> xAxis logAxis
      in case lpXScale (computeLayout emptyResolver spec) of
           LogScale{}    -> True `shouldBe` True
           LinearScale{} -> expectationFailure "expected LogScale"

    it "Phase 26 §E-1: traceLines (multi-chain) で chain ごとに線が分離 (= PLine 多数)" $
      let r n = case n of
            "iter"  -> Just (NumData (V.fromList [0, 1, 2, 0, 1, 2]))
            "value" -> Just (NumData (V.fromList [0.1, 0.2, 0.5, 0.0, 0.4, 0.6]))
            "chain" -> Just (TxtData (V.fromList ["1", "1", "1", "2", "2", "2"]))
            _ -> Nothing
          spec = layer (traceLines "iter" "value" "chain")
          ps = renderToPrimitives r (computeLayout r spec) spec
          lines_ = length [() | PLine{} <- ps]
      in lines_ `shouldSatisfy` (>= 4)  -- 2 chain × 2 segment 以上

    it "Phase 26 §E-6: dag で 3 node 2 edge の primitive 全体数 > 5 (= node shape + arrow + label)" $
      let nodes = [ dagNode "a" "alpha" NodeLatent 0.0 0.0
                  , dagNode "b" "beta"  NodeLatent 1.0 0.0
                  , dagNode "c" "y"     NodeObserved 0.5 1.0
                  ]
          edges_ = [ dagEdge "a" "c"
                   , dagEdge "b" "c"
                   ]
          spec = layer (dag nodes edges_)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
      in length ps `shouldSatisfy` (> 5)

    it "Phase 26 §E-6: dagPlot (Graph builder + ~>) で arrow PPath 含む" $
      let g = ("alpha" :: Data.Text.Text) ~> "y" <> "beta" ~> "y"
          spec = layer (Graphics.Hgg.DAG.dagPlot g)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          paths = length [() | PPath{} <- ps]
      in paths `shouldSatisfy` (>= 2)  -- arrow head + node 楕円 で複数

    it "Phase 26 A2: quiver は零でない矢印 1 本につき 3 PLine (本線 + 矢じり 2)" $
      -- 軸/格子線も PLine なので、 同じ x/y で全零ベクトル版との差分 = 矢印分だけ。
      -- 非零 2 本 (2 本目は零ベクトルで非描画) → 差分 = 2 × 3 = 6。
      -- ★ Phase 36 A: 矢印は元レンジのまま plotArea でクリップ (range 非拡張・clip は
      --   primitive 数不変) なので、 両版の軸/格子線は一致し差分 = 矢印分だけ。
      let xs = inline [0.0, 1.0, 2.0]; ys = inline [0.0, 0.0, 0.0]
          mkLines us vs =
            let spec = layer (quiver xs ys us vs)
                ps = renderToPrimitives emptyResolver (computeLayout emptyResolver spec) spec
            in length [() | PLine{} <- ps]
          withArrows = mkLines (inline [1.0, 0.0, 1.0]) (inline [0.0, 0.0, 1.0])
          noArrows   = mkLines (inline [0.0, 0.0, 0.0]) (inline [0.0, 0.0, 0.0])
      in (withArrows - noArrows) `shouldBe` 6

    it "Phase 26 A2: quiver requiredAes = x/y/u/v・layerCols で 4 列解決" $ do
      let ly = quiver (inline [0.0]) (inline [0.0]) (inline [1.0]) (inline [1.0])
      requiredAes MQuiver `shouldBe` [AesX, AesY, AesU, AesV]
      length (layerCols ly) `shouldBe` 4

    it "Phase 26 §C-2 #13: parallelCoords 3 列 で N+1 軸線 (= 3 軸) が出る" $
      let spec = layer (parallelCoords [ inline [1.0, 2.0, 3.0]
                                       , inline [4.0, 5.0, 6.0]
                                       , inline [7.0, 8.0, 9.0] ])
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          -- 縦軸 3 本以上 (= 軸 + 各 row の polyline)
          lines_ = length [() | PLine{} <- ps]
      in lines_ `shouldSatisfy` (>= 3)

    it "Phase 26 §C-2 #10: marginal で X/Y histogram の PRect が追加される" $
      let baseSpec = layer (scatter (inline [0.0, 1.0, 2.0, 3.0, 4.0])
                                   (inline [0.0, 1.0, 4.0, 9.0, 16.0]))
          extSpec  = baseSpec <> marginal
          n0 = length [() | PRect{} <- renderToPrimitives emptyResolver
                              (computeLayout emptyResolver baseSpec) baseSpec]
          n1 = length [() | PRect{} <- renderToPrimitives emptyResolver
                              (computeLayout emptyResolver extSpec) extSpec]
      in (n1 - n0) `shouldSatisfy` (>= 20)  -- 20 bins × 2 軸 minimum

    it "Phase 26 §C-2 #12: facet 3 値 で panel が 3 つ出る (= 各 panel の header PText)" $
      let r n = case n of
                  "x" -> Just (NumData (V.fromList [1, 2, 3, 1, 2, 3, 1, 2, 3]))
                  "y" -> Just (NumData (V.fromList [1, 4, 9, 1, 4, 9, 1, 4, 9]))
                  "g" -> Just (TxtData (V.fromList ["A", "A", "A", "B", "B", "B", "C", "C", "C"]))
                  _   -> Nothing
          spec = layer (scatter "x" "y") <> facet "g"
          ps   = renderToPrimitives r (computeLayout r spec) spec
          texts = [t | PText _ t _ <- ps]
      in do
           ("A" `elem` texts) `shouldBe` True
           ("B" `elem` texts) `shouldBe` True
           ("C" `elem` texts) `shouldBe` True

    it "Phase 26 §C-2 #8: statMean が水平 PLine を 1 本生成 (= renderStatLine 直接 check)" $
      let r n = case n of
                  "y" -> Just (NumData (V.fromList [0, 1, 4, 9, 16]))
                  _   -> Nothing
          spec = layer (statMean "y")
          ps   = renderToPrimitives r (computeLayout r spec) spec
          -- 軸 tick の PLine も含まれるが、 lyKind = MStatMean の layer は 1 本だけ生成
          -- 確認用: PLine の中で plot area 幅の水平線 = stat line
          a = lpPlotArea (computeLayout r spec)
          isHorizFull (PLine (Point x1 _) (Point x2 _) _) =
            abs (x1 - rX a) < 0.01 && abs (x2 - (rX a + rW a)) < 0.01
          isHorizFull _ = False
          fullHoriz = filter isHorizFull ps
      in length fullHoriz `shouldSatisfy` (>= 1)
    it "Phase 26 §C-2 #15: MScatter3D を含めても render は通る (= placeholder)" $
      let spec = layer (mempty { lyKind = pure MScatter3D })
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
      in length [() | PCircle{} <- ps] `shouldBe` 0  -- 3D は描画しない

    it "Phase 60: tile が連続軸で 4 セルを隙間なくベタ塗り + カテゴリ 2 色 (離散 colorBy)" $
      -- 2×2 の決定グリッド (x∈{0,1}, y∈{0,1}, class A/B) を tile で塗る。
      let r n = case n of
            "x" -> Just (NumData (V.fromList [0, 1, 0, 1]))
            "y" -> Just (NumData (V.fromList [0, 0, 1, 1]))
            "c" -> Just (TxtData (V.fromList ["A", "A", "B", "B"]))
            _   -> Nothing
          spec = layer (tile "x" "y" "c")
          ps   = renderToPrimitives r (computeLayout r spec) spec
          -- tile セル = 枠なし PRect・非白・大 (背景や凡例 chip を幅で除外)
          cells = [ (x, y, w, col)
                  | PRect (Rect x y w _) (FillStyle col _) Nothing <- ps
                  , col /= "#ffffff", w > 100 ]
          colors = Data.List.nub [ c | (_, _, _, c) <- cells ]
          rows = Data.List.groupBy (\(_,y1,_,_) (_,y2,_,_) -> abs (y1 - y2) < 0.01)
                   (Data.List.sortOn (\(_,y,_,_) -> y) cells)
          -- 同一 row の隣接 2 セル: 左の右端 == 右の左端 (隙間なし)
          gapFree row = case Data.List.sortOn (\(x,_,_,_) -> x) row of
            ((x1,_,w1,_) : (x2,_,_,_) : _) -> abs ((x1 + w1) - x2) < 0.01
            _                              -> False
      in do
           length cells  `shouldBe` 4          -- 2×2 = 4 セル
           length colors `shouldBe` 2          -- カテゴリ A/B → 離散 2 色
           all gapFree rows `shouldBe` True    -- 隙間なし (格子間隔で敷き詰め)

    it "Phase 26 §C-2 #6: errorY で各点 3 本 (vertical + 2 cap) 追加、 3 点 = 9 本" $
      let r n = case n of
                  "x"  -> Just (NumData (V.fromList [0, 1, 2]))
                  "y"  -> Just (NumData (V.fromList [0, 1, 4]))
                  "ey" -> Just (NumData (V.fromList [0.5, 0.3, 0.8]))
                  _    -> Nothing
          baseSpec = layer (scatter "x" "y")
          errSpec  = layer (scatter "x" "y" <> errorY "ey")
          n0 = length [() | PLine{} <- renderToPrimitives r
                              (computeLayout r baseSpec) baseSpec]
          n1 = length [() | PLine{} <- renderToPrimitives r
                              (computeLayout r errSpec) errSpec]
      in (n1 - n0) `shouldBe` 9

    it "Phase 26 §C-2 #5: scatter + connect で PLine が n-1 本追加" $
      let baseSpec = layer (scatter (inline [0.0, 1.0, 2.0, 3.0])
                                   (inline [0.0, 1.0, 4.0, 9.0]))
          withCSpec = layer (scatter (inline [0.0, 1.0, 2.0, 3.0])
                                    (inline [0.0, 1.0, 4.0, 9.0])
                              <> connect)
          n0 = length [() | PLine{} <- renderToPrimitives emptyResolver
                              (computeLayout emptyResolver baseSpec) baseSpec]
          n1 = length [() | PLine{} <- renderToPrimitives emptyResolver
                              (computeLayout emptyResolver withCSpec) withCSpec]
      in (n1 - n0) `shouldBe` 3

    it "Phase 26 §C-2 #4: hoverCols で PCircle の title が col 値を含む" $
      let r n = case n of
                  "x" -> Just (NumData (V.fromList [0, 1, 2]))
                  "y" -> Just (NumData (V.fromList [0, 1, 4]))
                  "g" -> Just (NumData (V.fromList [10, 20, 30]))
                  _   -> Nothing
          spec = layer (scatter "x" "y" <> hoverCols ["g"])
          ps   = renderToPrimitives r (computeLayout r spec) spec
          labels = [t | PCircle _ _ _ _ (Just t) <- ps]
      in any (Data.Text.isInfixOf "g: 10") labels `shouldBe` True

    it "Phase 26 §C-2 #3: refIdentity を付けると y=x の PLine が 1 本 追加" $
      let baseSpec = layer (scatter (inline [0.0, 1.0, 2.0]) (inline [0.0, 1.0, 4.0]))
          plain   = renderToPrimitives emptyResolver
                      (computeLayout emptyResolver baseSpec) baseSpec
          withRef = renderToPrimitives emptyResolver
                      (computeLayout emptyResolver (baseSpec <> refIdentity))
                      (baseSpec <> refIdentity)
          n1 = length [() | PLine{} <- plain]
          n2 = length [() | PLine{} <- withRef]
      in (n2 - n1) `shouldBe` 1
    it "Phase 26 §C-2 #3: refHorizontal 3 + refVertical 1 で計 +2 PLine" $
      let baseSpec = layer (scatter (inline [0.0, 1.0, 2.0]) (inline [0.0, 1.0, 4.0]))
          extSpec  = baseSpec <> refHorizontal 3 <> refVertical 1
          n1 = length [() | PLine{} <- renderToPrimitives emptyResolver
                              (computeLayout emptyResolver baseSpec) baseSpec]
          n2 = length [() | PLine{} <- renderToPrimitives emptyResolver
                              (computeLayout emptyResolver extSpec) extSpec]
      in (n2 - n1) `shouldBe` 2

    it "Phase 26 §C-2 #2: AxisDecimalFmt 2 が tick 表示に反映 ('1.50' 等)" $
      let spec = layer (scatter (inline [0.0, 1.0, 2.0]) (inline [0.0, 1.0, 4.0]))
                   <> yAxis (axisFormat (AxisDecimalFmt 2))
          ps   = renderToPrimitives emptyResolver
                   (computeLayout emptyResolver spec) spec
          texts = [t | PText _ t _ <- ps]
          hasDot2 t = case Data.Text.breakOn "." t of
            (_, suffix) | Data.Text.length suffix == 3 -> True
            _ -> False
          decimal2 = filter hasDot2 texts
      in length decimal2 `shouldSatisfy` (>= 1)

  describe "Render" $ do
    it "scatter 3 点で PCircle 3 個" $
      let spec = layer (scatter (inline [0, 1, 2 :: Double])
                               (inline [0, 1, 4 :: Double]))
          ps   = renderToPrimitives emptyResolver
                   (computeLayout emptyResolver spec) spec
      in length [() | PCircle{} <- ps] `shouldBe` 3
    it "line 4 点で PLine 3 本 (= n-1 本)" $
      let spec = layer (line (inline [0, 1, 2, 3 :: Double])
                            (inline [0, 1, 4, 9 :: Double]))
          ps   = renderToPrimitives emptyResolver
                   (computeLayout emptyResolver spec) spec
          -- axisFrame + tickMarks にも PLine が混ざるので line layer 由来だけ
          -- 抽出するのは難しい。 ここでは全体の PLine 数だけ check (= 軸 tick
          -- 6 個 + line 3 本 + xMark/yMark 各 12 本程度 = それなりの数)
          nLines = length [() | PLine{} <- ps]
      in nLines `shouldSatisfy` (>= 3)
    it "ColByName で resolver から解決して描画" $
      let r n = case n of
                  "x" -> Just (NumData (V.fromList [0, 1, 2]))
                  "y" -> Just (NumData (V.fromList [0, 1, 4]))
                  _   -> Nothing
          spec = layer (scatter "x" "y")
          ps   = renderToPrimitives r (computeLayout r spec) spec
      in length [() | PCircle{} <- ps] `shouldBe` 3
    it "boxplot は PRect (箱) + PLine (median/髭) の組合せを出す" $
      let spec = layer (boxplot (inline [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 100.0]))
          ps   = renderToPrimitives emptyResolver
                   (computeLayout emptyResolver spec) spec
          nRects = length [() | PRect{} <- ps]
      in nRects `shouldSatisfy` (>= 2)  -- axis frame + box の最低 2 個

    it "density は PPath を 1 つ出す" $
      let spec = layer (density (inline [1.0, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0]))
          ps   = renderToPrimitives emptyResolver
                   (computeLayout emptyResolver spec) spec
      in length [() | PPath{} <- ps] `shouldBe` 1

    -- Phase 52.B1: subplots の入れ子が再帰描画される (renderSingle → renderToPrimitives、
    -- PS Render/Layer.purs:442 と同一方式)。 外側 subplots が内側 subplots を含むとき、
    -- 内側 panel の scatter 点まで描かれることを確認 (修正前は内側が無視され 3 点のみ)。
    it "B1 入れ子 subplots: 内側 scatter の点まで全部描画される" $
      let pts   = layer (scatter (inline [0, 1, 2 :: Double])
                                 (inline [0, 1, 4 :: Double]))
          inner = subplots [pts, pts] <> subplotCols 2   -- 内側: 2 panel × 3 点 = 6
          outer = subplots [inner, pts] <> subplotCols 1 -- 外側: 入れ子 + 単独 3 点
          ps    = renderToPrimitives emptyResolver
                    (computeLayout emptyResolver outer) outer
      in length [() | PCircle{} <- ps] `shouldBe` 9      -- 6 (入れ子) + 3 (単独)

    -- Phase 52.D (concat 合成): hconcat/vconcat ラッパ + 演算子 <-> (横) / <:> (縦)。
    -- subplots+subplotCols の薄ラッパで render/parity 影響なし。 演算子は同方向チェーンを
    -- 平坦化する (a <-> b <-> c = 3 等分列、 二項ネストにしない)。
    it "concat: hconcat [a,b,c] = subplots 3 要素 + subplotCols 3" $
      let s = hconcat [purePlot, purePlot, purePlot]
      in (length (vsSubplots s), getLast (vsSubplotCols s)) `shouldBe` (3, Just 3)

    it "concat: vconcat [a,b] = subplots 2 要素 + subplotCols 1" $
      let s = vconcat [purePlot, purePlot]
      in (length (vsSubplots s), getLast (vsSubplotCols s)) `shouldBe` (2, Just 1)

    it "concat: a <-> b <-> c は 3 要素に平坦化 (二項ネストでなく 3 等分列)" $
      let s = purePlot <-> purePlot <-> purePlot
      in (length (vsSubplots s), getLast (vsSubplotCols s)) `shouldBe` (3, Just 3)

    it "concat: <:> は横グループを単位として扱う (外側 cols 1・2 要素)" $
      let s = (purePlot <-> purePlot <-> purePlot) <:> purePlot
      in (length (vsSubplots s), getLast (vsSubplotCols s)) `shouldBe` (2, Just 1)

    it "concat: (a <-> b <-> c) <:> d == vconcat [hconcat [a,b,c], d] (同一 spec 構造)" $
      let a        = purePlot
          shape s  = ( getLast (vsSubplotCols s), length (vsSubplots s)
                     , [ (getLast (vsSubplotCols x), length (vsSubplots x)) | x <- vsSubplots s ] )
          opForm   = (a <-> a <-> a) <:> a
          listForm = vconcat [hconcat [a, a, a], a]
      in shape opForm `shouldBe` shape listForm

    it "concat: (a <-> b <-> c) <:> d を実描画すると 1 行目 3 列 + 2 行目で全パネル描画" $
      let a        = layer (scatter (inline [0, 1, 2 :: Double]) (inline [0, 1, 4 :: Double]))
          spec     = (a <-> a <-> a) <:> a
          ps       = renderToPrimitives emptyResolver (computeLayout emptyResolver spec) spec
      in length [() | PCircle{} <- ps] `shouldBe` 12     -- 4 パネル × 3 点

    -- Phase 18 A1: selectPanels (= subplot panel の名前選択 + 列挙順並べ替え)。
    -- panel 名 = 子 spec の vsTitle。 ggplot discrete limits と同じ「選択 + 順序」。
    let selPts  = layer (scatter (inline [0, 1, 2 :: Double]) (inline [0, 1, 4 :: Double]))
        selPanel nm = selPts <> title nm
        selGrid = subplots [selPanel "a", selPanel "b", selPanel "c"]
    it "P18 selectPanels: 名前で選択し列挙順に並べ替える" $
      let s = selGrid <> selectPanels ["c", "a"]
      in map (getLast . vsTitle) (selectedSubplots s)
           `shouldBe` [Just "c", Just "a"]

    it "P18 selectPanels: 不一致名は無視 (存在する名前だけ残る)" $
      let s = selGrid <> selectPanels ["zzz", "b"]
      in map (getLast . vsTitle) (selectedSubplots s) `shouldBe` [Just "b"]

    it "P18 selectPanels: 未指定なら全 panel をそのまま返す (従来不変)" $
      map (getLast . vsTitle) (selectedSubplots selGrid)
        `shouldBe` [Just "a", Just "b", Just "c"]

    it "P18 selectPanels: title 無し panel は選択時に落ちる" $
      let s = subplots [selPts, selPanel "a"] <> selectPanels ["a"]
      in length (selectedSubplots s) `shouldBe` 1

    it "P18 selectPanels: 実描画で選択 panel の点だけ描かれる" $
      let s  = selGrid <> selectPanels ["a", "c"] <> subplotCols 2
          ps = renderToPrimitives emptyResolver (computeLayout emptyResolver s) s
      in length [() | PCircle{} <- ps] `shouldBe` 6      -- 2 パネル × 3 点

    -- Phase 18 A2: scale{X,Y}DiscreteLimits (= ggplot scale_*_discrete(limits=))。
    -- ColTxt encoding の layer のカテゴリ行を選択 + 列挙順に並べ替え (全 encoding 整合)。
    it "P18 discrete limits (Y): forest の行を選択 + 列挙順に並べ替え (encX/errorX も追従)" $
      let s  = layer (forest (inlineCat ["a", "b", "c" :: Data.Text.Text])
                             (inline [1, 2, 3 :: Double])
                             (inline [0.1, 0.2, 0.3 :: Double]))
               <> scaleYDiscreteLimits ["c", "a"]
          l  = head (vsLayers (applyDiscreteLimits emptyResolver s))
          cat = case getLast (lyEncY l) of Just (ColTxt v) -> V.toList v; _ -> []
          est = case getLast (lyEncX l) of Just (ColNum v) -> V.toList v; _ -> []
          err = case getLast (lyErrorX l) of Just (ColNum v) -> V.toList v; _ -> []
      in (cat, est, err) `shouldBe` (["c", "a"], [3, 1], [0.3, 0.1])

    it "P18 discrete limits (X): bar の実描画で選択カテゴリの本数だけ PRect が出る" $
      let mk lim = let s = layer (bar (inlineCat ["p", "q", "r" :: Data.Text.Text])
                                      (inline [1, 2, 3 :: Double])) <> lim
                   in length [ () | PRect{} <- renderToPrimitives emptyResolver
                                                 (computeLayout emptyResolver s) s ]
      in (mk (scaleXDiscreteLimits ["r", "p"]), mk mempty)
           `shouldBe` (mk mempty - 1, mk mempty)   -- bar 3→2 本 (他 PRect は不変)

    it "P18 discrete limits: coord_flip と直交 (flip 後も aes 基準で効く)" $
      let mk lim = let s = layer (bar (inlineCat ["p", "q", "r" :: Data.Text.Text])
                                      (inline [1, 2, 3 :: Double]))
                           <> coordFlip <> lim
                   in length [ () | PRect{} <- renderToPrimitives emptyResolver
                                                 (computeLayout emptyResolver s) s ]
      in mk (scaleXDiscreteLimits ["p"]) `shouldBe` mk mempty - 2  -- 3→1 本

    it "P18 discrete limits: ColByName 列も resolver 経由 (bake) で filter される" $
      let res n = case n of
            "g" -> Just (TxtData (V.fromList ["p", "q", "r"]))
            "v" -> Just (NumData (V.fromList [1, 2, 3]))
            _   -> Nothing
          s  = layer (bar (ColByName "g") (ColByName "v"))
               <> scaleXDiscreteLimits ["q"]
          l  = head (vsLayers (applyDiscreteLimits res s))
          cat = case getLast (lyEncX l) of Just (ColTxt v) -> V.toList v; _ -> []
      in cat `shouldBe` ["q"]

    -- Phase 52.D2: streamgraph (= 中心化積層 area)。 color aes で系列分割し、 各系列を
    -- 塗り polygon (PPath) で描く。 baseline は -(Σy)/2 から (silhouette 中心化)。
    let streamR n = case n of
          "t" -> Just (NumData (V.fromList [0,1,2, 0,1,2 :: Double]))
          "v" -> Just (NumData (V.fromList [1,2,3, 2,2,1 :: Double]))
          "g" -> Just (TxtData (V.fromList ["a","a","a","b","b","b"]))
          _   -> Nothing
    it "D2 stream: 2 系列で PPath を 2 枚 (= 系列数ぶん) 出す" $
      let spec = layer (stream "t" "v" <> colorBy "g")
          ps   = renderToPrimitives streamR (computeLayout streamR spec) spec
      in length [() | PPath{} <- ps] `shouldBe` 2

    it "D2 stream: 1 系列なら PPath 1 枚" $
      let r1 n = case n of
            "t" -> Just (NumData (V.fromList [0,1,2 :: Double]))
            "v" -> Just (NumData (V.fromList [1,2,3 :: Double]))
            "g" -> Just (TxtData (V.fromList ["a","a","a"]))
            _   -> Nothing
          spec = layer (stream "t" "v" <> colorBy "g")
          ps   = renderToPrimitives r1 (computeLayout r1 spec) spec
      in length [() | PPath{} <- ps] `shouldBe` 1

    it "D2 stream: 中心化積層で y domain が負側に広がる (baseline=-Σy/2)" $
      let spec   = layer (stream "t" "v" <> colorBy "g")
          layout = computeLayout streamR spec
          -- 各 x 総和 max M=4 (x=1,2 で 2+2 / 3+1) → range [-2,2] を含む (pad で更に外側)
      in lsDomainLo (lpYScale layout) `shouldSatisfy` (< 0)

    -- Phase 52.D1: repeatFields = フィールド名を反復し 1 view/フィールドを生成して
    -- subplots に並べる (Vega-Lite repeat 相当)。 3 フィールド × 3 点 scatter = 9 circle。
    it "D1 repeatFields: フィールド数ぶんの panel が subplots に展開される" $
      let mk _f = layer (scatter (inline [0, 1, 2 :: Double])
                                 (inline [0, 1, 4 :: Double]))
          spec  = repeatFields (["a", "b", "c"] :: [Data.Text.Text]) mk
                    <> subplotCols 3
          ps    = renderToPrimitives emptyResolver
                    (computeLayout emptyResolver spec) spec
      in length [() | PCircle{} <- ps] `shouldBe` 9

    -- Phase 52.A11: DAG (MDAG・renderDAGOnly 経路) を subplot セル内に置くと、 修正前は
    -- area を viewport (subplot では 0 に潰れる) から絶対原点 (40,50) で作っていたため、
    -- DAG が自セルを無視し図全体の左上に漏れていた。 修正後は viewport=0 を subplot 文脈と
    -- 見て base 矩形を lpPlotArea (panelRect) に切替えるため各セルに収まる。 2 列に DAG を
    -- 並べ、 ノードラベル (PText) が左半分・右半分の両方に出ることを確認 (修正前は全て左上)。
    it "A11 subplot 内 DAG: 各セルに収まる (左右両半分にノードが出る)" $
      let dagSpec = layer (Graphics.Hgg.DAG.dagPlot
                            (("a" :: Data.Text.Text) ~> "b"))
          spec    = subplots [dagSpec, dagSpec] <> subplotCols 2  -- 横 2 セル
          lay     = computeLayout emptyResolver spec
          -- 図中点 (= viewport 幅の半分)。既定サイズ非依存に左右セルを判定する。
          midX    = fromIntegral (vsW (lpViewport lay)) / 2
          ps      = renderToPrimitives emptyResolver lay spec
          textXs  = [ x | PText (Point x _) _ _ <- ps ]
      in (any (> midX) textXs, any (< midX) textXs) `shouldBe` (True, True)

    -- Phase 8 C G7: facet_wrap 複数行 (5 群 ncol=3 → 2 行)。 全点が各 panel に描かれ、
    -- panel frame が 5 枚 + background で PRect >= 6 (= 折り返しても panel が潰れない)。
    it "G7 facetWrap 5 群 ncol=3: 全 20 点描画 + panel frame 5 枚" $
      let r n = case n of
                  "x" -> Just (NumData (V.fromList (concat (replicate 5 [1,2,3,4]))))
                  "y" -> Just (NumData (V.fromList
                           [1,4,9,16,2,5,8,12,3,6,9,15,2,3,7,10,4,8,11,14]))
                  "g" -> Just (TxtData (V.fromList
                           (concatMap (replicate 4) ["A","B","C","D","E"])))
                  _   -> Nothing
          spec = layer (scatter "x" "y" <> size 6) <> facetWrap "g" 3
          ps   = renderToPrimitives r (computeLayout r spec) spec
          nCircles = length [() | PCircle{} <- ps]
          nRects   = length [() | PRect{} <- ps]
      in (nCircles, nRects >= 6) `shouldBe` (20, True)

    it "ColorByCol で categorical 3 値 → 3 色の Okabe-Ito palette" $
      let r n = case n of
                  "x" -> Just (NumData (V.fromList [0, 1, 2, 3, 4, 5]))
                  "y" -> Just (NumData (V.fromList [0, 1, 4, 9, 16, 25]))
                  "g" -> Just (TxtData (V.fromList ["a", "b", "c", "a", "b", "c"]))
                  _   -> Nothing
          spec = layer (scatter "x" "y" <> colorBy "g")
          ps   = renderToPrimitives r (computeLayout r spec) spec
          colors = [c | PCircle _ _ (FillStyle c _) _ _ <- ps]
      in length (Data.List.nub colors) `shouldBe` 3

  describe "Phase 1 A2: Sugiyama rank assignment (= network simplex framework)" $ do
    it "linear chain a→b→c は rank 0,1,2" $
      let lg = Sugi.assignRanks
                 (Sugi.buildLayoutGraph ["a", "b", "c"]
                                        [("a", "b"), ("b", "c")])
          rankOf x = head [ Sugi.lnRank n | n <- Sugi.lgNodes lg, Sugi.lnId n == x ]
      in (rankOf "a", rankOf "b", rankOf "c") `shouldBe` (0, 1, 2)

    it "diamond a→b, a→c, b→d, c→d は a=0, b=c=1, d=2" $
      let lg = Sugi.assignRanks
                 (Sugi.buildLayoutGraph ["a", "b", "c", "d"]
                                        [("a","b"),("a","c"),("b","d"),("c","d")])
          rankOf x = head [ Sugi.lnRank n | n <- Sugi.lgNodes lg, Sugi.lnId n == x ]
      in [rankOf "a", rankOf "b", rankOf "c", rankOf "d"] `shouldBe` [0, 1, 1, 2]

    it "孤立 node は rank 0" $
      let lg = Sugi.assignRanks (Sugi.buildLayoutGraph ["x"] [])
      in map Sugi.lnRank (Sugi.lgNodes lg) `shouldBe` [0]

    it "結果は常に feasible (= rank(v) - rank(u) ≥ δ)" $
      let lg = Sugi.assignRanks
                 (Sugi.buildLayoutGraph ["a","b","c","d","e"]
                                        [("a","b"),("a","c"),("b","d"),("c","d"),("d","e"),("a","e")])
      in Sugi.isFeasible lg `shouldBe` True

  describe "Step3.1: 汎用 network simplex (networkSimplex, P4a x 座標ソルバ)" $ do
    let feasibleAll es r = all (\(t, h, d, _) ->
                                  Map.findWithDefault 0 h r - Map.findWithDefault 0 t r >= d) es
        obj es r = sum [ w * fromIntegral (Map.findWithDefault 0 h r - Map.findWithDefault 0 t r)
                       | (t, h, _, w) <- es ] :: Double

    it "一様 δ=ω=1 diamond は longest-path と一致 (a0 b1 c1 d2)" $
      let es = [("a","b",1,1),("a","c",1,1),("b","d",1,1),("c","d",1,1)]
          r  = Sugi.networkSimplex ["a","b","c","d"] es
      in (Map.findWithDefault (-1) "a" r, Map.findWithDefault (-1) "b" r,
          Map.findWithDefault (-1) "c" r, Map.findWithDefault (-1) "d" r)
           `shouldBe` (0, 1, 1, 2)

    it "longest-path が非最適な異δ案件で最適目的値に到達 (a→c δ1, b→c δ5 → obj 6)" $
      let es = [("a","c",1,1),("b","c",5,1)]
          r  = Sugi.networkSimplex ["a","b","c"] es
      in (feasibleAll es r, obj es r) `shouldBe` (True, 6)

    it "Ω 重み (1:8) で重い chain を直線化 (t0 m1 b2)" $
      let es = [("t","m",1,8),("m","b",1,8),("t","b",2,1)]
          r  = Sugi.networkSimplex ["t","m","b"] es
      in (Map.findWithDefault (-1) "t" r, Map.findWithDefault (-1) "m" r,
          Map.findWithDefault (-1) "b" r, feasibleAll es r)
           `shouldBe` (0, 1, 2, True)

    it "孤立 node は 0" $
      Sugi.networkSimplex ["x","y"] [] `shouldBe` Map.fromList [("x",0),("y",0)]

    it "非連結成分は独立に解け各成分の最小が 0" $
      let es = [("a","b",1,1),("c","d",3,1)]
          r  = Sugi.networkSimplex ["a","b","c","d"] es
      in (feasibleAll es r,
          Map.findWithDefault (-1) "a" r, Map.findWithDefault (-1) "b" r,
          Map.findWithDefault (-1) "c" r, Map.findWithDefault (-1) "d" r)
           `shouldBe` (True, 0, 1, 0, 3)

  describe "Step3.2: aux-graph x 座標 (P4a, dummy 直線化 + chain body 外分離)" $ do
    -- 長 edge (= 自 chain と並走する skip) の dummy 列が、 chain node の body の
    -- 外へ出て、 かつ Ω=8 直線化で collinear (= 同 x) になることを assignCoords 経由で検証。
    -- これが P4a の核心 (= large funnel collapse の layout 層 主因の根治)。
    it "並走 skip の dummy は collinear (= 同 x、 |Δx| < 1e-9)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["a0","a1","a2","a3","a4"]
                 [("a0","a1"),("a1","a2"),("a2","a3"),("a3","a4")  -- chain
                 ,("a0","a4")]                                      -- 並走 skip (dummy 3 個)
          (g1, om) = Sugi.assignOrder g0
          coords = Sugi.assignCoords [] g1 om
          dumXs = [ x | (k, x) <- Map.toList coords, Data.Text.isPrefixOf "__dummy_" k ]
      in case dumXs of
           [] -> expectationFailure "dummy が無い (skip が dummy 化されていない)"
           _  -> maximum dumXs - minimum dumXs `shouldSatisfy` (< 1e-9)

    it "並走 skip の dummy 列は chain node 列から分離 (= 同 x でない)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["a0","a1","a2","a3","a4"]
                 [("a0","a1"),("a1","a2"),("a2","a3"),("a3","a4"),("a0","a4")]
          (g1, om) = Sugi.assignOrder g0
          coords = Sugi.assignCoords [] g1 om
          dumX = head [ x | (k, x) <- Map.toList coords, Data.Text.isPrefixOf "__dummy_" k ]
          chainX = Map.findWithDefault (-1) "a1" coords  -- 中間 chain node
      in abs (dumX - chainX) `shouldSatisfy` (> 1e-6)

    -- Phase 39 Step8 (P8) A1: cluster border 制約 (graphviz pos_clusters) を
    -- P4a aux simplex へ注入。 plate メンバに左右 border node + contain/keepout
    -- edge を張り、 非メンバが box の外へ・box が tight になることを raw 座標で検証。
    it "P8 A1 keepout: 非メンバ q が plate メンバ x 区間の外 (auxSimplexCoords)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["r","p0","p1","q"]
                 [("r","p0"),("r","p1"),("r","q")]
          (g1, om0) = Sugi.assignOrder g0
          om = Sugi.applyPlateConstraints [["p0","p1"]] om0
          c  = Sugi.auxSimplexCoords [["p0","p1"]] g1 om
          ps = [c Map.! "p0", c Map.! "p1"]
          q  = c Map.! "q"
      in (q < minimum ps || q > maximum ps) `shouldBe` True

    it "P8 A1 keepout: plate 有りは非メンバ⇄member 間隔が plate 無し以上 (border margin)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["r","p0","p1","q"]
                 [("r","p0"),("r","p1"),("r","q")]
          (g1, om0) = Sugi.assignOrder g0
          -- 同一 order (plate 制約済) に対し border edge の有無だけ変える公正比較
          om = Sugi.applyPlateConstraints [["p0","p1"]] om0
          gap pl = let c = Sugi.auxSimplexCoords pl g1 om
                       ps = [c Map.! "p0", c Map.! "p1"]
                   in minimum [ abs (c Map.! "q" - p) | p <- ps ]
      in gap [["p0","p1"]] `shouldSatisfy` (>= gap [])

    -- Phase 39 P8 A4-2 separate_subclust: 同 rank に並ぶ兄弟 plate (= 包含関係に無い)
    -- の隣接 border 間に graphviz @make_aux_edge(rn_left, ln_right, CL_OFFSET, 0)@ を
    -- 張り、 兄弟 plate box が重ならないよう CL_OFFSET ぶんの隙間を simplex 解に確保する。
    -- faithful 証拠 = 兄弟 plate 間の member gap が plate 内 member gap より広いこと
    -- (= border contain margin + CL_OFFSET が plate 内 nodesep を上回る・raw 座標で検証)。
    it "P8 A4-2 separate_subclust: 兄弟 plate 間 gap > plate 内 gap (raw simplex)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["r","p0","p1","q0","q1"]
                 [("r","p0"),("r","p1"),("r","q0"),("r","q1")]
          (g1, om0) = Sugi.assignOrder g0
          plates = [["p0","p1"], ["q0","q1"]]
          om = Sugi.applyPlateConstraints plates om0
          c  = Sugi.auxSimplexCoords plates g1 om
          -- 4 member の x を昇順に。 plate 内 2 member は連続するので
          -- 並びは [plateL_m0, plateL_m1, plateR_m0, plateR_m1]。
          [a, b, cc, d] = sort [c Map.! k | k <- ["p0","p1","q0","q1"]]
          gMid   = cc - b   -- 兄弟 plate 間 (separate_subclust + border margin)
          gLeft  = b  - a   -- 左 plate 内 (nodesep のみ)
          gRight = d  - cc  -- 右 plate 内 (nodesep のみ)
      in (gMid > gLeft, gMid > gRight) `shouldBe` (True, True)

    -- Phase 39 P8 A4-2 完全忠実 point pipeline: 'auxSimplexCoordsW' は per-node 実半幅
    -- (hwMap) を LR 制約 'auxSepOf' に反映する (= graphviz の point 一貫 layout)。
    -- 幅広 node は隣接 sep を押し広げるため、 同 rank 全体の span が広がることを検証する。
    it "P8 A4-2 point pipeline: 幅広 node は同 rank の span を広げる (size-aware)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["r","a","b","c"]
                 [("r","a"),("r","b"),("r","c")]
          (g1, om) = Sugi.assignOrder g0
          spanOf m = let xs = [m Map.! k | k <- ["a","b","c"]]
                     in maximum xs - minimum xs
          narrow = Sugi.auxSimplexCoordsW Map.empty [] g1 om          -- 一律 fallback 半幅
          wide   = Sugi.auxSimplexCoordsW (Map.fromList [("b", 80)]) [] g1 om
      in spanOf wide `shouldSatisfy` (> spanOf narrow)

    -- Phase 19 A4: rank 引き締め (source 引き下げ + エッジ無し plate メンバ)
    it "tightenSourceRanks: 深い消費者を持つ source は直前 rank へ (a→b→c, s→c)" $
      let lg = Sugi.tightenSourceRanks []
                 (Sugi.assignRanks
                   (Sugi.buildLayoutGraph ["a", "b", "c", "s"]
                                          [("a","b"),("b","c"),("s","c")]))
          rankOf x = head [ Sugi.lnRank n | n <- Sugi.lgNodes lg, Sugi.lnId n == x ]
      in (rankOf "a", rankOf "b", rankOf "c", rankOf "s", Sugi.isFeasible lg)
           `shouldBe` (0, 1, 2, 1, True)

    it "tightenSourceRanks: エッジ無し node は所属 plate の最小 rank へ" $
      let lg = Sugi.tightenSourceRanks [["b", "c", "g"]]
                 (Sugi.assignRanks
                   (Sugi.buildLayoutGraph ["a", "b", "c", "g"]
                                          [("a","b"),("b","c")]))
          rankOf x = head [ Sugi.lnRank n | n <- Sugi.lgNodes lg, Sugi.lnId n == x ]
      in (rankOf "g", rankOf "b") `shouldBe` (1, 1)

    it "tightenSourceRanks: 浅い source / plate 無しは no-op (既存図ビット不変)" $
      let mk = Sugi.assignRanks
                 (Sugi.buildLayoutGraph ["a","b","c","d"]
                                        [("a","b"),("a","c"),("b","d"),("c","d")])
      in Sugi.tightenSourceRanks [] mk `shouldBe` mk

    -- Phase 19 A5 → Phase 39 P8: plate 枠の重なり解消。 旧 cosmetic 'applyPlateBands'
    -- (帯分離) は撤去済 (Step8)。 現在は P8 cluster 制約 (border node + contain/keepout)
    -- が simplex 内で member x 区間を分離するため、 同じ構造的不変条件が faithful 経路で成立する。
    it "P8 cluster 制約: 2 plate のメンバ x 区間が分離し非メンバは帯外 (旧 applyPlateBands 置換)" $
      let mkN i = DAGNode i i NodeLatent Nothing 0 0
          nodes = map mkN ["h", "b0", "b1", "x", "mu", "y", "s"]
          es    = [ DAGEdge f t Nothing Nothing
                  | (f, t) <- [("h","b0"),("h","b1"),("b0","mu"),("b1","mu")
                              ,("x","mu"),("mu","y"),("s","y")] ]
          plates = [ DAGPlate "G" ["b0", "b1"], DAGPlate "O" ["x", "mu", "y"] ]
          (pos, _) = Graphics.Hgg.DAG.layoutHierarchicalFullWithPlates nodes es plates
          xOf i = head [ dnX n | n <- pos, dnId n == i ]
          gXs = [xOf "b0", xOf "b1"]
          oXs = [xOf "x", xOf "mu", xOf "y"]
          disjoint = maximum gXs < minimum oXs || maximum oXs < minimum gXs
          -- s (rank 2 = O の rank 範囲内・非メンバ) は O メンバ区間の外
          sOut = xOf "s" < minimum oXs || xOf "s" > maximum oXs
      in (disjoint, sOut) `shouldBe` (True, True)

    -- Phase 20 → Phase 39 P8: nested の兄弟 plate 分離。 旧 'applyPlateBands' 再帰版は
    -- 撤去済 (Step8)。 現在は P8 cluster 制約 + separate_subclust が同 rank の兄弟 cluster
    -- 間に CL_OFFSET を確保することで faithful に区間分離する。
    it "P8 cluster 制約: nested の兄弟 plate の x 区間が分離 (旧 applyPlateBands 置換)" $
      let mkN i = DAGNode i i NodeLatent Nothing 0 0
          -- school plate ⊃ {classA, classB} の入れ子。 各 class に 2 ノード +
          -- school 直下に s0。 root h → 各ノード → 観測 y。
          ids   = ["h", "a0", "a1", "b0", "b1", "s0", "y"]
          nodes = map mkN ids
          es    = [ DAGEdge f t Nothing Nothing
                  | (f, t) <- [("h","a0"),("h","a1"),("h","b0"),("h","b1")
                              ,("h","s0")
                              ,("a0","y"),("a1","y"),("b0","y"),("b1","y")
                              ,("s0","y")] ]
          plates = [ DAGPlate "school" ["a0", "a1", "b0", "b1", "s0"]
                   , DAGPlate "classA" ["a0", "a1"]
                   , DAGPlate "classB" ["b0", "b1"] ]
          (pos, _) = Graphics.Hgg.DAG.layoutHierarchicalFullWithPlates nodes es plates
          xOf i = head [ dnX n | n <- pos, dnId n == i ]
          aXs = [xOf "a0", xOf "a1"]
          bXs = [xOf "b0", xOf "b1"]
          -- 兄弟 nested plate (classA / classB) の x 区間が交わらない
          sibDisjoint = maximum aXs < minimum bXs || maximum bXs < minimum aXs
          -- s0 (school メンバ・非 class メンバ) は両 class 区間の外
          s0Out = all (\xs -> xOf "s0" < minimum xs || xOf "s0" > maximum xs)
                      [aXs, bXs]
          -- 全 nested メンバは school の帯 (= school メンバ全体の包) に居る前提で
          -- 帯内に収まる (parent bbox を壊さない)
          schoolXs = [xOf i | i <- ["a0","a1","b0","b1","s0"]]
          inParent = all (\x -> x >= minimum schoolXs && x <= maximum schoolXs)
                         (aXs ++ bXs)
      in (sibDisjoint, s0Out, inParent) `shouldBe` (True, True, True)

    -- ★ Phase 44.1: skip edge が plate 箱を貫通しない (edge 幾何回帰ゲート)。
    -- a → plate{b,c} → d + skip a→d で、a→d の routing が plate 箱の **内部**へ侵入しない
    -- ことを pt 空間で検証する。graphviz は skip edge を cluster 箱の外へ回す (= 箱貫通 0)。
    -- ★ Phase 39 P8 A2 (e164df01) の stopgap (applyPlateBands) 撤去で a→d が箱の角を抉る
    -- 回帰が入ったが、layout keepout / path 本数 test では捕まらなかった (= 本 test で恒久検出)。
    -- 制御点だけでなく cubic Bézier を実サンプルする (角抉りは制御点が箱外でも曲線が箱に入るため)。
    it "Phase 44.1 回帰ゲート: skip edge a→d が plate 箱を貫通しない (cubic 実サンプル)" $
      let mkN i = DAGNode i i NodeLatent Nothing 0 0
          nodes  = map mkN ["a", "b", "c", "d"]
          es     = [ DAGEdge f t Nothing Nothing
                   | (f, t) <- [("a","b"),("b","d"),("a","c"),("c","d"),("a","d")] ]
          plates = [ DAGPlate "plate" ["b", "c"] ]
          (pos, routed) = Graphics.Hgg.DAG.layoutHierarchicalFullWithPlates nodes es plates
          radius   = 20 :: Double
          toScreen = dagToScreen radius pos LayoutHierarchical
          nodeMap  = [ (dnId n, n) | n <- pos ]
          look k   = head [ n | n <- pos, dnId n == k ]
          obs      = ER.dagObstacles toScreen radius pos nodeMap plates routed
          ad       = head [ e | e@(DAGEdge f t _ _) <- routed, f == "a", t == "d" ]
          adPath   = (\(DAGEdge _ _ p _) -> p) ad
          rt       = ER.routeEdge toScreen obs (look "a") (look "d") adPath radius 0 1
          -- EdgeRoute → 細サンプル点列。 CubicPath は 3 点ずつの cubic Bézier を評価、
          -- それ以外は制御点間を線形補間 (折れ線近似)。
          bez (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) t =
            let u = 1 - t
            in Point (u*u*u*ax + 3*u*u*t*bx + 3*u*t*t*cx + t*t*t*dx)
                     (u*u*u*ay + 3*u*u*t*by + 3*u*t*t*cy + t*t*t*dy)
          sampleCubic (p0:c1:c2:p3:rest) =
            [ bez p0 c1 c2 p3 t | t <- [0, 0.05 .. 1.0] ] ++ sampleCubic (p3:rest)
          sampleCubic _ = []
          lerp (Point x1 y1) (Point x2 y2) t = Point (x1+(x2-x1)*t) (y1+(y2-y1)*t)
          samplePoly ps = concat [ [ lerp p q t | t <- [0, 0.1 .. 1.0] ] | (p, q) <- zip ps (drop 1 ps) ]
          samples = case rt of
                      ER.CubicPath ps     -> sampleCubic ps
                      ER.BezierPath ps    -> samplePoly ps
                      ER.SplinePath ps    -> samplePoly ps
                      ER.StraightArrow p q -> samplePoly [p, q]
          box = ER.plateBoxPt toScreen radius nodeMap plates (head plates)
          inside (Point x y) = case box of
            Just (xlo, ylo, xhi, yhi) -> x > xlo && x < xhi && y > ylo && y < yhi
            Nothing                   -> False
      in any inside samples `shouldBe` False

    -- ★ Phase 53 A4: per-edge box 回廊 (= 他 edge の dummy lane 侵入禁止) の回帰ゲート。
    -- 並走する 2 本の skip edge (a→z / b→z、 dummy lane が隣接) で、 各 edge の spline が
    -- **相手 lane の box (半幅 9pt)** の内側へ入らないことを rank band 近傍の実サンプルで
    -- 検証する。 graphviz maximal_bbox の「隣接 virtual node で clip」 の忠実化 (= corr6
    -- braid の根治機構) を恒久検出する。
    it "Phase 53 A4 回帰ゲート: 並走 skip edge が相手の dummy lane に侵入しない" $
      let mkN i = DAGNode i i NodeLatent Nothing 0 0
          nodes  = map mkN ["a", "b", "p", "q", "z"]
          es     = [ DAGEdge f t Nothing Nothing
                   | (f, t) <- [("a","p"),("b","p"),("p","q"),("q","z"),("a","z"),("b","z")] ]
          (pos, routed) = Graphics.Hgg.DAG.layoutHierarchicalFull nodes es
          radius   = 20 :: Double
          toScreen = dagToScreen radius pos LayoutHierarchical
          nodeMap  = [ (dnId n, n) | n <- pos ]
          look k   = head [ n | n <- pos, dnId n == k ]
          obs      = ER.dagObstacles toScreen radius pos nodeMap [] routed
          pathOf f t = (\(DAGEdge _ _ p _) -> p)
                         (head [ e | e@(DAGEdge f' t' _ _) <- routed, f' == f, t' == t ])
          routeOf f t = ER.routeEdge toScreen obs (look f) (look t) (pathOf f t) radius 0 1
          -- 相手 lane の dummy 座標 (screen)
          dummiesOf f t = case pathOf f t of
            Just chain -> [ toScreen x y
                          | (x, y) <- take (length chain - 2) (drop 1 chain) ]
            Nothing    -> []
          bez (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) t =
            let u = 1 - t
            in Point (u*u*u*ax + 3*u*u*t*bx + 3*u*t*t*cx + t*t*t*dx)
                     (u*u*u*ay + 3*u*u*t*by + 3*u*t*t*cy + t*t*t*dy)
          sampleCubic (p0:c1:c2:p3:rest) =
            [ bez p0 c1 c2 p3 t | t <- [0, 0.05 .. 1.0] ] ++ sampleCubic (p3:rest)
          sampleCubic _ = []
          samplesOf r = case r of
            ER.CubicPath ps  -> sampleCubic ps
            ER.SplinePath ps -> ps
            ER.BezierPath ps -> ps
            ER.StraightArrow p' q' -> [p', q']
          -- spline (f,t) が相手 lane (f',t') の dummy へ x 距離 5pt 未満に近づく
          -- rank band 近傍 (|y差| ≤ 4pt) のサンプルが無いこと
          invades (f, t) (f', t') = or
            [ abs (px - dx) < 5
            | Point dx dy <- dummiesOf f' t'
            , Point px py <- samplesOf (routeOf f t)
            , abs (py - dy) <= 4 ]
          lanesSeparate = case (dummiesOf "a" "z", dummiesOf "b" "z") of
            (da@(_:_), db@(_:_)) -> and [ abs (ax - bx) >= 18 - 1e-6
                                        | (Point ax _, Point bx _) <- zip da db ]
            _                    -> False
      in ( lanesSeparate
         , invades ("a", "z") ("b", "z")
         , invades ("b", "z") ("a", "z") ) `shouldBe` (True, False, False)

    -- Phase 39 A3: fit の bbox ≤ canvas 回帰ゲート。 renderDAGStandalone は
    -- fitPrimsToArea で全 primitive (plate 枠・ラベル・ノード・矢印・skip edge) を
    -- area 内へ収めるはず。 plate + free node (σ) + plate 跨ぎ skip edge を含む DAG を
    -- 縦横様々な canvas 寸法で描き、 bbox が area を一切超えないことを数値検証する。
    it "renderDAGStandalone: 全 primitive bbox が canvas area 内 (A3 はみ出しゼロ)" $
      let g = ("mu" :: Data.Text.Text) ~> "t1" <> "mu" ~> "t2"
            <> "t1" ~> "y" <> "t2" ~> "y" <> "s" ~> "y" <> "mu" ~> "y"
          plate = DAGPlate "grp (n=2)" ["t1", "t2"]
          lyr   = Graphics.Hgg.DAG.dagPlotWithPlates g [plate]
          pal   = themePalette ThemeLight
          eps   = 0.5  -- FP 誤差許容
          fits (w, h) =
            let prims = renderDAGStandalone (Rect 0 0 w h) pal lyr
            in case primsBBoxDAG prims of
                 Nothing -> False
                 Just (xlo, ylo, xhi, yhi) ->
                   xlo >= negate eps && ylo >= negate eps
                   && xhi <= w + eps && yhi <= h + eps
      in map fits [(600, 400), (300, 500), (800, 220), (220, 800)]
         `shouldBe` [True, True, True, True]

    -- Phase 23: plate 枠 = glyph bbox (中心 ± nodeExtent)。 旧実装 (中心 bbox +
    -- 固定 pad radius*1.6) では label ≥ 9 文字のノードが水平端で枠を超えていた
    -- (analyze Phase 63.2 の実測再現 = 長 label Data box が 22.3px 突き抜け)。
    it "renderPlate: 長 label ノードの glyph box が plate 枠に収まる (Phase 23)" $
      let nodes = [ DAGNode "x_duration_long" "x_duration_long" NodeData
                      (Just "Data") 0 0
                  , DAGNode "y" "y" NodeObserved (Just "NegativeBinomial") 0 0 ]
          es     = [ DAGEdge "x_duration_long" "y" Nothing Nothing ]
          plates = [ DAGPlate "obs (4)" ["x_duration_long", "y"] ]
          (pos, routed) =
            Graphics.Hgg.DAG.layoutHierarchicalFullWithPlates nodes es plates
          spec = layer (dagFromListsWithPlates pos routed LayoutHierarchical plates)
                   <> widthUnit (760 *~ px) <> heightUnit (520 *~ px)
          ps = renderToPrimitives emptyResolver (computeLayout emptyResolver spec) spec
          -- plate 枠 = fill-opacity 0 の PRect / node glyph 箱 = opacity > 0 の
          -- PRect (背景の白 rect は除外)
          frames = [ r | PRect r (FillStyle _ o) _ <- ps, o == 0 ]
          boxes  = [ r | PRect r (FillStyle c o) _ <- ps
                       , o > 0, c /= Data.Text.pack "#ffffff" ]
          contains (Rect fx fy fw fh) (Rect bx by bw bh) =
            fx <= bx && bx + bw <= fx + fw && fy <= by && by + bh <= fy + fh
      in case frames of
           [frame] -> (length boxes >= 1, all (contains frame) boxes)
                        `shouldBe` (True, True)
           _ -> expectationFailure ("plate 枠 PRect が 1 個でない: "
                                    <> show (length frames))

    it "一様 δ=ω=1 では longest-path が edge length sum 最適 (= assignRanks と一致)" $
      let g0 = Sugi.buildLayoutGraph ["a","b","c","d"]
                                     [("a","b"),("a","c"),("b","d"),("c","d")]
          lpOnly = Sugi.longestPathRanking g0
          full   = Sugi.assignRanks g0
      in Sugi.edgeLengthSum full `shouldBe` Sugi.edgeLengthSum lpOnly

    it "決定論性: 同 input は同 rank (= 2 回実行で完全一致)" $
      let g = Sugi.buildLayoutGraph ["x","y","z","w"]
                                    [("x","y"),("y","z"),("x","w"),("w","z")]
          r1 = Sugi.assignRanks g
          r2 = Sugi.assignRanks g
      in r1 `shouldBe` r2

    it "後方互換: dagPlot の y 座標は新 rank 経由でも旧 longest-path と一致" $
      let g = ("alpha" :: Data.Text.Text) ~> "y" <> "beta" ~> "y" <> "alpha" ~> "sigma" <> "sigma" ~> "y"
          spec = layer (Graphics.Hgg.DAG.dagPlot g)
          -- 旧実装と同じ rank 構造: alpha=0, beta=0, sigma=1, y=2
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
      in length [() | PPath{} <- ps] `shouldSatisfy` (>= 4)  -- 4 node 形状 + arrow

  describe "Step6 R2: funnel (Mononen, graphviz Pshortestpath 相当)" $ do
    -- 規約: portal.left=小x / portal.right=大x、path は下方向 (y 増加)。
    it "wide channel (障害物なし) → 直線 (src,goal のみ・重複なし)" $
      let portals = [ (Point 5 0,  Point 5 0)
                    , (Point 0 10, Point 10 10)
                    , (Point 0 20, Point 10 20)
                    , (Point 5 30, Point 5 30) ]
      in ER.funnel portals `shouldBe` [Point 5 0, Point 5 30]

    it "右側障害物 → 左の角で taut に曲がる (cone 不変条件 OK)" $
      -- y=10 で free 区間 [2,10] (= x<2 が塞がれる)。 src/goal は x=0。
      -- 最短路は (0,0)→(2,10)→(0,20) で角 (2,10) を通る。
      let portals = [ (Point 0 0,  Point 0 0)
                    , (Point 2 10, Point 10 10)
                    , (Point 0 20, Point 0 20) ]
      in ER.funnel portals `shouldBe` [Point 0 0, Point 2 10, Point 0 20]

    it "taut 折れ線は左右往復しない (旧 zigzag 回帰防止)" $
      -- 3 連続 gate が右側を x≥2 に制限。 taut 路の x は単峰 (出て戻る) で、
      -- 局所 peak は高々 1 個 = 左右往復ジグザグでないこと。
      let portals = [ (Point 0 0,  Point 0 0)
                    , (Point 2 10, Point 10 10)
                    , (Point 2 20, Point 10 20)
                    , (Point 2 30, Point 10 30)
                    , (Point 0 40, Point 0 40) ]
          xs    = [ x | Point x _ <- ER.funnel portals ]
          peaks = length [ () | (a, b, c) <- zip3 xs (drop 1 xs) (drop 2 xs)
                              , b > a, b > c ]
      in peaks `shouldSatisfy` (<= 1)

    it "buildChannel+funnel: 端点が片寄っても dummy lane に沿う (L字 shortcut しない)" $
      -- dummy lane = x13、 端点は x70/x52 (右寄り)。 旧 (free 区間全幅 portal) は funnel が
      -- lane を無視し x≈52 へ shortcut → L字 → R3 bulge。 狭い窓 portal なら経路内部は
      -- dummy lane (x13±portalHalfWidth=6) 近傍に留まる。
      let guide = [ Point 70 0, Point 13 30, Point 13 60, Point 13 90, Point 52 120 ]
          taut  = ER.funnel (ER.buildChannel [] guide)
          interiorXs = [ x | Point x _ <- drop 1 (init taut) ]
      in interiorXs `shouldSatisfy` all (<= 19 + 1e-9)

  describe "Step6 R3: cubic solver + Proutespline (graphviz route.c)" $ do
    let approxRoots want got = case got of
          Right rs -> let s = sort rs
                      in length s == length want
                         && and (zipWith (\a b -> abs (a - b) < 1e-6) s (sort want))
          Left ()  -> False
    it "solve3: (x-1)(x-2)(x-3) → {1,2,3}" $
      -- x³ -6x² +11x -6
      ER.solve3 (-6, 11, -6, 1) `shouldSatisfy` approxRoots [1, 2, 3]
    it "solve3: x³ - x → {-1,0,1}" $
      ER.solve3 (0, -1, 0, 1) `shouldSatisfy` approxRoots [-1, 0, 1]
    it "solve3: 二重根 (x)(x-2)² → {0,2}" $
      -- x³ -4x² +4x
      ER.solve3 (0, 4, -4, 1) `shouldSatisfy` approxRoots [0, 2]
    it "solve3: 線形 2x+4 → {-2}" $
      ER.solve3 (4, 2, 0, 0) `shouldSatisfy` approxRoots [-2]

    it "proutespline: 障害物なし直線 taut → 始点/終点を保持した cubic" $
      let inps = [Point 0 0, Point 0 30, Point 0 60]
          ctrl = ER.proutespline [] inps (Point 0 1) (Point 0 1)
      in (head ctrl, last ctrl) `shouldBe` (Point 0 0, Point 0 60)
    it "proutespline: 制御点列は 始点 + 3k 個 (cubic segment の倍数)" $
      let inps = [Point 0 0, Point 0 30, Point 0 60]
          ctrl = ER.proutespline [] inps (Point 0 1) (Point 0 1)
      in (length ctrl - 1) `mod` 3 `shouldBe` 0

  describe "Phase 1 A3: order assignment (= dummy + median + transpose)" $ do
    it "insertDummies: 長 edge (rank 差 3) で dummy 2 個 + 短 edge 3 本に展開" $
      let g0 = Sugi.buildLayoutGraph ["a", "b"] [("a", "b")]
          -- 手動で b の rank を 3 に
          g1 = g0 { Sugi.lgNodes = [ Sugi.LNode "a" 0 False
                                   , Sugi.LNode "b" 3 False ] }
          g2 = Sugi.insertDummies g1
          dummies = [ n | n <- Sugi.lgNodes g2, Sugi.lnDummy n ]
      in (length dummies, length (Sugi.lgEdges g2)) `shouldBe` (2, 3)

    it "insertDummies: rank 差 1 の edge は触らない (= 元のまま)" $
      let g = Sugi.assignRanks (Sugi.buildLayoutGraph ["a","b"] [("a","b")])
          g2 = Sugi.insertDummies g
      in (length (Sugi.lgNodes g2), length (Sugi.lgEdges g2)) `shouldBe` (2, 1)

    it "bilayerCrossings: 2 edge 交差ペアで 1" $
      let edges_ = [("a", "y"), ("b", "x")]
      in Sugi.bilayerCrossings edges_ ["a", "b"] ["x", "y"] `shouldBe` 1

    it "bilayerCrossings: 平行 edge は 0" $
      let edges_ = [("a", "x"), ("b", "y")]
      in Sugi.bilayerCrossings edges_ ["a", "b"] ["x", "y"] `shouldBe` 0

    it "K3,3 風 reverse pattern (= A→Z, B→Y, C→X) は median sweep で crossings 3 → 0" $
      let g0 = Sugi.assignRanks $
                 Sugi.buildLayoutGraph ["A","B","C","X","Y","Z"]
                                       [("A","Z"),("B","Y"),("C","X")]
          ini = Sugi.initialOrder g0
          (g1, finalOrd) = Sugi.assignOrder g0
          cIni = Sugi.countCrossings g0 ini
          cFin = Sugi.countCrossings g1 finalOrd
      in (cIni, cFin) `shouldBe` (3, 0)

    it "決定論性: 同 input → 同 OrderMap (= 2 回 assignOrder 一致)" $
      let g0 = Sugi.assignRanks $
                 Sugi.buildLayoutGraph ["a","b","c","d","e","f"]
                                       [("a","d"),("a","e"),("b","f"),("c","d")]
          (_, o1) = Sugi.assignOrder g0
          (_, o2) = Sugi.assignOrder g0
      in o1 `shouldBe` o2

    it "countCrossings は最終 ≤ 初期 (= sweep が必ず改善 or 維持)" $
      let g0 = Sugi.assignRanks $
                 Sugi.buildLayoutGraph ["a","b","c","p","q","r"]
                                       [("a","q"),("a","r"),("b","p"),("c","p"),("c","r")]
          ini = Sugi.initialOrder g0
          (g1, fin) = Sugi.assignOrder g0
          cIni = Sugi.countCrossings g0 ini
          cFin = Sugi.countCrossings g1 fin
      in cFin <= cIni `shouldBe` True

    it "dummy 込み全 LayoutGraph で feasible (= rank 差 = δ = 1 を保つ)" $
      let g0 = Sugi.assignRanks $
                 Sugi.buildLayoutGraph ["a","b","c"] [("a","c"),("a","b"),("b","c")]
          (g1, _) = Sugi.assignOrder g0
      in Sugi.isFeasible g1 `shouldBe` True

  describe "Phase 1 A4: Brandes-Köpfe coordinate assignment (= TD+BU median)" $ do
    it "単一 chain a→b→c は全 node 同 x (= 垂直整列、 |Δx| < 1e-9)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b","c"]
                 [("a","b"),("b","c")]
          (g1, om) = Sugi.assignOrder g0
          coords = Sugi.assignCoords [] g1 om
          [xa, xb, xc] = map (\k -> coords Map.! k) ["a","b","c"]
      in maximum (map abs [xa - xb, xb - xc]) `shouldSatisfy` (< 1e-9)

    it "対称 diamond a→b,a→c,b→d,c→d で a と d は同 x、 b と c が対称" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b","c","d"]
                 [("a","b"),("a","c"),("b","d"),("c","d")]
          (g1, om) = Sugi.assignOrder g0
          coords = Sugi.assignCoords [] g1 om
          [xa, xb, xc, xd] = map (\k -> coords Map.! k) ["a","b","c","d"]
      in do
           abs (xa - xd) `shouldSatisfy` (< 1e-9)
           -- b と c は a/d の中心 (= (xa) と対称) → xb + xc ≈ 2 * xa
           abs ((xb + xc) - 2 * xa) `shouldSatisfy` (< 1e-9)

    it "coord 範囲 [0, 1] (= 正規化)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["p","q","r","s"]
                 [("p","r"),("q","r"),("r","s")]
          (g1, om) = Sugi.assignOrder g0
          coords = Map.elems (Sugi.assignCoords [] g1 om)
      in do
           minimum coords `shouldSatisfy` (>= 0)
           maximum coords `shouldSatisfy` (<= 1)

    it "決定論性: 同 input → 同 coords (= 2 回 assignCoords 一致)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b","c","d","e"]
                 [("a","b"),("a","c"),("b","d"),("c","d"),("d","e")]
          (g1, om) = Sugi.assignOrder g0
          c1 = Sugi.assignCoords [] g1 om
          c2 = Sugi.assignCoords [] g1 om
      in c1 `shouldBe` c2

    it "rank が 1 つ (= source 群のみ) は等間隔 [0..1]" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b","c"] []
          (g1, om) = Sugi.assignOrder g0
          coords = Sugi.assignCoords [] g1 om
          xs = sort [ coords Map.! k | k <- ["a","b","c"] ]
      in (head xs, last xs) `shouldBe` (0, 1)

    it "computeOneDir 単独: top-down は source 側 anchor、 bottom-up は sink 側 anchor (= 2 候補で値が違う)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b","c","d","e"]
                 [("a","c"),("b","c"),("c","d"),("c","e")]
          (g1, om) = Sugi.assignOrder g0
          xTD = Sugi.computeOneDir True  g1 om
          xBU = Sugi.computeOneDir False g1 om
      in xTD `shouldNotBe` xBU

  describe "Phase 1 A5: edge routing (= dummy 経由 + Catmull-Rom spline)" $ do
    it "insertDummiesWithChains: 長 edge (rank 差 3) chain は 4 要素 [from, d1, d2, to]" $
      let g0 = Sugi.buildLayoutGraph ["a","b"] [("a","b")]
          g1 = g0 { Sugi.lgNodes = [ Sugi.LNode "a" 0 False
                                   , Sugi.LNode "b" 3 False ] }
          (_, chainMap) = Sugi.insertDummiesWithChains g1
          chain = chainMap Map.! ("a", "b")
      in length chain `shouldBe` 4

    it "insertDummiesWithChains: 短 edge は chain 2 要素 [from, to]" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b"] [("a","b")]
          (_, chainMap) = Sugi.insertDummiesWithChains g0
      in chainMap Map.! ("a", "b") `shouldBe` ["a", "b"]

    it "assignOrderFull: chainMap が assignOrder 結果と整合 (= 全 edge に対応 chain あり)" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph ["a","b","c","d"]
                 [("a","b"),("a","d"),("c","d")]
          (_, _, chainMap) = Sugi.assignOrderFull g0
          keys = Map.keys chainMap
      in do
           ("a","b") `elem` keys `shouldBe` True
           ("a","d") `elem` keys `shouldBe` True
           ("c","d") `elem` keys `shouldBe` True

    it "DAG.dagPlot 長 edge を含む graph で routedEdges の dePath が Just (= spline 描画)" $
      let g = ("a" :: Data.Text.Text) ~> "d"          -- 直接 long edge (rank 0 → 3 予定)
           <> "a" ~> "b" <> "b" ~> "c" <> "c" ~> "d"  -- 経路 chain
          spec = layer (Graphics.Hgg.DAG.dagPlot g)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          -- spline edge は PPath (= curve)、 矢印ヘッドも PPath。 多めに含まれるはず。
          paths = length [() | PPath{} <- ps]
      in paths `shouldSatisfy` (>= 8)  -- 4 node 楕円 + 4 短 edge 矢印 + 1 long edge spline + 1 long edge 矢印 = 10 程度

    it "DAG.dagPlot 短 edge のみ graph では dePath が全て Nothing (= 直線描画)" $
      let g = ("a" :: Data.Text.Text) ~> "b" <> "b" ~> "c"
          spec = layer (Graphics.Hgg.DAG.dagPlot g)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          -- 短 edge では PLine (= 直線) が edge ごとに 1 本
          plines = length [() | PLine{} <- ps]
      in plines `shouldSatisfy` (>= 2)

    it "DAGEdge backward compat: dagEdge は dePath = Nothing default" $
      let e = dagEdge "x" "y"
      in dePath e `shouldBe` Nothing

  describe "Phase 1 A6: plate-aware ordering" $ do
    it "applyPlateConstraints: 2 plate ([a1,a2] と [b1,b2]) で同 rank 内 contiguous" $
      let -- 初期 order が [a1, b1, a2, b2] (= 交互) であっても plate 制約後は a1,a2 隣接 / b1,b2 隣接
          om0 = Map.fromList [(0, ["a1", "b1", "a2", "b2"])]
          plates = [["a1", "a2"], ["b1", "b2"]]
          om1 = Sugi.applyPlateConstraints plates om0
          row = om1 Map.! 0
          -- 同 plate の index 差が 1 (= 隣接) であること
          ixOf v = head [ i | (i, x) <- zip [0 :: Int ..] row, x == v ]
      in do
           abs (ixOf "a1" - ixOf "a2") `shouldBe` 1
           abs (ixOf "b1" - ixOf "b2") `shouldBe` 1

    it "applyPlateConstraints 空 plates: 入力 OrderMap と同一" $
      let om0 = Map.fromList [(0, ["a", "b", "c"])]
      in Sugi.applyPlateConstraints [] om0 `shouldBe` om0

    it "applyPlateConstraints: 非 plate node は元順序を保つ" $
      let om0 = Map.fromList [(0, ["x", "a1", "y", "a2", "z"])]
          plates = [["a1", "a2"]]
          om1 = Sugi.applyPlateConstraints plates om0
          row = om1 Map.! 0
          -- x, y, z の元順序が破壊されていない (= median 安定 sort)
          posMap = Map.fromList (zip row [0 :: Int ..])
      in do
           (posMap Map.! "x") < (posMap Map.! "y") `shouldBe` True
           (posMap Map.! "y") < (posMap Map.! "z") `shouldBe` True

    it "dagPlotWithPlates: plate 渡しても layout 走る (= PRect plate box が出る)" $
      let g = ("a1" :: Data.Text.Text) ~> "y"
           <> "a2" ~> "y" <> "b1" ~> "y" <> "b2" ~> "y"
          plates = [ DAGPlate "plate-a" ["a1", "a2"]
                   , DAGPlate "plate-b" ["b1", "b2"]
                   ]
          spec = layer (Graphics.Hgg.DAG.dagPlotWithPlates g plates)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          -- plate 2 個分の bounding box (= PRect) + plate label
          rects = length [() | PRect{} <- ps]
      in rects `shouldSatisfy` (>= 2)

    it "Phase 1 A7 (port snap): latent (ellipse) 水平方向 port は cx ± rx に snap" $
      let n = Graphics.Hgg.Easy.dagNode "v" "v" NodeLatent 0 0
          -- baseR = 20、 dist 無し → rx = ry = 20
          p = edgePortPoint n (Point 100 100) (Point 200 100) 20
      in case p of
           Point px py -> do
             abs (px - 120) `shouldSatisfy` (< 1e-9)
             abs (py - 100) `shouldSatisfy` (< 1e-9)

    it "Phase 1 A7 (port snap): data (rect) 水平方向 port は cx + rx に snap" $
      let n = Graphics.Hgg.Easy.dagNode "v" "v" NodeData 0 0
          p = edgePortPoint n (Point 0 0) (Point 100 0) 20
      in case p of
           Point px py -> do
             abs (px - 20) `shouldSatisfy` (< 1e-9)
             abs py `shouldSatisfy` (< 1e-9)

    it "Phase 1 A8 決定論性: 全 pipeline (= layoutHierarchicalFullWithPlates) を 2 回実行で完全一致" $
      let nodes = [ Graphics.Hgg.Easy.dagNode i i NodeLatent 0 0
                  | i <- ["a","b","c","d","e","f"] ]
          edges_ = [ dagEdge "a" "c", dagEdge "b" "c", dagEdge "c" "d"
                   , dagEdge "c" "e", dagEdge "d" "f", dagEdge "e" "f"
                   , dagEdge "a" "f"  -- long edge → dummy 入る
                   ]
          plates = [ DAGPlate "P" ["c", "d"] ]
          run = Graphics.Hgg.DAG.layoutHierarchicalFullWithPlates nodes edges_ plates
          r1 = run
          r2 = run
      in r1 `shouldBe` r2

    it "graphviz parity bench: small case (N=10, 13 edges) で crossings = 0 (= 内部基準値)" $
      let nodeIds = ["a","b","c","d","e","f","g","h","i","j"]
          es = [ ("a","c"),("b","c"),("c","d"),("c","e")
               , ("d","f"),("e","f"),("d","g"),("e","h")
               , ("f","i"),("g","j"),("h","j"),("i","j"),("a","j") ]
          g0 = Sugi.assignRanks (Sugi.buildLayoutGraph nodeIds es)
          (g1, om, _) = Sugi.assignOrderFull g0
      in Sugi.countCrossings g1 om `shouldBe` 0

    it "Phase 1 並列 edge: 同 (from, to) を 3 本書くと PPath spline が 3 本 描画される" $
      let g = ("a" :: Data.Text.Text) ~> "b" <> "a" ~> "b" <> "a" ~> "b"
          spec = layer (Graphics.Hgg.DAG.dagPlot g)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          -- 並列 3 本それぞれ spline edge (PPath) + 矢印 (PPath) = 6 PPath 増加 (+ node 2 個)
          paths = length [() | PPath{} <- ps]
      in paths `shouldSatisfy` (>= 8)  -- 2 node 楕円 + 3 spline + 3 矢印 = 8

    it "Phase 1 並列 edge: 1 本のみ (= parCount=1) なら従来の PLine 直線 (= spline 化しない)" $
      let g = ("a" :: Data.Text.Text) ~> "b"
          spec = layer (Graphics.Hgg.DAG.dagPlot g)
          ps = renderToPrimitives emptyResolver
                 (computeLayout emptyResolver spec) spec
          plines = length [() | PLine{} <- ps]
      in plines `shouldSatisfy` (>= 1)

    it "Phase 1 A8 決定論性: assignRanks + assignOrder + applyPlateConstraints + assignCoords 全体" $
      let g0 = Sugi.assignRanks $ Sugi.buildLayoutGraph
                 ["x","y","z","w","u"]
                 [("x","y"),("y","z"),("x","w"),("w","z"),("z","u")]
          (g1, o, _) = Sugi.assignOrderFull g0
          op = Sugi.applyPlateConstraints [["w","z"]] o
          c1 = Sugi.assignCoords [] g1 op
          c2 = Sugi.assignCoords [] g1 op
      in c1 `shouldBe` c2

    it "Phase 1 A7 (port snap): rect 対角 45° は短辺の方向で先に交点 (= min(rx/|ux|, ry/|uy|))" $
      let n = Graphics.Hgg.Easy.dagNode "v" "v" NodeData 0 0
          p = edgePortPoint n (Point 0 0) (Point 100 100) 20
      in case p of
           -- ★A15: nodeExtent で可変サイズ。 NodeData "v" (1 行・dist 無し) は
           -- rx = max 20 (1*6.6/2+8) = 20、 ry = max (20*0.7) (1*14/2+4) = 14。
           -- ux = uy = √2/2 ゆえ短辺 ry=14 が先 → t = 14/(√2/2)、 port = (14, 14)。
           Point px py -> do
             abs (px - 14) `shouldSatisfy` (< 1e-9)
             abs (py - 14) `shouldSatisfy` (< 1e-9)

    it "dagPlotWithPlates: plate メンバが contiguous (= 同 plate の x が近い)" $
      let g = ("a1" :: Data.Text.Text) ~> "z"
           <> "a2" ~> "z" <> "b1" ~> "z" <> "b2" ~> "z"
          plates = [ DAGPlate "A" ["a1", "a2"]
                   , DAGPlate "B" ["b1", "b2"]
                   ]
          spec = layer (Graphics.Hgg.DAG.dagPlotWithPlates g plates)
          dagSpec = case getLast (lyDAG (head (vsLayers spec))) of
                      Just ds -> ds
                      Nothing -> error "no dag"
          ns = dsNodes dagSpec
          xOf nid = case [dnX n | n <- ns, dnId n == nid] of
            (x:_) -> x
            _     -> 999
          a1 = xOf "a1"; a2 = xOf "a2"; b1 = xOf "b1"; b2 = xOf "b2"
          insideA = abs (a1 - a2)
          insideB = abs (b1 - b2)
          between = min (abs (a1 - b1)) (abs (a2 - b2))
      in do
           insideA `shouldSatisfy` (< between)
           insideB `shouldSatisfy` (< between)

    -- =======================================================================
    -- Phase 53 A3: rank=same (assignRanksGrouped + P3e flat-edge ordering)
    -- =======================================================================
    it "Phase 53 A3-2: assignRanksGrouped group 無し = 旧 pipeline (breakCycles→assignRanks→tighten) とビット一致" $
      let ids = ["s","a","b","t","c"]
          es  = [("s","a"),("a","b"),("b","a"),("b","t"),("c","c"),("s","c")]
          plateIds = [["c","t"]]
          old = Sugi.tightenSourceRanks plateIds $ Sugi.assignRanks $
                  Sugi.buildLayoutGraph ids (Sugi.breakCycles ids es)
          new = Sugi.assignRanksGrouped [] plateIds ids es
      in new `shouldBe` old

    it "Phase 53 A3-2: rank group で member が同 rank + group 内 edge が flat 化 (原方向保持)" $
      let lg = Sugi.assignRanksGrouped [["b","c"]] []
                 ["a","b","c","d"]
                 [("a","b"),("a","c"),("b","c"),("b","d"),("c","d")]
          rk i = head [Sugi.lnRank n | n <- Sugi.lgNodes lg, Sugi.lnId n == i]
          flats = [ (Sugi.leFrom e, Sugi.leTo e)
                  | e <- Sugi.lgEdges lg
                  , rk (Sugi.leFrom e) == rk (Sugi.leTo e) ]
      in do
           rk "b" `shouldBe` rk "c"
           rk "a" `shouldSatisfy` (< rk "b")
           rk "d" `shouldSatisfy` (> rk "b")
           flats `shouldBe` [("b","c")]

    it "Phase 53 A3-3: flatReorder で flat edge が左→右 (from が to より左) に並ぶ" $
      let lg = Sugi.assignRanksGrouped [["b","c"]] []
                 ["a","b","c","d"]
                 [("a","b"),("a","c"),("c","b"),("b","d"),("c","d")]  -- flat: c→b
          (_, om) = Sugi.assignOrder lg
          rk i = head [Sugi.lnRank n | n <- Sugi.lgNodes lg, Sugi.lnId n == i]
          orderAt = Map.findWithDefault [] (rk "b") om
          ixOf v = length (takeWhile (/= v) orderAt)
      in ixOf "c" `shouldSatisfy` (< ixOf "b")  -- 初期 ID 辞書順 [b,c] からの反転を要求

    it "Phase 53 A3-3: flat 閉路 (b⇄c) でも落ちず決定論的" $
      let lg = Sugi.assignRanksGrouped [["b","c"]] [] ["a","b","c"]
                 [("a","b"),("a","c"),("b","c"),("c","b")]
          (_, om1) = Sugi.assignOrder lg
          (_, om2) = Sugi.assignOrder lg
      in om1 `shouldBe` om2

    it "Phase 53 A3: dagPlotWithRankGroups end-to-end (同 dnY + 非隣接 flat edge の迂回 dePath)" $
      let g = ("r" :: Data.Text.Text) ~> "a" <> "r" ~> "m" <> "r" ~> "b"
           <> "a" ~> "m" <> "m" ~> "b" <> "a" ~> "b"
          spec = layer (Graphics.Hgg.DAG.dagPlotWithRankGroups g [["a","m","b"]])
          dagSpec = case getLast (lyDAG (head (vsLayers spec))) of
                      Just ds -> ds
                      Nothing -> error "no dag"
          ns = dsNodes dagSpec
          yOf nid = case [dnY n | n <- ns, dnId n == nid] of
            (y:_) -> y
            _     -> 999
          pathOf f t = case [ dePath e | e <- dsEdges dagSpec
                            , deFrom e == f, deTo e == t ] of
            (p:_) -> p
            _     -> Nothing
      in do
           yOf "a" `shouldBe` yOf "m"
           yOf "m" `shouldBe` yOf "b"
           -- 隣接 flat edge (a→m / m→b) = 水平直線 (dePath 無し)
           pathOf "a" "m" `shouldBe` Nothing
           pathOf "m" "b" `shouldBe` Nothing
           -- 非隣接 flat edge (a→b、 間に m) = rank 上側 gap の waypoint 1 点
           case pathOf "a" "b" of
             Just [(_, y0), (_, ym), (_, y1)] -> do
               y0 `shouldBe` yOf "a"
               y1 `shouldBe` yOf "b"
               ym `shouldBe` yOf "a" - 0.5
             other -> expectationFailure ("unexpected dePath: " <> show other)

  -- =========================================================================
  -- Phase 11 A1: validate / compile / diagnostics
  -- =========================================================================
  describe "Validate (Phase 11 A1)" $ do
    let rXY n = case n of
          "x"   -> Just (NumData (V.fromList [1, 2, 3]))
          "y"   -> Just (NumData (V.fromList [4, 5, 6]))
          "grp" -> Just (TxtData (V.fromList ["a", "b", "a"]))
          _     -> Nothing

    it "完全な scatter は診断ゼロ" $
      validatePlot rXY (layer (scatter "x" "y")) `shouldBe` []

    it "必須 aesthetic 欠落を検出 (histogram は x 必須、 空 layer)" $
      let emptyHist = mempty { lyKind = First (Just MHistogram) } :: Layer
          diags = validatePlot emptyResolver (purePlot { vsLayers = [emptyHist] })
      in any isMissing diags `shouldBe` True

    it "解決できない列名で ColumnNotFound" $
      let diags = validatePlot rXY (layer (scatter "xxx" "y"))
      in any isNotFound diags `shouldBe` True

    it "ColumnNotFound に編集距離 suggestion が付く (validatePlotWith)" $
      let known = ["x", "y", "grp"]
          diags = validatePlotWith known rXY (layer (scatter "yy" "x"))
          sugg  = [cs | PlotError (ColumnNotFound _ cs) _ <- diags]
      in case sugg of
           (cs : _) -> cs `shouldSatisfy` (\xs -> "y" `elem` xs)
           []       -> expectationFailure "ColumnNotFound が出ていない"

    it "errorX に文字列列で ColumnTypeMismatch" $
      let diags = validatePlot rXY (layer (forest "y" "grp" "grp"))
          -- forest errCol = "grp" (文字列) → errorX 数値要求に不一致
      in any isTypeMismatch diags `shouldBe` True

    it "空プロットは EmptyPlot error" $
      validatePlot emptyResolver purePlot `shouldBe` [PlotError EmptyPlot (DiagnosticContext Nothing Nothing)]

    it "compilePlot: error があれば Left" $
      case compilePlot emptyResolver purePlot of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "EmptyPlot を素通しした"

    it "compilePlot: 正常 spec は Right" $
      case compilePlot rXY (layer (scatter "x" "y")) of
        Right c -> length (vsLayers (compiledSpec c)) `shouldBe` 1
        Left ds -> expectationFailure ("予期せぬ error: " <> show ds)

    it "capability: hover + SVG backend は BackendUnsupported warning" $
      let spec  = layer (scatter "x" "y" <> hoverCols ["grp"])
          warns = checkCapability svgCapability spec
      in any isHoverWarn warns `shouldBe` True

    it "capability: hover + Canvas backend は warning 無し" $
      let spec = layer (scatter "x" "y" <> hoverCols ["grp"])
      in filter isHoverWarn (checkCapability canvasCapability spec) `shouldBe` []

  -- =========================================================================
  -- Phase 11 A2: Monoid 合成規則の conformance (design/monoid-semantics.md と一致)
  -- =========================================================================
  describe "Monoid 合成規則 (Phase 11 A2)" $ do
    it "Layer: lyKind は first wins (scatter<>line は MScatter)" $
      let l = scatter "a" "b" <> line "c" "d"
      in getFirst (lyKind l) `shouldBe` Just MScatter

    it "Layer: lyEncX/Y は last wins (scatter<>line で c/d が残る)" $
      let l = scatter "a" "b" <> line "c" "d"
      in (getLast (lyEncX l), getLast (lyEncY l))
           `shouldBe` (Just (ColByName "c"), Just (ColByName "d"))

    it "Layer: lyHover は concat" $
      let l = hoverCols ["a"] <> hoverCols ["b", "c"]
      in lyHover l `shouldBe` [ColByName "a", ColByName "b", ColByName "c"]

    it "Layer: lyAlpha は last wins" $
      getLast (lyAlpha (alpha 0.3 <> alpha 0.7)) `shouldBe` Just 0.7

    it "Layer: lyColorCats は last-nonempty wins (concat ではない)" $
      lyColorCats (colorCats ["a", "b"] <> colorCats ["c"]) `shouldBe` ["c"]

    it "Layer: 空 colorCats を後に合成しても前者が残る" $
      lyColorCats (colorCats ["a", "b"] <> mempty) `shouldBe` ["a", "b"]

    it "VisualSpec: vsLayers は concat (layer<>layer で 2 層)" $
      length (vsLayers (layer (scatter "x" "y") <> layer (line "x" "z"))) `shouldBe` 2

    it "VisualSpec: vsTitle は last wins" $
      getLast (vsTitle (title "a" <> title "b")) `shouldBe` Just "b"

    it "VisualSpec: vsRefLines は concat" $
      length (vsRefLines (refHorizontal 0 <> refHorizontal 1)) `shouldBe` 2

    it "Monoid 則: 左単位元 (mempty <> s == s) for VisualSpec" $
      let s = layer (scatter "x" "y") <> title "t"
      in (mempty <> s) `shouldBe` s

  -- =========================================================================
  -- Phase 11 A3: Easy 層 (値直接受け + overlay)
  -- =========================================================================
  describe "Easy 層 (Phase 11 A3)" $ do
    it "points xs ys ≡ scatter (inline xs) (inline ys)" $
      points [1, 2, 3] [4, 5, 6] `shouldBe` scatter (inline [1, 2, 3 :: Double]) (inline [4, 5, 6 :: Double])

    it "lineXY ≡ line (inline ..) (inline ..)" $
      lineXY [1, 2] [3, 4] `shouldBe` line (inline [1, 2 :: Double]) (inline [3, 4 :: Double])

    it "hist xs ≡ histogram (inline xs)" $
      hist [1, 2, 3] `shouldBe` histogram (inline [1, 2, 3 :: Double])

    it "plotY は index を x に取る (= 0,1,2)" $
      case getLast (lyEncX (plotY [10, 20, 30])) of
        Just (ColNum v) -> V.toList v `shouldBe` [0, 1, 2]
        _               -> expectationFailure "encX が ColNum でない"

    it "overlay [a,b] は 2 layer の VisualSpec" $
      length (vsLayers (overlay [points [1] [2], lineXY [1] [2]])) `shouldBe` 2

    it "plots は overlay の別名" $
      plots [points [1] [2]] `shouldBe` overlay [points [1] [2]]

  -- =========================================================================
  -- Phase 11 A4-a: scale reverse (軸反転 = range 入替)
  -- =========================================================================
  describe "scale reverse (Phase 11 A4-a)" $ do
    let mk extra = computeLayout emptyResolver (overlay [points [0, 5, 10] [0, 5, 10]] <> extra)
        normal = mk mempty

    it "reverseX setter は vsReverseX のみ立てる" $
      (getLast (vsReverseX reverseX), getLast (vsReverseY reverseX))
        `shouldBe` (Just True, Nothing)

    it "通常 X は単調増加 (x=0 が x=10 より小 px)" $
      (scaleApply (lpXScale normal) 0 < scaleApply (lpXScale normal) 10) `shouldBe` True

    it "reverseX で X が単調減少 (x=0 が x=10 より大 px)" $
      let rev = mk reverseX
      in (scaleApply (lpXScale rev) 0 > scaleApply (lpXScale rev) 10) `shouldBe` True

    it "reverseX は range 入替なので px の和が保存 (rev v + normal v = 一定)" $
      let rev = mk reverseX
          s0  = scaleApply (lpXScale rev) 0 + scaleApply (lpXScale normal) 0
          s10 = scaleApply (lpXScale rev) 10 + scaleApply (lpXScale normal) 10
      in abs (s0 - s10) `shouldSatisfy` (< 1e-9)

    it "reverseY で Y が単調増加 (通常は減少 = 上が大)" $
      let revY' = mk reverseY
      in (scaleApply (lpYScale revY') 0 < scaleApply (lpYScale revY') 10) `shouldBe` True

    it "reverse 無指定なら scale は従来通り (X 増加・Y 減少)" $
      ( scaleApply (lpXScale normal) 0 < scaleApply (lpXScale normal) 10
      , scaleApply (lpYScale normal) 0 > scaleApply (lpYScale normal) 10 )
        `shouldBe` (True, True)

  -- =========================================================================
  -- Phase 11 A7-a: coord_cartesian(xlim,ylim) = データ非破棄 zoom
  -- =========================================================================
  describe "coord_cartesian zoom (Phase 11 A7-a)" $ do
    -- 11 点 (x=0..10) の scatter。 zoom x∈[2,6] で窓外 8 点は描画 clip だが残る。
    let xs11 = [0,1,2,3,4,5,6,7,8,9,10] :: [Double]
        spec extra = overlay [points xs11 xs11] <> extra
        mk extra = computeLayout emptyResolver (spec extra)
        zoom = mk (coordCartesian 2 6 0 40)
        a = lpPlotArea zoom

    it "coordCartesianX setter は vsCoordXLim のみ立てる" $
      ( getLast (vsCoordXLim (coordCartesianX 2 6))
      , getLast (vsCoordYLim (coordCartesianX 2 6)) )
        `shouldBe` (Just (2, 6), Nothing)

    it "coordCartesian は X/Y 両 lim を合成する" $
      ( getLast (vsCoordXLim (coordCartesian 2 6 0 40))
      , getLast (vsCoordYLim (coordCartesian 2 6 0 40)) )
        `shouldBe` (Just (2, 6), Just (0, 40))

    it "zoom 範囲の下端/上端が panel 左/右端に張り付く (domain 上書き)" $
      ( abs (scaleApply (lpXScale zoom) 2 - rX a) < 1e-6
      , abs (scaleApply (lpXScale zoom) 6 - (rX a + rW a)) < 1e-6 )
        `shouldBe` (True, True)

    it "窓外データ (x=0) は panel 左端より外に投影される (= clip 対象)" $
      (scaleApply (lpXScale zoom) 0 < rX a) `shouldBe` True

    it "データは落とさない (zoom でも 11 点すべて PCircle が出る)" $
      let ps = renderToPrimitives emptyResolver zoom (spec (coordCartesian 2 6 0 40))
      in length [() | PCircle{} <- ps] `shouldBe` 11

    it "zoom 時は glyph を panel に clip (PClipPush/PClipPop が発行される)" $
      let ps = renderToPrimitives emptyResolver zoom (spec (coordCartesian 2 6 0 40))
      in ( length [() | PClipPush{} <- ps], length [() | PClipPop <- ps] )
           `shouldBe` (1, 1)

    it "zoom 無指定なら clip プリミティブは出ない (従来同一)" $
      let l  = mk mempty
          ps = renderToPrimitives emptyResolver l (spec mempty)
      in length [() | PClipPush{} <- ps] `shouldBe` 0

  -- =========================================================================
  -- Phase 11 A7-b: facet free scales (panel ごと独立 domain)
  -- =========================================================================
  describe "facet free scales (Phase 11 A7-b)" $ do
    -- 2 群 A/B で y のスケールが大きく違う (A: 1..2, B: 100..200)。
    let facetRes nm = case nm of
          "x" -> Just (NumData (V.fromList [1, 2, 1, 2]))
          "y" -> Just (NumData (V.fromList [1, 2, 100, 200]))
          "g" -> Just (TxtData (V.fromList ["A", "A", "B", "B"]))
          _   -> Nothing
        baseSpec = layer (scatter "x" "y" <> colorBy "g") <> facet "g"
        renderWith extra =
          let s = baseSpec <> extra
          in renderToPrimitives facetRes (computeLayout facetRes s) s
        textCount ps = length [() | PText{} <- ps]

    it "facetScales setter は vsFacetScales を立てる" $
      getLast (vsFacetScales (facetScales FacetFree)) `shouldBe` Just FacetFree

    it "freeScaleX / freeScaleY の真理値表" $
      ( map freeScaleX [FacetFixed, FacetFreeX, FacetFreeY, FacetFree]
      , map freeScaleY [FacetFixed, FacetFreeX, FacetFreeY, FacetFree] )
        `shouldBe` ( [False, True, False, True], [False, False, True, True] )

    it "free scales は fixed より PText が多い (各 panel に独立 y 軸が出る)" $
      (textCount (renderWith (facetScales FacetFree)) > textCount (renderWith mempty))
        `shouldBe` True

    it "free-y は panel B の大きい値の tick ラベル (150) を含む" $
      let ps = renderWith (facetScales FacetFreeY)
          texts = [t | PText _ t _ <- ps]
      in elem "150" texts `shouldBe` True

    -- facet_grid free scales + space (列ごと x / 行ごと y 共有 domain)
    let gridRes nm = case nm of
          "x" -> Just (NumData (V.fromList [0, 1, 0, 10, 0, 1, 0, 10]))   -- col L: 0..1, col R: 0..10
          "y" -> Just (NumData (V.fromList [1, 2, 1, 2, 100, 200, 100, 200])) -- row T: 1..2, row B: 100..200
          "c" -> Just (TxtData (V.fromList ["L", "L", "R", "R", "L", "L", "R", "R"]))
          "r" -> Just (TxtData (V.fromList ["T", "T", "T", "T", "B", "B", "B", "B"]))
          _   -> Nothing
        gridSpec extra = layer (scatter "x" "y") <> facetGrid "r" "c" <> extra
        renderGrid extra =
          let s = gridSpec extra
          in renderToPrimitives gridRes (computeLayout gridRes s) s

    it "facetSpace setter は vsFacetSpace を立てる" $
      getLast (vsFacetSpace (facetSpace SpaceFree)) `shouldBe` Just SpaceFree

    -- ★ Phase 34: tick ラベルは break ベクトル全体で小数桁統一 (formatTicksGG)。
    -- col R (0..10) の break は [0,2.5,5,7.5,10] ゆえ "10.0" (ggplot も "0.0|2.5|..|10.0")。
    it "facet_grid free-x は列ごとに x tick が異なる (col R の 10.0 が出る)" $
      let texts = [t | PText _ t _ <- renderGrid (facetScales FacetFreeX)]
      in elem "10.0" texts `shouldBe` True

    it "facet_grid space free-x で列幅が x 範囲に比例 (R 列が L 列より広い)" $
      let psFree = renderGrid (facetScales FacetFreeX <> facetSpace SpaceFreeX)
          -- 上 strip 背景帯 (col 名) の PRect は h = stripTopH(18)。 幅 = 列幅。
          -- col L (x 0..1) < col R (x 0..10) なので R が約 10 倍広い。
          stripWidths = [ w | PRect (Rect _ _ w h) _ _ <- psFree, abs (h - 18) < 0.01 ]
      in case stripWidths of
           (wL : wR : _) -> (wR > wL * 5) `shouldBe` True
           _             -> expectationFailure "col strip 幅が 2 つ取れない"

  -- =========================================================================
  -- Phase 11 A7-c: coord_polar (極座標投影)
  -- =========================================================================
  describe "coord_polar (Phase 11 A7-c)" $ do
    let lay = computeLayout emptyResolver
                (overlay [points [0, 1, 2, 3] [0, 1, 2, 3]] <> coordPolar)
        (ccx, ccy, cmaxR) = polarCenter lay

    it "coordPolar setter は vsCoord = CoordPolarX を立てる" $
      getLast (vsCoord coordPolar) `shouldBe` Just CoordPolarX

    it "coordPolarY setter は vsCoord = CoordPolarY を立てる" $
      getLast (vsCoord coordPolarY) `shouldBe` Just CoordPolarY

    it "isPolar: polar のみ True" $
      map isPolar [CoordCartesian, CoordFlip, CoordPolarX, CoordPolarY]
        `shouldBe` [False, False, True, True]

    it "polarPoint: r=0 は中心、 θ=0 r=1 は真上 (cx, cy-maxR)" $
      let (x0, y0) = polarPoint lay 0 0
          (xt, yt) = polarPoint lay 0 1
      in ( abs (x0 - ccx) < 1e-9 && abs (y0 - ccy) < 1e-9
         , abs (xt - ccx) < 1e-9 && abs (yt - (ccy - cmaxR)) < 1e-9 )
           `shouldBe` (True, True)

    it "polarPoint: θ=0.25 (= 90°) r=1 は右 (cx+maxR, cy)" $
      let (xr, yr) = polarPoint lay 0.25 1
      in ( abs (xr - (ccx + cmaxR)) < 1e-6, abs (yr - ccy) < 1e-6 )
           `shouldBe` (True, True)

    it "polar の grid は同心円 (PCircle) を含む (直交 grid line でなく円)" $
      let ps = renderToPrimitives emptyResolver lay
                 (overlay [points [0, 1, 2, 3] [0, 1, 2, 3]] <> coordPolar)
      in (length [() | PCircle{} <- ps] > 0) `shouldBe` True

    it "polar + bar は扇形 (PPath) を bar の数だけ出す" $
      let s = layer (bars [1, 2, 3, 4] [4, 7, 5, 9]) <> coordPolar
          ps = renderToPrimitives emptyResolver (computeLayout emptyResolver s) s
      in length [() | PPath{} <- ps] `shouldBe` 4

  -- =========================================================================
  -- Phase 11 A4-b: linetype aesthetic (固定 + categorical 群分け)
  -- =========================================================================
  describe "linetype (Phase 11 A4-b)" $ do
    it "lineTypeDash: Solid=[] / Dashed=[4,4]" $
      (lineTypeDash LtSolid, lineTypeDash LtDashed) `shouldBe` ([], [4, 4])

    it "lineTypeForIndex 巡回: 0=Solid, 1=Dashed, 6=Solid" $
      (lineTypeForIndex 0, lineTypeForIndex 1, lineTypeForIndex 6)
        `shouldBe` (LtSolid, LtDashed, LtSolid)

    it "linetype setter は lyLinetype を立てる" $
      getLast (lyLinetype (linetype LtDashed)) `shouldBe` Just LtDashed

    it "line + linetype LtDashed で線分 (3点=2本) の lsDash が [4,4]" $
      let spec = layer (line (inline [0, 1, 2 :: Double]) (inline [0, 1, 2 :: Double])
                        <> linetype LtDashed)
          ps = renderToPrimitives emptyResolver (computeLayout emptyResolver spec) spec
      in length [ () | PLine _ _ (LineStyle _ _ d) <- ps, d == [4, 4] ] `shouldBe` 2

    it "linetypeBy で群 B (3点=2本) のみ dashed、 群 A は実線" $
      let spec = layer (line (inline [0, 1, 2, 0, 1, 2 :: Double])
                             (inline [0, 1, 2, 3, 4, 5 :: Double])
                        <> linetypeBy (inlineCat (["A", "A", "A", "B", "B", "B"] :: [Data.Text.Text])))
          ps = renderToPrimitives emptyResolver (computeLayout emptyResolver spec) spec
      in length [ () | PLine _ _ (LineStyle _ _ d) <- ps, d == [4, 4] ] `shouldBe` 2

  -- =========================================================================
  -- Phase 11 A4-c: legendTitle (= scale name / labs(color=))
  -- =========================================================================
  describe "legendTitle (Phase 11 A4-c)" $ do
    it "legendTitle setter は vsLegendTitle を立てる" $
      getLast (vsLegendTitle (legendTitle "Series")) `shouldBe` Just (Data.Text.pack "Series")

    it "未指定なら vsLegendTitle = Nothing (= 従来通り凡例タイトル非表示)" $
      getLast (vsLegendTitle (mempty :: VisualSpec)) `shouldBe` Nothing

    it "legendTitle 指定で凡例に PText 'Series' が出る (color group + legend)" $
      let res k = case k of
            "x" -> Just (NumData (V.fromList [0, 1, 2, 3 :: Double]))
            "y" -> Just (NumData (V.fromList [0, 1, 2, 3 :: Double]))
            "g" -> Just (TxtData (V.fromList ["A", "A", "B", "B"]))
            _   -> Nothing
          spec = layer (scatter (ColByName "x") (ColByName "y") <> colorBy (ColByName "g"))
                 <> legend <> legendTitle "Series"
          ps = renderToPrimitives res (computeLayout res spec) spec
      in any (\p -> case p of PText _ t _ -> t == Data.Text.pack "Series"; _ -> False) ps
           `shouldBe` True

  -- =========================================================================
  -- Phase 11 A4-d: 明示 breaks / labels (= ggplot scale_*_continuous(breaks=,labels=))
  -- =========================================================================
  describe "explicit breaks/labels (Phase 11 A4-d)" $ do
    let res k = case k of
          "x" -> Just (NumData (V.fromList [0, 100 :: Double]))
          "y" -> Just (NumData (V.fromList [0, 100 :: Double]))
          _   -> Nothing
        baseSpec extra = layer (scatter (ColByName "x") (ColByName "y")) <> extra

    it "axisBreaksAt setter は axTickVals を立てる" $
      axTickValsOf (Last (Just (axisBreaksAt [0, 25, 50]))) `shouldBe` [0, 25, 50]

    it "axisBreaksLabeled は axTickVals/axTickLabels を対で立てる" $
      let as = axisBreaksLabeled [(0, "lo"), (50, "mid"), (100, "hi")]
      in ( axTickValsOf (Last (Just as))
         , axTickLabelsOf (Last (Just as)) )
         `shouldBe` ([0, 50, 100], map Data.Text.pack ["lo", "mid", "hi"])

    it "axisBreaksAt で lpXTicks が明示値に上書きされる (範囲内のみ)" $
      let spec = baseSpec (xAxis (axisBreaksAt [0, 25, 50, 75, 100]))
          l = computeLayout res spec
      in lpXTicks l `shouldBe` [0, 25, 50, 75, 100]

    it "範囲外の break は censor される" $
      -- padded range は概ね [-5,105] なので 200 は落ちる
      let spec = baseSpec (xAxis (axisBreaksAt [0, 50, 200]))
          l = computeLayout res spec
      in lpXTicks l `shouldBe` [0, 50]

    it "axisBreaksLabeled で lpXTickLabels が整列して入る" $
      let spec = baseSpec (xAxis (axisBreaksLabeled [(0, "lo"), (50, "mid"), (100, "hi")]))
          l = computeLayout res spec
      in (lpXTicks l, lpXTickLabels l)
           `shouldBe` ([0, 50, 100], map Data.Text.pack ["lo", "mid", "hi"])

    it "breaks のみ (labels 無し) なら lpXTickLabels は空 (= 値 format に委ねる)" $
      let spec = baseSpec (xAxis (axisBreaksAt [0, 50, 100]))
          l = computeLayout res spec
      in lpXTickLabels l `shouldBe` []

    it "未指定なら従来通り (lpXTickLabels 空・auto tick)" $
      let l = computeLayout res (baseSpec mempty)
      in lpXTickLabels l `shouldBe` []

    it "明示ラベルが render の tick PText に出る" $
      let spec = baseSpec (xAxis (axisBreaksLabeled [(0, "start"), (100, "end")]))
          ps = renderToPrimitives res (computeLayout res spec) spec
          hasTxt s = any (\p -> case p of PText _ t _ -> t == Data.Text.pack s; _ -> False) ps
      in (hasTxt "start", hasTxt "end") `shouldBe` (True, True)

  -- =========================================================================
  -- Phase 11 A4-e: 色/サイズ scale 拡充 (manual / gradient2 / size)
  -- =========================================================================
  describe "color/size scales (Phase 11 A4-e)" $ do
    let circFills ps = [ c | PCircle _ _ (FillStyle c _) _ _ <- ps ]
        circRadii ps = [ rad | PCircle _ rad _ _ _ <- ps ]
        tp s = Data.Text.pack s

    it "scaleColorManual setter は vsColorManual を立てる" $
      getLast (vsColorManual (scaleColorManual [(tp "A", tp "#ff0000")]))
        `shouldBe` Just [(tp "A", tp "#ff0000")]

    it "scaleColorGradient2 setter は vsColorGradient2 を立てる" $
      getLast (vsColorGradient2 (scaleColorGradient2 (tp "#00f") (tp "#fff") (tp "#f00") 0.0))
        `shouldBe` Just (tp "#00f", tp "#fff", tp "#f00", 0.0)

    it "scaleSize setter は vsSizeRange を立てる" $
      getLast (vsSizeRange (scaleSize 2 12)) `shouldBe` Just (2, 12)

    it "scaleColorManual で該当カテゴリが指定色になる (未登録は palette)" $
      let res k = case k of
            "x" -> Just (NumData (V.fromList [0, 1, 2, 3 :: Double]))
            "y" -> Just (NumData (V.fromList [0, 1, 2, 3 :: Double]))
            "g" -> Just (TxtData (V.fromList ["A", "A", "B", "B"]))
            _   -> Nothing
          spec = layer (scatter (ColByName "x") (ColByName "y") <> colorBy (ColByName "g"))
                 <> scaleColorManual [(tp "A", tp "#123456"), (tp "B", tp "#abcdef")]
          fills = circFills (renderToPrimitives res (computeLayout res spec) spec)
      -- 先頭 4 = データ点、 末尾 2 = 凡例 swatch。 両方とも manual 色 (= 凡例と panel が一致)。
      in fills `shouldBe` map tp ["#123456", "#123456", "#abcdef", "#abcdef", "#123456", "#abcdef"]

    it "scaleColorGradient2 で midpoint 値が mid 色になる" $
      let res k = case k of
            "x" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "y" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "z" -> Just (NumData (V.fromList [-1, 0, 1 :: Double]))  -- midpoint 0 が中央
            _   -> Nothing
          spec = layer (scatter (ColByName "x") (ColByName "y") <> colorContinuousBy (ColByName "z"))
                 <> scaleColorGradient2 (tp "#0000ff") (tp "#ffffff") (tp "#ff0000") 0.0
          fills = circFills (renderToPrimitives res (computeLayout res spec) spec)
      in (fills !! 1) `shouldBe` tp "#ffffff"   -- z=0 (midpoint) → mid 色 (白)

    it "scaleSize で sizeBy の直径範囲が指定値になる (★Phase 34 A3: size=直径ゆえ半径=直径/2)" $
      let res k = case k of
            "x" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "y" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "s" -> Just (NumData (V.fromList [10, 20, 30 :: Double]))
            _   -> Nothing
          spec = layer (scatter (ColByName "x") (ColByName "y") <> sizeBy (ColByName "s"))
                 <> scaleSize 4 16
          radii = circRadii (renderToPrimitives res (computeLayout res spec) spec)
      in (minimum radii, maximum radii) `shouldBe` (2, 8)  -- 直径範囲 (4,16) → 半径 (2,8)

  -- =========================================================================
  -- Phase 19: color 凡例整合 (glyph 色と凡例 swatch が同じ正本を参照する)
  -- =========================================================================
  describe "Phase 19: color 凡例整合" $ do
    let circFills ps = [ c | PCircle _ _ (FillStyle c _) _ _ <- ps ]
        tp = Data.Text.pack

    -- A1 再現: `<>` 重畳の ColorByCol で glyph が layer 内 nub、 凡例が全 layer
    -- union を引いてズレる。 layer2 ("C" のみ) の glyph は凡例 "C" swatch と
    -- 同色でなければならない (旧バグ: palette 先頭 = 凡例 "A" の色になる)。
    it "重畳 ColorByCol レイヤの glyph 色 = 凡例 swatch 色 (A1)" $
      let res k = case k of
            "x1" -> Just (NumData (V.fromList [0, 1 :: Double]))
            "y1" -> Just (NumData (V.fromList [0, 1 :: Double]))
            "g1" -> Just (TxtData (V.fromList ["A", "B"]))
            "x2" -> Just (NumData (V.fromList [2 :: Double]))
            "y2" -> Just (NumData (V.fromList [2 :: Double]))
            "g2" -> Just (TxtData (V.fromList ["C"]))
            _    -> Nothing
          spec = layer (scatter (ColByName "x1") (ColByName "y1") <> colorBy (ColByName "g1"))
              <> layer (scatter (ColByName "x2") (ColByName "y2") <> colorBy (ColByName "g2"))
          fills = circFills (renderToPrimitives res (computeLayout res spec) spec)
      -- 円 6 個 = data (A,B,C) + 凡例 swatch (A,B,C union 順)
      in (length fills, fills !! 2 == fills !! 5, fills !! 2 /= fills !! 3)
           `shouldBe` (6, True, True)

    it "単一 ColorByCol layer は従来配色のまま (glyph = 凡例・回帰)" $
      let res k = case k of
            "x" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "y" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "g" -> Just (TxtData (V.fromList ["A", "B", "A"]))
            _   -> Nothing
          spec = layer (scatter (ColByName "x") (ColByName "y") <> colorBy (ColByName "g"))
          fills = circFills (renderToPrimitives res (computeLayout res spec) spec)
      -- data (A,B,A) + 凡例 (A,B): glyph と凡例が対応し、 A 2 点は同色
      in (length fills, fills !! 0 == fills !! 3, fills !! 1 == fills !! 4,
          fills !! 0 == fills !! 2, fills !! 0 /= fills !! 1)
           `shouldBe` (5, True, True, True, True)

    -- A2 再現: bar + ColorByCol が PosIdentity で無条件 renderBarSimple (単色)
    -- に落ち、 本体単色なのに凡例は palette swatch を並べる。
    it "bar PosIdentity + ColorByCol で本体が色分けされ凡例と一致 (A2)" $
      let res k = case k of
            "x" -> Just (TxtData (V.fromList ["a", "b"]))
            "y" -> Just (NumData (V.fromList [1, 2 :: Double]))
            "g" -> Just (TxtData (V.fromList ["A", "B"]))
            _   -> Nothing
          spec = layer (bar (ColByName "x") (ColByName "y") <> colorBy (ColByName "g"))
          prims = renderToPrimitives res (computeLayout res spec) spec
          -- 背景 PRect (#ffffff) と凡例キー背景 (grey95 #f2f2f2・Phase 34) を除外し
          -- bar 本体 + 凡例 swatch のみ拾う
          rectFills = [ c | PRect _ (FillStyle c _) _ <- prims
                          , c /= tp "#ffffff", c /= tp "#f2f2f2" ]
      -- PRect = bar 本体 (A,B) + 凡例 swatch (A,B)。 本体 2 色が分かれ、
      -- 凡例 swatch と pairwise 一致する
      in (length rectFills, rectFills !! 0 == rectFills !! 2,
          rectFills !! 1 == rectFills !! 3, rectFills !! 0 /= rectFills !! 1)
           `shouldBe` (4, True, True, True)

    -- Phase 30 A3: 固定 shape combinator (bare=固定・shapeBy より優先)
    it "shape s は固定で全点に適用され shapeBy より優先 (A3)" $
      let ly = scatter (ColByName "x") (ColByName "y")
                 <> shape MShTriangle <> shapeBy (ColByName "g")
      in pointShapeAt ly emptyResolver 0 `shouldBe` MShTriangle
    it "shape 未指定かつ shapeBy なしは MShCircle (A3)" $
      let ly = scatter (ColByName "x") (ColByName "y")
      in pointShapeAt ly emptyResolver 0 `shouldBe` MShCircle

    -- A2 はみ出し fix: 旧実装は categorical x を row index (0..n-1) に置いて
    -- おり、 カテゴリ重複行が x domain を超えて plot 域外に描かれていた。
    -- cat index 配置で重複行は同 slot に重ね描き (ggplot identity 同型)。
    it "bar categorical x の重複行が plot 域内 (cat index 配置・A2)" $
      let res k = case k of
            "x" -> Just (TxtData (V.fromList ["a", "b", "a"]))
            "y" -> Just (NumData (V.fromList [1, 2, 3 :: Double]))
            _   -> Nothing
          spec  = layer (bar (ColByName "x") (ColByName "y"))
          lay   = computeLayout res spec
          area  = lpPlotArea lay
          rects = [ rc | PRect rc (FillStyle c _) _
                           <- renderToPrimitives res lay spec
                       , c /= tp "#ffffff" ]
      in (length rects,
          all (\rc -> rX rc + rW rc <= rX area + rW area + 1e-9) rects,
          rX (rects !! 0) == rX (rects !! 2))   -- 重複 cat "a" は同 slot
           `shouldBe` (3, True, True)

    it "bar PosIdentity + ColorStatic は従来単色のまま (回帰)" $
      let res k = case k of
            "x" -> Just (TxtData (V.fromList ["a", "b"]))
            "y" -> Just (NumData (V.fromList [1, 2 :: Double]))
            _   -> Nothing
          spec = layer (bar (ColByName "x") (ColByName "y")
                        <> color (fromHex "#336699"))
          prims = renderToPrimitives res (computeLayout res spec) spec
          rectFills = [ c | PRect _ (FillStyle c _) _ <- prims, c /= tp "#ffffff" ]
      in rectFills `shouldBe` [tp "#336699", tp "#336699"]

  -- =========================================================================
  -- Phase 11 A5-a: labs サブシステム (subtitle / caption / tag + labs まとめ setter)
  -- =========================================================================
  describe "labs (Phase 11 A5-a)" $ do
    let tp = Data.Text.pack
        textsOf ps = [ t | PText _ t _ <- ps ]

    it "subtitle / caption / tag setter は各 field を立てる" $
      ( getLast (vsSubtitle (subtitle (tp "sub")))
      , getLast (vsCaption  (caption  (tp "cap")))
      , getLast (vsTag      (tag      (tp "T"))) )
        `shouldBe` (Just (tp "sub"), Just (tp "cap"), Just (tp "T"))

    it "labs まとめ setter は指定した label だけ合成する" $
      let s = labs emptyLabs { labsTitle = Just (tp "ti"), labsSubtitle = Just (tp "su")
                             , labsCaption = Just (tp "ca"), labsTag = Just (tp "tg")
                             , labsX = Just (tp "xx"), labsY = Just (tp "yy")
                             , labsColor = Just (tp "co") }
      in ( getLast (vsTitle s), getLast (vsSubtitle s), getLast (vsCaption s)
         , getLast (vsTag s), getLast (vsXLabel s), getLast (vsYLabel s)
         , getLast (vsLegendTitle s) )
           `shouldBe` ( Just (tp "ti"), Just (tp "su"), Just (tp "ca")
                      , Just (tp "tg"), Just (tp "xx"), Just (tp "yy"), Just (tp "co") )

    it "subtitle / caption / tag は描画され PText に出る" $
      let res k = case k of
            "x" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            "y" -> Just (NumData (V.fromList [0, 1, 2 :: Double]))
            _   -> Nothing
          spec = layer (scatter (ColByName "x") (ColByName "y"))
                 <> title (tp "T") <> subtitle (tp "sub") <> caption (tp "cap") <> tag (tp "G")
          ts = textsOf (renderToPrimitives res (computeLayout res spec) spec)
      in all (`elem` ts) (map tp ["T", "sub", "cap", "G"]) `shouldBe` True

  -- =========================================================================
  -- Phase 11 A5-c: guides (reverse / ncol / nrow + guideColorNone)
  -- =========================================================================
  describe "guides (Phase 11 A5-c)" $ do
    let tp = Data.Text.pack
        gres k = case k of
          "x" -> Just (NumData (V.fromList [0, 1, 2, 3 :: Double]))
          "y" -> Just (NumData (V.fromList [0, 1, 2, 3 :: Double]))
          "g" -> Just (TxtData (V.fromList ["A", "A", "B", "B"]))
          _   -> Nothing
        legendTexts spec =
          [ (t, py) | PText (Point _ py) t _ <- renderToPrimitives gres (computeLayout gres spec) spec
                    , t `elem` map tp ["A", "B"] ]
        baseSpec = layer (scatter (ColByName "x") (ColByName "y") <> colorBy (ColByName "g"))

    it "legendReverse / legendNcol / legendNrow setter が各 field を立てる" $
      ( getLast (vsLegendReverse legendReverse)
      , getLast (vsLegendNcol (legendNcol 2))
      , getLast (vsLegendNrow (legendNrow 3)) )
        `shouldBe` (Just True, Just 2, Just 3)

    it "guideColorNone は色凡例を消す (= 凡例テキスト無し)" $
      let spec = baseSpec <> legend <> guideColorNone
      in legendTexts spec `shouldBe` []

    it "legendReverse でキー順が逆になる (A が下、 B が上)" $
      let spec = baseSpec <> legend <> legendReverse
          ys = [ py | (lbl, py) <- legendTexts spec, lbl == tp "A" || lbl == tp "B" ]
          yA = head [ py | (lbl, py) <- legendTexts spec, lbl == tp "A" ]
          yB = head [ py | (lbl, py) <- legendTexts spec, lbl == tp "B" ]
      in (yB < yA, length ys) `shouldBe` (True, 2)

    it "legendReverse 無しは従来順 (A が上、 B が下)" $
      let spec = baseSpec <> legend
          yA = head [ py | (lbl, py) <- legendTexts spec, lbl == tp "A" ]
          yB = head [ py | (lbl, py) <- legendTexts spec, lbl == tp "B" ]
      in (yA < yB) `shouldBe` True

  -- =========================================================================
  -- Phase 11 A6: geom_text / geom_label (データ駆動ラベル)
  -- =========================================================================
  describe "text / label (Phase 11 A6)" $ do
    let tp = Data.Text.pack
        gres k = case k of
          "x" -> Just (NumData (V.fromList [1, 2, 3 :: Double]))
          "y" -> Just (NumData (V.fromList [1, 2, 3 :: Double]))
          "l" -> Just (TxtData (V.fromList ["a", "b", "c"]))
          _   -> Nothing
        textsOf ps = [ t | PText _ t _ <- ps ]
        rectsOf ps = [ r | r@PRect{} <- ps ]

    it "text は MText + lyLabel を立てる" $
      let ly = text (ColByName "x") (ColByName "y") (ColByName "l")
      in (getFirst (lyKind ly), getLast (lyLabel ly))
           `shouldBe` (Just MText, Just (ColByName "l"))

    it "text で各点に label 列の文字が出る" $
      let spec = layer (text (ColByName "x") (ColByName "y") (ColByName "l"))
          ts = textsOf (renderToPrimitives gres (computeLayout gres spec) spec)
      in all (`elem` ts) (map tp ["a", "b", "c"]) `shouldBe` True

    it "label は文字 + 背景矩形 (各点) を出す" $
      let spec = layer (label (ColByName "x") (ColByName "y") (ColByName "l"))
          prims = renderToPrimitives gres (computeLayout gres spec) spec
          ts = textsOf prims
          -- 背景矩形 (label box) = panel 背景/枠 を除いた幅の狭い矩形が 3 個
          boxes = [ () | PRect (Rect _ _ w _) _ _ <- prims, w < 100 ]
      in (all (`elem` ts) (map tp ["a", "b", "c"]), length boxes) `shouldBe` (True, 3)

  -- =========================================================================
  -- Phase 11 A6-2: Q-Q plot (geom_qq)
  -- =========================================================================
  describe "qq (Phase 11 A6-2)" $ do
    let sres k = case k of
          "s" -> Just (NumData (V.fromList [3.0, 1.0, 4.0, 1.5, 5.0, 9.0, 2.0]))
          _   -> Nothing
        circlesOf ps = [ (cx, cy) | PCircle (Point cx cy) _ _ _ _ <- ps ]

    it "qq は MQQ + encY を立てる (encX は持たない)" $
      let ly = qq (ColByName "s")
      in (getFirst (lyKind ly), getLast (lyEncY ly), getLast (lyEncX ly))
           `shouldBe` (Just MQQ, Just (ColByName "s"), Nothing)

    it "invNormCdf は対称で中央が 0 (Φ⁻¹(0.5)=0, Φ⁻¹(0.975)≈1.96)" $
      let mid  = abs (invNormCdf 0.5) < 1e-9
          sym  = abs (invNormCdf 0.975 + invNormCdf 0.025) < 1e-6
          z975 = abs (invNormCdf 0.975 - 1.959964) < 1e-4
      in (mid, sym, z975) `shouldBe` (True, True, True)

    it "qqPoints は y を昇順 (order statistic) に並べ x も単調増加" $
      let pts = qqPoints [3.0, 1.0, 4.0, 1.5, 5.0]
          ys  = map snd pts
          xs  = map fst pts
          asc zs = and (zipWith (<=) zs (drop 1 zs))
      in (ys, asc ys, asc xs) `shouldBe` ([1.0, 1.5, 3.0, 4.0, 5.0], True, True)

    it "qq で sample 点数ぶんの円が出る (= 7 個)" $
      let spec = layer (qq (ColByName "s"))
          ps   = renderToPrimitives sres (computeLayout sres spec) spec
      in length (circlesOf ps) `shouldBe` 7

  -- =========================================================================
  -- Phase 11 A6-3: heatmap (geom_tile)
  -- =========================================================================
  describe "heatmap (Phase 11 A6-3)" $ do
    -- 2×2 grid (long-form): (A,P)=1 (A,Q)=2 (B,P)=3 (B,Q)=4
    let hres k = case k of
          "hx" -> Just (TxtData (V.fromList ["A", "A", "B", "B"]))
          "hy" -> Just (TxtData (V.fromList ["P", "Q", "P", "Q"]))
          "hv" -> Just (NumData (V.fromList [1.0, 2.0, 3.0, 4.0]))
          _    -> Nothing
        -- セル矩形 = 連続色塗りの矩形 (白の panel/canvas 背景・h=3.5 の凡例 strip を除外)
        cellRects ps = [ () | PRect (Rect _ _ w h) (FillStyle f _) _ <- ps
                            , w > 50, h > 50, f /= "#ffffff" ]

    it "heatmap は MHeatmap + encX/encY + ColorByContinuous を立てる" $
      let ly = heatmap (ColByName "hx") (ColByName "hy") (ColByName "hv")
          isContinuous = case getLast (lyColor ly) of
            Just (ColorByContinuous (ColByName "hv")) -> True
            _                                         -> False
      in ( getFirst (lyKind ly)
         , getLast (lyEncX ly), getLast (lyEncY ly), isContinuous )
           `shouldBe` ( Just MHeatmap, Just (ColByName "hx")
                      , Just (ColByName "hy"), True )

    it "heatmap で grid セル数ぶんの矩形が出る (= 4 個)" $
      let spec = layer (heatmap (ColByName "hx") (ColByName "hy") (ColByName "hv"))
          ps   = renderToPrimitives hres (computeLayout hres spec) spec
      in length (cellRects ps) `shouldBe` 4

  -- =========================================================================
  -- contour (= 等高線図、 marching squares)
  -- =========================================================================
  describe "contour (等高線、 marching squares)" $ do
    -- 連続 x/y/z (5×5 grid = 25 点)、 z = x+y。 等値線を描く。
    let grid = [ (x, y) | x <- [0.0, 1.0, 2.0, 3.0, 4.0], y <- [0.0, 1.0, 2.0, 3.0, 4.0] ]
        cres k = case k of
          "cx" -> Just (NumData (V.fromList (map fst grid)))
          "cy" -> Just (NumData (V.fromList (map snd grid)))
          "cz" -> Just (NumData (V.fromList (map (\(x,y) -> x + y) grid)))
          _    -> Nothing
        -- 旧 binned heatmap のセル矩形 (白 0.3px 枠)。 等高線化で出なくなったことを確認。
        cellRectsC ps = [ () | PRect _ (FillStyle f _) (Just (StrokeStyle sc sw)) <- ps
                             , f /= "#ffffff", sc == "#ffffff", sw == 0.3 ]

    it "contour は MContour + encX/encY + ColorByContinuous を立てる" $
      let ly = contour (ColByName "cx") (ColByName "cy") (ColByName "cz")
          isCont = case getLast (lyColor ly) of
            Just (ColorByContinuous (ColByName "cz")) -> True
            _                                         -> False
      in (getFirst (lyKind ly), getLast (lyEncX ly), getLast (lyEncY ly), isCont)
           `shouldBe` (Just MContour, Just (ColByName "cx"), Just (ColByName "cy"), True)

    it "contour は等値線 (PLine) を描き、 binned heatmap の塗り矩形は出さない" $
      let spec   = layer (contour (ColByName "cx") (ColByName "cy") (ColByName "cz"))
          ps     = renderToPrimitives cres (computeLayout cres spec) spec
          nLines = length [ () | PLine{} <- ps ]
      -- 等高線は多数の線分、 旧 binned heatmap の塗り矩形は 0。
      in (cellRectsC ps == [], nLines > 30) `shouldBe` (True, True)

  -- =========================================================================
  -- Phase 11 A6-4: ECDF (stat_ecdf)
  -- =========================================================================
  describe "ecdf (Phase 11 A6-4)" $ do
    let eres k = case k of
          "es" -> Just (NumData (V.fromList [3.0, 1.0, 4.0, 1.0, 5.0]))
          _    -> Nothing
        linesOf ps = [ () | PLine{} <- ps ]

    it "ecdf は MEcdf + encX を立てる (encY は持たない)" $
      let ly = ecdf (ColByName "es")
      in (getFirst (lyKind ly), getLast (lyEncX ly), getLast (lyEncY ly))
           `shouldBe` (Just MEcdf, Just (ColByName "es"), Nothing)

    it "ecdfPoints は右連続の階段頂点を返す (n=4 → (x1,0) から始まり 2n 頂点)" $
      let pts = ecdfPoints [3.0, 1.0, 4.0, 2.0]
          ys  = map snd pts
      in (length pts, head pts, last ys) `shouldBe` (8, (1.0, 0.0), 1.0)

    it "ecdf の階段は 2n-1 本の線分 (n=5 → 9 本、 grid は別ストローク)" $
      let spec = layer (ecdf (ColByName "es"))
          ps   = renderToPrimitives eres (computeLayout eres spec) spec
          -- ecdf 線は default 色 (grid は pal.axis)。 default 色の線分のみ数える。
          ecLines = [ () | PLine _ _ (LineStyle col _ _) <- ps, col == "#1f77b4" ]
      in length ecLines `shouldBe` 9

  -- =========================================================================
  -- Phase 11 A6-4b: 区間 geom (linerange / pointrange / crossbar)
  -- =========================================================================
  describe "linerange / pointrange / crossbar (Phase 11 A6-4b)" $ do
    let rres k = case k of
          "rx" -> Just (NumData (V.fromList [1.0, 2.0, 3.0]))
          "ry" -> Just (NumData (V.fromList [3.0, 4.0, 5.0]))
          "re" -> Just (NumData (V.fromList [0.5, 0.6, 0.4]))
          _    -> Nothing
        render s = renderToPrimitives rres (computeLayout rres s) s
        circlesN ps = length [() | PCircle{} <- ps]
        rangeLines ps = length [() | PLine _ _ (LineStyle col _ _) <- ps, col == "#1f77b4"]
        -- Phase 41: crossbar 箱幅はデータ単位 (≈0.9×catUnitPx) になり px 固定 20px から
        --   広がった (x=[1,2,3] で ≈139px)。 上限を 60→300 に緩め panel 等の全幅矩形だけ除外。
        cellRectsR ps = length [() | PRect (Rect _ _ w _) (FillStyle f _) _ <- ps
                                   , w < 300, w > 2, f == "#1f77b4"]

    it "lineRange は MLineRange + x/y/errorY を立てる" $
      let ly = lineRange (ColByName "rx") (ColByName "ry") (ColByName "re")
      in (getFirst (lyKind ly), getLast (lyEncX ly), getLast (lyEncY ly), getLast (lyErrorY ly))
           `shouldBe` (Just MLineRange, Just (ColByName "rx"), Just (ColByName "ry"), Just (ColByName "re"))

    it "linerange は 3 本の縦線・点無し" $
      let ps = render (layer (lineRange (ColByName "rx") (ColByName "ry") (ColByName "re")))
      in (rangeLines ps, circlesN ps) `shouldBe` (3, 0)

    it "pointrange は 3 本の縦線 + 3 中心点" $
      let ps = render (layer (pointRange (ColByName "rx") (ColByName "ry") (ColByName "re")))
      in (rangeLines ps, circlesN ps) `shouldBe` (3, 3)

    it "crossbar は 3 箱 + 3 中央水平線" $
      let ps = render (layer (crossbar (ColByName "rx") (ColByName "ry") (ColByName "re")))
      in (cellRectsR ps, rangeLines ps) `shouldBe` (3, 3)

  -- Phase 41: resolutionOf (ggplot resolution(x) = 最小正間隔)。 cap データ単位化の基準。
  describe "resolutionOf (Phase 41)" $ do
    it "等間隔グリッドは間隔を返す" $
      resolutionOf [0, 2, 4, 6] `shouldBe` 2.0
    it "categorical 整数位置は 1" $
      resolutionOf [0, 1, 2, 3] `shouldBe` 1.0
    it "単一値は 1 (間隔なし)" $
      resolutionOf [5, 5, 5] `shouldBe` 1.0
    it "不揃いは最小正間隔" $
      resolutionOf [0, 1, 3, 3.5] `shouldBe` 0.5
    it "空は 1" $
      resolutionOf [] `shouldBe` 1.0

  -- =========================================================================
  -- Phase 11 A6-4c: stat_function (関数サンプリング → inline line)
  -- =========================================================================
  describe "statFunction (Phase 11 A6-4c)" $ do
    it "statFunction は f を n 点サンプルした inline line (MLine + ColNum) を作る" $
      let ly = statFunction (\x -> x * 2) 0.0 10.0 6
      in case (getFirst (lyKind ly), getLast (lyEncX ly), getLast (lyEncY ly)) of
           (Just MLine, Just (ColNum xs), Just (ColNum ys)) ->
             (V.toList xs, V.toList ys)
               `shouldBe` ([0.0, 2.0, 4.0, 6.0, 8.0, 10.0], [0.0, 4.0, 8.0, 12.0, 16.0, 20.0])
           other -> expectationFailure ("unexpected: " <> show other)

    it "statFunction の n<2 は 2 に切り上げ (端点 2 点)" $
      let ly = statFunction (\x -> x) 1.0 5.0 1
      in case getLast (lyEncX ly) of
           Just (ColNum xs) -> V.toList xs `shouldBe` [1.0, 5.0]
           _                -> expectationFailure "encX should be inline ColNum"

  describe "Phase 16 stat-in (statLm / statSmooth)" $ do
    it "statLm は MStatLM + encX/encY を持つ Layer" $
      case (getFirst (lyKind (statLm "x" "y")), getLast (lyEncX (statLm "x" "y"))
           , getLast (lyEncY (statLm "x" "y"))) of
        (Just MStatLM, Just _, Just _) -> True `shouldBe` True
        other -> expectationFailure ("unexpected: " <> show other)
    it "statSmooth は MStatSmooth + lyBinCount=n" $
      case (getFirst (lyKind (statSmooth "x" "y" 8)), getLast (lyBinCount (statSmooth "x" "y" 8))) of
        (Just MStatSmooth, Just 8) -> True `shouldBe` True
        other -> expectationFailure ("unexpected: " <> show other)
    it "装飾が通常 geom と同じく Layer field に乗る (statLm <> stroke 2 <> colorStatic)" $
      let ly = statLm "x" "y" <> stroke 2 <> color (fromHex "#d62728")
      in getLast (lyStroke ly) `shouldBe` Just 2
    it "renderer は未解決 MStat* を skip (band PPath = 0)" $
      let r n = case n of
            "x" -> Just (NumData (V.fromList [1,2,3,4,5]))
            "y" -> Just (NumData (V.fromList [2,4,6,8,10]))
            _   -> Nothing
          spec = layer (statLm "x" "y")
          ps   = renderToPrimitives r (computeLayout r spec) spec
      in length [() | PPath{} <- ps] `shouldBe` 0

  -- =========================================================================
  -- Phase 40 A3: hexbin binning core (hexbinCells = d3-hexbin)
  -- =========================================================================
  describe "Phase 40 A3: hexbinCells (六角ビニング)" $ do
    it "件数の総和 = 範囲内の点数 (件数保存)" $
      let pts = [ (x, y) | x <- [0.05, 0.15 .. 0.95], y <- [0.05, 0.15 .. 0.95] ]
          cells = hexbinCells 6 (0, 1) (0, 1) pts
      in sum (map hexCount cells) `shouldBe` length pts
    it "同一座標の点は 1 セルに集約 (件数 = 点数)" $
      let cells = hexbinCells 8 (0, 1) (0, 1) (replicate 7 (0.5, 0.5))
      in (length cells, map hexCount cells) `shouldBe` (1, [7])
    it "各セルは 6 頂点 (pointy-top)" $
      let cells = hexbinCells 4 (0, 1) (0, 1) [(0.3, 0.3), (0.7, 0.8)]
      in all ((== 6) . length . hexVerts) cells `shouldBe` True
    it "退化入力 (bins<=0 / 空) は空" $
      (hexbinCells 0 (0,1) (0,1) [(0.5,0.5)], hexbinCells 5 (0,1) (0,1) [])
        `shouldBe` ([], [])

  -- =========================================================================
  -- Phase 7 A7: gallery primitive count 回帰 test (golden)
  --   全 gallery spec を render し Primitive 本数を golden と突合。 1 chart を直すと
  --   別が静かに壊れる連鎖を機械検知する (目視に頼らない回帰検知の土台)。
  -- =========================================================================
  describe "gallery primitive count 回帰 (Phase 7 A7)" $
    it "全 gallery spec の primitive 本数が golden と一致" $ do
      mGalleryDir <- findGalleryDir
      case mGalleryDir of
        -- fixture (design/gallery) 非同梱の環境 (公開ツリー等) では skip。
        Nothing -> pendingWith "design/gallery fixture が無い環境のため skip"
        Just galleryDir -> do
          actual <- galleryCountsString galleryDir
          let goldenPath = galleryDir ++ "/primitive-counts.golden"
          exists <- doesFileExist goldenPath
          if not exists
            then writeFile goldenPath actual
                   >> pendingWith "golden 初回生成 (次回実行から比較)"
            else do golden <- readFile goldenPath
                    actual `shouldBe` golden


  -- =========================================================================
  -- Phase 24 A4: contour バグ修正 (規則 grid 直入力) + griddata + level + filled
  -- =========================================================================
  describe "Phase 24 A4: Griddata (規則 grid 検出 + k 近傍 IDW)" $ do
    it "detectGrid: 規則 grid を補間なしで厳密復元 (行 = y)" $
      Griddata.detectGrid [ (x, y, x * 10 + y) | x <- [0, 1, 2], y <- [0, 1] ]
        `shouldBe` Just ([0, 1, 2], [0, 1], [[0, 10, 20], [1, 11, 21]])
    it "detectGrid: 歯抜けの散布は Nothing (resampleKNN へ fallback)" $
      Griddata.detectGrid [(0, 0, 1), (1, 0, 2), (0, 1, 3)] `shouldBe` Nothing
    it "resampleKNN: データ点と一致するノードはその z に収束 (局所重み)" $
      let (_, _, g) = Griddata.resampleKNN 4 3 3 [ (x, y, x + y) | x <- [0, 1, 2], y <- [0, 1, 2] ]
      in abs ((g !! 0 !! 0) - 0) + abs ((g !! 2 !! 2) - 4) < 1e-6 `shouldBe` True

  describe "Phase 24 A4: contour level 指定 + filled contour" $ do
    let gridPts = [ (x, y, x * x + y * y) | i <- [0 .. 10 :: Int], j <- [0 .. 10 :: Int]
                  , let x = -2 + 0.4 * fromIntegral i, let y = -2 + 0.4 * fromIntegral j ]
        xs3 = [a | (a, _, _) <- gridPts]; ys3 = [b | (_, b, _) <- gridPts]
        zs3 = [c | (_, _, c) <- gridPts]
        mkSpec extra = layer (contour (inline xs3) (inline ys3) (inline zs3) <> extra)
        primsOf spec = renderToPrimitives emptyResolver (computeLayout emptyResolver spec) spec
        lineColors spec = Data.List.nub
          [ c | PLine _ _ (LineStyle c _ _) <- primsOf spec, c /= tpA "#888888", c /= tpA "#bbbbbb"
              , c /= tpA "#dddddd", c /= tpA "#444444", c /= tpA "#333333" ]
        tpA = Data.Text.pack
    it "既定 8 レベル (内側等間隔・クランプ廃止)" $
      length (lineColors (mkSpec mempty)) `shouldBe` 8
    it "contourLevels 4 で 4 レベル" $
      length (lineColors (mkSpec (contourLevels 4))) `shouldBe` 4
    it "contourBreaks [2] で 1 レベルのみ" $
      length (lineColors (mkSpec (contourBreaks [2]))) `shouldBe` 1
    it "contourFilled: 塗り PPath が出る (帯色 = level+1 種)" $
      let spec = layer (contourFilled (inline xs3) (inline ys3) (inline zs3)
                          <> contourLevels 4)
          fills = Data.List.nub [ c | PPath _ (FillStyle c _) _ <- primsOf spec ]
      in length fills `shouldBe` 5

  describe "Math.Special: logGamma" $ do
    it "logGamma 1 = 0 (Γ1=1)"      $ abs (logGamma 1)               < 1e-10 `shouldBe` True
    it "logGamma 2 = 0 (Γ2=1)"      $ abs (logGamma 2)               < 1e-10 `shouldBe` True
    it "logGamma 3 = ln 2"          $ abs (logGamma 3 - log 2)       < 1e-9  `shouldBe` True
    it "logGamma 5 = ln 24"         $ abs (logGamma 5 - log 24)      < 1e-9  `shouldBe` True
    it "logGamma 0.5 = ln √π"       $ abs (logGamma 0.5 - log (sqrt pi)) < 1e-8 `shouldBe` True

  describe "Math.Special: regIncompleteBeta" $ do
    it "I_x(1,1) = x (一様 CDF)" $
      all (\x -> abs (regIncompleteBeta 1 1 x - x) < 1e-9) [0.1,0.3,0.5,0.7,0.9]
        `shouldBe` True
    it "I_0.5(2,2) = 0.5 (対称)" $ abs (regIncompleteBeta 2 2 0.5 - 0.5) < 1e-9 `shouldBe` True
    it "端点 I_0 = 0 / I_1 = 1" $
      (regIncompleteBeta 3 5 0 == 0 && regIncompleteBeta 3 5 1 == 1) `shouldBe` True
    it "対称律 I_0.5(a,b) = 1 - I_0.5(b,a)" $
      abs (regIncompleteBeta 2 5 0.5 - (1 - regIncompleteBeta 5 2 0.5)) < 1e-10 `shouldBe` True
    it "単調増加 (x↑ で I↑)" $
      let xs = [0.05,0.1..0.95] in
      and (zipWith (<) (map (regIncompleteBeta 3 4) xs) (map (regIncompleteBeta 3 4) (tail xs)))
        `shouldBe` True

  describe "Math.Special: betaQuantile" $ do
    it "betaQuantile 0.5 1 1 = 0.5"  $ abs (betaQuantile 0.5 1 1 - 0.5) < 1e-9 `shouldBe` True
    it "betaQuantile 0.5 3 3 = 0.5 (対称)" $ abs (betaQuantile 0.5 3 3 - 0.5) < 1e-9 `shouldBe` True
    it "逆関数往復 I(betaQuantile q) ≈ q" $
      all (\(q,a,b) -> abs (regIncompleteBeta a b (betaQuantile q a b) - q) < 1e-9)
          [ (0.025,2,9), (0.5,5,5), (0.975,2,9), (0.1,1,1), (0.9,7,3) ]
        `shouldBe` True
    it "Benard 中央順位近似 (median ≈ (i-0.3)/(n+0.4))" $
      let n = 10 :: Int
          ok i = abs (betaQuantile 0.5 (fromIntegral i) (fromIntegral (n-i+1))
                      - (fromIntegral i - 0.3) / (fromIntegral n + 0.4)) < 0.01
      in all ok [1 .. n] `shouldBe` True

  where
    isMissing (PlotError MissingAesthetic{} _) = True
    isMissing _                                = False
    isNotFound (PlotError ColumnNotFound{} _)  = True
    isNotFound _                               = False
    isTypeMismatch (PlotError ColumnTypeMismatch{} _) = True
    isTypeMismatch _                                  = False
    isHoverWarn (PlotWarning (BackendUnsupported _ FeatHover) _) = True
    isHoverWarn _                                                = False

-- ===========================================================================
-- Phase 7 A7: gallery primitive count 回帰 test の helper (module level)
-- ===========================================================================

-- | design/gallery/specs/**/*.json を全て render し、 case ごとの Primitive
--   constructor 別本数を 1 行にまとめた文字列を返す (golden 比較用)。
--   ⚠ repo root を cwd として実行する前提 (cabal test を repo root から)。
galleryCountsString :: FilePath -> IO String
galleryCountsString galleryDir = do
  let specsDir = galleryDir ++ "/specs"
      prefix   = specsDir ++ "/"
  files <- listJsonRec specsDir
  rows  <- mapM (countRow prefix) (sort files)
  pure (unlines rows)
  where
    countRow prefix f = do
      bs <- BL.readFile f
      let rel = drop (length prefix) f
      case eitherDecode bs of
        Left err   -> pure (rel ++ ": DECODE-ERROR " ++ err)
        Right spec -> do
          let lay    = computeLayout emptyResolver spec
              prims  = renderToPrimitives emptyResolver lay spec
              counts = Map.toAscList
                         (Map.fromListWith (+) [(ctorName p, 1 :: Int) | p <- prims])
          pure (rel ++ ": " ++ unwords [c ++ "=" ++ show n | (c, n) <- counts])

-- | cwd から design/gallery を探す (cabal test の cwd が repo root か package
--   dir か実行環境で異なるため、 数段上まで候補を辿る)。
--   fixture 非同梱の環境 (公開ツリー等) では 'Nothing' (test 側で pendingWith skip)。
findGalleryDir :: IO (Maybe FilePath)
findGalleryDir = go [ up n ++ "design/gallery" | n <- [0 .. 4 :: Int] ]
  where
    up n = concat (replicate n "../")
    go []     = pure Nothing
    go (d:ds) = do
      e <- doesDirectoryExist d
      if e then pure (Just d) else go ds

-- | design/gallery/specs 配下を再帰列挙し .json のみ返す。
listJsonRec :: FilePath -> IO [FilePath]
listJsonRec dir = do
  entries <- listDirectory dir
  fmap concat (mapM step entries)
  where
    step e = do
      let full = dir </> e
      isDir <- doesDirectoryExist full
      if isDir then listJsonRec full
               else pure [full | takeExtension full == ".json"]

-- | Primitive の constructor 名 (count 集計キー)。
ctorName :: Primitive -> String
ctorName p = case p of
  PLine{}          -> "PLine"
  PRect{}          -> "PRect"
  PCircle{}        -> "PCircle"
  PPath{}          -> "PPath"
  PText{}          -> "PText"
  PClipPush{}      -> "PClipPush"
  PClipPop         -> "PClipPop"
  PTransformPush{} -> "PTransformPush"
  PTransformPop    -> "PTransformPop"

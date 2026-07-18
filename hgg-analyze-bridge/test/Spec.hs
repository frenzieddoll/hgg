-- | Phase 2 A2 test: ModelGraph → DAGSpec 変換の round-trip 保存。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Monoid
import qualified Data.Text

import qualified Hgg.Plot.Spec   as Spec
import           Hgg.Plot.Palette (ggplotHue)
import           Hanalyze.Model.HBM  (ModelGraph (..), Node (..),
                                      NodeKind (..))

import           Hgg.Plot.Bridge.Analyze
                                     (modelGraphToDAGEdges,
                                      modelGraphToDAGNodes,
                                      modelGraphToDAGPlates,
                                      modelGraphToDAGSpec,
                                      modelGraphToVisualSpec,
                                      renderModelGraphPDF,
                                      renderModelGraphPNG,
                                      renderModelGraphSVG,
                                      renderModelGraphSVGBytes)
import qualified Hgg.Plot.Bridge.Analyze.Internal as I

import           Hgg.Plot.Bridge.Stat (resolveStats)
import           Hgg.Plot.Frame       ((|>>), bpSpec, bpResolver)
import qualified Data.Vector         as V
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame as DX
import qualified Hanalyze.Model.LM   as LM
import           Hanalyze.Model.Core (fittedList)
import           Data.List           (sortOn)
import           Data.Monoid         (getFirst, getLast)

import           Test.Hspec

-- ===========================================================================
-- 代表 ModelGraph fixture
-- ===========================================================================

-- | 単純 LM 風: alpha (latent) + beta (latent) → y (observed, N=100)
simpleLM :: ModelGraph
simpleLM = ModelGraph
  { mgNodes =
      [ Node "alpha" LatentN          "Normal"   Set.empty []
      , Node "beta"  LatentN          "Normal"   Set.empty []
      , Node "y"     (ObservedN 100)  "Normal"   Set.empty []
      ]
  , mgEdges = [("alpha", "y"), ("beta", "y")]
  , mgPlates = Map.empty
  }

-- | 階層 LM 風: mu_g + sigma_g → mu (group plate G=5) → y (record plate R=200)
hierLM :: ModelGraph
hierLM = ModelGraph
  { mgNodes =
      [ Node "mu_g"  LatentN          "Normal"   Set.empty []
      , Node "sig_g" LatentN          "HalfCauchy" Set.empty []
      , Node "mu"    LatentN          "Normal"   Set.empty ["group"]
      , Node "y"     (ObservedN 200)  "Normal"   Set.empty ["record"]
      ]
  , mgEdges = [("mu_g", "mu"), ("sig_g", "mu"), ("mu", "y")]
  , mgPlates = Map.fromList [("group", 5), ("record", 200)]
  }

-- | nested plate (= record 内に condition plate がある):
-- mu → mu_c (= condition plate C=3、 record plate 外側) → y (record N=200 + condition)
nestedPlate :: ModelGraph
nestedPlate = ModelGraph
  { mgNodes =
      [ Node "mu"   LatentN         "Normal" Set.empty []
      , Node "mu_c" LatentN         "Normal" Set.empty ["condition"]
      , Node "y"    (ObservedN 200) "Normal" Set.empty ["record", "condition"]
      ]
  , mgEdges = [("mu", "mu_c"), ("mu_c", "y")]
  , mgPlates = Map.fromList [("record", 200), ("condition", 3)]
  }

-- ===========================================================================
-- Tests
-- ===========================================================================

main :: IO ()
main = hspec $ do

  describe "Phase 16 系統 B (resolveStats = stat-in 委譲)" $ do
    let xs = V.fromList [1,2,3,4,5,6,7,8] :: V.Vector Double
        ys = V.fromList [2.1,3.9,6.2,7.8,10.1,12.0,13.8,16.2] :: V.Vector Double
        r name
          | name == ("x" :: Data.Text.Text) = Just (Spec.NumData xs)
          | name == "y"                      = Just (Spec.NumData ys)
          | otherwise                        = Nothing
        -- df 1 回参照: scatter + statLm を <> で重ね resolveStats で展開
        specLM = Spec.layer (Spec.scatter "x" "y") <> Spec.layer (Spec.statLm "x" "y")
        specSm = Spec.layer (Spec.scatter "x" "y") <> Spec.layer (Spec.statSmooth "x" "y" 4)

    it "statLm: scatter + 回帰線 + 信頼帯 = 3 layer に展開" $
      length (Spec.vsLayers (resolveStats r specLM)) `shouldBe` 3
    it "statSmooth: scatter + 曲線 = 2 layer (帯なし)" $
      length (Spec.vsLayers (resolveStats r specSm)) `shouldBe` 2
    it "解決できない列の stat は除去 (scatter のみ残る)" $
      let bad = Spec.layer (Spec.scatter "x" "y") <> Spec.layer (Spec.statLm "x" "z")
      in length (Spec.vsLayers (resolveStats r bad)) `shouldBe` 1
    it "未解決 stat は resolveStats 後に残らない (全て band/line に展開)" $
      let kinds = [ getFirst (Spec.lyKind l) | l <- Spec.vsLayers (resolveStats r specLM) ]
      in (Just Spec.MStatLM `elem` kinds) `shouldBe` False

    it "回帰線の ŷ が analyze fitWithCI (= fitLM 直呼び) と一致 (委譲の証跡)" $ do
      let df = DX.fromNamedColumns
                 [ ("x", DX.fromList (V.toList xs))
                 , ("y", DX.fromList (V.toList ys)) ]
          Just (frRef, _) = LM.fitWithCI 0.95 df "x" "y"
          refSorted = map snd (sortOn fst (zip (V.toList xs) (fittedList frRef)))
          resolved  = Spec.vsLayers (resolveStats r (Spec.layer (Spec.statLm "x" "y")))
          lineLayer = resolved !! 1                    -- [band, line]
          Just (Spec.ColNum gotV) = getLast (Spec.lyEncY lineLayer)
      and (zipWith (\a b -> abs (a - b) < 1e-9) (V.toList gotV) refSorted)
        `shouldBe` True

    it "装飾 (stroke) が展開後の回帰線に引き継がれる" $ do
      let styled   = Spec.layer (Spec.statLm "x" "y" <> Spec.stroke 3)
          resolved = Spec.vsLayers (resolveStats r styled)
          lineLayer = resolved !! 1
      getLast (Spec.lyStroke lineLayer) `shouldBe` Just 3

    it "df 1 回参照 (|>> の bpResolver を resolveStats に渡す)" $ do
      let df    = Map.fromList [ ("x", Spec.NumData xs), ("y", Spec.NumData ys) ]
                    :: Map.Map Data.Text.Text Spec.ColData
          bound = df |>> (Spec.layer (Spec.scatter "x" "y") <> Spec.layer (Spec.statLm "x" "y"))
          spec  = resolveStats (bpResolver bound) (bpSpec bound)
      length (Spec.vsLayers spec) `shouldBe` 3

    -- B1: 信頼水準可変 + smooth 帯版
    it "statLmLevel: statLm 同様 3 layer (band+line) に展開" $
      length (Spec.vsLayers (resolveStats r
                (Spec.layer (Spec.statLmLevel "x" "y" 0.99)))) `shouldBe` 2
    it "statLmLevel 0.99 の帯は 0.95 より広い (band 半幅で確認)" $ do
      let halfWidth lvl =
            let resolved = Spec.vsLayers (resolveStats r (Spec.layer lvl))
                bandLy   = head resolved              -- [band, line]
                Just (Spec.ColNum lo) = getLast (Spec.lyEncY  bandLy)
                Just (Spec.ColNum hi) = getLast (Spec.lyEncY2 bandLy)
            in V.sum (V.zipWith (\h l -> h - l) hi lo)
      halfWidth (Spec.statLmLevel "x" "y" 0.99)
        `shouldSatisfy` (> halfWidth (Spec.statLm "x" "y"))
    it "statSmoothCI: scatter なしでも band+line = 2 layer (帯あり)" $
      length (Spec.vsLayers (resolveStats r
                (Spec.layer (Spec.statSmoothCI "x" "y" 4)))) `shouldBe` 2
    it "statSmooth (帯なし) は 1 layer・statSmoothCI (帯あり) は 2 layer" $ do
      length (Spec.vsLayers (resolveStats r (Spec.layer (Spec.statSmooth   "x" "y" 4)))) `shouldBe` 1
      length (Spec.vsLayers (resolveStats r (Spec.layer (Spec.statSmoothCI "x" "y" 4)))) `shouldBe` 2

    -- B3: statPoly / statResid
    it "statPoly: band+line = 2 layer に展開" $
      length (Spec.vsLayers (resolveStats r (Spec.layer (Spec.statPoly "x" "y" 2)))) `shouldBe` 2
    it "statResid: scatter 1 layer (残差 vs fitted)" $
      length (Spec.vsLayers (resolveStats r (Spec.layer (Spec.statResid "x" "y")))) `shouldBe` 1
    it "statResid の y = 残差 (= y - fitted、 委譲先 fitLM と一致)" $ do
      let df = DX.fromNamedColumns
                 [ ("x", DX.fromList (V.toList xs))
                 , ("y", DX.fromList (V.toList ys)) ]
          Just (frRef, _) = LM.fitWithCI 0.95 df "x" "y"
          residRef = zipWith (-) (V.toList ys) (fittedList frRef)
          resolved = Spec.vsLayers (resolveStats r (Spec.layer (Spec.statResid "x" "y")))
          Just (Spec.ColNum gotV) = getLast (Spec.lyEncY (head resolved))
      and (zipWith (\a b -> abs (a - b) < 1e-9) (V.toList gotV) residRef)
        `shouldBe` True
    it "statPoly deg=1 の ŷ は statLm と一致 (poly(x,1) = 線形)" $ do
      let polyY = let resolved = Spec.vsLayers (resolveStats r (Spec.layer (Spec.statPoly "x" "y" 1)))
                      Just (Spec.ColNum v) = getLast (Spec.lyEncY (resolved !! 1))
                  in V.toList v
          lmY   = let resolved = Spec.vsLayers (resolveStats r (Spec.layer (Spec.statLm "x" "y")))
                      Just (Spec.ColNum v) = getLast (Spec.lyEncY (resolved !! 1))
                  in V.toList v
      and (zipWith (\a b -> abs (a - b) < 1e-7) polyY lmY) `shouldBe` True

  describe "Phase 16 B2 (group 別 fit = geom_smooth(aes(color=g)))" $ do
    -- 2 群: g="a" (傾き 1) / g="b" (傾き 3)。 g 列は TxtData で解決。
    let n  = 6 :: Int
        xa = [1 .. fromIntegral n]
        xs = V.fromList (xa ++ xa)                            :: V.Vector Double
        ys = V.fromList (map (\x -> 1 * x + 0.5) xa
                      ++ map (\x -> 3 * x + 0.5) xa)          :: V.Vector Double
        gs = V.fromList (replicate n "a" ++ replicate n "b")  :: V.Vector Data.Text.Text
        rg name
          | name == ("x" :: Data.Text.Text) = Just (Spec.NumData xs)
          | name == "y"                     = Just (Spec.NumData ys)
          | name == "g"                     = Just (Spec.TxtData gs)
          | otherwise                       = Nothing

    it "色なし statLm は単群 (band+line = 2 layer)" $
      length (Spec.vsLayers (resolveStats rg (Spec.layer (Spec.statLm "x" "y")))) `shouldBe` 2
    it "color g つき statLm は 2 群 (band+line ×2 = 4 layer)" $
      length (Spec.vsLayers (resolveStats rg
                (Spec.layer (Spec.statLm "x" "y" <> Spec.colorBy "g")))) `shouldBe` 4
    it "群色 = 既定 theme series palette (= ColorByCol scatter と一致)" $ do
      let resolved = Spec.vsLayers (resolveStats rg
                       (Spec.layer (Spec.statLm "x" "y" <> Spec.colorBy "g")))
          -- line layer は index 1, 3 (各群の [band, line])
          colorOf l = getLast (Spec.lyColor l)
          -- ★Phase 28: 既定 series palette (themeSeriesPalette ThemeDefault) は具体色でなく
          --   hue sentinel ["__ggplot_hue__"] を返し、 renderer (Layout.catPal) /
          --   Bridge.resolveGrouped が群数 n で 'ggplotHue' n に展開する。 ここは 2 群なので
          --   群色は ggplotHue 2 (= ColorByCol scatter と同じ) と一致しなければならない。
          defPal = ggplotHue 2
      colorOf (resolved !! 1) `shouldBe` Just (Spec.ColorStatic (defPal !! 0))
      colorOf (resolved !! 3) `shouldBe` Just (Spec.ColorStatic (defPal !! 1))
    it "群別 fit の傾きが分離 (b 群の ŷ が a 群より急 = 末尾で大)" $ do
      let resolved = Spec.vsLayers (resolveStats rg
                       (Spec.layer (Spec.statLm "x" "y" <> Spec.colorBy "g")))
          lastY l = let Just (Spec.ColNum v) = getLast (Spec.lyEncY l) in V.last v
      -- a 群 line (idx1) の末尾 ŷ ≈ 6.5、 b 群 line (idx3) ≈ 18.5
      (lastY (resolved !! 3) > lastY (resolved !! 1)) `shouldBe` True

  describe "Phase 2 A2 mapNodeKind" $ do
    it "LatentN → NodeLatent" $
      I.mapNodeKind LatentN `shouldBe` Spec.NodeLatent
    it "ObservedN _ → NodeObserved (= 観測数は捨てる、 描画では使わない)" $ do
      I.mapNodeKind (ObservedN 1)   `shouldBe` Spec.NodeObserved
      I.mapNodeKind (ObservedN 200) `shouldBe` Spec.NodeObserved

  describe "Phase 2 A2 plateLabel" $ do
    it "name + size を \"<name> (N=<size>)\" に整形" $
      I.plateLabel "group" 5 `shouldBe` "group (N=5)"
    it "size 0 / 大きい数も整形できる" $ do
      I.plateLabel "" 0     `shouldBe` " (N=0)"
      I.plateLabel "x" 9999 `shouldBe` "x (N=9999)"

  describe "Phase 2 A2 modelGraphToDAGNodes" $ do
    it "simpleLM: node 数 = 元 mgNodes と一致 (= round-trip)" $
      length (modelGraphToDAGNodes simpleLM) `shouldBe` length (mgNodes simpleLM)
    it "simpleLM: node 名・ kind・ dist が正しく mapping" $ do
      let ns = modelGraphToDAGNodes simpleLM
          byId i = head [n | n <- ns, Spec.dnId n == i]
      Spec.dnKind  (byId "alpha") `shouldBe` Spec.NodeLatent
      Spec.dnKind  (byId "y")     `shouldBe` Spec.NodeObserved
      Spec.dnDist  (byId "alpha") `shouldBe` Just "Normal"
      Spec.dnLabel (byId "y")     `shouldBe` "y"
    it "空 distribution は dnDist = Nothing に正規化" $
      let mg = simpleLM { mgNodes = [ Node "n" LatentN "" Set.empty [] ] }
          (n:_) = modelGraphToDAGNodes mg
      in Spec.dnDist n `shouldBe` Nothing

  describe "Phase 2 A2 modelGraphToDAGEdges" $ do
    it "simpleLM: edge 数 = 元 mgEdges と一致" $
      length (modelGraphToDAGEdges simpleLM) `shouldBe` length (mgEdges simpleLM)
    it "edge from/to が保存、 dePath は Nothing default" $ do
      let es = modelGraphToDAGEdges simpleLM
          e = head es
      Spec.dePath e `shouldBe` Nothing
      (Spec.deFrom e, Spec.deTo e) `shouldBe` ("alpha", "y")

  describe "Phase 2 A2 modelGraphToDAGPlates" $ do
    it "plate 無し: 結果も空リスト" $
      modelGraphToDAGPlates simpleLM `shouldBe` []

    it "hierLM 2 plate: dpLabel に N=<size>、 dpNodeIds に該当 node 名" $ do
      let ps = modelGraphToDAGPlates hierLM
      length ps `shouldBe` 2
      let labels   = map Spec.dpLabel   ps
          memberss = map Spec.dpNodeIds ps
      "group (N=5)"    `elem` labels `shouldBe` True
      "record (N=200)" `elem` labels `shouldBe` True
      -- 各 plate の member が正しい
      let groupPlate  = head [p | p <- ps, Spec.dpLabel p == "group (N=5)"]
          recordPlate = head [p | p <- ps, Spec.dpLabel p == "record (N=200)"]
      Spec.dpNodeIds groupPlate  `shouldBe` ["mu"]
      Spec.dpNodeIds recordPlate `shouldBe` ["y"]
      -- 全 plate の memberss 連結が空でない
      concat memberss `shouldNotBe` []

    it "nested plate: 1 node が 2 plate に属するケースを正しく扱う" $ do
      let ps = modelGraphToDAGPlates nestedPlate
      -- y は record + condition の両方に member
      let recordP    = head [p | p <- ps, Spec.dpLabel p == "record (N=200)"]
          conditionP = head [p | p <- ps, Spec.dpLabel p == "condition (N=3)"]
      Spec.dpNodeIds recordP    `shouldBe` ["y"]
      ("y" `elem` Spec.dpNodeIds conditionP) `shouldBe` True

  describe "Phase 2 A2 modelGraphToDAGSpec round-trip" $ do
    it "simpleLM: (nodes, edges, plates) の length が ModelGraph 各要素数と一致" $ do
      let (ns, es, ps) = modelGraphToDAGSpec simpleLM
      length ns `shouldBe` length (mgNodes simpleLM)
      length es `shouldBe` length (mgEdges simpleLM)
      length ps `shouldBe` Map.size (mgPlates simpleLM)
    it "hierLM: 同上" $ do
      let (ns, es, ps) = modelGraphToDAGSpec hierLM
      length ns `shouldBe` length (mgNodes hierLM)
      length es `shouldBe` length (mgEdges hierLM)
      length ps `shouldBe` Map.size (mgPlates hierLM)
    it "nestedPlate: 同上 (= 多重所属 node も誤って数えない)" $ do
      let (ns, es, ps) = modelGraphToDAGSpec nestedPlate
      length ns `shouldBe` length (mgNodes nestedPlate)
      length es `shouldBe` length (mgEdges nestedPlate)
      length ps `shouldBe` Map.size (mgPlates nestedPlate)

  describe "Phase 2 A3 modelGraphToVisualSpec + renderModelGraphSVG" $ do
    it "simpleLM: VisualSpec が空でない layer を含む (= layout 適用された)" $ do
      let spec = modelGraphToVisualSpec simpleLM
      length (Spec.vsLayers spec) `shouldBe` 1

    it "hierLM: layout 適用済 (= dnX / dnY が 0 以外に埋まる)" $ do
      let spec = modelGraphToVisualSpec hierLM
          -- layer の DAGSpec から positioned nodes を取出し
          dagLayer = head (Spec.vsLayers spec)
          Just ds = Data.Monoid.getLast (Spec.lyDAG dagLayer)
          ns = Spec.dsNodes ds
          allXs = map Spec.dnX ns
          allYs = map Spec.dnY ns
      -- LayoutHierarchical 経由なら少なくとも 1 つは ≠ 0 (= layout 計算済)
      any (/= 0) allXs `shouldBe` True
      any (/= 0) allYs `shouldBe` True

    it "renderModelGraphSVG: ファイル書出し + 内容が SVG header を含む" $ do
      let tmpFile = "/tmp/hgg-analyze-bridge-test.svg"
      renderModelGraphSVG tmpFile "Test HBM" simpleLM
      contents <- readFile tmpFile
      take 4 contents `shouldBe` "<svg"

    it "renderModelGraphSVGBytes: Text 返却 + 同 spec 同 output" $ do
      let bytes1 = renderModelGraphSVGBytes "T" simpleLM
          bytes2 = renderModelGraphSVGBytes "T" simpleLM
      bytes1 `shouldBe` bytes2  -- 決定論性 (Phase 1 §10.5)
      "<svg" `Data.Text.isPrefixOf` bytes1 `shouldBe` True

    it "hierLM: SVG 出力に plate label が含まれる" $ do
      let bytes = renderModelGraphSVGBytes "Hier LM" hierLM
      "group (N=5)" `Data.Text.isInfixOf` bytes `shouldBe` True
      "record (N=200)" `Data.Text.isInfixOf` bytes `shouldBe` True

    it "node 名が SVG label として出力に含まれる" $ do
      let bytes = renderModelGraphSVGBytes "T" simpleLM
      "alpha" `Data.Text.isInfixOf` bytes `shouldBe` True
      "y"     `Data.Text.isInfixOf` bytes `shouldBe` True

  describe "Phase 2 A4 renderModelGraphPNG / PDF (= backend 未実装の stub)" $ do
    it "renderModelGraphPNG: 呼出時に error 投げる (= placeholder の honest 動作)" $
      renderModelGraphPNG "/tmp/x.png" "T" simpleLM
        `shouldThrow` anyErrorCall
    it "renderModelGraphPDF: 同上" $
      renderModelGraphPDF "/tmp/x.pdf" "T" simpleLM
        `shouldThrow` anyErrorCall

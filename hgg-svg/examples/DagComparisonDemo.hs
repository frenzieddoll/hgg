-- | Phase 1 A2-A4 完了レビュー用 比較 demo。
--
-- @
-- cabal run dag-comparison-demo
-- @
-- → design/dag-parity/ に 6 SVG を書き出す。
--
--   * k33-before.svg / k33-after.svg
--     ─ K3,3 風 reverse pattern (= A→Z, B→Y, C→X)。
--       v0.1 は ID alphabetical で 3 crossing、 A3+A4 後は 0 crossing。
--   * hbm-before.svg / hbm-after.svg
--     ─ 中規模 HBM ModelGraph (= 9 node、 latent 群 → tau 群 → y)。
--       v0.1 は alphabetical 等間隔、 A3+A4 後は median 整列 + 親 anchor。
--   * chains-before.svg / chains-after.svg (= A4 効果が明瞭)
--     ─ 2 平行 chain (a1→a2→a3、 b1→b2→b3)。
--       v0.1 は a/b が各 rank 内 alphabetical の左右 (= 直線 vertical)。
--       A4 後も a と b が垂直 chain として保たれる (= 親 anchor が効く)。
--     ─ 加えて cross-link (= a2→b3) を追加して、 A4 の TD+BU median 効果
--       (= 親 + 子の中庸位置) を視覚化する。
--
-- "before" は 'Graphics.Hgg.Spec.LayoutManual' + 手計算 alphabetical 位置で
-- v0.1 layout を再現 (= 旧 code を取り出さず、 既存の Manual layout 経路で simulate)。
-- "after" は通常の 'Graphics.Hgg.DAG.dagPlot' (= LayoutHierarchical default、
-- A2 network simplex + A3 median + transpose + A4 Brandes-Köpfe TD+BU median) を使う。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Backend.SVG (saveSVG)
import           Graphics.Hgg.Unit         (px, (*~))
import qualified Graphics.Hgg.DAG
import           Graphics.Hgg.DAG         ((~>))
import           Graphics.Hgg.Easy
import qualified Graphics.Hgg.Spec        as Spec
import           Data.Text                (Text)

main :: IO ()
main = do
  -- ===========================================================================
  -- K3,3 風 reverse pattern: A→Z, B→Y, C→X
  -- v0.1 (= alphabetical): top [A,B,C] / bottom [X,Y,Z] → 3 crossings
  -- A3 後 (= median heuristic): top [A,B,C] / bottom [Z,Y,X] → 0 crossings
  -- ===========================================================================
  let mkN i lbl x y kind = Spec.dagNode i lbl kind x y
      -- Before: alphabetical 等間隔 (= v0.1 layoutHierarchical 相当)
      k33Before =
        [ mkN "A" "A" 0.0  0.0  NodeLatent
        , mkN "B" "B" 0.5  0.0  NodeLatent
        , mkN "C" "C" 1.0  0.0  NodeLatent
        , mkN "X" "X" 0.0  1.0  NodeObserved
        , mkN "Y" "Y" 0.5  1.0  NodeObserved
        , mkN "Z" "Z" 1.0  1.0  NodeObserved
        ]
      k33Edges =
        [ Spec.dagEdge "A" "Z"
        , Spec.dagEdge "B" "Y"
        , Spec.dagEdge "C" "X"
        ]
      k33BeforeSpec = purePlot
        <> layer (Spec.dagFromLists k33Before k33Edges Spec.LayoutManual
                    <> size 22)
        <> title  "K3,3 reverse: BEFORE (v0.1 alphabetical, 3 crossings)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
      k33Graph = ("A" :: Text) ~> "Z"
              <> ("B" :: Text) ~> "Y"
              <> ("C" :: Text) ~> "X"
      k33AfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlot k33Graph <> size 22)
        <> title  "K3,3 reverse: AFTER (A2+A3, 0 crossings)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)

  saveSVG "design/dag-parity/k33-before.svg" k33BeforeSpec
  saveSVG "design/dag-parity/k33-after.svg"  k33AfterSpec

  -- ===========================================================================
  -- 中規模 HBM 風: alpha/beta が gamma を経由して y に、 直接 y にも貢献
  -- 接続を意図的に "alphabetical で reorder したくなる" 順にして比較
  -- ===========================================================================
  let hbmBefore =
        [ mkN "alpha"   "α"     0.00 0.0 NodeLatent
        , mkN "beta"    "β"     0.25 0.0 NodeLatent
        , mkN "gamma"   "γ"     0.50 0.0 NodeLatent
        , mkN "delta"   "δ"     0.75 0.0 NodeLatent
        , mkN "epsilon" "ε"     1.00 0.0 NodeLatent
        , mkN "tau1"    "τ₁"    0.00 0.5 NodeLatent
        , mkN "tau2"    "τ₂"    0.50 0.5 NodeLatent
        , mkN "tau3"    "τ₃"    1.00 0.5 NodeLatent
        , mkN "y"       "y obs" 0.50 1.0 NodeObserved
        ]
      hbmEdges =
        [ Spec.dagEdge "alpha"   "tau3"   -- ⇒ alphabetical で長距離 cross
        , Spec.dagEdge "beta"    "tau1"
        , Spec.dagEdge "gamma"   "tau2"
        , Spec.dagEdge "delta"   "tau1"
        , Spec.dagEdge "epsilon" "tau3"
        , Spec.dagEdge "tau1"    "y"
        , Spec.dagEdge "tau2"    "y"
        , Spec.dagEdge "tau3"    "y"
        ]
      hbmBeforeSpec = purePlot
        <> layer (Spec.dagFromLists hbmBefore hbmEdges Spec.LayoutManual
                    <> size 22)
        <> title  "HBM 9-node: BEFORE (v0.1 alphabetical)"
        <> theme  ThemeLight
        <> widthUnit (900 *~ px) <> heightUnit (600 *~ px)

      hbmGraph =
           ("alpha"   :: Text) ~> "tau3"
        <> ("beta"    :: Text) ~> "tau1"
        <> ("gamma"   :: Text) ~> "tau2"
        <> ("delta"   :: Text) ~> "tau1"
        <> ("epsilon" :: Text) ~> "tau3"
        <> ("tau1"    :: Text) ~> "y"
        <> ("tau2"    :: Text) ~> "y"
        <> ("tau3"    :: Text) ~> "y"
      hbmAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlot hbmGraph <> size 22)
        <> title  "HBM 9-node: AFTER (A2 rank + A3 median+transpose)"
        <> theme  ThemeLight
        <> widthUnit (900 *~ px) <> heightUnit (600 *~ px)

  saveSVG "design/dag-parity/hbm-before.svg" hbmBeforeSpec
  saveSVG "design/dag-parity/hbm-after.svg"  hbmAfterSpec

  -- ===========================================================================
  -- 2 平行 chain + 1 cross-link: A4 効果が一番分かるケース
  -- v0.1: 各 rank 内 alphabetical (a1, b1) / (a2, b2) / (a3, b3) で等間隔 0/1
  -- A4 後: TD+BU median で同 chain が垂直整列、 cross-link は緩やかに引かれる
  -- ===========================================================================
  let chainsBefore =
        [ mkN "a1" "a1" 0.0 0.0 NodeLatent
        , mkN "b1" "b1" 1.0 0.0 NodeLatent
        , mkN "a2" "a2" 0.0 0.5 NodeLatent
        , mkN "b2" "b2" 1.0 0.5 NodeLatent
        , mkN "a3" "a3" 0.0 1.0 NodeObserved
        , mkN "b3" "b3" 1.0 1.0 NodeObserved
        ]
      chainsEdges =
        [ Spec.dagEdge "a1" "a2"
        , Spec.dagEdge "a2" "a3"
        , Spec.dagEdge "b1" "b2"
        , Spec.dagEdge "b2" "b3"
        , Spec.dagEdge "a2" "b3"   -- cross-link
        ]
      chainsBeforeSpec = purePlot
        <> layer (Spec.dagFromLists chainsBefore chainsEdges Spec.LayoutManual
                    <> size 22)
        <> title  "Chains+cross: BEFORE (v0.1, evenly spaced)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
      chainsGraph =
           ("a1" :: Text) ~> "a2"
        <> ("a2" :: Text) ~> "a3"
        <> ("b1" :: Text) ~> "b2"
        <> ("b2" :: Text) ~> "b3"
        <> ("a2" :: Text) ~> "b3"
      chainsAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlot chainsGraph <> size 22)
        <> title  "Chains+cross: AFTER (A2+A3+A4 TD+BU median anchor)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)

  saveSVG "design/dag-parity/chains-before.svg" chainsBeforeSpec
  saveSVG "design/dag-parity/chains-after.svg"  chainsAfterSpec

  -- ===========================================================================
  -- Long edge (= rank 差 > 1) + spline routing: A5 効果が分かる
  -- a → b → c → d (chain) + a → d (long skip edge、 rank 差 3)
  -- v0.1: a→d は直線で b/c の塊を貫通する
  -- A5 後: a→d は dummy 経由の Catmull-Rom spline で迂回する
  -- ===========================================================================
  let longBefore =
        [ mkN "a" "a" 0.5 0.00 NodeLatent
        , mkN "b" "b" 0.5 0.33 NodeLatent
        , mkN "c" "c" 0.5 0.66 NodeLatent
        , mkN "d" "d" 0.5 1.00 NodeObserved
        ]
      longEdges =
        [ Spec.dagEdge "a" "b"
        , Spec.dagEdge "b" "c"
        , Spec.dagEdge "c" "d"
        , Spec.dagEdge "a" "d"   -- long edge (= skip 2 ranks)
        ]
      longBeforeSpec = purePlot
        <> layer (Spec.dagFromLists longBefore longEdges Spec.LayoutManual
                    <> size 22)
        <> title  "Long skip edge: BEFORE (v0.1, a→d 直線で b/c を貫通)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (500 *~ px)
      longGraph =
           ("a" :: Text) ~> "b"
        <> ("b" :: Text) ~> "c"
        <> ("c" :: Text) ~> "d"
        <> ("a" :: Text) ~> "d"   -- long
      longAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlot longGraph <> size 22)
        <> title  "Long skip edge: AFTER (A5 dummy + Catmull-Rom で迂回)"
        <> theme  ThemeLight
        <> widthUnit (600 *~ px) <> heightUnit (500 *~ px)

  saveSVG "design/dag-parity/long-before.svg" longBeforeSpec
  saveSVG "design/dag-parity/long-after.svg"  longAfterSpec

  -- ===========================================================================
  -- A6: plate (= cluster) aware layout
  -- 2 plate (= group A / group B) を持つ HBM 風モデル
  -- v0.1: plate 制約無し → A/B メンバが交互に並ぶことあり、 plate box が node 群を斜めに覆う
  -- A6 後: 同 plate メンバが rank 内で contiguous → plate box が綺麗な矩形
  -- ===========================================================================
  let plateNodes =
        [ mkN "muA"  "μ_A"  0.20 0.0 NodeLatent
        , mkN "muB"  "μ_B"  0.80 0.0 NodeLatent
        , mkN "a1"   "a₁"   0.05 0.5 NodeLatent
        , mkN "b1"   "b₁"   0.30 0.5 NodeLatent
        , mkN "a2"   "a₂"   0.55 0.5 NodeLatent
        , mkN "b2"   "b₂"   0.80 0.5 NodeLatent
        , mkN "y"    "y"    0.50 1.0 NodeObserved
        ]
      plateEdges =
        [ Spec.dagEdge "muA" "a1"
        , Spec.dagEdge "muA" "a2"
        , Spec.dagEdge "muB" "b1"
        , Spec.dagEdge "muB" "b2"
        , Spec.dagEdge "a1"  "y"
        , Spec.dagEdge "a2"  "y"
        , Spec.dagEdge "b1"  "y"
        , Spec.dagEdge "b2"  "y"
        ]
      plateA = Spec.DAGPlate "plate A (n=2)" ["a1", "a2"]
      plateB = Spec.DAGPlate "plate B (n=2)" ["b1", "b2"]
      plateBeforeSpec = purePlot
        <> layer (Spec.dagFromListsWithPlates plateNodes plateEdges
                    Spec.LayoutManual [plateA, plateB]
                    <> size 22)
        <> title  "Plate-aware: BEFORE (v0.1 a/b 交互配置、 plate box が斜め)"
        <> theme  ThemeLight
        <> widthUnit (800 *~ px) <> heightUnit (500 *~ px)
      plateGraph =
           ("muA" :: Text) ~> "a1" <> ("muA" :: Text) ~> "a2"
        <> ("muB" :: Text) ~> "b1" <> ("muB" :: Text) ~> "b2"
        <> ("a1"  :: Text) ~> "y"  <> ("a2"  :: Text) ~> "y"
        <> ("b1"  :: Text) ~> "y"  <> ("b2"  :: Text) ~> "y"
      plateAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlotWithPlates plateGraph [plateA, plateB]
                    <> size 22)
        <> title  "Plate-aware: AFTER (A6 plate メンバ contiguous、 box 矩形)"
        <> theme  ThemeLight
        <> widthUnit (800 *~ px) <> heightUnit (500 *~ px)

  saveSVG "design/dag-parity/plate-before.svg" plateBeforeSpec
  saveSVG "design/dag-parity/plate-after.svg"  plateAfterSpec

  -- ===========================================================================
  -- 並列 edge (= a→b を 3 本) の表現比較。 旧実装は完全に重なって 1 本にしか見えなかった。
  -- 新: 各並列 edge を perpendicular にずらした 3 点 spline 化、 dot 同等の「並ぶ曲線」 に。
  -- ===========================================================================
  let parGraph = (("a" :: Text) ~> "b")
              <> (("a" :: Text) ~> "b")
              <> (("a" :: Text) ~> "b")
              <> (("b" :: Text) ~> "c")
              <> (("b" :: Text) ~> "c")
      parAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlot parGraph <> size 22)
        <> title  "Parallel edges: AFTER (= perpendicular bend、 3 本 / 2 本)"
        <> theme  ThemeLight
        <> widthUnit (500 *~ px) <> heightUnit (500 *~ px)
  saveSVG "design/dag-parity/parallel-after.svg" parAfterSpec

  -- ===========================================================================
  -- Phase 39 A2-8a: plate 跨ぎ skip edge。
  -- mu→{t1,t2}→y, s→y, plate[t1,t2]、 さらに mu→y (= plate の rank を跨ぐ skip)。
  -- 期待: mu→y は plate 箱を貫通せず、 箱の縦全域を外側で迂回する (graphviz cluster と同様)。
  -- ===========================================================================
  let pcPlate = Spec.DAGPlate "plate (n=2)" ["t1", "t2"]
      pcGraph =
           ("mu" :: Text) ~> "t1" <> ("mu" :: Text) ~> "t2"
        <> ("t1" :: Text) ~> "y"  <> ("t2" :: Text) ~> "y"
        <> ("s"  :: Text) ~> "y"
        <> ("mu" :: Text) ~> "y"   -- plate 跨ぎ skip edge
      pcAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlotWithPlates pcGraph [pcPlate]
                    <> size 22)
        <> title  "Plate-crossing skip: mu->y は plate 箱を外迂回すべき"
        <> theme  ThemeLight
        <> widthUnit (700 *~ px) <> heightUnit (520 *~ px)
  saveSVG "design/dag-parity/plate-cross-after.svg" pcAfterSpec

  -- ===========================================================================
  -- 難ケース: plate box が src→snk skip の **直線経路上**に来る配置。
  -- src が plate 中央上、 snk が plate 中央下にあり、 src→snk を真っ直ぐ引くと
  -- box を貫通する。 box を避けて迂回できるか (= obstacle routing の本検証) を見る。
  -- 期待: src→snk は plate {p0,p1} を貫通せず外を迂回する。
  -- ===========================================================================
  let ptPlate = Spec.DAGPlate "plate (n=2)" ["p0", "p1"]
      ptGraph =
           ("src" :: Text) ~> "p0" <> ("src" :: Text) ~> "p1"
        <> ("p0"  :: Text) ~> "snk" <> ("p1" :: Text) ~> "snk"
        <> ("src" :: Text) ~> "snk"  -- box 直下を跨ぐ skip
      ptAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlotWithPlates ptGraph [ptPlate]
                    <> size 22)
        <> title  "Plate-through skip: src->snk は box を貫通せず迂回すべき"
        <> theme  ThemeLight
        <> widthUnit (640 *~ px) <> heightUnit (520 *~ px)
  saveSVG "design/dag-parity/plate-through-after.svg" ptAfterSpec

  -- ===========================================================================
  -- A4 検証用: nested plate (= 外側 plate の中に兄弟 inner plate 2 つ)。
  -- mu → {gA,gB}、 gA → {xa1,xa2}、 gB → {xb1,xb2}、 全 x → y。
  -- 外側 plate "model" が gA,gB,xa* ,xb* を、 inner "A"/"B" が各 xa*/xb* を囲む。
  -- 期待 (graphviz contain/separate_subclust): 外箱が内箱を完全内包し、 兄弟
  -- inner A/B が x 方向で重ならない。 現状 (A4 前) の重なりを実測する。
  -- ===========================================================================
  let nestGraph =
           ("mu" :: Text) ~> "gA" <> ("mu" :: Text) ~> "gB"
        <> ("gA" :: Text) ~> "xa1" <> ("gA" :: Text) ~> "xa2"
        <> ("gB" :: Text) ~> "xb1" <> ("gB" :: Text) ~> "xb2"
        <> ("xa1" :: Text) ~> "y" <> ("xa2" :: Text) ~> "y"
        <> ("xb1" :: Text) ~> "y" <> ("xb2" :: Text) ~> "y"
      nestOuter = Spec.DAGPlate "model"  ["gA", "gB", "xa1", "xa2", "xb1", "xb2"]
      nestA     = Spec.DAGPlate "A (n=2)" ["xa1", "xa2"]
      nestB     = Spec.DAGPlate "B (n=2)" ["xb1", "xb2"]
      nestAfterSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlotWithPlates nestGraph
                    [nestOuter, nestA, nestB] <> size 22)
        <> title  "Nested plate: 外箱が内箱を内包・兄弟 A/B は非重複であるべき"
        <> theme  ThemeLight
        <> widthUnit (760 *~ px) <> heightUnit (560 *~ px)
  saveSVG "design/dag-parity/nested-after.svg" nestAfterSpec

  -- ===========================================================================
  -- A4 stress 1: 3 レベル深い nest (outer ⊃ mid ⊃ inner)。 margin 累積を実測。
  -- 期待: 各境界が 1 段ぶん margin で離れる (= graphviz は各 level に CL_OFFSET)。
  -- ===========================================================================
  let deepGraph =
           ("a" :: Text) ~> "b" <> ("b" :: Text) ~> "c" <> ("c" :: Text) ~> "d"
      deepOuter = Spec.DAGPlate "L1" ["b", "c", "d"]
      deepMid   = Spec.DAGPlate "L2" ["c", "d"]
      deepInner = Spec.DAGPlate "L3" ["d"]
      deepSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlotWithPlates deepGraph
                    [deepOuter, deepMid, deepInner] <> size 22)
        <> title  "Deep nest (L1 superset of L2 superset of L3): margin 累積"
        <> theme  ThemeLight
        <> widthUnit (560 *~ px) <> heightUnit (640 *~ px)
  saveSVG "design/dag-parity/nested-deep-after.svg" deepSpec

  -- ===========================================================================
  -- A4 stress 2: 同 rank に 3 兄弟 inner plate。 兄弟分離 (= A1 keepout で
  -- 兄弟 member は互いに非member ゆえ排除される) を実測。 box 重なりが無いこと。
  -- ===========================================================================
  let triGraph =
           ("m" :: Text) ~> "p" <> ("m" :: Text) ~> "q" <> ("m" :: Text) ~> "r"
        <> ("p" :: Text) ~> "z" <> ("q" :: Text) ~> "z" <> ("r" :: Text) ~> "z"
      triOuter = Spec.DAGPlate "all" ["p", "q", "r"]
      triP = Spec.DAGPlate "P" ["p"]
      triQ = Spec.DAGPlate "Q" ["q"]
      triR = Spec.DAGPlate "R" ["r"]
      triSpec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlotWithPlates triGraph
                    [triOuter, triP, triQ, triR] <> size 22)
        <> title  "3 兄弟 inner plate: box 重なり無しであるべき"
        <> theme  ThemeLight
        <> widthUnit (720 *~ px) <> heightUnit (520 *~ px)
  saveSVG "design/dag-parity/nested-tri-after.svg" triSpec

  putStrLn "Wrote 15 SVGs to design/dag-parity/"
  putStrLn "  k33-before.svg / k33-after.svg       (= reverse pattern、 3 crossings -> 0)"
  putStrLn "  hbm-before.svg / hbm-after.svg       (= 9-node HBM、 alphabetical -> median+anchor)"
  putStrLn "  chains-before.svg / chains-after.svg (= A4 TD+BU 親 anchor が見える)"
  putStrLn "  long-before.svg / long-after.svg     (= A5 dummy 経由 spline で長 edge 迂回)"
  putStrLn "  plate-before.svg / plate-after.svg   (= A6 plate メンバ contiguous で box 矩形)"

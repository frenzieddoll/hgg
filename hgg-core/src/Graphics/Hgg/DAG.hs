-- |
-- Module      : Graphics.Hgg.DAG
-- Description : algebraic-graphs 流 DAG builder + layout (Phase 26 §E-6)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DAG (directed graph) を **polymorphic な Graph a + 代数演算** で構築し、
-- Graphics.Hgg.Spec.DAGSpec に変換して描画 layer を作る。 algebraic-graphs
-- (Mokhov) と同じ思想:
--
--   * 'overlay' / '(<>)' ─ graph を並置 (= 和集合)
--   * 'connect' / '(~>)' ─ 全 edge を貼る
--   * 'vertex'           ─ 単一 node
--   * 'empty'            ─ 何も無い
--
-- 例:
--
-- > hbmModel :: Graph Text
-- > hbmModel
-- >   =  "alpha" ~> "sigma" ~> "y"
-- >   <> "beta"  ~> "sigma"
-- >   <> "alpha" ~> "y"
-- >   <> "beta"  ~> "y"
-- >
-- > spec :: VisualSpec
-- > spec = purePlot
-- >   <> dagPlot hbmModel
-- >   <> title "HBM model"
--
-- node 属性 (= label / kind) は 'ToDAGNode' 型クラスで抽出。 'Text' は全 latent
-- default、 record や tuple で明示も可。 幽霊型 module (= 将来
-- @Graphics.Hgg.DAG.Typed@) も instance 追加だけで対応。
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.DAG
  ( -- * Graph algebra
    Graph(..)
  , empty
  , vertex
  , overlay
  , connect
  , (~>)
  , edges
  , vertices
    -- * Attribute extraction
  , ToDAGNode(..)
    -- * Layer
  , dagPlot
  , dagPlotWith
  , dagPlotWithPlates
  , dagPlotWithRankGroups
    -- * Layout
  , layoutHierarchical
  , layoutHierarchicalFull
  , layoutHierarchicalFullWithPlates
  , layoutHierarchicalFullWithConstraints
    -- * Inspection
  , toVertices
  , toEdges
  ) where

import           Graphics.Hgg.DAG.Internal.Sugiyama
                                      (LNode (..), LayoutGraph,
                                       applyPlateConstraints,
                                       assignCoords, auxSimplexCoordsW,
                                       assignOrderFull,
                                       assignRanksGrouped, lgNodes)
import           Graphics.Hgg.Layout (dagNodeBaseHalfWidth)
import           Graphics.Hgg.Spec   (DAGEdge (..), DAGLayoutAlgorithm (..),
                                      DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..), Layer, dagFromLists,
                                      dagFromListsWithPlates)
import           Data.List           (foldl', nub)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)

-- ===========================================================================
-- Graph algebra (= algebraic-graphs / Mokhov)
-- ===========================================================================

-- | 代数的 DAG。 'a' は vertex の identity (任意型)。
--
--   * 'Empty'   ─ 空
--   * 'Vertex'  ─ 単一 node
--   * 'Overlay' ─ 2 graph を並置 (= 和集合、 edges もそのまま)
--   * 'Connect' ─ 2 graph の全ペアに edge を張る (= 第 1 の全 vertex から
--                 第 2 の全 vertex へ)
data Graph a
  = Empty
  | Vertex   !a
  | Overlay  !(Graph a) !(Graph a)
  | Connect  !(Graph a) !(Graph a)
  deriving (Show, Eq, Functor)

instance Semigroup (Graph a) where (<>) = Overlay
instance Monoid    (Graph a) where mempty = Empty

empty :: Graph a
empty = Empty

vertex :: a -> Graph a
vertex = Vertex

overlay :: Graph a -> Graph a -> Graph a
overlay = Overlay

connect :: Graph a -> Graph a -> Graph a
connect = Connect

-- | edge を貼る軽量 operator。 `<>` (infixr 6) より tight に binding (= 7) して
-- `a ~> b <> c ~> d` が `(a ~> b) <> (c ~> d)` と parse される。
infix 7 ~>
(~>) :: a -> a -> Graph a
a ~> b = Connect (Vertex a) (Vertex b)

-- | edge リストから graph を作る (= 補助 helper、 既存パターンの移行用)。
edges :: [(a, a)] -> Graph a
edges es = foldl' Overlay Empty [ Connect (Vertex f) (Vertex t) | (f, t) <- es ]

-- | vertex 群を overlay。
vertices :: [a] -> Graph a
vertices = foldl' Overlay Empty . map Vertex

-- ===========================================================================
-- ToDAGNode (= attribute 抽出 type class)
-- ===========================================================================

-- | Graph の vertex 型 'a' から (id, label, kind) を取り出す。
-- ユーザ独自 newtype / 幽霊型でも instance 追加するだけで dagPlot に渡せる。
class Ord a => ToDAGNode a where
  toDAGNode :: a -> (Text, Text, DAGNodeKind)

-- | 最小 case: Text を id 兼 label、 kind = NodeLatent (default)。
instance ToDAGNode Text where
  toDAGNode t = (t, t, NodeLatent)

-- | (id, label, kind) tuple をそのまま。
instance ToDAGNode (Text, Text, DAGNodeKind) where
  toDAGNode = id

-- | DAGNode を直接 vertex として渡す場合 (= 位置情報含む、 LayoutManual 用)。
instance ToDAGNode DAGNode where
  toDAGNode n = (dnId n, dnLabel n, dnKind n)

-- ===========================================================================
-- Graph → Layer 変換 (= dagPlot)
-- ===========================================================================

-- | algebraic graph を VisualSpec の Layer に。 layout は階層 (= 推奨 default)。
--
-- > dagPlot ("a" ~> "b" ~> "c" <> "a" ~> "c")
dagPlot :: ToDAGNode a => Graph a -> Layer
dagPlot g = dagPlotWith LayoutHierarchical g

-- | layout algorithm を明示指定。
dagPlotWith :: ToDAGNode a => DAGLayoutAlgorithm -> Graph a -> Layer
dagPlotWith algo g =
  let vs = toVertices g
      es = toEdges    g
      -- 重複排除 + 属性抽出
      nodeAttrs = nub [ toDAGNode v | v <- vs ]
      nodeList = [ DAGNode i lbl k Nothing 0 0 | (i, lbl, k) <- nodeAttrs ]
      edgeList = [ DAGEdge (fstId f) (fstId t) Nothing Nothing | (f, t) <- es ]
      fstId v = let (i, _, _) = toDAGNode v in i
      -- layout 適用 (= 位置を埋める)
      (positioned, routedEdges) = case algo of
        LayoutManual       -> (nodeList, edgeList)  -- そのまま、 dnX/dnY は 0 のまま、 path 無し
        LayoutHierarchical -> layoutHierarchicalFull nodeList edgeList
  in dagFromLists positioned routedEdges algo

-- | Phase 1 A6: plate (= cluster) を伴う Graph DSL 用 helper。
-- LayoutHierarchical で plate-aware ordering (= 同 plate メンバが rank 内で
-- contiguous) を適用する。 plates 順は外側 → 内側 (= nested plate 用)。
dagPlotWithPlates :: ToDAGNode a => Graph a -> [DAGPlate] -> Layer
dagPlotWithPlates g plates =
  let vs = toVertices g
      es = toEdges    g
      nodeAttrs = nub [ toDAGNode v | v <- vs ]
      nodeList = [ DAGNode i lbl k Nothing 0 0 | (i, lbl, k) <- nodeAttrs ]
      edgeList = [ DAGEdge (fstId f) (fstId t) Nothing Nothing | (f, t) <- es ]
      fstId v = let (i, _, _) = toDAGNode v in i
      (positioned, routedEdges) =
        layoutHierarchicalFullWithPlates nodeList edgeList plates
  in dagFromListsWithPlates positioned routedEdges LayoutHierarchical plates

-- | Phase 53 A3: rank group (= graphviz @rank=same@) を伴う Graph DSL 用 helper。
-- 各 group の member id は同一 rank に置かれ、 group 内の edge は flat edge
-- (= 同 rank edge) として P3e 順序制約 (左→右) + P7b 水平/迂回 spline で描画される。
-- rank group は layout 制約であり出力 DAGSpec には載らない (= 計画 md A3-1)。
--
-- > dagPlotWithRankGroups ("a" ~> "b" <> "a" ~> "c") [["b", "c"]]
dagPlotWithRankGroups :: ToDAGNode a => Graph a -> [[Text]] -> Layer
dagPlotWithRankGroups g rankGroups =
  let vs = toVertices g
      es = toEdges    g
      nodeAttrs = nub [ toDAGNode v | v <- vs ]
      nodeList = [ DAGNode i lbl k Nothing 0 0 | (i, lbl, k) <- nodeAttrs ]
      edgeList = [ DAGEdge (fstId f) (fstId t) Nothing Nothing | (f, t) <- es ]
      fstId v = let (i, _, _) = toDAGNode v in i
      (positioned, routedEdges) =
        layoutHierarchicalFullWithConstraints nodeList edgeList [] rankGroups
  in dagFromLists positioned routedEdges LayoutHierarchical

-- | Graph 構造から vertex 列を抽出 (= 重複あり、 順序保持)。
toVertices :: Graph a -> [a]
toVertices g = go g []
  where
    go Empty            acc = acc
    go (Vertex a)       acc = a : acc
    go (Overlay x y)    acc = go x (go y acc)
    go (Connect x y)    acc = go x (go y acc)

-- | Graph 構造から edge 列を抽出 (= Connect が出すクロス積)。
toEdges :: Graph a -> [(a, a)]
toEdges g = go g
  where
    go Empty         = []
    go (Vertex _)    = []
    go (Overlay x y) = go x <> go y
    go (Connect x y) =
      let lvs = toVertices x
          rvs = toVertices y
      in go x <> go y <> [(l, r) | l <- lvs, r <- rvs]

-- ===========================================================================
-- Layout: 階層 (Sugiyama 簡易版)
-- ===========================================================================

-- | 階層 layout (= Sugiyama framework、 Phase 1 で network simplex rank assignment に置換)。
--
--   1. Step 2 'assignRanks' (= network simplex framework、 内部 'longestPathRanking' で初期解、 一様 δ=ω=1 では即時最適) で各 node の rank を決定
--   2. (Phase 1 未実装) Step 3 Order assignment、 Step 4 Coordinate assignment は今は alphabetical 等間隔 (= A3/A4 で置換予定)
--   3. y は rank に比例 (= 上から下)
--
-- domain 座標で返す (= 0..1 正規化)。 Render 側で scale 適用。
layoutHierarchical :: [DAGNode] -> [DAGEdge] -> [DAGNode]
layoutHierarchical nodes es = fst (layoutHierarchicalFull nodes es)

-- | 階層 layout の full 版 (= plate 無し)。 'layoutHierarchicalFullWithPlates' の薄 wrapper。
layoutHierarchicalFull
  :: [DAGNode] -> [DAGEdge] -> ([DAGNode], [DAGEdge])
layoutHierarchicalFull nodes es =
  layoutHierarchicalFullWithPlates nodes es []

-- | Phase 53 A3: rank group (= graphviz @rank=same@) も受ける full 版。
-- rank group は**入力時 layout 制約**で出力 spec には載せない (layout は builder
-- 時のみ実行され、 JSON 復元後の再 layout 経路が無いことを実測済 = 計画 md A3-1)。
layoutHierarchicalFullWithConstraints
  :: [DAGNode] -> [DAGEdge] -> [DAGPlate] -> [[Text]] -> ([DAGNode], [DAGEdge])
layoutHierarchicalFullWithConstraints nodes es plates rankGroups =
  let StageRouted ns es' =
        routeStage . coordStage . orderStage . rankStage $
          StageRaw nodes es plates rankGroups
  in (ns, es')

-- | Phase 1 A6 plate-aware: node の (x, y) 配置 + edge の 'dePath' を同時計算。
-- 'plates' が空でなければ post-process で同 plate メンバを rank 内 contiguous に。
-- 渡された plates 順を尊重 (= nested 用、 外側 → 内側)。
--
-- Phase 39 B1: 旧一枚岩 'let' を rank → order → coord → route の段階関数に分離。
-- 各段の中身 (assignRanks 等の呼出) は不変、 段間は record の包み直しのみ
-- (= 出力ビット不変、 golden 回帰で担保)。 段階型 (= 'StageRaw' 〜 'StageRouted')
-- が「どの段で何が産出されるか」 (chainMap = order 段, coordMap = coord 段) を
-- 型に明示し、 B2 (routing module 独立) の入力契約の土台にする。
layoutHierarchicalFullWithPlates
  :: [DAGNode] -> [DAGEdge] -> [DAGPlate] -> ([DAGNode], [DAGEdge])
layoutHierarchicalFullWithPlates nodes es plates =
  layoutHierarchicalFullWithConstraints nodes es plates []

-- ===========================================================================
-- Phase 39 B1: layout pipeline の段階型と段階関数
-- ===========================================================================

-- | 段階0: 入力そのまま (rank 前)。 後段で nodes (= 最終配置) / es (= routedE・
-- recenter) / plates (= banding・extent) を参照するため全段で保持する。
data StageRaw = StageRaw
  { srNodes      :: [DAGNode]
  , srEdges      :: [DAGEdge]
  , srPlates     :: [DAGPlate]
  , srRankGroups :: [[Text]]   -- ^ Phase 53 A3: graphviz rank=same 相当の同 rank group
  }

-- | 段階1: rank 割当済 (tightenSourceRanks 反映済) LayoutGraph。
data StageRanked = StageRanked
  { rkInput :: StageRaw
  , rkGraph :: LayoutGraph
  }

-- | 段階2: order 確定 + 元 edge → chain map (= skip edge routing 用) が産出される段。
data StageOrdered = StageOrdered
  { odInput :: StageRaw
  , odGraph :: LayoutGraph
  , odOrder :: Map.Map Int [Text]
  , odChain :: Map.Map (Text, Text) [Text]
  }

-- | 段階3: x 座標確定 (route 前)。 coordMap (= banding + recenter 反映済) が産出される段。
data StagePositioned = StagePositioned
  { psInput  :: StageRaw
  , psChain  :: Map.Map (Text, Text) [Text]
  , psRankOf :: Map.Map Text Int
  , psCoord  :: Map.Map Text Double
  }

-- | 段階4: 最終 (配置済 node + dePath 埋め edge)。
data StageRouted = StageRouted
  { rtNodes :: [DAGNode]
  , rtEdges :: [DAGEdge]
  }

-- | rank 段: longest-path / network simplex で rank 割当 → source 引き締め。
--
-- Phase 53 A3-2: 旧 breakCycles → assignRanks → tightenSourceRanks の直列は
-- 'assignRanksGrouped' に集約 (rank group 無しではビット一致、 test 担保)。
-- rank group 有りでは group を代表 node に併合して rank 割当し、 group 内 edge が
-- flat edge (= 同 rank edge) として下流へ流れる。
rankStage :: StageRaw -> StageRanked
rankStage raw =
  let nodes = srNodes raw; es = srEdges raw; plates = srPlates raw
      ids = map dnId nodes
      lg0' = assignRanksGrouped (srRankGroups raw) (map dpNodeIds plates)
               ids [ (deFrom e, deTo e) | e <- es ]
  in StageRanked raw lg0'

-- | order 段: dummy 挿入 + median heuristic + transpose、 元 edge → chain map 取得、
-- plate 制約 post-process。
orderStage :: StageRanked -> StageOrdered
orderStage (StageRanked raw lg0') =
  let -- A3: dummy 挿入 + median heuristic + transpose、 元 edge → chain map も取得
      (lgFinal, orderMap0, chainMap) = assignOrderFull lg0'
      -- A6: plate 制約 post-process (= 同 plate メンバを rank 内 contiguous に)
      orderMap = applyPlateConstraints (map dpNodeIds (srPlates raw)) orderMap0
  in StageOrdered raw lgFinal orderMap chainMap

-- | coord 段: P4a aux-graph network simplex で x 座標を解く。
--
-- Phase 39 Step8 (P8) A2: plate メンバ id を P4a simplex に渡し、 cluster border
-- 制約 (contain/keepout, graphviz @pos_clusters@・A1 実装) を x 座標へ直接反映する。
-- これで box 分離が simplex 由来になったため、 従来の cosmetic post-process
-- ('applyPlateBands' = 帯分離 / 'recenterNonPlateRows' = 帯後の重心緩和) を**撤去**
-- した ([[feedback-remove-stopgaps-when-real-algo-lands]])。
coordStage :: StageOrdered -> StagePositioned
coordStage (StageOrdered raw lgFinal orderMap chainMap) =
  let plates = srPlates raw
      -- ★ A4-3 EXPERIMENT (完全忠実): real-width simplex の **raw point 座標**を
      -- 正規化せず直接使う (= graphviz の point 一貫 pipeline)。 wpt rescale を介さない。
      hwMap = Map.fromList
        [ (dnId n, round (dagNodeBaseHalfWidth n) :: Int) | n <- srNodes raw ]
      coordMap = auxSimplexCoordsW hwMap (map dpNodeIds plates) lgFinal orderMap
      rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lgFinal ]
  in StagePositioned raw chainMap rankOf coordMap

-- | route 段: node 最終配置 (rank→y) + edge dePath (chain waypoint)。
--
-- Phase 39 Step8 (P8) A3: plate 箱迂回の応急処置 'routeLongEdgeDummies' (RLED) を
-- **撤去**した ([[feedback-remove-stopgaps-when-real-algo-lands]])。 撤去の根拠は
-- 二段構えの本実装が landing したこと:
--
--   1. layout 層 = A1 cluster border 制約 (@keepout_othernodes@) が plate 非メンバ
--      (long-edge dummy 含む) を simplex で箱外へ押し出す。 → guide 自体が箱外。
--   2. render 層 = 'Render.EdgeRoute.routeEdge' の box-channel + funnel が plate box
--      を障害物に取り、 guide を箱の外で taut spline へ整える。 → 幾何的に貫通しない。
--
-- 実測 (RLED 撤去 vs 存置): 実 HBM DAG (hbm-after) は **ビット不変**、 box を直線
-- 貫通する合成ケース (plate-through/plate-cross) は RLED の「箱辺に張り付く」 bend
-- より funnel の方が箱から余裕を持って外迂回し改善。 RLED は箱辺密着の cosmetic
-- でしかなかったことが裏付けられた。
routeStage :: StagePositioned -> StageRouted
routeStage (StagePositioned raw chainMap rankOf coordMap) =
  let nodes = srNodes raw; es = srEdges raw
      -- ★ A4-3 (完全忠実 point pipeline): y は **rank index** をそのまま返す。
      -- point 化 (× rankPitch = maxNodeH+ranksep) は radius が既知の render 側で行う
      -- (x は simplex の raw point ゆえ layout で確定・y だけ render で pitch を被せる)。
      yOf r = fromIntegral r
      posOf nid =
        let x = Map.findWithDefault 0.5 nid coordMap
            r = Map.findWithDefault 0   nid rankOf
        in (x, yOf r)
      positionedN = [ n { dnX = x, dnY = y }
                    | n <- nodes
                    , let (x, y) = posOf (dnId n) ]
      -- A5: 元 edge の chain から control 点列を埋める。 長 edge は rank 単位 dummy
      -- 経由 chain を返すのみ (= rank-level waypoint)。 plate box 回避の幾何 routing は
      -- 障害物が pt で確定する Render 側 (pt 空間 routesplines・Phase 39 A2-8) に移譲。
      --
      -- P7b 最小 (Phase 53 A3-4): flat edge (= 同 rank edge、 rank group 由来)。
      -- 間に他 real node が無ければ dePath 無し = 水平直線 (side port 同士)。
      -- 間に node があれば rank の**上側 gap** (= r - 0.5、 graphviz make_flat_edge が
      -- rank 上の空間へ逃がすのと同層) に waypoint を 1 点置き、 render 側の
      -- box-channel / funnel / proutespline に迂回 spline を作らせる。
      flatPath e =
        case (Map.lookup (deFrom e) rankOf, Map.lookup (deTo e) rankOf) of
          (Just rf, Just rt) | rf == rt ->
            let xF = Map.findWithDefault 0.5 (deFrom e) coordMap
                xT = Map.findWithDefault 0.5 (deTo e) coordMap
                (lo, hi) = (min xF xT, max xF xT)
                blocked = or [ x > lo && x < hi
                             | n <- nodes
                             , dnId n /= deFrom e, dnId n /= deTo e
                             , Map.lookup (dnId n) rankOf == Just rf
                             , let x = Map.findWithDefault 0.5 (dnId n) coordMap ]
            in if blocked
                 then Just [ (xF, yOf rf)
                           , ((xF + xT) / 2, yOf rf - 0.5)
                           , (xT, yOf rt) ]
                 else Nothing
          _ -> Nothing
      routedE = [ e { dePath = maybe (chainToPath e) Just (flatPath e) } | e <- es ]
      -- P2a: layout で back-edge が反転されている場合 chainMap の key は (to,from)。
      -- 直接 key が無ければ反転 key を引き、 chain を反転して原方向 (from→to) に戻す。
      -- acyclic 入力では常に直接 key がヒットする (= 非破壊)。
      chainToPath e = case Map.lookup (deFrom e, deTo e) chainMap of
        Just chain | length chain > 2 -> Just (map posOf chain)
        _ -> case Map.lookup (deTo e, deFrom e) chainMap of
               Just chain | length chain > 2 -> Just (map posOf (reverse chain))
               _                             -> Nothing
  in StageRouted positionedN routedE

-- ===========================================================================
-- Phase 1 A2 以前の 'computeDepths' (= longest path 直接実装) は削除済。
-- 同等処理は 'Graphics.Hgg.DAG.Internal.Sugiyama.longestPathRanking' に移動、
-- 'assignRanks' (= network simplex framework) 経由で呼ばれる。
-- ===========================================================================

-- Phase 39 Step8 (P8) A3: long-edge dummy を plate 箱外へ bend する応急処置
-- 'routeLongEdgeDummies' (RLED) は撤去した。 plate 箱迂回は A1 cluster border 制約
-- (layout) + 'Render.EdgeRoute' box-channel/funnel (render) の二段で本実装済。
-- 経緯と実測根拠は 'routeStage' の haddock を参照。

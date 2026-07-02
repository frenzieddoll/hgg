-- |
-- Module      : Hgg.Plot.DAG.Internal.Sugiyama
-- Description : Sugiyama framework 中間表現 + Step 2 Rank + Step 3 Order (Phase 1 A2-A3)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- Hgg.Plot.DAG 内部で使う Sugiyama framework の中間表現と各 step 実装。
-- 外向け Graph a / DAGSpec には漏らさない (= spec §10.3 dummy node 規律)。
--
-- 現状 (Phase 1 A2):
--
--   * LNode / LEdge / LayoutGraph 中間型
--   * Step 2 Rank assignment: network simplex (Gansner-Koutsofios-North-Vo 1993 §2.3)
--   * 全 edge の minimum length δ = 1、 weight ω = 1 が default
--     (= 現状 DAG.Graph の edge は属性無し、 将来 weight 拡張余地)
--
-- 設計判断: 一様 δ=1 / ω=1 の場合、 longest-path ranking が既に Σ edge length
-- 最適解 (= 証明: edge 数固定で各 edge の最小 rank diff = 1)。 そのため network
-- simplex の **反復改善 phase は実質 no-op** になる。 ただし将来 weight / 異δ
-- 拡張に備えて framework として実装し、 初期解 = longest-path、 反復 = 負 cut
-- value 探索 (= 該当無し → 即終了) という構造で書く。
--
-- 計算量:
--   * 初期 longest-path: O(V + E)
--   * tight tree 構築: O(V + E)
--   * cut value 計算: O(V × E) (= 各 tree edge について非 tree edge を走査)
--   * 反復: 一様 ω では 0 回、 一般には worst O(V × E) per iteration × V iterations
--
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.DAG.Internal.Sugiyama
  ( -- * 中間表現
    LNode (..)
  , LEdge (..)
  , LayoutGraph (..)
  , buildLayoutGraph
    -- * Step 2-0 (P2a): acyclic 化
  , breakCycles
    -- * Step 2: Rank assignment
  , assignRanks
  , longestPathRanking
  , tightTreeEdges
  , tightenSourceRanks
    -- * 汎用 network simplex (= P4a x 座標で使う共通ソルバ)
  , networkSimplex
  , networkSimplexBalanced
    -- * Step 3: Order assignment (= median heuristic + transpose)
  , OrderMap
  , insertDummies
  , insertDummiesWithChains
  , initialOrder
  , medianSweep
  , transposeOrder
  , assignOrder
  , assignOrderFull
  , countCrossings
  , bilayerCrossings
    -- * Step 4: Coordinate assignment (= full 4-candidate Brandes-Köpf)
  , assignCoords
  , assignCoordsW
  , auxSimplexCoords
  , auxSimplexCoordsW
  , brandesKopf
  , computeOneDir
    -- * Step 5 (Phase 1 A6): Plate (= cluster) 制約
  , applyPlateConstraints
    -- * Inspection (= test 用)
  , edgeLengthSum
  , isFeasible
  ) where

import           Data.List       (foldl', sort, sortBy)
import           Data.Maybe      (listToMaybe)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T

-- ===========================================================================
-- 中間表現
-- ===========================================================================

-- | 内部 node。 元の DAGNode から id を保持し、 rank を埋める。
-- dummy node (= A3 で長 edge 中継用) は 'lnDummy' で区別。
data LNode = LNode
  { lnId    :: !Text   -- ^ 元 node id (dummy なら "__dummy_<n>")
  , lnRank  :: !Int    -- ^ Step 2 で割当てる rank
  , lnDummy :: !Bool   -- ^ A3 で長 edge を分割するために追加した dummy か
  } deriving (Eq, Show)

-- | 内部 edge。 weight / minimum length δ を持つ。
-- Phase 1 A2 では全 edge weight=1, delta=1 だが将来拡張余地。
data LEdge = LEdge
  { leFrom   :: !Text
  , leTo     :: !Text
  , leDelta  :: !Int      -- ^ 最小 rank 差 (= δ)、 default 1
  , leWeight :: !Double   -- ^ edge weight (= ω)、 default 1.0
  } deriving (Eq, Show)

-- | Sugiyama framework の中間 graph。
data LayoutGraph = LayoutGraph
  { lgNodes :: ![LNode]
  , lgEdges :: ![LEdge]
  } deriving (Eq, Show)

-- | 元 (id, parents) ペア群から LayoutGraph を組み立てる。
-- すべての edge は δ=1 / ω=1 で初期化。 rank は未割当 (= 0)。
buildLayoutGraph
  :: [Text]            -- ^ 全 node id (順序保持、 stable iteration 用)
  -> [(Text, Text)]    -- ^ edge list (from, to)
  -> LayoutGraph
buildLayoutGraph ids es =
  LayoutGraph
    { lgNodes = [ LNode i 0 False | i <- ids ]
    , lgEdges = [ LEdge f t 1 1.0 | (f, t) <- es ]
    }

-- ===========================================================================
-- Step 2-0 (P2a): acyclic 化 (= graphviz acyclic.c 相当)
-- ===========================================================================

-- | DFS で back-edge を検出して反転し、 self-loop は rank 制約に寄与しないので
-- 除去する。 rank/order 用の acyclic edge 列を返す。
--
-- graphviz の 'acyclic.c' (decompose + break_cycles) と同じく「閉路を一時的に
-- 反転して DAG 化 → layout → 描画時に向きを戻す」 戦略の前半。 描画方向は呼出側
-- (DAG.hs) が原 edge で保持し、 chain lookup は反転 key fallback で吸収する。
--
-- **非破壊性**: 入力が既に DAG なら back-edge は存在せず、 self-loop も無ければ
-- edge は順序保持で不変。 = 現行の acyclic テストケース (large/medium/small/
-- isolated) には影響しない。 閉路入力でのみ rank が正しくなる
-- (従来は 'longestPathRanking' の「0 仮置き」 で誤った rank になっていた)。
--
-- DFS 着色: gray = 現在の stack 上、 black = 探索完了。 (u→v) で v が gray なら
-- back-edge。 起点は @ids@ 順に全 node を走査するので非連結成分も網羅する。
breakCycles :: [Text] -> [(Text, Text)] -> [(Text, Text)]
breakCycles ids es =
  let adj = Map.fromListWith (flip (++))
              [ (f, [t]) | (f, t) <- es, f /= t ]   -- self-loop は隣接から除外
      dfs acc@(gray, black, rev) u
        | Set.member u black = acc
        | otherwise =
            let gray1 = Set.insert u gray
                outs  = Map.findWithDefault [] u adj
                step (g, b, r) v
                  | Set.member v g = (g, b, Set.insert (u, v) r)  -- back-edge
                  | Set.member v b = (g, b, r)                    -- forward/cross
                  | otherwise      = dfs (g, b, r) v
                (gray2, black2, rev2) = foldl' step (gray1, black, rev) outs
            in (Set.delete u gray2, Set.insert u black2, rev2)
      (_, _, reversedSet) =
        foldl' dfs (Set.empty, Set.empty, Set.empty) ids
      orient (f, t)
        | f == t                        = Nothing       -- self-loop は除去
        | Set.member (f, t) reversedSet = Just (t, f)    -- back-edge は反転
        | otherwise                     = Just (f, t)
  in [ e | Just e <- map orient es ]

-- ===========================================================================
-- Step 2: Rank assignment (= network simplex)
-- ===========================================================================

-- | LayoutGraph の lnRank を埋める。 Phase 1 A2 採用 = network simplex。
--
-- 流れ (Gansner 1993 §2.3):
--
--   1. 'longestPathRanking' で初期 feasible ranking
--   2. 'buildTightTree' で tight edge から spanning tree
--   3. 'cutValues' で各 tree edge の cut value
--   4. 負 cut value の tree edge があれば置換 (= 'pivotOnce')、 無ければ最適
--   5. 反復終了後 rank を 0-base に正規化
--
-- 一様 δ=1 / ω=1 では step 1 で最適解。 反復は no-op になる。
assignRanks :: LayoutGraph -> LayoutGraph
assignRanks lg0 =
  let lg1 = longestPathRanking lg0
      lg2 = iterateSimplex lg1 (length (lgNodes lg0) * 4)  -- 上限 = 4V iteration
      lg3 = normalizeRanks lg2
  in lg3

-- | Step 2-1: longest-path ranking (= 各 node に「source からの最長 path 長」 を割当)。
-- 一様 δ=1 / ω=1 では Σ edge length 最適解。
longestPathRanking :: LayoutGraph -> LayoutGraph
longestPathRanking lg =
  let parents = Map.fromListWith (<>)
                  [ (leTo e, [(leFrom e, leDelta e)]) | e <- lgEdges lg ]
      ids = [ lnId n | n <- lgNodes lg ]
      go memo i = case Map.lookup i memo of
        Just r  -> (r, memo)
        Nothing ->
          let ps = Map.findWithDefault [] i parents
              -- cycle 安全: 自分を 0 で仮置き
              memo0 = Map.insert i 0 memo
              (memo', rs) = foldl'
                (\(m, acc) (p, d) ->
                    let (rp, m') = go m p
                    in (m', (rp + d) : acc))
                (memo0, [])
                ps
              r = if null rs then 0 else maximum rs
          in (r, Map.insert i r memo')
      finalMemo = foldl' (\m i -> snd (go m i)) Map.empty ids
      newNodes = [ n { lnRank = Map.findWithDefault 0 (lnId n) finalMemo }
                 | n <- lgNodes lg ]
  in lg { lgNodes = newNodes }

-- | Step 2-2〜5: simplex 反復。 上限 iteration 内で負 cut value が無くなるまで pivot。
-- 一様 δ=1 / ω=1 では即時終了 (= 負 cut value 無し)。
iterateSimplex :: LayoutGraph -> Int -> LayoutGraph
iterateSimplex lg 0       = lg
iterateSimplex lg budget =
  case pivotOnce lg of
    Nothing  -> lg  -- 最適解到達
    Just lg' -> iterateSimplex lg' (budget - 1)

-- | 1 回の pivot: 負 cut value の tree edge を非 tree edge と置換。
-- 該当無しなら 'Nothing'。
--
-- **設計判断 (= Phase 1 A2 honest stub)**:
--
-- 一様 δ=1 / ω=1 の場合、 longest-path ranking が既に Σ edge length 最適解
-- (= 各 edge の length が ≥ δ=1 の制約下で全 edge 合計を最小化、 longest-path
-- は各 node を最深位置に置くので「圧縮余地ゼロ」)。 したがって全 tree edge の
-- cut value は ≥ 0 になることが保証され、 pivot は発生しない。
--
-- 本関数は **将来 weight / 異 δ 拡張に備えた framework hook**。 現状は常に
-- 'Nothing' を返し、 'iterateSimplex' は初期解で即終了する。
--
-- 拡張時の実装方針 (TODO Phase 1+):
--
--   1. 'tightTreeEdges' で tight edge から spanning tree 抽出
--   2. 各 tree edge を切ったときの head/tail 側 partition を BFS で求め
--   3. 非 tree edge weight 差から cut value 計算
--   4. 最小 cut value < 0 なら非 tree edge の min slack で置換
pivotOnce :: LayoutGraph -> Maybe LayoutGraph
pivotOnce _ = Nothing

-- | tight tree edge (= rank(v) - rank(u) = δ(u,v) を満たす edge) を列挙。
-- pivot 実装時の前段として用意。 現状未使用。
tightTreeEdges :: LayoutGraph -> [LEdge]
tightTreeEdges lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      isTight e = case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
        (Just ru, Just rv) -> rv - ru == leDelta e
        _                  -> False
  in filter isTight (lgEdges lg)

-- | rank を 0-base に正規化 (= 最小 rank を 0 に shift)。
normalizeRanks :: LayoutGraph -> LayoutGraph
normalizeRanks lg =
  case lgNodes lg of
    [] -> lg
    ns ->
      let rmin = minimum (map lnRank ns)
          newNodes = [ n { lnRank = lnRank n - rmin } | n <- ns ]
      in lg { lgNodes = newNodes }

-- | Phase 19 A4: rank 引き締め ('assignRanks' の後処理)。
--
-- ① **source 引き下げ**: in-edge 無し・out-edge 有りの node を
--    @min(rank(succ) − δ)@ へ。 longest-path ranking は source を rank 0 に
--    固定するため、 深い消費者しか持たない source (data slot / sigma 等) の
--    edge が図を縦断し、 plate bbox (= メンバの bounding box) が縦に伸びる
--    (Σ edge length も非最適。 graphviz は source を消費者の直前 rank に置く)。
--    全 out-edge の rank 差 ≥ δ は min の取り方により維持される。
-- ② **エッジ無し plate メンバの引き寄せ**: edge を一切持たない node が
--    plate メンバなら、 同 plate の (edge を持つ) メンバの最小 rank へ
--    (フローティング解消・analyze の DataIx データノードで顕在化)。
--
-- 最後に 0-base へ再正規化する。 plate 無し・深い source 無しのグラフでは
-- no-op (= 既存図はビット不変)。
tightenSourceRanks :: [[Text]] -> LayoutGraph -> LayoutGraph
tightenSourceRanks plateMembers lg =
  let nodes   = lgNodes lg
      edges   = lgEdges lg
      rankOf0 = Map.fromList [ (lnId n, lnRank n) | n <- nodes ]
      hasIn   = Set.fromList (map leTo edges)
      hasOut  = Set.fromList (map leFrom edges)
      succOf  = Map.fromListWith (<>)
                  [ (leFrom e, [(leTo e, leDelta e)]) | e <- edges ]
      -- ① source 引き下げ
      rank1 nid r
        | nid `Set.member` hasIn = r
        | Just ss <- Map.lookup nid succOf =
            minimum [ Map.findWithDefault 0 v rankOf0 - d | (v, d) <- ss ]
        | otherwise = r
      rankOf1 = Map.mapWithKey rank1 rankOf0
      -- ② エッジ無し plate メンバ (plate は外側→内側順で渡される。
      --    最初に見つかった所属 plate = 最内でなくてよい: メンバ rank の
      --    min はどの所属 plate でも bbox を縮める方向)
      edgeless nid = not (nid `Set.member` hasIn) && not (nid `Set.member` hasOut)
      plateMin nid =
        case [ rs | members <- plateMembers, nid `elem` members
                  , let rs = [ r | m <- members, m /= nid
                                 , not (edgeless m)
                                 , Just r <- [Map.lookup m rankOf1] ]
                  , not (null rs) ] of
          (rs : _) -> Just (minimum rs)
          []       -> Nothing
      rank2 nid r
        | edgeless nid, Just r' <- plateMin nid = r'
        | otherwise = r
      rankOf2 = Map.mapWithKey rank2 rankOf1
      newNodes = [ n { lnRank = Map.findWithDefault (lnRank n) (lnId n) rankOf2 }
                 | n <- nodes ]
  in normalizeRanks lg { lgNodes = newNodes }

-- ===========================================================================
-- Inspection (= test 用)
-- ===========================================================================

-- | Σ ω(u,v) × (rank(v) - rank(u)) を返す (= rank assignment の目的関数)。
-- longest-path / network simplex の検算用。
edgeLengthSum :: LayoutGraph -> Double
edgeLengthSum lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      contrib e = case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
        (Just ru, Just rv) -> leWeight e * fromIntegral (rv - ru)
        _                  -> 0
  in sum (map contrib (lgEdges lg))

-- | feasibility check: 全 edge で rank(v) - rank(u) ≥ δ(u,v)。
isFeasible :: LayoutGraph -> Bool
isFeasible lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      check e = case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
        (Just ru, Just rv) -> rv - ru >= leDelta e
        _                  -> True
  in all check (lgEdges lg)

-- ===========================================================================
-- 汎用 network simplex (Gansner-Koutsofios-North-Vo 1993 §2.3)
--   = graphviz の network simplex に相当する共通ソルバ。 node 集合と
--   (tail, head, δ, ω) edge 群を受け、 各 node に整数座標 r を割当て
--     Σ ω · (r_head − r_tail)
--   を制約 r_head − r_tail ≥ δ の下で最小化する。
--
--   graphviz では rank.c (rank 割当) と position.c (x 座標 = aux graph 上の
--   同 simplex) の両方がこれを使う。 hgg では ranking は一様 δ=ω=1 で
--   longest-path が既に最適なため 'assignRanks' はそのまま据え置き、 本関数は
--   主に P4a x 座標割当 (= Ω 1:2:8 + nodesep の非一様 aux graph) で使う。
--
--   流れ:
--     1. initRankNS      : longest-path で feasible 初期解 (入力は DAG 前提)
--     2. feasibleTreeNS  : tight edge で spanning tree を成長 (min-slack で調整)
--     3. tightRanksTree  : tree を全 edge tight にする一意割当 (大域 shift 自由度
--                          を root=0 で固定)
--     4. optimize        : 負 cut value の tree edge を、 cut を逆向きに跨ぐ
--                          min-slack 非 tree edge と交換し、 新 tree で再割当
--     5. normalizeNS     : 最小 r を 0 に
--
--   非連結 graph は弱連結成分ごとに独立フレームで解く。
-- ===========================================================================

-- | 汎用 network simplex。 戻り値は全 node の整数座標 (= rank / x)。
-- balance は行わない (= ranking 用・最適頂点を 1 つ返す)。
networkSimplex :: [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
networkSimplex = networkSimplexWith False

-- | LR balance 付き network simplex (= graphviz position.c の @rank(g, 2)@ 相当)。
-- 最適到達後、 cut value 0 の tree edge を slack の中央へ寄せて対称化する
-- (= x 座標割当 P4a 用。 free node を隣接の重心へ寄せ左右対称にする)。
networkSimplexBalanced :: [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
networkSimplexBalanced = networkSimplexWith True

networkSimplexWith
  :: Bool -> [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
networkSimplexWith balance nodes edges =
  Map.unions [ solveComponent balance cn ce | (cn, ce) <- weakComponents nodes edges ]

-- | 弱連結成分に分解 (edge を無向視)。 edge を持たない孤立 node も 1 成分。
weakComponents
  :: [Text] -> [(Text, Text, Int, Double)]
  -> [([Text], [(Text, Text, Int, Double)])]
weakComponents nodes edges =
  let undirAdj = Map.fromListWith (<>)
        (concat [ [(t, [h]), (h, [t])] | (t, h, _, _) <- edges ])
      bfs visited [] = visited
      bfs visited (v : q) =
        let ns = [ u | u <- Map.findWithDefault [] v undirAdj
                     , not (Set.member u visited) ]
        in bfs (foldr Set.insert visited ns) (q ++ ns)
      go (seen, acc) v
        | Set.member v seen = (seen, acc)
        | otherwise =
            let comp  = bfs (Set.singleton v) [v]
                cn    = [ u | u <- nodes, Set.member u comp ]
                ce    = [ e | e@(t, h, _, _) <- edges
                            , Set.member t comp, Set.member h comp ]
            in (Set.union seen comp, acc ++ [(cn, ce)])
      (_, comps) = foldl' go (Set.empty, []) nodes
  in comps

-- | 連結成分 1 個を解く。 @balance@ なら最後に LR balance を掛ける。
solveComponent
  :: Bool -> [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
solveComponent balance cnodes cedges
  | null cnodes = Map.empty
  | null cedges = Map.fromList [ (v, 0) | v <- cnodes ]
  | otherwise =
      let r0           = initRankNS cnodes cedges
          tree0        = feasibleTreeNS cnodes cedges r0
          r1           = tightRanksTree cnodes cedges tree0
          budget       = 2 * length cedges + length cnodes
          (treeF, rF)  = optimizeNS budget cnodes cedges tree0 r1
          rB           = if balance then balanceLR cnodes cedges treeF rF else rF
      in normalizeNS rB

-- | longest-path feasible 初期 rank (各 node = source からの δ 重み最長 path)。
initRankNS :: [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
initRankNS cnodes cedges =
  let parents = Map.fromListWith (<>)
                  [ (h, [(t, d)]) | (t, h, d, _) <- cedges ]
      go memo v = case Map.lookup v memo of
        Just r  -> (r, memo)
        Nothing ->
          let ps        = Map.findWithDefault [] v parents
              memo0     = Map.insert v 0 memo  -- cycle 安全 (本来 DAG)
              (memo', rs) = foldl'
                (\(m, acc) (p, d) -> let (rp, m') = go m p in (m', (rp + d) : acc))
                (memo0, []) ps
              r = if null rs then 0 else maximum rs
          in (r, Map.insert v r memo')
      finalMemo = foldl' (\m v -> snd (go m v)) Map.empty cnodes
  in Map.fromList [ (v, Map.findWithDefault 0 v finalMemo) | v <- cnodes ]

-- | edge の slack = r_head − r_tail − δ (≥ 0 が feasible)。
slackNS :: Map Text Int -> (Text, Text, Int, Double) -> Int
slackNS r (t, h, d, _) =
  Map.findWithDefault 0 h r - Map.findWithDefault 0 t r - d

-- | tight edge で spanning tree を成長させ tree edge index 集合を返す。
-- spanning に満たない間は min-slack の incident 非 tree edge を選び tree を
-- 平行移動して tight 化し、 再成長する (Gansner93 feasible_tree)。
feasibleTreeNS
  :: [Text] -> [(Text, Text, Int, Double)] -> Map Text Int -> Set.Set Int
feasibleTreeNS cnodes cedges r0 =
  let iedges = zip [0 :: Int ..] cedges
      n      = length cnodes
      start  = head cnodes
      -- 現 rank での tight tree (1 edge ずつ追加して必ず tree を保つ)。
      tightTree r =
        let step (tn, te) =
              case [ (i, if Set.member t tn then h else t)
                   | (i, (t, h, d, _)) <- iedges
                   , not (Set.member i te)
                   , let inT = Set.member t tn
                         inH = Set.member h tn
                   , inT /= inH
                   , Map.findWithDefault 0 h r - Map.findWithDefault 0 t r - d == 0 ] of
                []          -> (tn, te)
                ((i, o) : _) -> step (Set.insert o tn, Set.insert i te)
        in step (Set.singleton start, Set.empty)
      loop r =
        let (tn, te) = tightTree r
        in if Set.size tn >= n
             then te
             else
               let cands = [ (slackNS r e, Set.member h tn)
                           | (_, e@(t, h, _, _)) <- iedges
                           , let inT = Set.member t tn
                                 inH = Set.member h tn
                           , inT /= inH ]
               in case cands of
                    [] -> te  -- 非連結 (理論上ここには来ない) → 現状で打切り
                    _  ->
                      let (sl, headInTree) = minimum cands
                          delta = if headInTree then negate sl else sl
                          r' = Map.mapWithKey
                                 (\v x -> if Set.member v tn then x + delta else x) r
                      in loop r'
  in loop r0

-- | spanning tree を全 edge tight にする一意 rank (root=start を 0 に固定)。
tightRanksTree
  :: [Text] -> [(Text, Text, Int, Double)] -> Set.Set Int -> Map Text Int
tightRanksTree cnodes cedges tree =
  let start = head cnodes
      -- tree edge を無向化: t→h は +d、 h→t は −d (r_head = r_tail + δ)
      adj = Map.fromListWith (<>) $ concat
        [ [(t, [(h, d)]), (h, [(t, negate d)])]
        | (i, (t, h, d, _)) <- zip [0 :: Int ..] cedges, Set.member i tree ]
      bfs visited rank [] = rank
      bfs visited rank (v : q) =
        let (visited', rank', new) =
              foldl' (\(vs, rk, nw) (u, dd) ->
                        if Set.member u vs
                          then (vs, rk, nw)
                          else ( Set.insert u vs
                               , Map.insert u (Map.findWithDefault 0 v rk + dd) rk
                               , u : nw ))
                     (visited, rank, []) (Map.findWithDefault [] v adj)
        in bfs visited' rank' (q ++ new)
      ranked = bfs (Set.singleton start) (Map.singleton start 0) [start]
  in Map.fromList [ (v, Map.findWithDefault 0 v ranked) | v <- cnodes ]

-- | negative cut value の tree edge を解消するまで pivot。
-- 戻り値 = (最終 spanning tree, rank)。 tree は balance で再利用する。
optimizeNS
  :: Int -> [Text] -> [(Text, Text, Int, Double)]
  -> Set.Set Int -> Map Text Int -> (Set.Set Int, Map Text Int)
optimizeNS budget cnodes cedges tree rank
  | budget <= 0 = (tree, rank)
  | otherwise =
      let iedges = zip [0 :: Int ..] cedges
          edgeOf i = cedges !! i
          -- tree edge le を外したときの tail 側成分 (= tail を含む node 集合)
          tailSide le =
            let (lt, _, _, _) = edgeOf le
                tadj = Map.fromListWith (<>) $ concat
                  [ [(t, [h]), (h, [t])]
                  | (i, (t, h, _, _)) <- iedges, Set.member i tree, i /= le ]
                bfs visited [] = visited
                bfs visited (v : q) =
                  let ns = [ u | u <- Map.findWithDefault [] v tadj
                               , not (Set.member u visited) ]
                  in bfs (foldr Set.insert visited ns) (q ++ ns)
            in bfs (Set.singleton lt) [lt]
          cutValue le =
            let tc = tailSide le
                contrib (t, h, _, w) =
                  let tIn = Set.member t tc
                      hIn = Set.member h tc
                  in if tIn && not hIn then w
                     else if not tIn && hIn then negate w
                     else 0
            in (sum (map contrib cedges), tc)
          negTreeEdges =
            [ (le, tc) | le <- Set.toList tree
                       , let (cv, tc) = cutValue le, cv < -1e-9 ]
      in case negTreeEdges of
           [] -> (tree, rank)  -- 最適到達
           ((le, tc) : _) ->
             -- entering edge: cut を逆向き (head 側→tail 側) に跨ぐ min-slack 非 tree edge
             let enters = [ (slackNS rank e, i)
                          | (i, e@(t, h, _, _)) <- iedges
                          , not (Set.member i tree)
                          , not (Set.member t tc)   -- tail が head 側
                          , Set.member h tc ]       -- head が tail 側
             in case enters of
                  [] -> (tree, rank)  -- 理論上来ない (cut<0 なら必ず存在)
                  _  ->
                    let (_, fe) = minimum enters
                        tree'   = Set.insert fe (Set.delete le tree)
                        rank'   = tightRanksTree cnodes cedges tree'
                    in optimizeNS (budget - 1) cnodes cedges tree' rank'

-- | LR balance (graphviz ns.c @balance@, mode 2)。 cut value 0 の tree edge を
-- 列挙し、 その edge を逆向きに跨ぐ非 tree edge の slack δ (= 動かせる余地) の
-- 半分だけ tail 側成分を中央へ寄せる。 cost は不変 (cut=0 = 微分 0) なので最適性
-- を保ったまま free node を対称化する。 single pass (graphviz と同様)。
balanceLR
  :: [Text] -> [(Text, Text, Int, Double)]
  -> Set.Set Int -> Map Text Int -> Map Text Int
balanceLR _cnodes cedges tree rank0 =
  let iedges = zip [0 :: Int ..] cedges
      edgeOf i = cedges !! i
      tailSide le =
        let (lt, _, _, _) = edgeOf le
            tadj = Map.fromListWith (<>) $ concat
              [ [(t, [h]), (h, [t])]
              | (i, (t, h, _, _)) <- iedges, Set.member i tree, i /= le ]
            bfs visited [] = visited
            bfs visited (v : q) =
              let ns = [ u | u <- Map.findWithDefault [] v tadj
                           , not (Set.member u visited) ]
              in bfs (foldr Set.insert visited ns) (q ++ ns)
        in bfs (Set.singleton lt) [lt]
      cutValue le tc =
        sum [ if Set.member t tc && not (Set.member h tc) then w
              else if not (Set.member t tc) && Set.member h tc then negate w
              else 0
            | (t, h, _, w) <- cedges ]
      step rank le =
        let tc = tailSide le
        in if abs (cutValue le tc) > 1e-9
             then rank  -- cut ≠ 0 は動かせない
             else
               let enters = [ Map.findWithDefault 0 h rank
                              - Map.findWithDefault 0 t rank - d
                            | (i, (t, h, d, _)) <- iedges
                            , not (Set.member i tree)
                            , not (Set.member t tc), Set.member h tc ]
               in case enters of
                    [] -> rank
                    _  -> let delta = minimum enters
                          in if delta < 2 then rank
                             else let half = delta `div` 2
                                  in Map.mapWithKey
                                       (\v x -> if Set.member v tc then x - half else x)
                                       rank
      -- tail 側を持つ tree edge のみ対象 (LR/straightening 両方含む)
  in foldl' step rank0 (Set.toList tree)

-- | 最小座標を 0 に正規化。
normalizeNS :: Map Text Int -> Map Text Int
normalizeNS m
  | Map.null m = m
  | otherwise  = let mn = minimum (Map.elems m) in Map.map (subtract mn) m

-- ===========================================================================
-- Step 3: Order assignment (Phase 1 A3)
--   Gansner-Koutsofios-North-Vo 1993 §3、 dot default 24 iteration の
--   median heuristic + transpose で同 rank 内の node 順を最適化。
--   長 edge (rank 差 > 1) は dummy node 経由の short edge 列に展開する。
-- ===========================================================================

-- | 各 rank の node 順 (= rank → 左から右の id 列)。
type OrderMap = Map Int [Text]

-- | 長 edge (= rank 差 > 1) を中間 rank の dummy node 経由の短 edge 列に展開。
-- dummy node は 'lnDummy = True' で区別、 id は @"__dummy_<n>"@。
-- 元 edge は削除され、 同 weight の短 edge 列に置換される。
insertDummies :: LayoutGraph -> LayoutGraph
insertDummies lg = fst (insertDummiesWithChains lg)

-- | 'insertDummies' + 元 edge → 経由 chain (= 始点と終点を含む id 列) を返す。
-- 短 edge (rank 差 1) も map に含まれ、 chain = [from, to] (= 2 要素)。
-- Phase 1 A5 edge routing で、 元 edge を chain 経由の control 点列で描画するために使う。
insertDummiesWithChains
  :: LayoutGraph -> (LayoutGraph, Map (Text, Text) [Text])
insertDummiesWithChains lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      step (accN, accE, accM, k) e =
        case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
          (Just ru, Just rv) | rv - ru > 1 ->
            let nDum = rv - ru - 1
                names = [ T.pack ("__dummy_" ++ show (k + i))
                        | i <- [0 .. nDum - 1] ]
                dnodes = zipWith (\nm r -> LNode nm r True) names [ru + 1 ..]
                chain = leFrom e : names ++ [leTo e]
                newE = zipWith (\f t -> LEdge f t 1 (leWeight e))
                               chain (tail chain)
            in ( accN ++ dnodes
               , accE ++ newE
               , Map.insert (leFrom e, leTo e) chain accM
               , k + nDum )
          _ ->
            ( accN
            , accE ++ [e]
            , Map.insert (leFrom e, leTo e) [leFrom e, leTo e] accM
            , k )
      (extra, newEdges, chainMap, _) =
        foldl' step ([], [], Map.empty, 0 :: Int) (lgEdges lg)
  in ( lg { lgNodes = lgNodes lg ++ extra, lgEdges = newEdges }
     , chainMap )

-- | rank ごとの初期順序 (= ID 辞書順、 決定論性のため)。
initialOrder :: LayoutGraph -> OrderMap
initialOrder lg =
  let grouped = Map.fromListWith (<>)
                  [ (lnRank n, [lnId n]) | n <- lgNodes lg ]
  in Map.map sort grouped

-- | 2 隣接 rank 間の交差数 (naive O(E^2))。
-- 'edges' は (u, v) ペア、 u は upper の id、 v は lower の id。
bilayerCrossings :: [(Text, Text)] -> [Text] -> [Text] -> Int
bilayerCrossings edges upper lower =
  let posU = Map.fromList (zip upper [0 :: Int ..])
      posL = Map.fromList (zip lower [0 :: Int ..])
      pairs = [ (pu, pl)
              | (u, v) <- edges
              , Just pu <- [Map.lookup u posU]
              , Just pl <- [Map.lookup v posL] ]
      go []                 = 0
      go ((pu1, pl1):rest)  =
        let c = length [ () | (pu2, pl2) <- rest
                            , (pu1 < pu2 && pl1 > pl2)
                              || (pu1 > pu2 && pl1 < pl2) ]
        in c + go rest
  in go pairs

-- | 全 rank pair の交差数合計。
countCrossings :: LayoutGraph -> OrderMap -> Int
countCrossings lg om =
  let rankMap = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      edgesAt r =
        [ (leFrom e, leTo e)
        | e <- lgEdges lg
        , Map.lookup (leFrom e) rankMap == Just r
        , Map.lookup (leTo e) rankMap   == Just (r + 1) ]
      ranks = Map.keys om
      maxR  = if null ranks then 0 else maximum ranks
  in sum [ bilayerCrossings (edgesAt r)
             (Map.findWithDefault [] r om)
             (Map.findWithDefault [] (r + 1) om)
         | r <- [0 .. maxR - 1] ]

-- | median: 偶数個なら 2 中央値の平均、 奇数個なら中央。
medianOf :: [Double] -> Maybe Double
medianOf [] = Nothing
medianOf xs =
  let s = sort xs
      n = length s
      mid = n `div` 2
  in Just $ if odd n
            then s !! mid
            else (s !! (mid - 1) + s !! mid) / 2

-- | 1 回 sweep (= median heuristic 1 pass)。
-- 'topDown' True = rank 増加方向、 False = 減少方向。
medianSweep :: LayoutGraph -> Bool -> OrderMap -> OrderMap
medianSweep lg topDown om0 =
  let ranks = sort (Map.keys om0)
      sweepDir = if topDown then ranks else reverse ranks
      adjOf v td =
        if td  -- 上から下に sweep → 各 node の median は **predecessors (= 上の rank)** で決める
          then [ leFrom e | e <- lgEdges lg, leTo   e == v ]
          else [ leTo   e | e <- lgEdges lg, leFrom e == v ]
      sweepOne r om' =
        let adjRank = if topDown then r - 1 else r + 1
            adjList = Map.findWithDefault [] adjRank om'
            posMap = Map.fromList (zip adjList [0 :: Int ..])
            posOf x = fromIntegral <$> Map.lookup x posMap
            here = Map.findWithDefault [] r om'
            tagged =
              [ (i, v, medianOf [p | nb <- adjOf v topDown, Just p <- [posOf nb]])
              | (i, v) <- zip [0 :: Int ..] here ]
            -- Nothing は元位置維持 (= stable sort)
            cmp (i1, _, m1) (i2, _, m2) = case (m1, m2) of
              (Just a, Just b) -> compare a b <> compare i1 i2
              (Just _, Nothing) -> LT
              (Nothing, Just _) -> GT
              (Nothing, Nothing) -> compare i1 i2
            sorted = sortBy cmp tagged
        in Map.insert r [ v | (_, v, _) <- sorted ] om'
  in foldl' (flip sweepOne) om0 sweepDir

-- | transpose: 同 rank 内の隣接 pair を試し交換、 交差数が下がるなら採用。
-- 上下 rank の edge を両方見て判定。
transposeOrder :: LayoutGraph -> OrderMap -> OrderMap
transposeOrder lg om0 =
  let rankMap = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      edgesBetween r1 r2 =
        [ (leFrom e, leTo e)
        | e <- lgEdges lg
        , Map.lookup (leFrom e) rankMap == Just r1
        , Map.lookup (leTo e) rankMap   == Just r2 ]
      tryRank r om' =
        let here   = Map.findWithDefault [] r       om'
            upper  = Map.findWithDefault [] (r - 1) om'
            lower  = Map.findWithDefault [] (r + 1) om'
            eUp    = edgesBetween (r - 1) r
            eDown  = edgesBetween r       (r + 1)
            crossOf order =
              bilayerCrossings eUp upper order
              + bilayerCrossings eDown order lower
            -- 隣接 pair を順に swap 試行、 交差数が減るなら採用 (= dot 流 transpose)
            doSwap acc i =
              if i + 1 >= length acc
                then acc
                else
                  let a   = acc !! i
                      b   = acc !! (i + 1)
                      swp = take i acc ++ [b, a] ++ drop (i + 2) acc
                  in if crossOf swp < crossOf acc
                       then doSwap swp (i + 1)
                       else doSwap acc (i + 1)
            optimized = doSwap here 0
        in Map.insert r optimized om'
      ranks = sort (Map.keys om0)
  in foldl' (flip tryRank) om0 ranks

-- | Step 3 メイン: dummy 挿入 + 24 iteration median sweep + transpose。
-- 戻り値 = (拡張済 LayoutGraph、 OrderMap)。 OrderMap は dummy も含む。
assignOrder :: LayoutGraph -> (LayoutGraph, OrderMap)
assignOrder lg0 = let (a, b, _) = assignOrderFull lg0 in (a, b)

-- | 'assignOrder' + 元 edge → chain map (= A5 edge routing 用)。
assignOrderFull
  :: LayoutGraph
  -> (LayoutGraph, OrderMap, Map (Text, Text) [Text])
assignOrderFull lg0 =
  let (lg, chainMap) = insertDummiesWithChains lg0
      ini = initialOrder lg
      iniCross = countCrossings lg ini
      step (best, bestC) i =
        let td = even (i :: Int)
            swept = medianSweep lg td best
            transposed = transposeOrder lg swept
            c = countCrossings lg transposed
        in if c < bestC then (transposed, c) else (best, bestC)
      (final, _) = foldl' step (ini, iniCross) [0 .. 23]
  in (lg, final, chainMap)

-- ===========================================================================
-- Step 4: Coordinate assignment (Phase 39 A2-5)
--   Brandes & Köpf 2002 "Fast and Simple Horizontal Coordinate Assignment"
--   の **完全な 4-candidate** 実装。 縦 {down, up} × 横 {left, right} の 4
--   alignment を計算し、 各方向で
--     (1) type-1 conflict マーク (inner segment 優先で非 inner segment を犠牲)
--     (2) vertical alignment (= median 近傍へ block 整列)
--     (3) horizontal compaction (= block を最小間隔で詰める)
--   を行い、 最後に 4 候補を **median balance** (最小幅候補に整列 → 各 node で
--   中央 2 値の平均) で統合する。
--
--   Phase 1 A4 は up/down の 2 候補 median のみ (= 'computeOneDir' 2 本) だった。
--   2 候補では source/sink 寄りの縦 bias しか相殺できず、 同 rank に分岐が多い
--   plate 図 (階層ベイズ DAG 等) で左右非対称が残る。 4 候補 BK は左右 alignment
--   も加えるため、 graphviz (= 同じ BK 系) に近い対称配置になる。
-- ===========================================================================

-- | 縦方向 = 層の処理順 (= block を上から作るか下から作るか)。
data VDir = DDown | DUp
-- | 横方向 = 同 rank 内の左寄せ / 右寄せ (= median が偶数個のときどちらに寄せるか)。
data HDir = HLeft | HRight deriving Eq

-- | block formation / compaction の最小間隔。 dummy が絡む隣接は近接許可
-- (= dummy は描画されないので chain node と近接させ長 edge spline の遠回りを抑える。
-- 'computeOneDir' と同値)。
bkMinSpacing, bkDummySpacing :: Double
bkMinSpacing   = 1.0
bkDummySpacing = 0.4

-- | Step 4 メイン: x 座標を割当て [0,1] 正規化。
-- 結果は OrderMap に含まれる全 node id (= dummy 含む) → x ∈ [0,1]。
--
-- Phase 39 Step3 (P4a): graphviz position.c に倣い Brandes-Köpf から
-- **aux graph network simplex** ('auxSimplexCoords') へ置換。 BK には無かった
-- 「dummy chain 直線化重み (Ω 1:2:8)」 と 「隣接対 nodesep 強制」 を simplex の
-- 目的関数/制約として同時最適化するため、 長 edge の dummy 列が並走 node 列の
-- 外へ独立縦列として分離する (= large の funnel collapse の layout 層 主因を根治)。
-- Phase 39 Step8 (P8): 'plates' (= cluster メンバ id 群) を P4a simplex に渡し、
-- cluster border 制約 ('clusterAuxEdges') を反映した x を解く。 plate 無し ([]) は
-- 従来と完全同一。
-- | 後方互換 wrapper (= 半幅情報なし = 全 real node 一律 'auxNodeHalfW')。
-- test 群はこちらを使い構造的不変条件 (collinear / keepout / gap≥) を検証する。
assignCoords :: [[Text]] -> LayoutGraph -> OrderMap -> Map Text Double
assignCoords = assignCoordsW Map.empty

-- | Phase 39 P8 A4-2: size-aware 版。 @hwMap@ = real node id → 横半幅 (px, 整数)
-- ('dagNodeBaseHalfWidth' を round したもの・DAG.coordStage が供給)。 simplex の
-- node 間隔/cluster border 制約を実 node 幅で解く (= 兄弟 plate の box 重なり根治)。
assignCoordsW :: Map Text Int -> [[Text]] -> LayoutGraph -> OrderMap -> Map Text Double
assignCoordsW hwMap plates lg om =
  let raw  = auxSimplexCoordsW hwMap plates lg om
      vals = Map.elems raw
      xMin = if null vals then 0 else minimum vals
      xMax = if null vals then 1 else maximum vals
      norm x = if xMax - xMin < 1e-9 then 0.5
               else (x - xMin) / (xMax - xMin)
  in Map.map norm raw

-- ===========================================================================
-- Step 4 (Phase 39 Step3 = P4a): aux graph network simplex で x 座標
--   graphviz position.c の @dot_position@ =
--     create_aux_edges → rank(aux, 2 = LR balance) → remove_aux_edges
--   を移植。 補助グラフは
--     ① straightening: 各 layout edge (u,v) を aux node a_e + 2 本の minlen-0
--        edge a_e→u, a_e→v (weight Ω) に変換。 a_e は min(x_u,x_v) へ浮き、
--        cost = Ω·|x_u − x_v| (= 直線化)。 Ω は端点の種類で
--          real-real 1 : real-virtual 2 : virtual-virtual 8
--        (dot 既定比。 dummy chain ほど強く直線に保つ)。
--     ② LR 制約: 同 rank の隣接対 (l,r) に edge l→r、 minlen = nodesep、 weight 0
--        (= 順序保持 + 最小間隔強制)。 dummy 絡みは間隔を詰める。
--   この aux graph 上で 'networkSimplexBalanced' を解くと、 並走する実 node 列と
--   long-edge dummy 列が nodesep 以上離れた独立縦列になる。
-- ===========================================================================

-- | LR 制約の最小間隔 = graphviz make_LR_constraints の
--   @width = ND_rw(left) + ND_lw(right) + nodesep@ を移植。
-- 各 node の **半幅** + nodesep の和を隣接間隔とする。 これにより
-- real node の隣に来る dummy は real の半幅ぶん外へ押し出され、 real node の
-- body 内側に潜り込まない (= long-edge dummy 列が並走 chain の node body の外に
-- 出る = funnel collapse の layout 層 主因を根治)。
--
-- 旧実装は dummy 絡みを一律に小間隔 (= 旧 BK bkDummySpacing 0.4) にしており、
-- dummy が real node body の内側に入っていた (= 並走 chain を貫通) のが large の
-- 主因だった。
--
-- 値は **偶数**にする: 全 minlen 偶数 → 全 rank 偶数和 → LR balance の
-- @delta `div` 2@ が丸め無しで厳密中央化される。
--
-- Phase 39 P8 A4-2 (改訂): 半幅は **一様** ('auxNodeHalfW') に戻した。 size-aware
-- (per-node 幅) は非兄弟グラフの位置を動かし long-edge routing を折る回帰を生んだ
-- (実測 2026-06-24)。 兄弟 plate box の重なりは separate_subclust (normalized gap)
-- + render binding-pair (実幅は radius 既知の render で考慮) で解く。
auxNodeHalfW, auxDummyHalfW, auxNodeSep :: Int
auxNodeHalfW  = 4   -- real node の半幅 (hwMap 欠落時 fallback)
auxDummyHalfW = 0   -- dummy (virtual) node の半幅 (graphviz でも極小)
auxNodeSep    = 18  -- ★ A4-3 EXPERIMENT: graphviz nodesep 既定 18pt (point 一貫)

-- | Phase 39 Step8 (P8): cluster (= plate) box の margin。 graphviz @CL_OFFSET@=8pt。
auxPlateMargin :: Int
auxPlateMargin = 8

-- | node a と b (= 同 rank で a が左・b が右隣) の最小間隔。
-- @hwOf@ = 各 node の半幅 (px・dummy/欠落は内部 fallback 済)。
auxSepOf :: (Text -> Int) -> Text -> Text -> Int
auxSepOf hwOf a b = hwOf a + hwOf b + auxNodeSep

-- | P4a 本体: aux graph を構築し simplex で x (整数) を解いて Double で返す。
-- 戻り値 = 実 node (dummy 含む・aux 除く) の raw x。 正規化は 'assignCoords' 側。
--
-- Phase 39 Step8 (P8): 'plates' (= cluster メンバ id リスト群) があれば
-- 'clusterAuxEdges' で graphviz position.c @pos_clusters@ 相当の cluster x 制約
-- (border node + contain/keepout edge) を aux graph に追加する。 plate 無しは
-- 従来と完全同一 (= 図ビット不変)。
auxSimplexCoords :: [[Text]] -> LayoutGraph -> OrderMap -> Map Text Double
auxSimplexCoords = auxSimplexCoordsW Map.empty

-- | Phase 39 P8 A4-2: size-aware 版。 @hwMap@ = real node id → 横半幅 (px)。
-- @hwOf@ は dummy を 'auxDummyHalfW'、 hwMap 欠落を 'auxNodeHalfW' へ fallback。
auxSimplexCoordsW
  :: Map Text Int -> [[Text]] -> LayoutGraph -> OrderMap -> Map Text Double
auxSimplexCoordsW hwMap plates lg om =
  let dummySet = Set.fromList [ lnId n | n <- lgNodes lg, lnDummy n ]
      isDum v  = Set.member v dummySet
      hwOf v   = if isDum v then auxDummyHalfW
                 else Map.findWithDefault auxNodeHalfW v hwMap
      omega u v
        | isDum u && isDum v = 8 :: Int
        | isDum u || isDum v = 2
        | otherwise          = 1
      -- ① straightening: edge ごとに aux node + 2 本の minlen-0 edge
      auxId i = "__auxpos_" <> T.pack (show (i :: Int))
      straightEdges = concat
        [ let u = leFrom e; v = leTo e
              w = fromIntegral (omega u v) * leWeight e
          in [ (auxId i, u, 0, w), (auxId i, v, 0, w) ]
        | (i, e) <- zip [0 ..] (lgEdges lg) ]
      -- ② LR 制約: 同 rank 隣接対に minlen = sep (= 半幅和 + nodesep), weight 0
      lrEdges =
        [ (l, r, auxSepOf hwOf l r, 0)
        | (_, layer) <- Map.toAscList om, (l, r) <- zip layer (drop 1 layer) ]
      -- ③ P8 cluster 制約: border node + contain/keepout/separate (graphviz pos_clusters)
      (clustNodes, clustEdges) = clusterAuxEdges hwOf om plates
      auxNodes = [ auxId i | (i, _) <- zip [0 ..] (lgEdges lg) ]
      realKeys = [ lnId n | n <- lgNodes lg ]
      allNodes = realKeys ++ auxNodes ++ clustNodes
      xInt = networkSimplexBalanced allNodes
               (straightEdges ++ lrEdges ++ clustEdges)
  in Map.fromList
       [ (v, fromIntegral (Map.findWithDefault 0 v xInt)) | v <- realKeys ]

-- | Phase 39 Step8 (P8): graphviz @lib/dotgen/position.c@ の @pos_clusters@
-- (= @create_aux_edges@ 内) が張る cluster x 制約を、 我々の aux graph network
-- simplex 用 edge として生成する。 一次ソースに忠実 (CL_OFFSET=8pt → 'auxPlateMargin'、
-- border label 無し → border.x=0、 nested は A4 で別途)。
--
-- 各 plate p に左右 border virtual node @ln_p@ / @rn_p@ を立て (graphviz
-- @make_lrvn@ = SLACKNODE)、 以下を張る:
--
--   * @contain_nodes@: 各 rank の最左 member へ @ln_p → 最左@ (minlen =
--     半幅 + margin)、 最右 member から @最右 → rn_p@ (同)。 = 箱の左右端確定。
--   * @contain_clustnodes@: @ln_p → rn_p@ (minlen 1, weight 128)。 = 箱を tight
--     に圧縮 (member を詰める強い重み)。
--   * @keepout_othernodes@: 各 rank で member ブロックの外側最近接 **非メンバ** u に
--     @u → ln_p@ / @rn_p → u@ (minlen = margin + 半幅)。 = 非メンバを箱外へ排除。
--   * @separate_subclust@: 同一 rank に並ぶ **兄弟** plate (= 互いに包含関係に無い)
--     の隣接 border 間に @rn_left → ln_right@ (minlen = CL_OFFSET)。 = 隣接箱の
--     margin ぶんの隙間を x 解に確保 (= P8 A4-2・兄弟 plate box 重なりの根治)。
--
-- ★ Phase 44.3: graphviz @pos_clusters@ にはもう一つ @contain_subclust@
-- (nested 親子 plate に @ln_p → ln_c@ / @rn_c → rn_p@・minlen CL_OFFSET・weight 128 を
-- 直接の子へ張り、 親箱が子箱を margin ぶん外側で囲む) があるが、 **意図的に未実装**。
-- 理由 (実測): 我々の plate box は 'plateBoxPt' が「直接 member glyph box ∪ **子 plate box
-- (再帰)** + 固定 margin」で描くため、 入れ子の clearance は **箱モデル側で既に保証**される。
-- contain_subclust を試作して nested/deep/tri 図を再生成しても幾何変化はゼロ (末尾桁 FP
-- ノイズのみ・PNG はバイト一致) で、 border 制約は box 描画へ伝播しなかった。 = graphviz には
-- 在るが **我々の描画経路では非寄与**ゆえ採用しない (図再生成 FP ノイズの実コストだけが残る)。
-- 将来 box を border node 由来へ変える場合はその文脈で再導入する。
-- ([[feedback-graphviz-only-faithful-algos]] / 2026-06-26 実測)。
--
-- これにより plate が box として x 分離し、 cosmetic な 'applyPlateBands' /
-- 'recenterNonPlateRows' (帯分離) が不要になる (= Step8 で撤去済)。
--
-- Phase 39 P8 A4-2: 半幅は固定 'auxNodeHalfW' でなく @hwOf@ (= 'auxSimplexCoordsW'
-- が hwMap から作る per-node 実半幅) を使う。 dummy/欠落の fallback は hwOf 内で済。
clusterAuxEdges
  :: (Text -> Int) -> OrderMap -> [[Text]]
  -> ([Text], [(Text, Text, Int, Double)])
clusterAuxEdges hwOf om plates =
  let lnOf i = "__plate_ln_" <> T.pack (show (i :: Int))
      rnOf i = "__plate_rn_" <> T.pack (show (i :: Int))
      msetAt i = Set.fromList (plates !! i)
      perPlate (idx, members)
        | null members = ([], [])
        | otherwise    = ([ln, rn], containE ++ clustE ++ keepoutE)
        where
          ln   = lnOf idx
          rn   = rnOf idx
          mset = Set.fromList members
          isMem v = Set.member v mset
          -- 各 rank での member ブロックと layer 全体 (keepout の隣接探索用)
          rowsOf =
            [ (layer, mem)
            | (_, layer) <- Map.toAscList om
            , let mem = filter isMem layer
            , not (null mem) ]
          -- contain_nodes: ln → 最左 / 最右 → rn
          containE = concat
            [ [ (ln, head mem, hwOf (head mem) + auxPlateMargin, 0)
              , (last mem, rn, hwOf (last mem) + auxPlateMargin, 0) ]
            | (_, mem) <- rowsOf ]
          -- contain_clustnodes: ln → rn (tight 圧縮 weight 128)
          clustE = [ (ln, rn, 1, 128) ]
          -- keepout_othernodes: member ブロックの外側最近接非メンバを箱外へ
          keepoutE = concat
            [ [ (u, ln, auxPlateMargin + hwOf u, 0) | Just u <- [leftWall] ]
              ++ [ (rn, u, auxPlateMargin + hwOf u, 0) | Just u <- [rightWall] ]
            | (layer, _) <- rowsOf
            , let memIdxs = [ i | (i, v) <- zip [0 ..] layer, isMem v ]
            , not (null memIdxs)
            , let lo = minimum memIdxs
                  hi = maximum memIdxs
                  leftWall  = listToMaybe
                    [ layer !! i | i <- [lo - 1, lo - 2 .. 0], not (isMem (layer !! i)) ]
                  rightWall = listToMaybe
                    [ layer !! i | i <- [hi + 1 .. length layer - 1], not (isMem (layer !! i)) ] ]
      perResults = map perPlate (zip [0 ..] plates)
      -- separate_subclust: rank ごとに member を持つ plate を最左 member 位置で
      -- 整列し、 隣接対が兄弟 (= 包含関係に無い) なら border 間に CL_OFFSET を張る。
      nested a b = let ma = msetAt a; mb = msetAt b
                   in ma == mb || Set.isSubsetOf ma mb || Set.isSubsetOf mb ma
      sepEdges = concat
        [ [ (rnOf a, lnOf b, auxPlateMargin, 0 :: Double) ]
        | (_, layer) <- Map.toAscList om
        , let present = [ (i, minimum idxs)
                        | i <- [0 .. length plates - 1]
                        , let idxs = [ k | (k, v) <- zip [0 ..] layer
                                         , Set.member v (msetAt i) ]
                        , not (null idxs) ]
              ordered = map fst (sortBy (\x y -> compare (snd x) (snd y)) present)
        , (a, b) <- zip ordered (drop 1 ordered)
        , not (nested a b) ]
  in (concatMap fst perResults, concatMap snd perResults ++ sepEdges)

-- | 4 候補を計算し median balance で統合した raw x (= 正規化前)。
-- 'assignCoords' から呼ばれる本体。 test/検算用に export。
brandesKopf :: LayoutGraph -> OrderMap -> Map Text Double
brandesKopf lg om =
  let runs = [ runBK vd hd lg om | vd <- [DDown, DUp], hd <- [HLeft, HRight] ]
      -- 各候補の (min, max)。 空候補は (0,0)。
      stats = [ if null vs then (0, 0) else (minimum vs, maximum vs)
              | (m, _) <- runs, let vs = Map.elems m ]
      widths = [ mx - mn | (mn, mx) <- stats ]
      -- 最小幅候補を整列基準にする (= BK balance)
      refIdx = snd (minimum (zip widths [0 :: Int ..]))
      (minRef, maxRef) = stats !! refIdx
      -- left alignment は min を、 right alignment は max を基準へ寄せる
      shifted =
        [ let (m, isLeft) = runs !! i
              (mn, mx)    = stats !! i
              delta       = if isLeft then minRef - mn else maxRef - mx
          in Map.map (+ delta) m
        | i <- [0 .. length runs - 1] ]
      keys = Set.toList (Set.unions (map Map.keysSet shifted))
      -- 各 node で 4 値の中央 2 値の平均 (= median of 4)
      median4 v =
        let vs = sort [ Map.findWithDefault 0 v m | m <- shifted ]
        in case vs of
             (_:b:c:_) -> (b + c) / 2
             _         -> if null vs then 0 else head vs
  in Map.fromList [ (v, median4 v) | v <- keys ]

-- | 1 候補 (vdir, hdir) の x 座標を計算。 戻り値 = (x map, 左寄せ候補か)。
-- 横は left/right で同 rank の処理順を反転 (= mirror)、 縦は down/up で層の
-- 処理順 (= 隣接層を上から見るか下から見るか) を反転して全候補を同一コードで得る。
-- right 候補は最後に座標を反転 (negate) して全候補を共通の左→右フレームへ揃える。
runBK :: VDir -> HDir -> LayoutGraph -> OrderMap -> (Map Text Double, Bool)
runBK vdir hdir lg om =
  let ranksAsc = sort (Map.keys om)
      procRanks = case vdir of DDown -> ranksAsc; DUp -> reverse ranksAsc
      orient base = case hdir of HLeft -> base; HRight -> reverse base
      procLayers = [ orient (Map.findWithDefault [] r om) | r <- procRanks ]
      posP = Map.fromList [ (v, k) | layer <- procLayers, (k, v) <- zip [0 :: Int ..] layer ]
      -- 直前 proc 層への隣接 (= down は predecessors、 up は successors)
      adjMap = case vdir of
        DDown -> Map.fromListWith (<>) [ (leTo e,   [leFrom e]) | e <- lgEdges lg ]
        DUp   -> Map.fromListWith (<>) [ (leFrom e, [leTo e])   | e <- lgEdges lg ]
      posOf v = Map.findWithDefault 0 v posP
      predsP v = sortBy (\a b -> compare (posOf a) (posOf b))
                        (Map.findWithDefault [] v adjMap)
      dummySet = Set.fromList [ lnId n | n <- lgNodes lg, lnDummy n ]
      isDum v  = Set.member v dummySet
      marked   = markType1 procLayers posP predsP isDum
      (root, aln) = verticalAlign procLayers predsP posP marked
      -- 各 node → 同 proc 層の左隣 node
      leftNbr = Map.fromList (concat [ zip (drop 1 layer) layer | layer <- procLayers ])
      sep a b = if isDum a || isDum b then bkDummySpacing else bkMinSpacing
      xRaw = horizCompact procLayers root aln leftNbr sep
      xGlobal = case hdir of HLeft -> xRaw; HRight -> Map.map negate xRaw
  in (xGlobal, hdir == HLeft)

-- | type-1 conflict (= 非 inner segment が inner segment と交差) をマーク。
-- inner segment = 両端が dummy node の segment。 マークされた segment は
-- 'verticalAlign' で整列に使われない (= inner segment = 長 edge を真っ直ぐ保つ)。
-- 戻り値 = (upper node, lower node) ペア集合 (proc 層の隣接 2 層単位)。
markType1
  :: [[Text]] -> Map Text Int -> (Text -> [Text]) -> (Text -> Bool)
  -> Set.Set (Text, Text)
markType1 procLayers posP predsP isDum =
  let posOf v = Map.findWithDefault 0 v posP
      markBetween (upper, lower) =
        let nLow = length lower
            -- v (lower, dummy) が inner segment の下端なら上端 dummy の pos
            innerPred v
              | isDum v   = case [ u | u <- predsP v, isDum u ] of
                              (u:_) -> Just (posOf u)
                              []    -> Nothing
              | otherwise = Nothing
            go (k0, l, acc) l1 =
              let v = lower !! l1
                  mInner = innerPred v
              in if l1 == nLow - 1 || mInner /= Nothing
                   then
                     let k1 = case mInner of
                                Just pu -> pu
                                Nothing -> length upper - 1
                         consume (lc, ac)
                           | lc > l1   = (lc, ac)
                           | otherwise =
                               let w = lower !! lc
                                   newMarks = [ (u, w)
                                              | u <- predsP w
                                              , let ku = posOf u
                                              , ku < k0 || ku > k1 ]
                               in consume (lc + 1, foldr Set.insert ac newMarks)
                         (l', acc') = consume (l, acc)
                     in (k1, l', acc')
                   else (k0, l, acc)
            (_, _, marks) = foldl' go (0 :: Int, 0 :: Int, Set.empty) [0 .. nLow - 1]
        in marks
  in Set.unions (map markBetween (zip procLayers (drop 1 procLayers)))

-- | vertical alignment: 各 node を median 近傍 (= 直前 proc 層) へ整列し block を作る。
-- 戻り値 = (root map, align map)。 root[v] = v の block 代表、
-- align[v] = block 内 (循環リンク) の次 node。
verticalAlign
  :: [[Text]] -> (Text -> [Text]) -> Map Text Int -> Set.Set (Text, Text)
  -> (Map Text Text, Map Text Text)
verticalAlign procLayers predsP posP marked =
  let allV   = concat procLayers
      posOf v = Map.findWithDefault 0 v posP
      stepLayer (root, aln) layer =
        let goV (root', aln', r) v =
              let ps = predsP v
                  d  = length ps
              in if d == 0
                   then (root', aln', r)
                   else
                     let lo = (d - 1) `div` 2
                         hi = d `div` 2
                         mids = if lo == hi then [lo] else [lo, hi]
                         tryMid (rt, al, rr) m
                           | Map.findWithDefault v v al /= v = (rt, al, rr) -- v 既整列
                           | otherwise =
                               let u  = ps !! m
                                   pu = posOf u
                               in if not (Set.member (u, v) marked) && rr < pu
                                    then let rootU = Map.findWithDefault u u rt
                                             al'   = Map.insert v rootU
                                                       (Map.insert u v al)
                                             rt'   = Map.insert v rootU rt
                                         in (rt', al', pu)
                                    else (rt, al, rr)
                     in foldl' tryMid (root', aln', r) mids
            (root'', aln'', _) = foldl' goV (root, aln, -1 :: Int) layer
        in (root'', aln'')
      root0 = Map.fromList [ (v, v) | v <- allV ]
      aln0  = Map.fromList [ (v, v) | v <- allV ]
  in foldl' stepLayer (root0, aln0) procLayers

-- | horizontal compaction: block を順序保ちつつ最小間隔で詰める (leftmost)。
-- BK Algorithm 3 を素直に実装。 state = (x, sink, shift) を手で threading。
horizCompact
  :: [[Text]]
  -> Map Text Text                 -- ^ root
  -> Map Text Text                 -- ^ align
  -> Map Text Text                 -- ^ 左隣 node
  -> (Text -> Text -> Double)      -- ^ 最小間隔
  -> Map Text Double
horizCompact procLayers root aln leftNbr sep =
  let allV  = concat procLayers
      roots = [ v | v <- allV, Map.findWithDefault v v root == v ]
      st0   = (Map.empty, Map.empty, Map.empty)
      (xF, sinkF, shiftF) = foldl' (\st v -> placeBlock v st) st0 roots
      -- placeBlock v: v は root。 x[v] 未確定なら block を配置。
      placeBlock v st@(xm, _, _)
        | Map.member v xm = st
        | otherwise =
            let st1 = let (xm0, sk0, sh0) = st in (Map.insert v 0 xm0, sk0, sh0)
                loop w stc =
                  let stc' = case Map.lookup w leftNbr of
                               Nothing -> stc
                               Just wl ->
                                 let u  = Map.findWithDefault wl wl root
                                     (xu0, su0, sh0) = placeBlock u stc
                                     d    = sep wl w
                                     xV   = Map.findWithDefault 0 v xu0
                                     xU   = Map.findWithDefault 0 u xu0
                                     sinkV = Map.findWithDefault v v su0
                                     sinkU = Map.findWithDefault u u su0
                                     su1   = if sinkV == v
                                               then Map.insert v sinkU su0
                                               else su0
                                     sinkV' = Map.findWithDefault v v su1
                                 in if sinkV' /= sinkU
                                      then let cur = Map.findWithDefault inf sinkU sh0
                                               nv  = min cur (xV - xU - d)
                                           in (xu0, su1, Map.insert sinkU nv sh0)
                                      else let (xm', _, _) = (xu0, su1, sh0)
                                               newX = max xV (xU + d)
                                           in (Map.insert v newX xm', su1, sh0)
                      nextW = Map.findWithDefault w w aln
                  in if nextW == v then stc' else loop nextW stc'
            in loop v st1
      inf = 1 / 0 :: Double
      finalX v =
        let r   = Map.findWithDefault v v root
            xr  = Map.findWithDefault 0 r xF
            sk  = Map.findWithDefault r r sinkF
            sh  = Map.findWithDefault inf sk shiftF
        in if sh < inf then xr + sh else xr
  in Map.fromList [ (v, finalX v) | v <- allV ]

-- ===========================================================================
-- Step 5 (Phase 1 A6): Plate (= cluster) 制約
--   median sweep 後の OrderMap を post-process し、 同 plate に属する node が
--   同 rank 内で連続するように再並べ替える。 各 plate の median 位置を維持して
--   並べ替えるため、 crossing 増加を最小化する。
--
--   nested plate: 渡された 'plates' 順を尊重し、 外側 → 内側 の順で適用する
--   (= 最初に外側が contiguous 化、 次に内側がさらに細かく contiguous 化)。
--   多重所属 (= 1 node が 2 plate に属する) は spec §10.4 で禁止、
--   plates 中で 後の plate が優先 (= 先のは無視) される簡略処理。
-- ===========================================================================

-- | 各 rank で plate メンバを contiguous にする。 plate id は plates 順の index
-- (= 後の plate が優先、 = nested plate の内側を後ろに置く運用を想定)。
applyPlateConstraints :: [[Text]] -> OrderMap -> OrderMap
applyPlateConstraints [] om = om
applyPlateConstraints plates om =
  let -- 各 node の plate id (= plates index、 後の plate が優先)
      plateOf = foldl' (\m (pid, ns) ->
                          foldl' (\mm v -> Map.insert v pid mm) m ns)
                       Map.empty
                       (zip [0 :: Int ..] plates)
      regroup order =
        let n = length order
            tagged = [ (i, v, Map.findWithDefault (negate (n + i + 1)) v plateOf)
                     | (i, v) <- zip [0 ..] order ]
            -- 各 plate id の median 位置 (= 元順序内での中央 index)
            byPid = Map.fromListWith (<>)
                      [ (pid, [i]) | (i, _, pid) <- tagged ]
            medianOfPid pid =
              let ps = sort (Map.findWithDefault [] pid byPid)
                  k  = length ps
              in if k == 0 then 0
                 else fromIntegral (ps !! (k `div` 2)) :: Double
            -- primary key: plate median 位置、 secondary: 元 index
            cmp (i1, _, p1) (i2, _, p2) =
              compare (medianOfPid p1) (medianOfPid p2) <> compare i1 i2
            sorted = sortBy cmp tagged
        in [ v | (_, v, _) <- sorted ]
  in Map.map regroup om

-- | 1 方向 sweep。 'topDown' True なら上 rank の median を anchor、 False なら下 rank。
-- 同 rank 内では A3 の order を尊重しつつ最小間隔を保証する。
-- 隣接ペアの少なくとも片方が dummy node なら spacing を 'dummyMinSpacing' に縮める
-- (= dummy は描画されないので chain node と近接させて長 edge spline の遠回りを抑える)。
computeOneDir :: Bool -> LayoutGraph -> OrderMap -> Map Text Double
computeOneDir topDown lg om =
  let ranks = sort (Map.keys om)
      sweep = if topDown then ranks else reverse ranks
      adjOf v td =
        if td then [ leFrom e | e <- lgEdges lg, leTo   e == v ]
              else [ leTo   e | e <- lgEdges lg, leFrom e == v ]
      dummySet = Set.fromList [ lnId n | n <- lgNodes lg, lnDummy n ]
      isDum v = Set.member v dummySet
      spacingFor a b = if isDum a || isDum b
                         then dummyMinSpacing
                         else minSpacing
      step acc r =
        let here = Map.findWithDefault [] r om
            wantOf i v =
              let nbs = adjOf v topDown
                  pxs = sort [ x | nb <- nbs, Just x <- [Map.lookup nb acc] ]
                  n = length pxs
              in if n == 0
                   then fromIntegral (i :: Int) * minSpacing
                   else pxs !! (n `div` 2)  -- floor median
            withWant = [ (v, wantOf i v) | (i, v) <- zip [0 ..] here ]
            packL []                     = []
            packL ((v0, x0) : rest)      =
              (v0, x0) : packR x0 v0 rest
            packR _    _     []                = []
            packR prev prevV ((v, x) : xs)     =
              let x' = max x (prev + spacingFor prevV v)
              in (v, x') : packR x' v xs
            packed = packL withWant
        in foldl' (\m (v, x) -> Map.insert v x m) acc packed
      minSpacing      = 1.0 :: Double
      dummyMinSpacing = 0.4 :: Double  -- dummy が絡む隣接は近接許可
  in foldl' step Map.empty sweep

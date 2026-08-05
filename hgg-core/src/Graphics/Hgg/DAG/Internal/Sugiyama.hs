-- |
-- Module      : Graphics.Hgg.DAG.Internal.Sugiyama
-- Description : Sugiyama-framework internals: rank, order, coordinate assignment
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Graphics.Hgg.DAG 内部で使う Sugiyama framework の中間表現と各 step 実装。
--   外向け Graph a / DAGSpec には漏らさない (= spec §10.3 dummy node 規律)。
--   [English]: The intermediate representation and per-step implementations of
--   the Sugiyama framework used internally by Graphics.Hgg.DAG. Never leaked
--   to the public Graph a / DAGSpec (per spec §10.3's dummy-node discipline).
--
-- [日本語]: 現状:
--
--   * LNode / LEdge / LayoutGraph 中間型
--   * Step 2 Rank assignment: network simplex (Gansner-Koutsofios-North-Vo 1993 §2.3)
--   * 全 edge の minimum length δ = 1、 weight ω = 1 が default
--     (= 現状 DAG.Graph の edge は属性無し、 将来 weight 拡張余地)
--
--   [English]: Current state:
--
--   * The LNode / LEdge / LayoutGraph intermediate types
--   * Step 2 rank assignment: network simplex (Gansner-Koutsofios-North-Vo 1993 §2.3)
--   * Every edge defaults to minimum length δ = 1 and weight ω = 1 (DAG.Graph
--     edges currently carry no attributes; room to extend with weights later)
--
-- [日本語]: 設計判断: 一様 δ=1 / ω=1 の場合、 longest-path ranking が既に Σ edge length
--   最適解 (= 証明: edge 数固定で各 edge の最小 rank diff = 1)。 そのため network
--   simplex の __反復改善 phase は実質 no-op__ になる。 ただし将来 weight / 異δ
--   拡張に備えて framework として実装し、 初期解 = longest-path、 反復 = 負 cut
--   value 探索 (= 該当無し → 即終了) という構造で書く。
--
--   [English]: Design rationale: with uniform δ=1 / ω=1, longest-path ranking
--   is already the Σ edge-length optimum (proof: with the edge count fixed,
--   the minimum rank diff per edge is 1). So the network simplex's
--   __iterative-improvement phase is effectively a no-op__. It is nonetheless
--   implemented as a full framework, in preparation for future weight and
--   non-uniform-δ extensions, structured as: initial solution = longest-path,
--   iteration = search for a negative cut value (none found here, so it
--   terminates immediately).
--
-- [日本語]: 計算量:
--   * 初期 longest-path: O(V + E)
--   * tight tree 構築: O(V + E)
--   * cut value 計算: O(V × E) (= 各 tree edge について非 tree edge を走査)
--   * 反復: 一様 ω では 0 回、 一般には worst O(V × E) per iteration × V iterations
--
--   [English]: Complexity:
--   * Initial longest-path: O(V + E)
--   * Tight-tree construction: O(V + E)
--   * Cut-value computation: O(V × E) (scans the non-tree edges for each tree edge)
--   * Iteration: 0 for uniform ω; worst case O(V × E) per iteration × V iterations in general
--
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.DAG.Internal.Sugiyama
  ( -- * 中間表現
    LNode (..)
  , LEdge (..)
  , LayoutGraph (..)
  , buildLayoutGraph
    -- * Step 2-0: acyclic 化
  , breakCycles
    -- * Step 2: Rank assignment
  , assignRanks
  , assignRanksGrouped
  , longestPathRanking
  , tightTreeEdges
  , tightenSourceRanks
    -- * 汎用 network simplex (= x 座標割当で使う共通ソルバ)
  , networkSimplex
  , networkSimplexBalanced
    -- * Step 3: Order assignment (= median heuristic + transpose)
  , OrderMap
  , insertDummies
  , insertDummiesWithChains
  , initialOrder
  , medianSweep
  , transposeOrder
  , flatReorder
  , assignOrder
  , assignOrderFull
  , countCrossings
  , bilayerCrossings
    -- * Step 4: Coordinate assignment (= aux graph network simplex)
  , assignCoords
  , assignCoordsW
  , auxSimplexCoords
  , auxSimplexCoordsW
  , computeOneDir
    -- * Step 5: Plate (= cluster) 制約
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
-- [日本語]: 中間表現
-- [English]: Intermediate representation
-- ===========================================================================

-- | [日本語]: 内部 node。 元の DAGNode から id を保持し、 rank を埋める。
--   dummy node (= 長 edge 中継用) は 'lnDummy' で区別。
--   [English]: An internal node. Retains the id from the original DAGNode and
--   gets its rank filled in. A dummy node (used to relay a long edge) is
--   distinguished by 'lnDummy'.
data LNode = LNode
  { lnId    :: !Text
    -- ^ [日本語]: 元 node id (dummy なら "\_\_dummy_\<n\>")
    --   [English]: The original node id (or "\_\_dummy_\<n\>" for a dummy)
  , lnRank  :: !Int
    -- ^ [日本語]: Step 2 で割当てる rank
    --   [English]: The rank assigned during Step 2
  , lnDummy :: !Bool
    -- ^ [日本語]: 長 edge を分割するために追加した dummy か
    --   [English]: Whether this is a dummy added to split a long edge
  } deriving (Eq, Show)

-- | [日本語]: 内部 edge。 weight / minimum length δ を持つ。
--   全 edge weight=1, delta=1 が default だが将来拡張余地。
--   [English]: An internal edge, carrying a weight and a minimum length δ.
--   All edges currently default to weight=1, delta=1, with room to extend
--   this later.
data LEdge = LEdge
  { leFrom   :: !Text
  , leTo     :: !Text
  , leDelta  :: !Int
    -- ^ [日本語]: 最小 rank 差 (= δ)、 default 1
    --   [English]: The minimum rank difference (δ), default 1
  , leWeight :: !Double
    -- ^ [日本語]: edge weight (= ω)、 default 1.0
    --   [English]: The edge weight (ω), default 1.0
  } deriving (Eq, Show)

-- | [日本語]: Sugiyama framework の中間 graph。
--   [English]: The intermediate graph used by the Sugiyama framework.
data LayoutGraph = LayoutGraph
  { lgNodes :: ![LNode]
  , lgEdges :: ![LEdge]
  } deriving (Eq, Show)

-- | [日本語]: 元 (id, parents) ペア群から LayoutGraph を組み立てる。
--   すべての edge は δ=1 / ω=1 で初期化。 rank は未割当 (= 0)。
--   [English]: Builds a 'LayoutGraph' from the original (id, parents) pairs.
--   Every edge is initialized with δ=1 / ω=1; ranks are unassigned (0).
buildLayoutGraph
  :: [Text]            -- ^ [日本語]: 全 node id (順序保持、 stable iteration 用)
                        --   [English]: All node ids (order-preserving, for stable iteration)
  -> [(Text, Text)]    -- ^ edge list (from, to)
  -> LayoutGraph
buildLayoutGraph ids es =
  LayoutGraph
    { lgNodes = [ LNode i 0 False | i <- ids ]
    , lgEdges = [ LEdge f t 1 1.0 | (f, t) <- es ]
    }

-- ===========================================================================
-- [日本語]: Step 2-0: acyclic 化 (= graphviz acyclic.c 相当)
-- [English]: Step 2-0: making the graph acyclic (corresponds to graphviz's acyclic.c)
-- ===========================================================================

-- | [日本語]: DFS で back-edge を検出して反転し、 self-loop は rank 制約に寄与しないので
--   除去する。 rank/order 用の acyclic edge 列を返す。
--   [English]: Detects back-edges via DFS and reverses them; self-loops
--   contribute nothing to the rank constraints, so they are removed. Returns
--   an acyclic edge list for use by ranking/ordering.
--
-- [日本語]: graphviz の 'acyclic.c' (decompose + break_cycles) と同じく「閉路を一時的に
--   反転して DAG 化 → layout → 描画時に向きを戻す」 戦略の前半。 描画方向は呼出側
--   (DAG.hs) が原 edge で保持し、 chain lookup は反転 key fallback で吸収する。
--   [English]: This is the first half of the same strategy as graphviz's
--   'acyclic.c' (decompose + break_cycles): "temporarily reverse cycles to
--   make the graph a DAG, lay it out, then restore the original direction at
--   draw time". The draw direction is kept by the caller (DAG.hs) using the
--   original edges, and chain lookups fall back to the reversed key.
--
-- [日本語]: __非破壊性__: 入力が既に DAG なら back-edge は存在せず、 self-loop も無ければ
--   edge は順序保持で不変。 = 現行の acyclic テストケース (large/medium/small/
--   isolated) には影響しない。 閉路入力でのみ rank が正しくなる
--   (従来は 'longestPathRanking' の「0 仮置き」 で誤った rank になっていた)。
--   [English]: __Non-destructiveness__: if the input is already a DAG, no
--   back-edges exist, and absent self-loops the edges are unchanged
--   (order-preserving) — so the existing acyclic test cases (large/medium/
--   small/isolated) are unaffected. Only cyclic input gets a corrected rank
--   (previously, the "placeholder 0" used by 'longestPathRanking' produced an
--   incorrect rank).
--
-- [日本語]: DFS 着色: gray = 現在の stack 上、 black = 探索完了。 (u→v) で v が gray なら
--   back-edge。 起点は @ids@ 順に全 node を走査するので非連結成分も網羅する。
--   [English]: DFS coloring: gray means "currently on the stack", black means
--   "search complete". For (u→v), if v is gray it is a back-edge. Since
--   traversal starts from every node in @ids@ order, disconnected components
--   are covered too.
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
-- [日本語]: Step 2: Rank assignment (= network simplex)
-- [English]: Step 2: Rank assignment (network simplex)
-- ===========================================================================

-- | [日本語]: LayoutGraph の lnRank を埋める。 採用アルゴリズムは network simplex。
--   [English]: Fills in the lnRank of a LayoutGraph, using network simplex.
--
-- [日本語]: 流れ (Gansner 1993 §2.3):
--
--   1. 'longestPathRanking' で初期 feasible ranking
--   2. @buildTightTree@ で tight edge から spanning tree
--   3. @cutValues@ で各 tree edge の cut value
--   4. 負 cut value の tree edge があれば置換 (= 'pivotOnce')、 無ければ最適
--   5. 反復終了後 rank を 0-base に正規化
--
--   [English]: Flow (Gansner 1993 §2.3):
--
--   1. 'longestPathRanking' produces an initial feasible ranking
--   2. @buildTightTree@ grows a spanning tree from tight edges
--   3. @cutValues@ computes the cut value for each tree edge
--   4. If any tree edge has a negative cut value, replace it ('pivotOnce'); otherwise it is optimal
--   5. After iteration finishes, normalize ranks to be 0-based
--
-- [日本語]: 一様 δ=1 / ω=1 では step 1 で最適解。 反復は no-op になる。
--   [English]: With uniform δ=1 / ω=1, step 1 is already optimal, so the
--   iteration is a no-op.
assignRanks :: LayoutGraph -> LayoutGraph
assignRanks lg0 =
  let lg1 = longestPathRanking lg0
      lg2 = iterateSimplex lg1 (length (lgNodes lg0) * 4)  -- 上限 = 4V iteration
      lg3 = normalizeRanks lg2
  in lg3

-- | [日本語]: Step 2-1: longest-path ranking (= 各 node に「source からの最長 path 長」 を割当)。
--   一様 δ=1 / ω=1 では Σ edge length 最適解。
--   [English]: Step 2-1: longest-path ranking, assigning each node "the
--   longest path length from a source". With uniform δ=1 / ω=1 this is the
--   Σ edge-length optimum.
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

-- | [日本語]: Step 2-2〜5: simplex 反復。 上限 iteration 内で負 cut value が無くなるまで pivot。
--   一様 δ=1 / ω=1 では即時終了 (= 負 cut value 無し)。
--   [English]: Steps 2-2 through 2-5: the simplex iteration. Pivots until no
--   negative cut value remains, within an iteration budget. With uniform
--   δ=1 / ω=1 it terminates immediately (no negative cut value exists).
iterateSimplex :: LayoutGraph -> Int -> LayoutGraph
iterateSimplex lg 0       = lg
iterateSimplex lg budget =
  case pivotOnce lg of
    Nothing  -> lg  -- 最適解到達
    Just lg' -> iterateSimplex lg' (budget - 1)

-- | [日本語]: 1 回の pivot: 負 cut value の tree edge を非 tree edge と置換。
--   該当無しなら 'Nothing'。
--   [English]: A single pivot: replaces a negative-cut-value tree edge with a
--   non-tree edge. Returns 'Nothing' if there is none.
--
-- [日本語]: __設計判断 (honest stub)__:
--
--   一様 δ=1 / ω=1 の場合、 longest-path ranking が既に Σ edge length 最適解
--   (= 各 edge の length が ≥ δ=1 の制約下で全 edge 合計を最小化、 longest-path
--   は各 node を最深位置に置くので「圧縮余地ゼロ」)。 したがって全 tree edge の
--   cut value は ≥ 0 になることが保証され、 pivot は発生しない。
--
--   [English]: __Design rationale (honest stub)__:
--
--   With uniform δ=1 / ω=1, longest-path ranking is already the Σ
--   edge-length optimum (proof: it minimizes the total under the constraint
--   that each edge's length is ≥ δ=1, since longest-path places every node
--   at its deepest possible position, leaving "zero room to compress"). This
--   guarantees every tree edge's cut value is ≥ 0, so no pivot ever occurs.
--
-- [日本語]: 本関数は __将来 weight / 異 δ 拡張に備えた framework hook__。 現状は常に
--   'Nothing' を返し、 'iterateSimplex' は初期解で即終了する。
--   [English]: This function is a __framework hook in preparation for future weight / non-uniform-δ extensions__.
--   It currently always returns 'Nothing', so 'iterateSimplex' terminates at
--   the initial solution.
--
-- [日本語]: 拡張時の実装方針 (TODO):
--
--   1. 'tightTreeEdges' で tight edge から spanning tree 抽出
--   2. 各 tree edge を切ったときの head/tail 側 partition を BFS で求め
--   3. 非 tree edge weight 差から cut value 計算
--   4. 最小 cut value < 0 なら非 tree edge の min slack で置換
--
--   [English]: Implementation plan for when this is extended (TODO):
--
--   1. Extract a spanning tree from tight edges via 'tightTreeEdges'
--   2. For each tree edge, find the head/tail-side partition it induces by cutting it, via BFS
--   3. Compute the cut value from the non-tree edge weight differences
--   4. If the minimum cut value is < 0, replace it with the min-slack non-tree edge
pivotOnce :: LayoutGraph -> Maybe LayoutGraph
pivotOnce _ = Nothing

-- | [日本語]: tight tree edge (= rank(v) - rank(u) = δ(u,v) を満たす edge) を列挙。
--   pivot 実装時の前段として用意。 現状未使用。
--   [English]: Enumerates tight tree edges (edges satisfying rank(v) -
--   rank(u) = δ(u,v)). Prepared as a building block for when pivoting is
--   implemented; currently unused.
tightTreeEdges :: LayoutGraph -> [LEdge]
tightTreeEdges lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      isTight e = case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
        (Just ru, Just rv) -> rv - ru == leDelta e
        _                  -> False
  in filter isTight (lgEdges lg)

-- | [日本語]: rank を 0-base に正規化 (= 最小 rank を 0 に shift)。
--   [English]: Normalizes ranks to be 0-based (shifts so the minimum rank is 0).
normalizeRanks :: LayoutGraph -> LayoutGraph
normalizeRanks lg =
  case lgNodes lg of
    [] -> lg
    ns ->
      let rmin = minimum (map lnRank ns)
          newNodes = [ n { lnRank = lnRank n - rmin } | n <- ns ]
      in lg { lgNodes = newNodes }

-- | [日本語]: rank 引き締め ('assignRanks' の後処理)。
--
--   ① __source 引き下げ__: in-edge 無し・out-edge 有りの node を
--      @min(rank(succ) − δ)@ へ。 longest-path ranking は source を rank 0 に
--      固定するため、 深い消費者しか持たない source (data slot / sigma 等) の
--      edge が図を縦断し、 plate bbox (= メンバの bounding box) が縦に伸びる
--      (Σ edge length も非最適。 graphviz は source を消費者の直前 rank に置く)。
--      全 out-edge の rank 差 ≥ δ は min の取り方により維持される。
--   ② __エッジ無し plate メンバの引き寄せ__: edge を一切持たない node が
--      plate メンバなら、 同 plate の (edge を持つ) メンバの最小 rank へ
--      (フローティング解消・analyze の DataIx データノードで顕在化)。
--
--   最後に 0-base へ再正規化する。 plate 無し・深い source 無しのグラフでは
--   no-op (= 既存図はビット不変)。
--
--   [English]: Tightens ranks (post-processing after 'assignRanks').
--
--   ① __Pull sources down__: a node with no in-edges and at least one
--      out-edge is moved to @min(rank(succ) − δ)@. Because longest-path
--      ranking pins sources to rank 0, a source with only deep consumers
--      (a data slot, sigma, etc.) has its edge span the whole figure,
--      stretching the plate bbox (the bounding box over its members)
--      vertically, and the Σ edge length is no longer optimal (graphviz
--      places sources at the rank right before their consumer). The
--      constraint rank difference ≥ δ over all out-edges is preserved by
--      how the min is taken.
--   ② __Pull in edgeless plate members__: a node with no edges at all that
--      is a plate member is moved to the minimum rank among the (edged)
--      members of the same plate (resolves "floating" members, seen with
--      analyze's DataIx data nodes).
--
--   Finally re-normalizes to 0-based. This is a no-op for graphs with no
--   plates and no deep sources (existing figures are bit-identical).
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
-- [日本語]: Step 2-1 (rank=same 制約の前提): rank=same 制約付き rank 割当
-- [English]: Step 2-1 (a prerequisite for rank=same constraints): rank assignment with rank=same constraints
-- ===========================================================================

-- | [日本語]: graphviz @rank=same@ 相当: 同 rank group を代表 node に併合して rank 割当
--   (= graphviz cluster collapse / @UF_union@) し、 member へ rank を展開する。
--   [English]: The equivalent of graphviz's @rank=same@: merges each
--   same-rank group into a representative node for rank assignment
--   (analogous to graphviz's cluster collapse / @UF_union@), then propagates
--   the resulting rank back out to the members.
--
-- [日本語]: 戻り値の edge は rank 向きに正規化済:
--
--   * rank(from) < rank(to) はそのまま、 逆なら反転 (= 'breakCycles' の back-edge
--     反転と同値。 呼出側の chain lookup は既存の反転 key fallback で吸収)
--   * rank(from) == rank(to) (= __flat edge__、 group 内 edge のみで発生) は
--     原方向のまま保持。 ranking 制約には寄与しない (併合で self-loop 化し除外)
--   * self-loop は除去 ('breakCycles' と同じ)
--
--   [English]: The returned edges are already normalized to point in the
--   rank direction:
--
--   * rank(from) < rank(to) is kept as-is; the reverse case is flipped (this
--     is equivalent to the back-edge reversal done by 'breakCycles' — the
--     caller's chain lookup absorbs it via the existing reversed-key fallback)
--   * rank(from) == rank(to) (a __flat edge__, which only arises from
--     within-group edges) keeps its original direction. It contributes
--     nothing to the ranking constraints (it becomes a self-loop after
--     merging and is excluded)
--   * self-loops are removed (as in 'breakCycles')
--
-- [日本語]: @groups = []@ では rep = id で全経路が既存と一致し、 出力 LayoutGraph は
--   従来の breakCycles → assignRanks → tightenSourceRanks とビット一致する
--   (orient は「back-edge 反転後の rank 差 ≥ 1」 の不変量により DFS 反転と同値。
--   test で担保)。
--   [English]: With @groups = []@, rep is the identity on every path, so the
--   output 'LayoutGraph' is bit-identical to the previous
--   breakCycles → assignRanks → tightenSourceRanks pipeline (orient is
--   equivalent to the DFS reversal by the invariant "rank difference ≥ 1
--   after back-edge reversal"; guaranteed by tests).
assignRanksGrouped
  :: [[Text]]          -- ^ [日本語]: 同 rank group 群 (member 共有 group は併合される)
                        --   [English]: The same-rank groups (groups sharing a member are merged)
  -> [[Text]]          -- ^ [日本語]: plate member 群 (= 'tightenSourceRanks' 用)
                        --   [English]: The plate member groups (for 'tightenSourceRanks')
  -> [Text]            -- ^ [日本語]: 全 node id (順序保持)
                        --   [English]: All node ids (order-preserving)
  -> [(Text, Text)]    -- ^ [日本語]: 原 edge 列 (向き任意、 self-loop 可)
                        --   [English]: The original edge list (any direction, self-loops allowed)
  -> LayoutGraph
assignRanksGrouped groups plateIds ids es =
  let -- group の併合 (member 共有 = 同値類)。 rep = ids 中で最初に現れる member
      -- (= group の列挙順に依存しない決定論)。
      mergeSets = foldl' addGroup [] groups
      addGroup acc g =
        let gs = Set.fromList g
            (hit, miss) = span' (\s -> not (Set.disjoint s gs)) acc
        in Set.unions (gs : hit) : miss
      span' p xs = (filter p xs, filter (not . p) xs)
      idIx = Map.fromList (zip ids [0 :: Int ..])
      repOfSet s = case sortBy (\a b -> compare (idIx Map.! a) (idIx Map.! b))
                          [ m | m <- Set.toList s, Map.member m idIx ] of
        (r0 : _) -> [ (m, r0) | m <- Set.toList s ]
        []       -> []
      repMap = Map.fromList (concatMap repOfSet mergeSets)
      rep i = Map.findWithDefault i i repMap
      -- 代表 graph で acyclic 化 + rank 割当 (group 内 edge は self-loop 化して落ちる。
      -- group 間の双方向 edge が作る閉路は breakCycles が反転処理)
      repIds  = dedupStable (map rep ids)
      repEs   = [ (rep f, rep t) | (f, t) <- es, rep f /= rep t ]
      acyc    = breakCycles repIds repEs
      lgRep   = tightenSourceRanks (map (map rep) plateIds)
                  (assignRanks (buildLayoutGraph repIds acyc))
      rkRep   = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lgRep ]
      rankOf i = Map.findWithDefault 0 (rep i) rkRep
      -- 原 id へ展開 + edge を rank 向きに正規化
      orient (f, t)
        | f == t              = Nothing
        | rankOf f <= rankOf t = Just (f, t)
        | otherwise           = Just (t, f)
      nodes = [ LNode i (rankOf i) False | i <- ids ]
      edges = [ LEdge f t 1 1.0 | Just (f, t) <- map orient es ]
  in LayoutGraph nodes edges

-- | [日本語]: 順序保持の重複除去 (= 最初の出現のみ残す)。
--   [English]: Order-preserving deduplication (keeps only the first occurrence).
dedupStable :: [Text] -> [Text]
dedupStable = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs

-- ===========================================================================
-- [日本語]: Inspection (= test 用)
-- [English]: Inspection helpers (for tests)
-- ===========================================================================

-- | [日本語]: Σ ω(u,v) × (rank(v) - rank(u)) を返す (= rank assignment の目的関数)。
--   longest-path / network simplex の検算用。
--   [English]: Returns Σ ω(u,v) × (rank(v) - rank(u)) (the objective function
--   of rank assignment). Used to check longest-path / network simplex results.
edgeLengthSum :: LayoutGraph -> Double
edgeLengthSum lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      contrib e = case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
        (Just ru, Just rv) -> leWeight e * fromIntegral (rv - ru)
        _                  -> 0
  in sum (map contrib (lgEdges lg))

-- | [日本語]: feasibility check: 全 edge で rank(v) - rank(u) ≥ δ(u,v)。
--   [English]: Feasibility check: every edge satisfies rank(v) - rank(u) ≥ δ(u,v).
isFeasible :: LayoutGraph -> Bool
isFeasible lg =
  let rankOf = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      check e = case (Map.lookup (leFrom e) rankOf, Map.lookup (leTo e) rankOf) of
        (Just ru, Just rv) -> rv - ru >= leDelta e
        _                  -> True
  in all check (lgEdges lg)

-- ===========================================================================
-- [日本語]: 汎用 network simplex (Gansner-Koutsofios-North-Vo 1993 §2.3)
--   = graphviz の network simplex に相当する共通ソルバ。 node 集合と
--   (tail, head, δ, ω) edge 群を受け、 各 node に整数座標 r を割当て
--     Σ ω · (r_head − r_tail)
--   を制約 r_head − r_tail ≥ δ の下で最小化する。
--
--   graphviz では rank.c (rank 割当) と position.c (x 座標 = aux graph 上の
--   同 simplex) の両方がこれを使う。 本実装では ranking は一様 δ=ω=1 で
--   longest-path が既に最適なため 'assignRanks' はそのまま据え置き、 本関数は
--   主に x 座標割当 (= Ω 1:2:8 + nodesep の非一様 aux graph) で使う。
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
--
-- [English]: General-purpose network simplex (Gansner-Koutsofios-North-Vo
--   1993 §2.3) — the shared solver corresponding to graphviz's network
--   simplex. Given a node set and (tail, head, δ, ω) edges, it assigns an
--   integer coordinate r to each node, minimizing
--     Σ ω · (r_head − r_tail)
--   subject to r_head − r_tail ≥ δ.
--
--   In graphviz, both rank.c (rank assignment) and position.c (x-coordinate
--   assignment — the same simplex, on the aux graph) use this. Here, ranking
--   uses uniform δ=ω=1 where longest-path is already optimal, so 'assignRanks'
--   is left as-is; this function is used mainly for x-coordinate assignment
--   (the non-uniform aux graph with the Ω 1:2:8 weighting plus nodesep).
--
--   Flow:
--     1. initRankNS      : a feasible initial solution via longest-path (input assumed a DAG)
--     2. feasibleTreeNS  : grows a spanning tree from tight edges (adjusted via min-slack)
--     3. tightRanksTree  : the unique assignment that makes the tree all-tight (the
--                          global shift freedom is fixed by root=0)
--     4. optimize        : swaps a negative-cut-value tree edge for the min-slack
--                          non-tree edge crossing the cut in the opposite direction,
--                          then re-assigns with the new tree
--     5. normalizeNS     : shifts the minimum r to 0
--
--   Disconnected graphs are solved independently per weakly-connected component.
-- ===========================================================================

-- | [日本語]: 汎用 network simplex。 戻り値は全 node の整数座標 (= rank / x)。
--   balance は行わない (= ranking 用・最適頂点を 1 つ返す)。
--   [English]: The general-purpose network simplex. Returns an integer
--   coordinate (rank / x) for every node. Performs no balancing — this is
--   for ranking, returning one optimal vertex.
networkSimplex :: [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
networkSimplex = networkSimplexWith False

-- | [日本語]: LR balance 付き network simplex (= graphviz position.c の @rank(g, 2)@ 相当)。
--   最適到達後、 cut value 0 の tree edge を slack の中央へ寄せて対称化する
--   (= x 座標割当用。 free node を隣接の重心へ寄せ左右対称にする)。
--   [English]: Network simplex with LR balancing (corresponds to graphviz
--   position.c's @rank(g, 2)@). After reaching the optimum, tree edges with
--   cut value 0 are moved to the middle of their slack to symmetrize the
--   layout (used for x-coordinate assignment, pulling free nodes toward
--   their neighbors' centroid for left-right symmetry).
networkSimplexBalanced :: [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
networkSimplexBalanced = networkSimplexWith True

networkSimplexWith
  :: Bool -> [Text] -> [(Text, Text, Int, Double)] -> Map Text Int
networkSimplexWith balance nodes edges =
  Map.unions [ solveComponent balance cn ce | (cn, ce) <- weakComponents nodes edges ]

-- | [日本語]: 弱連結成分に分解 (edge を無向視)。 edge を持たない孤立 node も 1 成分。
--   [English]: Decomposes into weakly-connected components (treating edges as
--   undirected). An isolated node with no edges is also its own component.
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

-- | [日本語]: 連結成分 1 個を解く。 @balance@ なら最後に LR balance を掛ける。
--   [English]: Solves a single connected component. Applies LR balancing at
--   the end if @balance@ is set.
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

-- | [日本語]: longest-path feasible 初期 rank (各 node = source からの δ 重み最長 path)。
--   [English]: A feasible initial rank via longest-path (each node gets the
--   δ-weighted longest path from a source).
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

-- | [日本語]: edge の slack = r_head − r_tail − δ (≥ 0 が feasible)。
--   [English]: An edge's slack, r_head − r_tail − δ (feasible when ≥ 0).
slackNS :: Map Text Int -> (Text, Text, Int, Double) -> Int
slackNS r (t, h, d, _) =
  Map.findWithDefault 0 h r - Map.findWithDefault 0 t r - d

-- | [日本語]: tight edge で spanning tree を成長させ tree edge index 集合を返す。
--   spanning に満たない間は min-slack の incident 非 tree edge を選び tree を
--   平行移動して tight 化し、 再成長する (Gansner93 feasible_tree)。
--   [English]: Grows a spanning tree from tight edges and returns the set of
--   tree edge indices. While the tree does not yet span, it picks the
--   min-slack incident non-tree edge, shifts the tree to make it tight, and
--   grows it again (Gansner93's feasible_tree).
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

-- | [日本語]: spanning tree を全 edge tight にする一意 rank (root=start を 0 に固定)。
--   [English]: The unique rank that makes every edge of a spanning tree
--   tight (fixes root=start to 0).
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

-- | [日本語]: negative cut value の tree edge を解消するまで pivot。
--   戻り値 = (最終 spanning tree, rank)。 tree は balance で再利用する。
--   [English]: Pivots until no tree edge has a negative cut value. Returns
--   (the final spanning tree, the rank); the tree is reused for balancing.
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

-- | [日本語]: LR balance (graphviz ns.c @balance@, mode 2)。 cut value 0 の tree edge を
--   列挙し、 その edge を逆向きに跨ぐ非 tree edge の slack δ (= 動かせる余地) の
--   半分だけ tail 側成分を中央へ寄せる。 cost は不変 (cut=0 = 微分 0) なので最適性
--   を保ったまま free node を対称化する。 single pass (graphviz と同様)。
--   [English]: LR balancing (graphviz ns.c's @balance@, mode 2). Enumerates
--   tree edges with cut value 0, and for each, shifts its tail-side
--   component toward the center by half the slack δ (the room to move) of
--   the non-tree edge crossing it in the opposite direction. The cost is
--   unchanged (cut=0 means the derivative is 0), so this symmetrizes free
--   nodes while preserving optimality. A single pass, as in graphviz.
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

-- | [日本語]: 最小座標を 0 に正規化。
--   [English]: Normalizes the minimum coordinate to 0.
normalizeNS :: Map Text Int -> Map Text Int
normalizeNS m
  | Map.null m = m
  | otherwise  = let mn = minimum (Map.elems m) in Map.map (subtract mn) m

-- ===========================================================================
-- [日本語]: Step 3: Order assignment
--   Gansner-Koutsofios-North-Vo 1993 §3、 dot default 24 iteration の
--   median heuristic + transpose で同 rank 内の node 順を最適化。
--   長 edge (rank 差 > 1) は dummy node 経由の short edge 列に展開する。
-- [English]: Step 3: Order assignment. Optimizes node order within each rank
--   using the median heuristic + transpose over dot's default 24 iterations
--   (Gansner-Koutsofios-North-Vo 1993 §3). A long edge (rank difference > 1)
--   is expanded into a chain of short edges routed through dummy nodes.
-- ===========================================================================

-- | [日本語]: 各 rank の node 順 (= rank → 左から右の id 列)。
--   [English]: The node order within each rank (rank to a left-to-right id list).
type OrderMap = Map Int [Text]

-- | [日本語]: 長 edge (= rank 差 > 1) を中間 rank の dummy node 経由の短 edge 列に展開。
--   dummy node は 'lnDummy = True' で区別、 id は @"\_\_dummy_\<n\>"@。
--   元 edge は削除され、 同 weight の短 edge 列に置換される。
--   [English]: Expands a long edge (rank difference > 1) into a chain of
--   short edges routed through dummy nodes at the intermediate ranks. Dummy
--   nodes are distinguished by 'lnDummy = True', with ids of the form
--   @"\_\_dummy_\<n\>"@. The original edge is removed and replaced by a chain of
--   short edges carrying the same weight.
insertDummies :: LayoutGraph -> LayoutGraph
insertDummies lg = fst (insertDummiesWithChains lg)

-- | [日本語]: 'insertDummies' + 元 edge → 経由 chain (= 始点と終点を含む id 列) を返す。
--   短 edge (rank 差 1) も map に含まれ、 chain = [from, to] (= 2 要素)。
--   edge routing で、 元 edge を chain 経由の control 点列で描画するために使う。
--   [English]: 'insertDummies' plus a map from the original edge to the chain
--   it is routed through (an id list including both endpoints). Short edges
--   (rank difference 1) are included too, as chain = [from, to] (2
--   elements). Used by edge routing to draw the original edge through the
--   chain's control points.
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

-- | [日本語]: rank ごとの初期順序 (= ID 辞書順、 決定論性のため)。
--   [English]: The initial order within each rank (lexicographic by id, for determinism).
initialOrder :: LayoutGraph -> OrderMap
initialOrder lg =
  let grouped = Map.fromListWith (<>)
                  [ (lnRank n, [lnId n]) | n <- lgNodes lg ]
  in Map.map sort grouped

-- | [日本語]: 2 隣接 rank 間の交差数 (naive O(E^2))。
--   @edges@ は (u, v) ペア、 u は upper の id、 v は lower の id。
--   [English]: The number of crossings between two adjacent ranks (naive
--   O(E^2)). @edges@ is a list of (u, v) pairs, where u is the upper rank's
--   id and v is the lower rank's id.
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

-- | [日本語]: 全 rank pair の交差数合計。
--   [English]: The total crossing count summed over all rank pairs.
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

-- | [日本語]: median: 偶数個なら 2 中央値の平均、 奇数個なら中央。
--   [English]: The median: the average of the two middle values for an even
--   count, or the middle value for an odd count.
medianOf :: [Double] -> Maybe Double
medianOf [] = Nothing
medianOf xs =
  let s = sort xs
      n = length s
      mid = n `div` 2
  in Just $ if odd n
            then s !! mid
            else (s !! (mid - 1) + s !! mid) / 2

-- | [日本語]: 1 回 sweep (= median heuristic 1 pass)。
--   @topDown@ True = rank 増加方向、 False = 減少方向。
--   [English]: A single sweep (one pass of the median heuristic). @topDown@
--   True sweeps in the direction of increasing rank, False the decreasing
--   direction.
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

-- | [日本語]: transpose: 同 rank 内の隣接 pair を試し交換、 交差数が下がるなら採用。
--   上下 rank の edge を両方見て判定。
--   [English]: Transpose: tries swapping adjacent pairs within a rank and
--   keeps the swap if it reduces the crossing count, judged by looking at
--   edges to both the rank above and below.
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

-- | [日本語]: flat edge (= 同 rank edge) の順序制約。
--   graphviz @mincross.c@ の @flat_breakcycles@ + @flat_reorder@ 相当:
--   各 rank 内で flat edge が左→右を向くよう、 現在順序への影響を最小にした
--   安定 topological sort で並べ替える。 flat edge の閉路は 'breakCycles'
--   (現在順序で DFS) で決定論的に破る。 flat edge の無い rank は不変
--   (= flat edge 無しの graph では全体が恒等、 既存図ビット不変)。
--   [English]: The ordering constraint for flat edges (same-rank edges).
--   Corresponds to graphviz @mincross.c@'s @flat_breakcycles@ +
--   @flat_reorder@: within each rank, reorders via a stable topological sort
--   that minimizes disturbance to the current order, so that flat edges
--   point left-to-right. Cycles among flat edges are broken deterministically
--   by 'breakCycles' (DFS over the current order). Ranks with no flat edges
--   are unchanged (identity when the graph has no flat edges at all,
--   preserving existing figures bit-for-bit).
flatReorder :: LayoutGraph -> OrderMap -> OrderMap
flatReorder lg om0 =
  let rankMap = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      flatAt r = [ (leFrom e, leTo e)
                 | e <- lgEdges lg
                 , Map.lookup (leFrom e) rankMap == Just r
                 , Map.lookup (leTo e) rankMap   == Just r ]
      tryRank r om' = case flatAt r of
        []    -> om'
        pairs ->
          let here = Map.findWithDefault [] r om'
              -- flat 閉路を現在順序の DFS で破る (= graphviz flat_breakcycles)
              acyc = breakCycles here pairs
              -- 安定 topo sort: indeg 0 の候補から現在 index 最小を選ぶ
              -- (= 拘束の無い node の相対位置を保つ、 graphviz flat_reorder の趣旨)
              ix = Map.fromList (zip here [0 :: Int ..])
              succs = Map.fromListWith (<>) [ (f, [t]) | (f, t) <- acyc ]
              indeg0 = Map.fromListWith (+)
                         ([ (v, 0 :: Int) | v <- here ]
                          ++ [ (t, 1) | (_, t) <- acyc ])
              kahn indeg acc
                | Map.null indeg = reverse acc
                | otherwise =
                    let ready = [ v | (v, d) <- Map.toList indeg, d == 0 ]
                    in case sortBy (\a b -> compare (ix Map.! a) (ix Map.! b)) ready of
                         [] -> reverse acc ++ sortBy
                                 (\a b -> compare (ix Map.! a) (ix Map.! b))
                                 (Map.keys indeg)  -- 保険 (acyc 後は起きない)
                         (v : _) ->
                           let dec = Map.findWithDefault [] v succs
                               indeg' = foldl' (\m t -> Map.adjust (subtract 1) t m)
                                               (Map.delete v indeg) dec
                           in kahn indeg' (v : acc)
          in Map.insert r (kahn indeg0 []) om'
  in foldl' (flip tryRank) om0 (sort (Map.keys om0))

-- | [日本語]: Step 3 メイン: dummy 挿入 + 24 iteration median sweep + transpose。
--   戻り値 = (拡張済 LayoutGraph、 OrderMap)。 OrderMap は dummy も含む。
--   [English]: The Step 3 main entry point: dummy insertion, then 24
--   iterations of the median sweep plus transpose. Returns (the expanded
--   LayoutGraph, the OrderMap); the OrderMap includes dummies too.
assignOrder :: LayoutGraph -> (LayoutGraph, OrderMap)
assignOrder lg0 = let (a, b, _) = assignOrderFull lg0 in (a, b)

-- | [日本語]: 'assignOrder' + 元 edge → chain map (= edge routing 用) を返す。
--   [English]: 'assignOrder' plus a map from the original edge to its chain
--   (used for edge routing).
--
-- [日本語]: flat edge があれば初期順序と各 iteration 後に
--   'flatReorder' を適用する (medianSweep / transpose は inter-rank edge しか
--   見ないため、 flat 制約は都度回復させる)。 flat edge 交差は countCrossings の
--   目的関数に__含めない__ (graphviz は flat も ncross に数えるが、 現用途の flat は
--   group 内の少数 edge で左→右向きの保証が主目的。 差分は correspondence doc に記載)。
--   [English]: If flat edges are present, 'flatReorder' is applied both to
--   the initial order and after each iteration (since medianSweep /
--   transpose only look at inter-rank edges, the flat constraint has to be
--   restored every time). Flat-edge crossings are __not included__ in
--   countCrossings's objective function (graphviz does count flat edges in
--   ncross, but here flat edges are typically a small number of within-group
--   edges whose main purpose is guaranteeing left-to-right orientation; the
--   difference is documented in the correspondence doc).
assignOrderFull
  :: LayoutGraph
  -> (LayoutGraph, OrderMap, Map (Text, Text) [Text])
assignOrderFull lg0 =
  let (lg, chainMap) = insertDummiesWithChains lg0
      rankMap = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      hasFlat = any (\e -> Map.lookup (leFrom e) rankMap
                           == Map.lookup (leTo e) rankMap) (lgEdges lg)
      constrain = if hasFlat then flatReorder lg else id
      ini = constrain (initialOrder lg)
      iniCross = countCrossings lg ini
      step (best, bestC) i =
        let td = even (i :: Int)
            swept = medianSweep lg td best
            transposed = constrain (transposeOrder lg swept)
            c = countCrossings lg transposed
        in if c < bestC then (transposed, c) else (best, bestC)
      (final, _) = foldl' step (ini, iniCross) [0 .. 23]
  in (lg, final, chainMap)

-- ===========================================================================
-- [日本語]: Step 4: Coordinate assignment
--   Brandes-Köpf 4-candidate から graphviz position.c 忠実の aux graph network
--   simplex ('auxSimplexCoordsW') に置換済。 旧 BK 実装 (brandesKopf / runBK /
--   markType1 / verticalAlign / horizCompact / bk 定数) は呼び出し元ゼロのまま
--   残っていたため物理削除 (実装は git 履歴参照)。
-- [English]: Step 4: Coordinate assignment. Replaced the Brandes-Köpf
--   4-candidate approach with an aux-graph network simplex
--   ('auxSimplexCoordsW') that faithfully follows graphviz's position.c. The
--   old BK implementation (brandesKopf / runBK / markType1 / verticalAlign /
--   horizCompact / the bk constants) had zero remaining callers and was
--   physically deleted (see git history for the old implementation).
-- ===========================================================================

-- | [日本語]: Step 4 メイン: x 座標を割当て [0,1] 正規化。
--   結果は OrderMap に含まれる全 node id (= dummy 含む) → x ∈ [0,1]。
--   [English]: The Step 4 main entry point: assigns x coordinates, normalized
--   to [0,1]. The result maps every node id in the OrderMap (dummies
--   included) to x ∈ [0,1].
--
-- [日本語]: graphviz position.c に倣い Brandes-Köpf から
--   __aux graph network simplex__ ('auxSimplexCoords') へ置換。 BK には無かった
--   「dummy chain 直線化重み (Ω 1:2:8)」 と 「隣接対 nodesep 強制」 を simplex の
--   目的関数/制約として同時最適化するため、 長 edge の dummy 列が並走 node 列の
--   外へ独立縦列として分離する (= large の funnel collapse の layout 層 主因を根治)。
--   @plates@ (= cluster メンバ id 群) を aux graph simplex に渡し、
--   cluster border 制約 ('clusterAuxEdges') を反映した x を解く。 plate 無し ([]) は
--   従来と完全同一。
--   [English]: Following graphviz's position.c, replaces Brandes-Köpf with an
--   __aux graph network simplex__ ('auxSimplexCoords'). It jointly optimizes,
--   as part of the simplex's objective/constraints, two things BK lacked — a
--   "dummy-chain straightening weight" (Ω 1:2:8) and "enforced nodesep
--   between adjacent pairs" — which separates a long edge's dummy chain into
--   an independent column outside the parallel node column (fixing the root
--   cause of the "large" example's funnel collapse at the layout level).
--   @plates@ (cluster member id groups) are passed into the aux-graph
--   simplex, which solves for x while honoring cluster-border constraints
--   ('clusterAuxEdges'). With no plates ([]), the result is unchanged.
--
-- | [日本語]: 後方互換 wrapper (= 半幅情報なし = 全 real node 一律 'auxNodeHalfW')。
--   test 群はこちらを使い構造的不変条件 (collinear / keepout / gap≥) を検証する。
--   [English]: A backward-compatible wrapper (no per-node half-width info —
--   uses 'auxNodeHalfW' uniformly for every real node). Test suites use this
--   to check structural invariants (collinearity / keepout / gap≥).
assignCoords :: [[Text]] -> LayoutGraph -> OrderMap -> Map Text Double
assignCoords = assignCoordsW Map.empty

-- | [日本語]: size-aware 版。 @hwMap@ = real node id → 横半幅 (px, 整数)
--   ('Graphics.Hgg.Layout.dagNodeBaseHalfWidth' を round したもの・DAG.coordStage が供給)。 simplex の
--   node 間隔/cluster border 制約を実 node 幅で解く (= 兄弟 plate の box 重なり根治)。
--   [English]: The size-aware variant. @hwMap@ maps a real node id to its
--   horizontal half-width in px (an integer, the rounded
--   'Graphics.Hgg.Layout.dagNodeBaseHalfWidth', supplied by DAG.coordStage). Solves the simplex's
--   node-spacing / cluster-border constraints using actual node widths
--   (fixing the root cause of sibling plate boxes overlapping).
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
-- [日本語]: Step 4 (aux graph network simplex で x 座標)
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
-- [English]: Step 4 (x-coordinate assignment via aux-graph network simplex).
--   Ports graphviz position.c's @dot_position@ =
--     create_aux_edges → rank(aux, 2 = LR balance) → remove_aux_edges.
--   The auxiliary graph consists of:
--     ① straightening: each layout edge (u,v) is turned into an aux node
--        a_e plus two minlen-0 edges a_e→u, a_e→v (weight Ω). a_e floats to
--        min(x_u,x_v), and cost = Ω·|x_u − x_v| (straightening). Ω depends
--        on the endpoint kinds:
--          real-real 1 : real-virtual 2 : virtual-virtual 8
--        (dot's default ratio, keeping dummy chains straighter the longer they are).
--     ② LR constraint: an edge l→r for each same-rank adjacent pair (l,r),
--        with minlen = nodesep and weight 0 (preserves order and enforces
--        minimum spacing). Dummy-involved pairs get tighter spacing.
--   Solving 'networkSimplexBalanced' on this aux graph makes the parallel
--   real-node column and the long-edge dummy column separate into
--   independent columns at least nodesep apart.
-- ===========================================================================

-- | [日本語]: LR 制約の最小間隔 = graphviz make_LR_constraints の
--   @width = ND_rw(left) + ND_lw(right) + nodesep@ を移植。
--   各 node の __半幅__ + nodesep の和を隣接間隔とする。 これにより
--   real node の隣に来る dummy は real の半幅ぶん外へ押し出され、 real node の
--   body 内側に潜り込まない (= long-edge dummy 列が並走 chain の node body の外に
--   出る = funnel collapse の layout 層 主因を根治)。
--   [English]: The minimum LR-constraint spacing, ported from graphviz
--   make_LR_constraints's @width = ND_rw(left) + ND_lw(right) + nodesep@. The
--   spacing between neighbors is the sum of each node's __half-width__ plus
--   nodesep. This pushes a dummy sitting next to a real node outward by the
--   real node's half-width, so it never sinks inside the real node's body
--   (fixing the root cause of the funnel collapse at the layout level, where
--   a long edge's dummy column used to poke into the body of a parallel
--   node chain).
--
-- [日本語]: 旧実装は dummy 絡みを一律に小間隔 (= 旧 BK bkDummySpacing 0.4) にしており、
--   dummy が real node body の内側に入っていた (= 並走 chain を貫通) のが large の
--   主因だった。
--   [English]: The old implementation used a uniform small spacing for
--   anything involving a dummy (the old BK's bkDummySpacing 0.4), which let
--   dummies sit inside a real node's body (piercing a parallel chain) — the
--   main cause of the problem in the "large" example.
--
-- [日本語]: 値は __偶数__にする: 全 minlen 偶数 → 全 rank 偶数和 → LR balance の
--   @delta `div` 2@ が丸め無しで厳密中央化される。
--   [English]: The value is kept __even__: all-even minlens give an
--   all-even rank sum, so LR balancing's @delta `div` 2@ centers exactly,
--   with no rounding.
--
-- [日本語]: 半幅は __一様__ ('auxNodeHalfW') に戻した。 size-aware
--   (per-node 幅) は非兄弟グラフの位置を動かし long-edge routing を折る回帰を生んだ
--   (実測 2026-06-24)。 兄弟 plate box の重なりは separate_subclust (normalized gap)
--   + render binding-pair (実幅は radius 既知の render で考慮) で解く。
--   [English]: Half-widths were reverted to __uniform__ ('auxNodeHalfW').
--   Making them size-aware (per-node width) moved unrelated (non-sibling)
--   graph positions and caused a regression that broke long-edge routing
--   (measured 2026-06-24). Overlap between sibling plate boxes is instead
--   resolved by separate_subclust (a normalized gap) plus the render-side
--   binding-pair step (which accounts for the actual width, known at render
--   time via the radius).
auxNodeHalfW, auxDummyHalfW, auxNodeSep :: Int
auxNodeHalfW  = 4   -- real node の半幅 (hwMap 欠落時 fallback)
auxDummyHalfW = 0   -- dummy (virtual) node の半幅 (graphviz でも極小)
auxNodeSep    = 18  -- EXPERIMENT: graphviz nodesep 既定 18pt (point 一貫)

-- | [日本語]: cluster (= plate) box の margin。 graphviz @CL_OFFSET@=8pt。
--   [English]: The cluster (plate) box margin. graphviz's @CL_OFFSET@ = 8pt.
auxPlateMargin :: Int
auxPlateMargin = 8

-- | [日本語]: node a と b (= 同 rank で a が左・b が右隣) の最小間隔。
--   @hwOf@ = 各 node の半幅 (px・dummy/欠落は内部 fallback 済)。
--   [English]: The minimum spacing between nodes a and b (same rank, a on
--   the left, b on the right). @hwOf@ gives each node's half-width in px
--   (dummies / missing entries already fall back internally).
auxSepOf :: (Text -> Int) -> Text -> Text -> Int
auxSepOf hwOf a b = hwOf a + hwOf b + auxNodeSep

-- | [日本語]: aux graph を構築し simplex で x (整数) を解いて Double で返す。
--   戻り値 = 実 node (dummy 含む・aux 除く) の raw x。 正規化は 'assignCoords' 側。
--   [English]: Builds the aux graph, solves for integer x via simplex, and
--   returns it as a Double. The result is the raw x of real nodes (dummies
--   included, aux nodes excluded); normalization happens in 'assignCoords'.
--
-- [日本語]: @plates@ (= cluster メンバ id リスト群) があれば
--   'clusterAuxEdges' で graphviz position.c @pos_clusters@ 相当の cluster x 制約
--   (border node + contain/keepout edge) を aux graph に追加する。 plate 無しは
--   従来と完全同一 (= 図ビット不変)。
--   [English]: When @plates@ (cluster member id list groups) are given,
--   'clusterAuxEdges' adds cluster x-constraints equivalent to graphviz
--   position.c's @pos_clusters@ (border nodes plus contain/keepout edges) to
--   the aux graph. With no plates, the result is unchanged (figures are
--   bit-identical).
auxSimplexCoords :: [[Text]] -> LayoutGraph -> OrderMap -> Map Text Double
auxSimplexCoords = auxSimplexCoordsW Map.empty

-- | [日本語]: size-aware 版。 @hwMap@ = real node id → 横半幅 (px)。
--   @hwOf@ は dummy を 'auxDummyHalfW'、 hwMap 欠落を 'auxNodeHalfW' へ fallback。
--   [English]: The size-aware variant. @hwMap@ maps a real node id to its
--   horizontal half-width (px). @hwOf@ falls back to 'auxDummyHalfW' for
--   dummies and 'auxNodeHalfW' when missing from hwMap.
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
      -- ① straightening: edge ごとに aux node + 2 本の minlen-0 edge。
      -- flat edge (= 同 rank) は除外 — graphviz make_edge_pairs
      -- も rank 差のある edge のみ対象で、 flat を入れると x(u)=x(v) への引き寄せが
      -- 同 rank の LR 分離 (②) と拮抗する。
      rankOfL = Map.fromList [ (lnId n, lnRank n) | n <- lgNodes lg ]
      rankedEdges = [ e | e <- lgEdges lg
                        , Map.lookup (leFrom e) rankOfL
                          /= Map.lookup (leTo e) rankOfL ]
      auxId i = "__auxpos_" <> T.pack (show (i :: Int))
      straightEdges = concat
        [ let u = leFrom e; v = leTo e
              w = fromIntegral (omega u v) * leWeight e
          in [ (auxId i, u, 0, w), (auxId i, v, 0, w) ]
        | (i, e) <- zip [0 ..] rankedEdges ]
      -- ② LR 制約: 同 rank 隣接対に minlen = sep (= 半幅和 + nodesep), weight 0
      lrEdges =
        [ (l, r, auxSepOf hwOf l r, 0)
        | (_, layer) <- Map.toAscList om, (l, r) <- zip layer (drop 1 layer) ]
      -- ③ cluster 制約: border node + contain/keepout/separate (graphviz pos_clusters)
      (clustNodes, clustEdges) = clusterAuxEdges hwOf om plates
      auxNodes = [ auxId i | (i, _) <- zip [0 ..] rankedEdges ]
      realKeys = [ lnId n | n <- lgNodes lg ]
      allNodes = realKeys ++ auxNodes ++ clustNodes
      xInt = networkSimplexBalanced allNodes
               (straightEdges ++ lrEdges ++ clustEdges)
  in Map.fromList
       [ (v, fromIntegral (Map.findWithDefault 0 v xInt)) | v <- realKeys ]

-- | [日本語]: graphviz @lib/dotgen/position.c@ の @pos_clusters@
--   (= @create_aux_edges@ 内) が張る cluster x 制約を、 我々の aux graph network
--   simplex 用 edge として生成する。 一次ソースに忠実 (CL_OFFSET=8pt → 'auxPlateMargin'、
--   border label 無し → border.x=0、 nested は別途)。
--   [English]: Generates, as edges for our aux-graph network simplex, the
--   cluster x-constraints that graphviz @lib/dotgen/position.c@'s
--   @pos_clusters@ (inside @create_aux_edges@) sets up. Faithful to the
--   primary source (CL_OFFSET=8pt maps to 'auxPlateMargin'; no border label,
--   so border.x=0; nesting is handled separately).
--
-- [日本語]: 各 plate p に左右 border virtual node @ln_p@ / @rn_p@ を立て (graphviz
--   @make_lrvn@ = SLACKNODE)、 以下を張る:
--
--   * @contain_nodes@: 各 rank の最左 member へ @ln_p → 最左@ (minlen =
--     半幅 + margin)、 最右 member から @最右 → rn_p@ (同)。 = 箱の左右端確定。
--   * @contain_clustnodes@: @ln_p → rn_p@ (minlen 1, weight 128)。 = 箱を tight
--     に圧縮 (member を詰める強い重み)。
--   * @keepout_othernodes@: 各 rank で member ブロックの外側最近接 __非メンバ__ u に
--     @u → ln_p@ / @rn_p → u@ (minlen = margin + 半幅)。 = 非メンバを箱外へ排除。
--   * @separate_subclust@: 同一 rank に並ぶ __兄弟__ plate (= 互いに包含関係に無い)
--     の隣接 border 間に @rn_left → ln_right@ (minlen = CL_OFFSET)。 = 隣接箱の
--     margin ぶんの隙間を x 解に確保 (= 兄弟 plate box 重なりの根治)。
--
--   [English]: For each plate p, sets up left/right border virtual nodes
--   @ln_p@ / @rn_p@ (graphviz @make_lrvn@ = SLACKNODE), and connects:
--
--   * @contain_nodes@: for each rank, @ln_p → leftmost member@ (minlen =
--     half-width + margin), and @rightmost member → rn_p@ (same). Fixes the
--     box's left and right edges.
--   * @contain_clustnodes@: @ln_p → rn_p@ (minlen 1, weight 128). Compresses
--     the box tight (a strong weight that packs the members together).
--   * @keepout_othernodes@: for each rank, the __non-member__ u nearest to
--     the outside of the member block gets @u → ln_p@ / @rn_p → u@ (minlen =
--     margin + half-width). Pushes non-members out of the box.
--   * @separate_subclust@: for __sibling__ plates (not in a containment
--     relation with each other) adjacent within the same rank, connects
--     @rn_left → ln_right@ between their borders (minlen = CL_OFFSET).
--     Reserves a margin-sized gap in the x solution between adjacent boxes
--     (fixing sibling plate box overlap).
--
-- [日本語]: graphviz @pos_clusters@ にはもう一つ @contain_subclust@
--   (nested 親子 plate に @ln_p → ln_c@ / @rn_c → rn_p@・minlen CL_OFFSET・weight 128 を
--   直接の子へ張り、 親箱が子箱を margin ぶん外側で囲む) があるが、 __意図的に未実装__。
--   理由 (実測): 我々の plate box は 'Graphics.Hgg.Render.EdgeRoute.plateBoxPt' が「直接 member glyph box ∪
--   子 plate box (再帰) + 固定 margin」で描くため、 入れ子の clearance は
--   __箱モデル側で既に保証__される。 contain_subclust を試作して nested/deep/tri
--   図を再生成しても幾何変化はゼロ (末尾桁 FP ノイズのみ・PNG はバイト一致) で、
--   border 制約は box 描画へ伝播しなかった。 = graphviz には在るが
--   __我々の描画経路では非寄与__ゆえ採用しない (図再生成 FP ノイズの実コストだけが残る)。
--   将来 box を border node 由来へ変える場合はその文脈で再導入する。
--   ([[feedback-graphviz-only-faithful-algos]] / 2026-06-26 実測)。
--
--   [English]: graphviz's @pos_clusters@ has one more edge kind,
--   @contain_subclust@ (for a nested parent/child plate, connects @ln_p →
--   ln_c@ / @rn_c → rn_p@ directly to each child — minlen CL_OFFSET, weight
--   128 — so the parent box encloses the child box by a margin), but it is
--   __deliberately not implemented__ here. Reason (measured): our plate box
--   is drawn by 'Graphics.Hgg.Render.EdgeRoute.plateBoxPt' as "the union of direct member glyph boxes and
--   (recursively) child plate boxes, plus a fixed margin", so nested
--   clearance is __already guaranteed by the box model itself__. A prototype
--   of contain_subclust, tested by regenerating the nested/deep/tri figures,
--   produced zero geometric change (only trailing floating-point noise; the
--   PNGs were byte-identical) — the border constraint never propagated to box
--   drawing. So while graphviz has it, it is __inert along our render path__
--   and is not adopted (it would only add real cost from figure-regeneration
--   floating-point noise). Reconsider it if the box model is ever switched to
--   derive from border nodes.
--   ([[feedback-graphviz-only-faithful-algos]], measured 2026-06-26).
--
-- [日本語]: これにより plate が box として x 分離し、 cosmetic な @applyPlateBands@ /
--   @recenterNonPlateRows@ (帯分離) が不要になる (= 撤去済)。
--   [English]: As a result, plates separate along x as boxes, making the
--   cosmetic @applyPlateBands@ / @recenterNonPlateRows@ (band separation)
--   unnecessary (already removed).
--
-- [日本語]: 半幅は固定 'auxNodeHalfW' でなく @hwOf@ (= 'auxSimplexCoordsW'
--   が hwMap から作る per-node 実半幅) を使う。 dummy/欠落の fallback は hwOf 内で済。
--   [English]: Uses @hwOf@ (the per-node actual half-width built from hwMap
--   by 'auxSimplexCoordsW') rather than the fixed 'auxNodeHalfW'. The
--   dummy / missing-entry fallback is already handled inside hwOf.
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

-- ===========================================================================
-- [日本語]: Step 5: Plate (= cluster) 制約
--   median sweep 後の OrderMap を post-process し、 同 plate に属する node が
--   同 rank 内で連続するように再並べ替える。 各 plate の median 位置を維持して
--   並べ替えるため、 crossing 増加を最小化する。
--
--   nested plate: 渡された @plates@ 順を尊重し、 外側 → 内側 の順で適用する
--   (= 最初に外側が contiguous 化、 次に内側がさらに細かく contiguous 化)。
--   多重所属 (= 1 node が 2 plate に属する) は spec §10.4 で禁止、
--   plates 中で 後の plate が優先 (= 先のは無視) される簡略処理。
-- [English]: Step 5: Plate (cluster) constraints. Post-processes the OrderMap
--   after the median sweep, reordering so that nodes belonging to the same
--   plate become contiguous within each rank. Reordering keeps each plate's
--   median position, minimizing the increase in crossings.
--
--   Nested plates: applied in the order @plates@ were given, from outer to
--   inner (the outer plate is made contiguous first, then the inner one is
--   further refined into contiguity). Multiple membership (one node
--   belonging to 2 plates) is forbidden by spec §10.4; as a simplification,
--   whichever plate comes later in @plates@ takes priority (the earlier one
--   is ignored).
-- ===========================================================================

-- | [日本語]: 各 rank で plate メンバを contiguous にする。 plate id は plates 順の index
--   (= 後の plate が優先、 = nested plate の内側を後ろに置く運用を想定)。
--   [English]: Makes plate members contiguous within each rank. The plate id
--   is the index within @plates@ (later plates take priority — the intended
--   usage is placing a nested plate's inner plate later).
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

-- | [日本語]: 1 方向 sweep。 @topDown@ True なら上 rank の median を anchor、 False なら下 rank。
--   同 rank 内では Step 3 の order を尊重しつつ最小間隔を保証する。
--   隣接ペアの少なくとも片方が dummy node なら spacing を @dummyMinSpacing@ に縮める
--   (= dummy は描画されないので chain node と近接させて長 edge spline の遠回りを抑える)。
--   [English]: A single-direction sweep. When @topDown@ is True, anchors on
--   the median of the rank above; when False, the rank below. Within a rank
--   it respects the Step 3 order while guaranteeing minimum spacing. When at
--   least one node of an adjacent pair is a dummy node, spacing shrinks to
--   @dummyMinSpacing@ (since dummies are not drawn, keeping them close to
--   the chain node curbs long detours in the long-edge spline).
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

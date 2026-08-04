-- |
-- Module      : Graphics.Hgg.Render.EdgeRoute
-- Description : Pt-space routing geometry for DAG edges (obstacle avoidance, ports, control points)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: routing を描画 (Render.Special) から分離した純幾何 module。
--   pt 空間で toScreen・radius・plate bbox (障害物) を受け、 edge の制御点列と
--   描画 style ('EdgeRoute') を返す。 Primitive 生成や ThemePalette には依存しない
--   (= 描画は呼出側 'Graphics.Hgg.Render.Special.renderEdge' の責務)。 段階型と対になる「routing 入力契約」。
--   [English]: A pure geometry module that separates edge routing from
--   rendering (Render.Special). Given toScreen, the radius, and plate bounding
--   boxes (obstacles) in pt space, it returns an edge's control point sequence
--   and its drawing style ('EdgeRoute'). It has no dependency on Primitive
--   generation or ThemePalette (drawing is the responsibility of the caller
--   'Graphics.Hgg.Render.Special.renderEdge'). It is the "routing input contract" that pairs with the
--   staged types.
--
-- [日本語]: node 形状幾何 ('nodeExtent' / 'edgePortPoint') も routing が依存するため本 module に
--   置き、 描画側 (renderNode 等) は本 module から import する (= 下位 = 幾何、
--   上位 = 描画 の層分け)。
--   [English]: Node shape geometry ('nodeExtent' / 'edgePortPoint') is also
--   kept in this module because routing depends on it, and the drawing side
--   (renderNode and friends) imports it from here (lower layer = geometry,
--   upper layer = drawing).
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Render.EdgeRoute
  ( -- * routing 結果
    EdgeRoute (..)
  , routeEdge
  , spreadPorts
    -- * 障害物モデル (A-1)
  , Box (..)
  , Obstacles (..)
  , dagObstacles
  , plateBoxPt
  , plateChildrenOf
    -- * channel + funnel (A-2 / A-3)
  , buildChannel
  , funnel
    -- * box 拘束 Bézier fit (R3 = graphviz Proutespline)
  , proutespline
  , solve3
    -- * node 形状幾何 (port 交点)
  , edgePortPoint
  , nodeExtent
  , nodeShowsDist
  , dagLabelFs
  ) where

import           Graphics.Hgg.Layout (dagLabelFs, dagNodeBaseHalfWidth)
import           Graphics.Hgg.Primitive     (Point (..))
import           Graphics.Hgg.Spec   (DAGEdge (..), DAGNode (..),
                                      DAGNodeKind (..), DAGPlate (..))
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)

-- | [日本語]: edge routing の結果 = 制御点列 + 描画 style。 ThemePalette/Primitive 非依存。
--
--     * 'StraightArrow' = 単独 short edge (直線 + 矢印)
--     * 'SplinePath'    = 並列 short / 長 edge 非迂回 (Catmull-Rom・呼出側で平滑化)
--     * 'BezierPath'    = 長 edge の plate box 迂回 (箱角 waypoint を平滑化せず通す)
--     * 'CubicPath'     = R3 (Step6 P7a): graphviz Proutespline の box 拘束 cubic Bézier
--                         fit。 先頭 = 始点、 以後 3 点ずつ (制御点1, 制御点2, 終点) の
--                         cubic segment 列。
--   [English]: The result of edge routing: a control point sequence plus a
--   drawing style. Independent of ThemePalette/Primitive.
--
--     * 'StraightArrow' — a lone short edge (a straight line plus an arrowhead)
--     * 'SplinePath'    — parallel short edges, or long edges with no
--                         detour (Catmull-Rom, smoothed by the caller)
--     * 'BezierPath'    — a long edge detouring around a plate box (box-corner
--                         waypoints are passed through without smoothing)
--     * 'CubicPath'     — R3 (Step6 P7a): a box-constrained cubic Bezier fit
--                         from graphviz's Proutespline. The first point is the
--                         start point, followed by cubic segments in groups of
--                         three (control point 1, control point 2, end point).
data EdgeRoute
  = StraightArrow Point Point
  | SplinePath [Point]
  | BezierPath [Point]
  | CubicPath [Point]
  deriving (Show, Eq)

-- | [日本語]: edge の制御点列と style を pt 空間で決定する純関数 (= 'Graphics.Hgg.Render.Special.renderEdge' から routing 部を抽出)。
--   並列 edge は perpendicular に offset、 長 edge は graphviz routesplines:
--   障害物 ('Obstacles') から box-channel を作り (A-2)、 funnel 最短折れ線 (A-3) を通す。
--   [English]: A pure function that determines an edge's control point
--   sequence and style in pt space (the routing portion extracted from
--   'Graphics.Hgg.Render.Special.renderEdge'). Parallel edges get a perpendicular offset; long edges
--   follow graphviz's routesplines approach: a box-channel is built from the
--   obstacles ('Obstacles') (A-2), and the shortest path is routed through it
--   with the funnel algorithm (A-3).
routeEdge
  :: (Double -> Double -> Point)
  -> Obstacles                            -- ^ [日本語]: A-1: node + plate 障害物 (pt 空間)
                                           --   [English]: A-1: node and plate obstacles (pt space)
  -> DAGNode -> DAGNode -> Maybe [(Double, Double)]
  -> Double
  -> Int -> Int  -- ^ parIx, parCount
  -> EdgeRoute
routeEdge toScreen obs from to mPath radius parIx parCount =
  let fromCenter = toScreen (dnX from) (dnY from)
      toCenter   = toScreen (dnX to)   (dnY to)
      Point fx fy = fromCenter
      Point tx ty = toCenter
      edx = tx - fx; edy = ty - fy
      elen = sqrt (edx * edx + edy * edy)
      (perpx, perpy) = if elen > 1e-12 then (-edy / elen, edx / elen) else (0, 0)
      offsetMag =
        if parCount > 1
          then (fromIntegral parIx - fromIntegral (parCount - 1) / 2)
               * (radius * 1.4)
          else 0
      offsetP (Point px py) =
        Point (px + perpx * offsetMag) (py + perpy * offsetMag)
  in case mPath of
       Just [_, wp, _] | abs (fy - ty) < 1e-9 ->
         -- P7b 最小 (Phase 53 A3-4): flat edge (= 同 rank edge) の迂回 arc。
         -- chain は [from, gap-waypoint, to] の 3 点 (DAG.routeStage 産)。
         -- 'buildChannel' / 'funnel' は y 単調な rank 下り chain 前提で、 flat の
         -- V 字 guide を潰して中間 node を貫通する (実測: flat5 a→b が m 中心を
         -- 通過) ため通さず、 graphviz make_flat_edge と同じく rank 間 gap を
         -- 通る arc を直接作る。 並列 flat edge は waypoint ごと perpendicular
         -- offset で分離。
         let wpP = offsetP (let (wx, wy) = wp in toScreen wx wy)
             fromPortS = edgePortPoint from fromCenter wpP radius
             toPortS   = edgePortPoint to   toCenter   wpP radius
         in SplinePath [fromPortS, wpP, toPortS]
       Just chain | length chain >= 2 ->
         -- A-2/A-3: dummy chain を guide に box-channel を作り funnel で最短折れ線へ。
         -- 内部制御点は並列 spread 用に perpendicular bend を掛けてから guide にする。
         let inner    = [ toScreen x y | (x, y) <- take (length chain - 2) (drop 1 chain) ]
             innerOff = map offsetP inner
             guide0   = fromCenter : innerOff ++ [toCenter]
             -- A-1: この edge が避ける障害物 (= 端点を含む箱は除外)。
             eboxes   = edgeBoxes obs from to fromCenter toCenter
             -- ★ Phase 52 A7: guide (dummy lane) が box を貫くと 'buildChannel' の
             -- portal が行ごとに反対側へ飛び (pushOut の側 flip)、 taut が box 内部を
             -- 対角に横切って node に重なって描画される (実測: dense15 x4→x15 が
             -- x13 box を貫通)。 guide 段階で貫通線分に迂回 corner を挿入して
             -- channel を一貫させる (= graphviz P4a が dummy を box 外へ置くのと同層)。
             -- taut 段階の補正だと taut が barrier 上を沿走して fit が直角に縮退する。
             -- dedup 6pt は corner と旧 waypoint の近接 backtrack 掃除。
             guide    = dedupTaut 6.0 (avoidBoxTaut eboxes guide0)
             portals  = buildChannel eboxes guide
             taut     = funnel portals          -- src .. snk (端点含む taut 折れ線)
             interior = drop 1 (initSafe taut)  -- 端点を除く taut waypoint
             -- A-5 (暫定): port は taut 経路の端 segment 方向へ取る (自然入射)。
             firstDir = case interior of (p:_) -> p; [] -> toCenter
             lastDir  = case reverse interior of (p:_) -> p; [] -> fromCenter
             fromPortS = edgePortPoint from fromCenter firstDir radius
             toPortS   = edgePortPoint to   toCenter   lastDir  radius
             -- R3 (Step6 P7a): taut 折れ線 (port→port) に box 拘束 cubic Bézier を
             -- graphviz Proutespline で fit。 channel 境界 = portal の左鎖 + 右鎖。
             tautPorts = fromPortS : interior ++ [toPortS]
             barriers  = channelBarriers portals
             -- ★ A3.3 (2026-06-24): endpoint slope は taut の端 segment 方向 (= 自然な
             -- port 入射方向) を渡す。graphviz の `±π/2 constrained` は **内部の
             -- box-segment 境界** に適用される拘束であり、**実端点 (src/snk port) の接線は
             -- port 方向 (斜め)**。一次実測: dot 14.1.5 の `edge src snk` gold は始点
             -- `(1.9076,2.3025)→(1.9989,2.2455)` = 接線 (+0.091,-0.057) で水平寄りの斜め
             -- (垂直でない)。A3.1 は端点も垂直と誤って拘束し、A3.2 の y-sweep で taut が
             -- 4 点クリーン化した後は **斜め近接 + 強制垂直の衝突で overshoot (内側 S)** を
             -- 生んでいた。自然方向に戻すと S が消え単一 bow に (A3.1 の cusp も y-sweep で
             -- taut が V でなくなったため再発しない)。
             ev0 = case tautPorts of
                     (a:b:_) -> vnorm (vsub b a)
                     _       -> Point 0 1
             ev1 = case reverse tautPorts of
                     (a:b:_) -> vnorm (vsub a b)
                     _       -> Point 0 1
             ctrl   = proutespline barriers tautPorts ev0 ev1
         in CubicPath ctrl
       _ | parCount > 1 ->
         -- 並列 short edge: 中点 bend 1 点を挟む 3 点 spline で「並ぶ曲線」 を出す
         let midRaw  = Point ((fx + tx) / 2) ((fy + ty) / 2)
             midBent = offsetP midRaw
             fromPortS = edgePortPoint from fromCenter midBent radius
             toPortS   = edgePortPoint to   toCenter   midBent radius
             pts       = [fromPortS, midBent, toPortS]
         in SplinePath pts
       _ ->
         -- 単独 short edge: 従来の直線 + 矢印
         let fromPort = edgePortPoint from fromCenter toCenter radius
             toPort   = edgePortPoint to   toCenter   fromCenter radius
         in StraightArrow fromPort toPort

initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

-- ===========================================================================
-- A-1: 障害物モデル (pt 空間の軸並行矩形)
-- ===========================================================================

-- | [日本語]: pt 空間の軸並行矩形 (xlo ≤ xhi, ylo ≤ yhi)。 routing の障害物 / channel box 共用。
--   [English]: An axis-aligned rectangle in pt space (xlo <= xhi, ylo <= yhi),
--   shared by routing obstacles and channel boxes.
data Box = Box !Double !Double !Double !Double  -- ^ xlo ylo xhi yhi
  deriving (Show, Eq)

-- | [日本語]: routing 用障害物集合。 node glyph box (= id 付き・端点除外用) と plate box を分けて保持。
--
--   'obLanes' = 各 edge の dummy lane box 列 ((from, to) key 付き)。
--   graphviz @make_regular_edge@ の per-edge boxes は「rank order 上の左右隣接
--   オブジェクト (__virtual node 含む__) で clip した回廊」 (@maximal_bbox@)。
--   'buildChannel' の free 区間 clip は既に「最寄り crossing box = 隣接オブジェクト」
--   なので、 他 edge の dummy lane を障害物に足せば channel がそのまま
--   「自レーンの box 回廊」 になる (= 他 edge の dummy レーンに侵入不能)。
--   [English]: The set of obstacles used for routing. Keeps node glyph boxes
--   (with ids, for excluding endpoints) and plate boxes separate.
--
--   'obLanes' is, per edge, the sequence of dummy lane boxes (keyed by
--   (from, to)). graphviz's @make_regular_edge@ per-edge boxes are "the
--   corridor clipped by the rank-order left/right neighboring objects
--   (__including virtual nodes__)" (@maximal_bbox@). Since the free-interval
--   clipping in 'buildChannel' already treats "the nearest crossing box" as
--   "the neighboring object", adding other edges' dummy lanes to the obstacle set
--   turns the channel into "this lane's own box corridor" (it cannot enter
--   another edge's dummy lane).
data Obstacles = Obstacles
  { obNodes  :: [(Text, Box)]
    -- ^ [日本語]: node id → glyph box (clearance margin 込み)
    --   [English]: node id to glyph box (clearance margin included)
  , obPlates :: [Box]
    -- ^ [日本語]: plate 枠 box (clearance margin 込み)
    --   [English]: plate frame box (clearance margin included)
  , obLanes  :: [((Text, Text), [Box])]
    -- ^ [日本語]: edge (from, to) → dummy lane box 列 (chain 内部 waypoint の virtual node box)
    --   [English]: edge (from, to) to its dummy lane box sequence (virtual
    --   node boxes for the chain's interior waypoints)
  } deriving (Show, Eq)

-- | [日本語]: clearance margin (= spline が箱に接しないための余白)。 graphviz: cluster 8pt。
--   node 4→8pt を試したが channel が狭まり分割接合の junction kink (157°/54°) が
--   再発したため 4pt に据え置き (routes CSV + analyze-kinks.py で確認)。 かすり対策は
--   box 辺 barrier (@boxBarriers@) 側で行う。
--   [English]: The clearance margin (the space kept so that splines don't
--   touch a box). graphviz uses 8pt for clusters. Trying 4->8pt for nodes
--   narrowed the channel and reintroduced junction kinks (157/54 degrees) at
--   split joins, so it is kept at 4pt (confirmed with the routes CSV and
--   analyze-kinks.py). Near-miss avoidance is instead handled on the box-edge
--   barrier side (@boxBarriers@).
obNodeMargin, obPlateMargin :: Double
obNodeMargin  = 4
obPlateMargin = 8

-- | [日本語]: 全 node glyph box (+margin) と plate box (+margin) を pt 空間で構築する (A-1)。
--
--   @edges@ から dummy lane box ('obLanes') も構築する。 chain 内部
--   waypoint (= long-edge dummy) ごとに幅 'laneHalfW'、 高さ = その rank の band
--   (同 y の real node の最大 ry) の virtual node box を置く。 flat edge
--   (端点同 rank) の gap waypoint は rank line 上のオブジェクトではないため対象外。
--   [English]: Builds every node glyph box (+margin) and plate box (+margin)
--   in pt space (A-1).
--
--   Also builds dummy lane boxes ('obLanes') from @edges@. For each interior
--   chain waypoint (a long-edge dummy), it places a virtual node box of width
--   'laneHalfW' and height equal to that rank's band (the max ry among real
--   nodes at the same y). A flat edge's (same-rank endpoints) gap waypoint is
--   excluded, since it is not an object on a rank line.
dagObstacles :: (Double -> Double -> Point) -> Double
             -> [DAGNode] -> [(Text, DAGNode)] -> [DAGPlate] -> [DAGEdge]
             -> Obstacles
dagObstacles toScreen radius nodes nodeMap plates edges =
  Obstacles
    { obNodes =
        [ (dnId n, Box (cx - rx - obNodeMargin) (cy - ry - obNodeMargin)
                       (cx + rx + obNodeMargin) (cy + ry + obNodeMargin))
        | n <- nodes
        , let Point cx cy = toScreen (dnX n) (dnY n)
        , let (rx, ry) = nodeExtent n radius ]
    , obPlates =
        [ Box (xlo - obPlateMargin) (ylo - obPlateMargin)
              (xhi + obPlateMargin) (yhi + obPlateMargin)
        | p <- plates
        , Just (xlo, ylo, xhi, yhi) <- [plateBoxPt toScreen radius nodeMap plates p] ]
    , obLanes =
        [ ((deFrom e, deTo e), laneBoxes chain)
        | e <- edges
        , Just chain <- [dePath e]
        , length chain > 2
        , not (isFlatChain chain) ]
    }
  where
    -- rank band の半高: 同 y (= 同 rank line) の real node の最大 ry。 real node の
    -- 無い all-dummy rank は 1 行 node 相当へ fallback。 graphviz の virtual node box
    -- が rank の band を占める (= 交差は rank 間 gap で起きる) のと同層。
    rankHalfH py =
      case [ ry | n <- nodes
                , let Point _ cy = toScreen (dnX n) (dnY n)
                , abs (cy - py) < 1e-6
                , let (_, ry) = nodeExtent n radius ] of
        [] -> max (radius * 0.7) ((dagLabelFs + 3) / 2 + 4)
        rs -> maximum rs
    isFlatChain chain = case (chain, reverse chain) of
      ((_, y0) : _, (_, yn) : _) -> abs (y0 - yn) < 1e-9
      _                          -> False
    laneBoxes chain =
      [ Box (px - laneHalfW) (py - hh) (px + laneHalfW) (py + hh)
      | (x, y) <- take (length chain - 2) (drop 1 chain)
      , let Point px py = toScreen x y
      , let hh = rankHalfH py ]

-- | [日本語]: dummy lane box の半幅 (pt) = nodesep/2 (auxNodeSep 18 の半分)。
--   graphviz @maximal_bbox@ は隣接 virtual node との中点 (= 自 box 右端 + nodesep/2)
--   まで回廊を開くため、 隣接 lane の回廊同士はちょうど tile して重ならない。
--   半幅 9pt の lane 障害物で clip すると同じ境界になる。
--   [English]: Half-width (pt) of a dummy lane box: nodesep/2 (half of
--   auxNodeSep 18). graphviz's @maximal_bbox@ opens the corridor up to the
--   midpoint with the neighboring virtual node (its own box's right edge plus
--   nodesep/2), so adjacent lane corridors tile exactly without overlapping.
--   Clipping with a lane obstacle of half-width 9pt yields the same boundary.
laneHalfW :: Double
laneHalfW = 9

-- | [日本語]: この edge が避けるべき障害物 box 群。 端点 (from/to) の node box と、 端点中心を
--   内側に含む box (= 端点が属する plate 等) は除外する (= edge は正規にそこへ接続する)。
--
--   他 edge の dummy lane box ('obLanes') も避ける = per-edge box 回廊。
--   自 lane と、 同一端点対の並列 edge (chain 共有・perpendicular offset で分離済) の
--   lane は除外する。
--   [English]: The set of obstacle boxes this edge must avoid. Excludes the
--   node boxes of its endpoints (from/to) and any box whose interior contains
--   an endpoint's center (e.g. the plate an endpoint belongs to), since the
--   edge legitimately connects there.
--
--   It also avoids other edges' dummy lane boxes ('obLanes'), giving each
--   edge its own per-edge box corridor. It excludes its own lane and the
--   lanes of parallel edges sharing the same endpoint pair (which share the
--   chain and are already separated by a perpendicular offset).
edgeBoxes :: Obstacles -> DAGNode -> DAGNode -> Point -> Point -> [Box]
edgeBoxes obs from to srcC snkC =
  let nodeB = [ b | (i, b) <- obNodes obs, i /= dnId from, i /= dnId to ]
      laneB = [ b | ((f, t), bs) <- obLanes obs
                  , not (f == dnId from && t == dnId to)
                  , not (f == dnId to && t == dnId from)
                  , b <- bs ]
      allB  = nodeB ++ obPlates obs ++ laneB
  in [ b | b <- allB, not (boxContains b srcC), not (boxContains b snkC) ]

-- | [日本語]: 点が box の interior にあるか (境界は外側扱い)。
--   [English]: Whether a point is in a box's interior (the boundary counts as
--   outside).
boxContains :: Box -> Point -> Bool
boxContains (Box xlo ylo xhi yhi) (Point x y) =
  x > xlo && x < xhi && y > ylo && y < yhi

-- ===========================================================================
-- A-2: box-channel 構築 (guide 折れ線 + 障害物 → portal 列)
-- ===========================================================================

-- | [日本語]: guide 折れ線 (端点含む・y 単調を想定) と障害物から funnel 用 portal 列を作る。
--   各内部 guide 点の y 水平線上で、 guide の x を含む free 区間 (左右最寄り障害物に
--   clip) を求め、 (左点, 右点) の portal に。 端点 (src/snk) は退化 portal として両端に置く。
--   free 区間が退化/逆転したら guide 点をそのまま通す退化 portal にフォールバック
--   (= その点は funnel の強制通過点になる。 'funnel' の退化 portal 扱いを参照)。
--
--   ★ R1 (Step6 P7a・2026-06-24): 片側に障害物が無いときの壁を __graph bbox 端 (有限値)__
--   に clip する (旧: ±Infinity)。graphviz @maximal_bbox@ (dotsplines.c) は隣 node が無ければ
--   cluster/graph 境界へ clip するため壁は常に有限。旧 ±Inf は funnel の @tri@ 外積を
--   Infinity 化して符号崩壊 → 直線 collapse を招いていた (correspondence doc §4-B)。
--
--   ★ R2-fix (2026-06-24): portal を free 区間__全幅__でなく __dummy x まわりの狭い窓__
--   ([gx-w, gx+w] を free 区間で clip) にする。graphviz @maximal_bbox@ は virtual node 自身の
--   細い幅 (lw≈1pt) 基準で box を作るため box は dummy に密着する。旧実装は free 区間全幅を
--   portal にしていたため、端点が片寄ると funnel が dummy lane を無視して chain 寄りへ
--   shortcut し L 字 (角 1 個) になり、R3 の cubic fit が暴走 (bulge) していた。狭い窓に
--   すると funnel が collinear な dummy lane に沿い、graphviz と同じ滑らかな bow になる。
--   [English]: Builds a portal sequence for the funnel algorithm from a guide
--   polyline (includes the endpoints, assumed y-monotone) and the obstacles.
--   At the y of each interior guide point, it finds the free interval
--   containing the guide's x (clipped by the nearest left/right obstacles)
--   and turns it into a (left point, right point) portal. The endpoints
--   (src/snk) are placed at both ends as degenerate portals. If a free
--   interval degenerates or inverts, it falls back to a degenerate portal
--   that simply passes the guide point through (that point then becomes a
--   forced pass-through point for the funnel; see how 'funnel' handles
--   degenerate portals).
--
--   R1 (Step6 P7a, 2026-06-24): when one side has no obstacle, the wall is
--   clipped to __the graph bbox edge (a finite value)__ (previously ±Infinity).
--   graphviz's @maximal_bbox@ (dotsplines.c) always clips to the
--   cluster/graph boundary when there is no neighboring node, so the wall is
--   always finite. The old ±Inf turned the funnel's @tri@ cross product into
--   Infinity, collapsing its sign and causing a degenerate straight-line
--   collapse (correspondence doc §4-B).
--
--   R2-fix (2026-06-24): a portal is now __a narrow window around the dummy's x__
--   ([gx-w, gx+w] clipped by the free interval), not __the full free interval__.
--   graphviz's @maximal_bbox@ builds its box based on the
--   virtual node's own thin width (lw ~= 1pt), so the box hugs the dummy
--   tightly. The old implementation used the full free interval as the
--   portal, so when an endpoint was off-center the funnel would ignore the
--   dummy lane and shortcut toward the chain, producing an L-shape (a single
--   corner) that made R3's cubic fit run away (bulge). With a narrow window,
--   the funnel hugs the collinear dummy lane and produces a smooth bow, just
--   like graphviz.
buildChannel :: [Box] -> [Point] -> [(Point, Point)]
buildChannel boxes guide = case guide of
  []  -> []
  [p] -> [(p, p)]
  (p0@(Point _ y0) : rest) ->
    let pn@(Point _ yn) = last rest
        interiorGys     = [ gy | Point _ gy <- initSafe rest ]   -- dummy lane の y
        -- R1: 有限フォールバック壁。 全 box edge + guide x の外側へ channelMargin だけ
        -- 余白を取った graph bbox 端。 隣 box の無い側はここまで開く (= 拘束なし相当)。
        xsAll = [ x | Box xlo _ xhi _ <- boxes, x <- [xlo, xhi] ]
                ++ [ gx | Point gx _ <- guide ]
        gloX  = minimum xsAll - channelMargin
        ghiX  = maximum xsAll + channelMargin
        -- ★ A3.2 (2026-06-24): graphviz box-stack の rank 境界に相当する portal を
        -- **障害物の上下エッジ y** で張る。矩形障害物の周りでは funnel がその上角・下角を
        -- 2 つの waypoint として丸める (= graphviz Pshortestpath が cluster bbox 角を経由
        -- する忠実構造)。旧実装は dummy の y 1 点でしか portal を張らず waypoint が 1 個 →
        -- taut が単一屈曲 V → Proutespline が splinefits 第一段 (polyLen 短縮) 棄却で dummy
        -- 分割 → 各 2 点 forceflag 直線 → テント角になっていた。box の内側 epsY だけ寄せた
        -- 高さで sample し (strict cross 判定に乗せる)、上下角を確実に waypoint 化する。
        yLo = min y0 yn; yHi = max y0 yn
        boxEdgeYs = concat [ [ylo + epsY, yhi - epsY] | Box _ ylo _ yhi <- boxes ]
        eventYs0  = filter (\y -> y > yLo + epsY && y < yHi - epsY)
                           (boxEdgeYs ++ interiorGys)
        -- sweep 方向に整列・近接重複除去 (src→snk の y 単調順)。
        ascending = yn >= y0
        eventYs   = dedupNear (if ascending then sortAsc eventYs0
                                            else reverse (sortAsc eventYs0))
        mkPortal gy =
          let gx0      = guideXAt guide gy   -- dummy lane の x (側の選択に使う)
              crossing = [ b | b@(Box _ ylo _ yhi) <- boxes, ylo < gy, gy < yhi ]
              -- ★ Phase 44.4: guide x が crossing box の **内部**にあると (= layout が
              -- radius floor のズレで dummy を箱境界の内側に置いた場合)、box の左右どちらの
              -- エッジも「正しい側」の clip 条件 (xhi ≤ gx / xlo ≥ gx) に乗らず free 区間が
              -- 全開 (gloX, ghiX) に退化し、funnel が箱内を通って spline が箱を貫通する
              -- (Phase 39 P8 A2 stopgap 撤去で露呈した回帰)。graphviz `routesplines` /
              -- `maximal_bbox` は channel box を cluster と重ねず必ず外側へ clip するため、
              -- guide を内包する box の **近い辺** へ push し、その外側 free 区間で portal を
              -- 張る (= 箱をハード障害物にして spline 全体を箱外に保つ)。
              pushOut x (Box xlo _ xhi _)
                | xlo < x && x < xhi = if x - xlo <= xhi - x then xlo else xhi
                | otherwise          = x
              gx       = foldl pushOut gx0 crossing
              -- 隣接 box (or graph 端) までの free 区間 (= maximal_bbox の隣接クリップ)。
              freeL = maximum (gloX : [ xhi | Box _ _ xhi _ <- crossing, xhi <= gx ])
              freeR = minimum (ghiX : [ xlo | Box xlo _ _ _ <- crossing, xlo >= gx ])
          in if freeL < freeR
               then (Point freeL gy, Point freeR gy)        -- (左点, 右点)
               else (Point gx gy, Point gx gy)              -- 退化: lane x を強制通過
    in (p0, p0) : map mkPortal eventYs ++ [(pn, pn)]

-- | [日本語]: guide 折れ線 (y 単調を想定) の高さ @y@ における x を線形補間する。
--   box-stack portal の「側」 (どの障害物が左/右か) を決めるのに使う。
--   [English]: Linearly interpolates the x of a guide polyline (assumed
--   y-monotone) at height @y@. Used to decide the "side" of a box-stack
--   portal (which obstacle is left/right).
guideXAt :: [Point] -> Double -> Double
guideXAt pts y = go pts
  where
    go (Point x1 y1 : more@(Point x2 y2 : _))
      | inSeg y y1 y2 =
          if abs (y2 - y1) < 1e-9 then x1
          else x1 + (x2 - x1) * (y - y1) / (y2 - y1)
      | otherwise = go more
    go [Point x _] = x
    go _           = 0
    inSeg t a b = (t >= min a b - 1e-9) && (t <= max a b + 1e-9)

-- | [日本語]: 昇順ソート (挿入ソート・小規模 event 列向け)。
--   [English]: Ascending sort (insertion sort, for small event lists).
sortAsc :: [Double] -> [Double]
sortAsc = foldr ins []
  where
    ins x [] = [x]
    ins x (z : zs) | x <= z    = x : z : zs
                   | otherwise = z : ins x zs

-- | [日本語]: 近接した y を 1 つに畳む (portal の零高さセグメント除け)。
--   [English]: Collapses nearby y values into one (removes zero-height portal
--   segments).
dedupNear :: [Double] -> [Double]
dedupNear [] = []
dedupNear (x : xs) = x : go x xs
  where
    go _ [] = []
    go prev (z : zs) | abs (z - prev) < epsY = go prev zs
                     | otherwise             = z : go z zs

-- | [日本語]: box 内側へ寄せて portal を sample する高さオフセット (pt)。 strict cross 判定に
--   乗せ、box 上端・下端の角を確実に waypoint 化する。 近接 y の畳み込み閾値も兼ねる。
--   [English]: The height offset (pt) used to sample a portal, pulled slightly
--   inside a box. This puts the sample on the strict-cross test, so a box's
--   top and bottom corners are reliably turned into waypoints. Also doubles
--   as the threshold for collapsing nearby y values.
epsY :: Double
epsY = 0.75

-- | [日本語]: R1 フォールバック壁の余白 (pt)。 graph bbox 端からさらに外へ取る隙間。
--   [English]: Margin (pt) for the R1 fallback wall: extra clearance taken
--   further outside the graph bbox edge.
channelMargin :: Double
channelMargin = 16

-- ===========================================================================
-- A-3: funnel (stringpulling) 最短折れ線
-- ===========================================================================

-- | [日本語]: portal 列 ((左点, 右点) の列・先頭=src 末尾=snk の退化 portal) を通る最短折れ線を
--   funnel アルゴリズムで求める。 戻り = src .. snk の折れ線 (端点含む)。
--
--   ★ R2 (Step6 P7a・2026-06-24): graphviz @Pshortestpath@ (shortest.c の三角形分割 +
--   deque funnel + @ccw@) と __数学的に同一__な教科書的 Lee funnel
--   (Mononen "Simple Stupid Funnel Algorithm") に置換。box-stack polygon では三角形分割の
--   対角線 = box 重なり portal なので portal-funnel = 三角形分割 funnel (= 新規アルゴでなく
--   Pshortestpath そのもの)。旧自前 apex-jump funnel は cone 不変条件違反で左右壁を交互
--   往復する zigzag を生んでいた (correspondence doc §4-C)。
--
--   規約: portal.left = 小 x 側 / portal.right = 大 x 側、path は下方向 (y 増加)。
--   'triarea2' は canonical 定義 (bx*ay - ax*by)。right 壁が左へ寄ると triarea2 ≤ 0 で funnel が
--   締まる (手計算検証済)。退化 portal (left==right) は 'vequal' 分岐で素通り。
--   [English]: Finds the shortest polyline through a portal sequence (a list
--   of (left point, right point) pairs, with degenerate portals for src at
--   the head and snk at the tail) using the funnel algorithm. Returns the
--   src .. snk polyline (endpoints included).
--
--   R2 (Step6 P7a, 2026-06-24): replaced with the textbook Lee funnel
--   (Mononen's "Simple Stupid Funnel Algorithm"), __mathematically identical__
--   to graphviz's @Pshortestpath@ (shortest.c's triangulation
--   plus deque funnel plus @ccw@). In a box-stack polygon, a triangulation
--   diagonal is exactly a box-overlap portal, so portal-funnel is
--   triangulation-funnel (not a new algorithm, but Pshortestpath itself). The
--   old hand-rolled apex-jump funnel violated the cone invariant and produced
--   a zigzag that bounced between the left and right walls (correspondence
--   doc §4-C).
--
--   Convention: portal.left is the smaller-x side, portal.right the
--   larger-x side, and the path runs downward (increasing y). 'triarea2' uses
--   the canonical definition (bx*ay - ax*by); when the right wall moves left,
--   triarea2 <= 0 tightens the funnel (verified by hand calculation).
--   Degenerate portals (left == right) pass straight through via the
--   'vequal' branch.
funnel :: [(Point, Point)] -> [Point]
funnel [] = []
funnel ps
  | n <= 1    = [apex0]
  | otherwise = dedupConsec (go cap 1 apex0 0 apex0 0 apex0 0 [apex0])
  where
    n        = length ps
    leftAt i  = fst (ps !! i)
    rightAt i = snd (ps !! i)
    apex0    = fst (head ps)
    goal     = fst (ps !! (n - 1))
    -- funnel は線形 (Mononen で証明済) だが、不正 portal 列での無限 restart を防ぐ保険。
    cap      = 8 * n + 64 :: Int
    -- 状態: fuel / 走査 i / apex (idx ai) / 左壁 lp (idx li) / 右壁 rp (idx ri) / 逆順 acc
    go fuel i apex ai lp li rp ri acc
      | fuel <= 0 = reverse (goal : acc)   -- 保険発火 (理論上到達せず)
      | i >= n    = reverse (goal : acc)
      | otherwise =
          let r = rightAt i
          -- 右壁の更新
          in if triarea2 apex rp r <= 0
               then if vequal apex rp || triarea2 apex lp r > 0
                      then stepLeft fuel i apex ai lp li r i acc   -- 締める (rp:=r, ri:=i)
                      else go (fuel - 1) (li + 1) lp li lp li lp li (lp : acc)  -- right が left 越え → left を確定
               else stepLeft fuel i apex ai lp li rp ri acc        -- 右更新スキップ
    -- 左壁の更新 (右更新を経た後・同 i)
    stepLeft fuel i apex ai lp li rp ri acc =
      let l = leftAt i
      in if triarea2 apex lp l >= 0
           then if vequal apex lp || triarea2 apex rp l < 0
                  then go (fuel - 1) (i + 1) apex ai l i rp ri acc   -- 締める (lp:=l, li:=i) → i 前進
                  else go (fuel - 1) (ri + 1) rp ri rp ri rp ri (rp : acc)  -- left が right 越え → right を確定
           else go (fuel - 1) (i + 1) apex ai lp li rp ri acc        -- 左更新スキップ → i 前進

-- | [日本語]: 連続する同一点 (vequal) を 1 つに畳む。 Mononen funnel は goal を末尾に必ず append
--   するため、 funnel が goal で collapse すると末尾が重複しうる。 R3 spline fit の零長
--   セグメント除けも兼ねる。
--   [English]: Collapses consecutive identical points (per 'vequal') into
--   one. Since the Mononen funnel always appends goal at the end, the tail
--   can end up duplicated when the funnel collapses onto goal. This also
--   removes zero-length segments before the R3 spline fit.
dedupConsec :: [Point] -> [Point]
dedupConsec [] = []
dedupConsec (x : xs) = x : go x xs
  where
    go _ [] = []
    go prev (y : ys)
      | vequal prev y = go prev ys
      | otherwise     = y : go y ys

-- | [日本語]: 三角形 (a,b,c) の符号付き面積 ×2 (Mononen canonical: bx*ay - ax*by)。
--   [English]: Twice the signed area of triangle (a,b,c) (Mononen's canonical
--   definition: bx*ay - ax*by).
triarea2 :: Point -> Point -> Point -> Double
triarea2 (Point ax' ay') (Point bx' by') (Point cx' cy') =
  let ax = bx' - ax'; ay = by' - ay'
      bx = cx' - ax'; by = cy' - ay'
  in bx * ay - ax * by

vequal :: Point -> Point -> Bool
vequal (Point ax ay) (Point bx by) =
  abs (ax - bx) < 1e-9 && abs (ay - by) < 1e-9

-- ===========================================================================
-- R3: box 拘束 cubic Bézier fit (graphviz Proutespline・route.c の忠実移植)
-- ===========================================================================
-- 一次根拠 = lib/pathplan/route.c (mkspline / splinefits / reallyroutespline /
-- splineisinside / splineintersectsline) + lib/pathplan/solvers.c (solve3)。
-- taut 折れ線に cubic Bézier を 1 本 fit → channel 境界 (barrier 線分) を逸脱しなければ
-- 採用、 逸脱すれば最大偏差点で分割して再帰。 これで graphviz と同じ滑らかな迂回弧になる。

-- 簡易ベクトル演算 (Point を 2D ベクトルとして扱う)。
vsub, vadd :: Point -> Point -> Point
vsub (Point ax ay) (Point bx by) = Point (ax - bx) (ay - by)
vadd (Point ax ay) (Point bx by) = Point (ax + bx) (ay + by)
vscale :: Double -> Point -> Point
vscale k (Point x y) = Point (k * x) (k * y)
vdot :: Point -> Point -> Double
vdot (Point ax ay) (Point bx by) = ax * bx + ay * by
vlen :: Point -> Double
vlen p = sqrt (vdot p p)
vdist :: Point -> Point -> Double
vdist a b = vlen (vsub a b)
vnorm :: Point -> Point
vnorm p = let l = vlen p in if l > 1e-12 then vscale (1 / l) p else p

-- | [日本語]: funnel 後の taut 補正。 'buildChannel' の portal は guide が box を
--   貫く行で @pushOut@ により反対側へ飛ぶことがあり (側 flip)、 連続 portal 間の
--   channel 多角形が box をまたぐ → taut 線分が box 内部を対角に横切る
--   (実測: dense15 x4→x15 の taut (32.4,176)→(75.6,190.2) が x13 box を貫通)。
--   box 内部を実質的に横切る線分 (貫通長 > 'boxCrossEps') に、 貫通側の box 角
--   waypoint を挿入して外周へ迂回させる。 端点が box 境界上に乗るだけの接触は対象外。
--   [English]: A post-funnel correction of the taut path. In a row where the
--   guide pierces a box, the portal of 'buildChannel' can jump to the opposite
--   side via @pushOut@ (a side flip), so the channel polygon between
--   consecutive portals can straddle the box, causing a taut segment to cut
--   diagonally through the box's interior (observed: in dense15, the taut
--   segment x4->x15 (32.4,176)->(75.6,190.2) pierces the x13 box). For a
--   segment that substantially crosses a box's interior (crossing length >
--   'boxCrossEps'), it inserts the box-corner waypoints on the crossing side
--   to route around the outside. A segment that merely touches the box
--   boundary at an endpoint is excluded.
avoidBoxTaut :: [Box] -> [Point] -> [Point]
avoidBoxTaut boxes = go (8 :: Int)
  where
    go 0 pts = pts
    go n pts =
      let pts' = pass pts
      in if length pts' == length pts then pts else go (n - 1) pts'
    pass (a : b : rest) = case firstCross a b of
      Just corners -> a : corners ++ pass (b : rest)
      Nothing      -> a : pass (b : rest)
    pass xs = xs
    firstCross a b =
      case [ cs | bx <- boxes, Just cs <- [crossCorners bx a b] ] of
        (cs : _) -> Just cs
        []       -> Nothing

-- | [日本語]: 線分 (a,b) が box 内部を横切るとき、 迂回に挿入する box 角列 (a→b 順)。
--   Liang-Barsky で貫通区間を求め、 貫通長が 'boxCrossEps' 以下 (角の接触等) は無視。
--   迂回側 (上辺経由 / 下辺経由 / 左右) は総距離が短い方を選ぶ。
--   [English]: When segment (a,b) crosses a box's interior, returns the box
--   corner waypoints to insert as a detour (in a-to-b order). The crossing
--   interval is found with Liang-Barsky clipping; a crossing length at or
--   below 'boxCrossEps' (e.g. a corner touch) is ignored. Whichever detour
--   side (top / bottom / left / right) has the shorter total distance is
--   chosen.
crossCorners :: Box -> Point -> Point -> Maybe [Point]
crossCorners (Box xlo ylo xhi yhi) a@(Point ax ay) b@(Point bx by) =
  let dx = bx - ax; dy = by - ay
      -- Liang-Barsky clip
      clip p q (t0, t1)
        | abs p < 1e-12 = if q < 0 then Nothing else Just (t0, t1)
        | otherwise =
            let r = q / p
            in if p < 0
                 then if r > t1 then Nothing else Just (max t0 r, t1)
                 else if r < t0 then Nothing else Just (t0, min t1 r)
      mtt = pure (0, 1)
        >>= clip (-dx) (ax - xlo) >>= clip dx (xhi - ax)
        >>= clip (-dy) (ay - ylo) >>= clip dy (yhi - ay)
  in case mtt of
       Just (t0, t1)
         | (t1 - t0) * sqrt (dx * dx + dy * dy) > boxCrossEps ->
             let entry = Point (ax + dx * t0) (ay + dy * t0)
                 exit_ = Point (ax + dx * t1) (ay + dy * t1)
                 -- 周回 corner 列 (時計回り): TL → TR → BR → BL。
                 -- ★ corner は box 外側へ 'cornerClear' だけ斜めに逃がす: box 辺
                 -- barrier 上に waypoint が正確に乗ると丸め fit が全て barrier 判定で
                 -- 棄却され forceflag 直角に縮退する (実測: junction 89.9°/45°)。
                 c = cornerClear
                 tl = Point (xlo - c) (ylo - c); tr = Point (xhi + c) (ylo - c)
                 br = Point (xhi + c) (yhi + c); bl = Point (xlo - c) (yhi + c)
                 ring = [tl, tr, br, bl]
                 -- entry/exit が乗る辺 index (TL-TR=0, TR-BR=1, BR-BL=2, BL-TL=3)
                 edgeOf (Point x y) =
                   snd $ minimum
                     [ (abs (y - ylo), 0 :: Int), (abs (x - xhi), 1)
                     , (abs (y - yhi), 2), (abs (x - xlo), 3) ]
                 e0 = edgeOf entry; e1 = edgeOf exit_
                 -- 辺 e0 から e1 へ時計回り / 反時計回りに渡る corner 列
                 cw  = [ ring !! ((i + 1) `mod` 4) | i <- takeWhile (/= e1) (iterate ((`mod` 4) . (+ 1)) e0) ]
                 ccw = [ ring !! (i `mod` 4) | i <- takeWhile (/= e1) (iterate ((`mod` 4) . (+ 3)) e0) ]
                 plen ps = polyLen (entry : ps ++ [exit_])
             in if e0 == e1
                  then Nothing   -- 同一辺内の接触 (貫通ではない)
                  else Just (if plen cw <= plen ccw then cw else ccw)
       _ -> Nothing

-- | [日本語]: box 貫通とみなす最小貫通長 (pt)。 角の接触・境界沿いを除外する。
--   [English]: The minimum crossing length (pt) counted as a box penetration.
--   Excludes corner touches and boundary-hugging contacts.
boxCrossEps :: Double
boxCrossEps = 2.0

-- | [日本語]: 迂回 corner waypoint を box から斜め外側へ逃がす量 (pt)。 spline の丸めが
--   box 辺 barrier に触れない余地を作る。
--   [English]: The amount (pt) by which a detour corner waypoint is pushed
--   diagonally outside the box. Gives the spline's rounding room so it
--   doesn't touch the box-edge barrier.
cornerClear :: Double
cornerClear = 2.0

-- | [日本語]: 近接 taut 点の畳み込み (端点は保持)。 corner 挿入 ('avoidBoxTaut') で旧 waypoint と
--   角が 1pt 未満で並ぶ backtrack を掃除し、 spline fit の零長セグメント荒れを防ぐ。
--   [English]: Collapses nearby taut points (endpoints are preserved). Cleans
--   up backtracks where a corner inserted by 'avoidBoxTaut' ends up within
--   1pt of an old waypoint, preventing zero-length-segment noise in the
--   spline fit.
dedupTaut :: Double -> [Point] -> [Point]
dedupTaut eps pts = case pts of
  []       -> []
  [_]      -> pts
  (p0 : rest) ->
    let lastP = last rest
        mids  = initSafe rest
        step acc q = if vdist (head acc) q >= eps then q : acc else acc
        kept  = reverse (foldl step [p0] mids)
        kept' = if length kept > 1 && vdist (last kept) lastP < eps
                  then initSafe kept else kept
    in kept' ++ [lastP]

-- (★ A7 試行メモ: box 4 辺を barrier に足す案は、 channel 壁 = box 辺ゆえ taut が
--  box 縁を沿走する正常区間まで「barrier 上のライド」 として fit 全棄却 →
--  forceflag 直角に縮退したため撤回。 貫通対策は 'avoidBoxTaut' の corner 挿入のみ。)

-- | [日本語]: portal 列から channel 境界の barrier 線分群を作る。 左鎖 (portal.left を上→下に連結)
--   と右鎖 (portal.right を連結) の各隣接ペア。 spline はこの内側に留まる。
--   [English]: Builds the channel-boundary barrier segments from a portal
--   sequence: each adjacent pair in the left chain (portal.left connected
--   top-to-bottom) and the right chain (portal.right connected). The spline
--   stays inside these.
channelBarriers :: [(Point, Point)] -> [(Point, Point)]
channelBarriers portals =
  let lefts  = map fst portals
      rights = map snd portals
      segs xs = filter (\(a, b) -> not (vequal a b)) (zip xs (drop 1 xs))
  in segs lefts ++ segs rights

-- | [日本語]: graphviz Proutespline 入口。 barriers (channel 境界線分) + taut 折れ線 (端点含む) +
--   端点接線方向 (ev0=始点, ev1=終点・__単位ベクトル__) から cubic Bézier 制御点列を返す。
--   戻り = [始点, c1, c2, 終点, c1, c2, 終点, ...] (= 先頭始点 + 3 点ずつの cubic segment)。
--
--   graphviz は endpoint slope を __呼出側 (dotsplines.c) が渡す__ 設計なので本 port も
--   ev0/ev1 を引数で受ける。 graphviz の @P->start.theta=-π/2 / P->end.theta=π/2 /
--   constrained@ は __内部の box-segment 境界__ に適用される拘束で、
--   __実端点 (src/snk port) の接線は port 方向 (斜め)__ である (一次実測: dot 14.1.5
--   gold は端点で斜め接線)。 呼出側 (routeEdge A3.3) は taut の端 segment 方向 =
--   自然 port 方向を渡す。
--   (A3.1 で一時 rank 方向の垂直を渡したが、 これは narrow-portal 時代の symmetric V
--    taut への対症で、 A3.2 の y-sweep で taut が 4 点クリーン化した後は斜め近接 +
--    強制垂直の衝突で内側 S を生むため A3.3 で自然方向へ戻した。)
--   [English]: The entry point for graphviz's Proutespline. Given the
--   barriers (channel boundary segments), the taut polyline (endpoints
--   included), and the endpoint tangent directions (ev0 = start, ev1 = end,
--   __unit vectors__), it returns a cubic Bezier control point sequence.
--   Returns [start, c1, c2, end, c1, c2, end, ...] (the leading start point
--   followed by cubic segments in groups of three).
--
--   graphviz is designed so that endpoint slope is __supplied by the caller (dotsplines.c)__,
--   so this port also takes ev0/ev1 as arguments. graphviz's
--   @P->start.theta=-pi/2 / P->end.theta=pi/2 / constrained@ is a constraint
--   applied to __internal box-segment boundaries__, whereas the tangent at the
--   actual endpoints (src/snk ports) __follows the port direction (diagonal)__
--   (primary observation: dot 14.1.5's gold output has a diagonal tangent at
--   the endpoints). The caller (routeEdge A3.3) passes
--   the taut path's end-segment direction, i.e. the natural port direction.
--   (A3.1 briefly passed a rank-direction perpendicular, a workaround from
--   the narrow-portal era for a symmetric-V taut path; once A3.2's y-sweep
--   cleaned the taut path down to four points, the clash between the
--   near-diagonal approach and the forced perpendicular produced an inward S,
--   so A3.3 reverted to the natural direction.)
proutespline :: [(Point, Point)] -> [Point] -> Point -> Point -> [Point]
proutespline _ []  _   _   = []
proutespline _ [p] _   _   = [p]
proutespline barriers inps ev0 ev1 =
  head inps : reallyroutespline barriers inps (vnorm ev0) (vnorm ev1)

-- | [日本語]: route.c reallyroutespline。 1 本 fit を試み、 失敗なら最大偏差点で分割し再帰。
--   戻り = 3 点ずつの cubic segment 列 (始点は含まない)。
--   [English]: route.c's reallyroutespline. Attempts a single fit, and on
--   failure splits at the point of maximum deviation and recurses. Returns
--   cubic segments in groups of three (the start point is not included).
reallyroutespline :: [(Point, Point)] -> [Point] -> Point -> Point -> [Point]
reallyroutespline barriers inps ev0 ev1 =
  let (pa, va, pb, vb) = mkspline inps ev0 ev1
  in case splinefits barriers pa va pb vb inps of
       Just cps -> cps
       Nothing  ->
         let spliti = maxDevIndex inps
             cip    = inps !! spliti
             v1     = vnorm (vsub cip (inps !! (spliti - 1)))
             v2     = vnorm (vsub (inps !! (spliti + 1)) cip)
             splitv = vnorm (vadd v1 v2)
         in reallyroutespline barriers (take (spliti + 1) inps) ev0 splitv
            ++ reallyroutespline barriers (drop spliti inps) splitv ev1

-- | [日本語]: route.c mkspline。 input 折れ線 + 端点単位方向 ev0/ev1 から、 端点接線の scale を
--   最小二乗で解く。 戻り = (始点, 始点接線ベクトル, 終点, 終点接線ベクトル)。
--   [English]: route.c's mkspline. From the input polyline and the endpoint
--   unit directions ev0/ev1, solves for the endpoint tangent scales by least
--   squares. Returns (start point, start tangent vector, end point, end
--   tangent vector).
mkspline :: [Point] -> Point -> Point -> (Point, Point, Point, Point)
mkspline inps ev0 ev1 =
  let p0  = head inps
      p3  = last inps
      -- 弦長パラメタ化 t ∈ [0,1]
      cum = scanl (+) 0 (zipWith vdist inps (drop 1 inps))
      tot = last cum
      ts  = if tot > 1e-12 then map (/ tot) cum else map (const 0) cum
      terms =
        [ (a0, a1, tmp)
        | (pt, t) <- zip inps ts
        , let a0  = vscale (b1 t) ev0
              a1  = vscale (negate (b2 t)) ev1
              tmp = vsub pt (vadd (vscale (b01 t) p0) (vscale (b23 t) p3)) ]
      c00 = sum [ vdot a0 a0  | (a0, _ , _ ) <- terms ]
      c01 = sum [ vdot a0 a1  | (a0, a1, _ ) <- terms ]
      c11 = sum [ vdot a1 a1  | (_ , a1, _ ) <- terms ]
      x0  = sum [ vdot a0 tmp | (a0, _ , tmp) <- terms ]
      x1  = sum [ vdot a1 tmp | (_ , a1, tmp) <- terms ]
      det01 = c00 * c11 - c01 * c01
      s0d = (x0 * c11 - x1 * c01) / det01     -- detX1/det01
      s3d = (c00 * x1 - c01 * x0) / det01     -- det0X/det01
      d01 = vdist p0 p3 / 3
      (s0, s3)
        | abs det01 < 1e-6 || s0d <= 0 || s3d <= 0 = (d01, d01)
        | otherwise                                 = (s0d, s3d)
  in (p0, vscale s0 ev0, p3, vscale s3 ev1)

-- | [日本語]: route.c splinefits。 mkspline の接線を a/3 倍 (a=4 から半減) しつつ control 点を作り、
--   channel 内に収まる最大 (= 滑らかな) ものを採用。 inpn==2 は強制採用 (forceflag)。
--
--   ★ (2026-07-08): channel 内でも control polygon が taut 比
--   'hairpinCap' 倍を超える候補は hairpin (接線暴走) として棄却する。
--   真因 (実測 = design/phase52-kink/): taut が 3 点 + 屈曲が終端寄りだと
--   'mkspline' の最小二乗が厳密解に退化し接線 scale が爆発 (kink 辺 = taut 比
--   2.18 倍超、 健全辺 ≤ ~1.3 倍)。 graphviz は box 列が taut を密に拘束するため
--   顕在化しないが、 我々の channel は片側が graph bbox 端 (R1) まで開くことが
--   あり、 暴走 S 字が「channel 内」 と誤判定されていた。 棄却後は a 半減で
--   平坦化 → それでも合わなければ従来どおり分割 (= graphviz と同じ収束先)。
--   [English]: route.c's splinefits. Builds control points while scaling
--   mkspline's tangents by a/3 (halving a from 4), and adopts the largest
--   (smoothest) one that fits inside the channel. inpn==2 is force-accepted
--   (forceflag).
--
--   (2026-07-08): even inside the channel, a candidate whose control polygon
--   exceeds the taut ratio by more than 'hairpinCap' is rejected as a hairpin
--   (a tangent runaway). Root cause (from measurement, see
--   design/phase52-kink/): when the taut path has 3 points and the bend sits
--   near the end, the least squares of 'mkspline' degenerates to an exact solution
--   and the tangent scale explodes (kink edges show a taut ratio over 2.18x,
--   versus at most ~1.3x for healthy edges). graphviz doesn't show this
--   because its box sequence constrains the taut path tightly, but our
--   channel can open all the way to the graph bbox edge on one side (R1), and
--   the runaway S-shape was being misjudged as "inside the channel". After
--   rejection, halving a flattens it; if that still doesn't fit, it falls
--   back to splitting as before (converging to the same result as graphviz).
splinefits :: [(Point, Point)] -> Point -> Point -> Point -> Point -> [Point] -> Maybe [Point]
splinefits barriers pa va pb vb inps = goA 4 True
  where
    forceflag = length inps == 2
    inLen     = polyLen inps
    goA a first =
      let s1  = vadd pa (vscale (a / 3) va)
          s2  = vsub pb (vscale (a / 3) vb)
          sps = [pa, s1, s2, pb]
      in if first && polyLen sps < inLen - 1e-3
           then Nothing                                  -- control polygon が短すぎ → 分割へ
           else if splineisinside barriers sps && polyLen sps <= hairpinCap * inLen
             then Just [s1, s2, pb]
             else if a < 0.005
               then if forceflag then Just [s1, s2, pb] else Nothing
               else goA (if a > 0.01 then a / 2 else 0) False

-- | [日本語]: hairpin 判定の control polygon 長 / taut 長 の上限比。
--   実測 (routes-before.csv): kink 5 辺 = 2.18〜2.6 倍 / 健全辺 ≤ ~1.3 倍。
--   [English]: The upper-bound ratio of control-polygon length to taut length
--   used for the hairpin check. Measured (routes-before.csv): kink edges = a
--   2.18x-2.6x ratio versus at most ~1.3x for healthy edges.
hairpinCap :: Double
hairpinCap = 1.5

-- | [日本語]: inps[0]..inps[n-1] の chord (始点-終点) から最も離れた内部点の index。
--   [English]: The index of the interior point farthest from the chord
--   (start-end) of inps[0]..inps[n-1].
maxDevIndex :: [Point] -> Int
maxDevIndex inps =
  let p0 = head inps
      pn = last inps
      n  = length inps
      ds = [ (distToSeg (inps !! i) p0 pn, i) | i <- [1 .. n - 2] ]
  in if null ds then 1 else snd (maximum ds)

-- | [日本語]: 点 p から線分 (a,b) への距離。
--   [English]: The distance from point p to segment (a,b).
distToSeg :: Point -> Point -> Point -> Double
distToSeg p a b =
  let ab = vsub b a
      l2 = vdot ab ab
  in if l2 < 1e-12 then vdist p a
     else let t = max 0 (min 1 (vdot (vsub p a) ab / l2))
          in vdist p (vadd a (vscale t ab))

polyLen :: [Point] -> Double
polyLen ps = sum (zipWith vdist ps (drop 1 ps))

-- | [日本語]: route.c splineisinside。 cubic (sps=[P0,c1,c2,P3]) が barrier 線分のいずれかを
--   内部交差すれば外 (False)。
--   [English]: route.c's splineisinside. Returns False (outside) if the cubic
--   (sps=[P0,c1,c2,P3]) crosses the interior of any barrier segment.
splineisinside :: [(Point, Point)] -> [Point] -> Bool
splineisinside barriers sps = not (any crosses barriers)
  where
    crosses bar = case splineIntersectsLine sps bar of
      Left ()    -> False                                 -- 退化 (4) は continue (= 非交差扱い)
      Right roots -> any (\t -> t > 1e-3 && t < 1 - 1e-3) roots

-- | [日本語]: route.c splineintersectsline。 cubic (sps) と線分 lps の交差 t (spline 側) を返す。
--   Left () = 退化 (graphviz の rootn==4 = 直線が spline 上に乗る/解無限)。
--   [English]: route.c's splineintersectsline. Returns the intersection t
--   (spline-side) of a cubic (sps) with segment lps. @Left ()@ is the
--   degenerate case (graphviz's rootn==4: the line lies on the spline / an
--   infinite solution).
splineIntersectsLine :: [Point] -> (Point, Point) -> Either () [Double]
splineIntersectsLine sps (lp0@(Point l0x l0y), lp1@(Point l1x l1y))
  | vequal lp0 lp1 = Right []                             -- 退化 barrier (点) は無視
  | xc1 == 0 && yc1 == 0 = Right []                       -- (到達しない・上で除外済)
  | xc1 == 0  =                                           -- 垂直線
      case solve3 (sub0 cx xc0) of
        Left ()    -> Left ()
        Right rs   -> Right [ t | t <- rs, t >= 0, t <= 1
                                , let sv = (evalCubic cy t - yc0) / yc1
                                , sv >= 0, sv <= 1 ]
  | otherwise =                                            -- 一般線
      let rat   = yc1 / xc1
          combo = ( c0y - rat * c0x + rat * xc0 - yc0
                  , c1y - rat * c1x
                  , c2y - rat * c2x
                  , c3y - rat * c3x )
      in case solve3 combo of
           Left ()  -> Left ()
           Right rs -> Right [ t | t <- rs, t >= 0, t <= 1
                                 , let sv = (evalCubic cx t - xc0) / xc1
                                 , sv >= 0, sv <= 1 ]
  where
    [Point p0x p0y, Point p1x p1y, Point p2x p2y, Point p3x p3y] = sps
    cx@(c0x, c1x, c2x, c3x) = points2coeff p0x p1x p2x p3x
    cy@(c0y, c1y, c2y, c3y) = points2coeff p0y p1y p2y p3y
    xc0 = l0x; xc1 = l1x - l0x
    yc0 = l0y; yc1 = l1y - l0y
    sub0 (a, b, c, d) k = (a - k, b, c, d)

-- | [日本語]: Bézier control 値 (1D) → power-basis 係数 (c0 + c1 t + c2 t² + c3 t³)。
--   [English]: Converts 1D Bezier control values to power-basis coefficients
--   (c0 + c1 t + c2 t^2 + c3 t^3).
points2coeff :: Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
points2coeff p0 p1 p2 p3 =
  ( p0
  , 3 * (p1 - p0)
  , 3 * (p0 - 2 * p1 + p2)
  , p3 - 3 * p2 + 3 * p1 - p0 )

-- | [日本語]: power-basis cubic を t で評価。
--   [English]: Evaluates a power-basis cubic at t.
evalCubic :: (Double, Double, Double, Double) -> Double -> Double
evalCubic (a, b, c, d) t = a + t * (b + t * (c + t * d))

-- | [日本語]: 実 cubic 求解 (solvers.c solve3 相当)。 戻り Right = 実根列、 Left () = 退化 (恒等0)。
--   係数は power basis (c0 + c1 x + c2 x² + c3 x³)。
--   [English]: Solves a real cubic (equivalent to solvers.c's solve3).
--   Returns @Right@ with the list of real roots, or @Left ()@ if degenerate
--   (identically zero). Coefficients are in power basis (c0 + c1 x + c2 x^2
--   + c3 x^3).
solve3 :: (Double, Double, Double, Double) -> Either () [Double]
solve3 (c0, c1, c2, c3)
  | abs c3 < tiny = solve2 (c0, c1, c2)
  | otherwise =
      let a = c2 / c3; b = c1 / c3; c = c0 / c3
          p = b - a * a / 3
          q = 2 * a * a * a / 27 - a * b / 3 + c
          shift = - a / 3
          disc = q * q / 4 + p * p * p / 27
      in Right $ map (+ shift) $
         if disc > tiny
           then [ cbrt (- q / 2 + sqrt disc) + cbrt (- q / 2 - sqrt disc) ]
           else if disc < - tiny
             then let m = 2 * sqrt (- p / 3)
                      th = acos (clampU ((3 * q) / (p * m))) / 3
                  in [ m * cos (th - 2 * pi * fromIntegral k / 3) | k <- [0, 1, 2 :: Int] ]
             else let u = cbrt (- q / 2) in [2 * u, - u]
  where tiny = 1e-12

solve2 :: (Double, Double, Double) -> Either () [Double]
solve2 (c0, c1, c2)
  | abs c2 < tiny = solve1 (c0, c1)
  | otherwise =
      let disc = c1 * c1 - 4 * c2 * c0
      in if disc < - tiny then Right []
         else if disc < tiny then Right [ - c1 / (2 * c2) ]
         else let s = sqrt disc in Right [ (- c1 + s) / (2 * c2), (- c1 - s) / (2 * c2) ]
  where tiny = 1e-12

solve1 :: (Double, Double) -> Either () [Double]
solve1 (c0, c1)
  | abs c1 < tiny = if abs c0 < tiny then Left () else Right []
  | otherwise     = Right [ - c0 / c1 ]
  where tiny = 1e-12

cbrt :: Double -> Double
cbrt x = signum x * (abs x ** (1 / 3))

clampU :: Double -> Double
clampU = max (-1) . min 1

-- | [日本語]: Bernstein 基底 (b01 = B0+B1, b23 = B2+B3)。 mkspline 用。
--   [English]: Bernstein basis functions (b01 = B0+B1, b23 = B2+B3), used by
--   mkspline.
b1, b2, b01, b23 :: Double -> Double
b1 t  = 3 * t * (1 - t) * (1 - t)
b2 t  = 3 * t * t * (1 - t)
b01 t = (1 - t) ** 3 + b1 t
b23 t = b2 t + t ** 3

-- ===========================================================================
-- Phase 52 A6: port 分散 (同一 node の近接重複 port を境界に沿って扇状に)
-- ===========================================================================

-- | [日本語]: route 端点の種別 (発 = 始点 / 着 = 終点)。
--   [English]: The kind of a route endpoint (source = start point / sink =
--   end point).
data PortEnd = SrcEnd | SnkEnd deriving (Eq, Show)

-- | [日本語]: 同一 node を共有する複数 edge の port が近接重複 ('portClusterEps'
--   以内) するとき、 node 境界に沿って 'portSep' 間隔の扇状に分散する post-pass。
--   graphviz P6 sameports 段 (correspondence doc Step 5、 未実装) の実用版。
--   route 全体は動かさず __端点 + 隣接制御点を同 delta 平行移動__ するだけなので
--   曲線形状は保たれる (delta は数 pt)。 bake ('Graphics.Hgg.Render.Special.dagBakeRoutes') と live
--   ('Graphics.Hgg.Render.Special.renderDAGStandalone') の両 pipeline が同順で呼ぶ (= HS/PS parity 維持)。
--   [English]: A post-pass that, when multiple edges sharing the same node
--   have ports that closely overlap (within 'portClusterEps'), fans them out
--   along the node boundary at 'portSep' intervals. A practical stand-in for
--   graphviz's P6 sameports stage (correspondence doc Step 5, not
--   implemented). It doesn't move the whole route, only
--   __translates the endpoint and its adjacent control point by the same delta__,
--   so the curve's shape is preserved (delta is a few pt). Both the bake
--   ('Graphics.Hgg.Render.Special.dagBakeRoutes') and live ('Graphics.Hgg.Render.Special.renderDAGStandalone') pipelines call this in
--   the same order (maintaining HS/PS parity).
spreadPorts
  :: (Double -> Double -> Point) -> Double
  -> [(DAGNode, DAGNode)]   -- ^ [日本語]: 各 route の (from, to)。 routes と同順
                            --   [English]: each route's (from, to), in the
                            --   same order as routes
  -> [EdgeRoute] -> [EdgeRoute]
spreadPorts toScreen radius ends routes =
  let idx = zip [0 :: Int ..] (zip ends routes)
      occs = concat
        [ [ (dnId f, SrcEnd, i, f), (dnId t, SnkEnd, i, t) ]
        | (i, ((f, t), _)) <- idx ]
      nids = dedupTexts [ nid | (nid, _, _, _) <- occs ]
      deltas = concatMap nodeDeltas nids
      applyAll r i =
        foldl (\acc (j, e, d) -> if j == i then adjustEnd e d acc else acc) r deltas
  in [ applyAll r i | (i, (_, r)) <- idx ]
  where
    routeOf i = routes !! i
    -- node ごとの port cluster を検出し、 各 member の (routeIx, End, delta) を返す
    nodeDeltas nid =
      let members =
            [ (i, end, n)
            | (i, (f, t)) <- zip [0 :: Int ..] ends
            , (end, n) <- [(SrcEnd, f), (SnkEnd, t)]
            , dnId n == nid ]
      in case members of
           ((_, _, n0) : _) | length members >= 2 ->
             let center = toScreen (dnX n0) (dnY n0)
                 withGeo =
                   [ (i, end, n, port, ang (vsub adj center))
                   | (i, end, n) <- members
                   , let r = routeOf i
                   , let port = endPoint end r
                   , let adj  = adjPoint end r ]
                 -- port 角で整列 → 近接 (境界弧距離 < portClusterEps) を cluster 化
                 sorted = sortOnD (\(_, _, _, p, _) -> ang (vsub p center)) withGeo
                 clusters = clusterBy
                   (\(_, _, _, p1, _) (_, _, _, p2, _) -> vdist p1 p2 < portClusterEps)
                   sorted
             in concatMap (fanCluster center) clusters
           _ -> []
    fanCluster center cl
      | length cl < 2 = []
      | otherwise =
          let k = length cl
              -- 隣接点方向の角度順に並べ、 扇の並びと route の向きを一致させる
              ordered = sortOnD (\(_, _, _, _, aAdj) -> aAdj) cl
              rEff = maximum (1 : [ vdist p center | (_, _, _, p, _) <- cl ])
              dTh  = portSep / rEff
              phi0 = ang (vsub (avgP [ p | (_, _, _, p, _) <- cl ]) center)
          in [ (i, end, vsub newPort port)
             | (j, (i, end, n, port, _)) <- zip [0 :: Int ..] ordered
             , let phi = phi0 + (fromIntegral j - fromIntegral (k - 1) / 2) * dTh
             , let target = vadd center (Point (100 * cos phi) (100 * sin phi))
             , let newPort = edgePortPoint n center target radius ]
    endPoint SrcEnd r = case samplePts r of (p : _) -> p; [] -> Point 0 0
    endPoint SnkEnd r = case reverse (samplePts r) of (p : _) -> p; [] -> Point 0 0
    -- 扇の並び順に使う「進入回廊」 点 = 端から弧長 'portBackDist' 手前の経路点。
    -- port 直近の制御点 (局所接線) だと近接方向の edge 対で順序が逆転し、 分散後に
    -- 経路がクロスする (実測: corr6 chd 着の genetics/exercise が接線角 0.03rad 差で
    -- 逆順 → 交差)。 回廊は経路サンプル折れ線を端から遡って取る。
    adjPoint end r =
      let poly = case end of
            SrcEnd -> samplePts r
            SnkEnd -> reverse (samplePts r)
      in walkBack portBackDist poly
    walkBack _ []  = Point 0 0
    walkBack _ [p] = p
    walkBack budget (p : q : rest)
      | d >= budget = q
      | otherwise   = walkBack (budget - d) (q : rest)
      where d = vdist p q
    -- route を折れ線近似 (cubic は各セグメント 8 分割)
    samplePts r = case r of
      StraightArrow a b -> [a, b]
      SplinePath ps     -> ps
      BezierPath ps     -> ps
      CubicPath (p0 : rest) -> p0 : concat
        [ [ bezPt a c1 c2 b (fromIntegral k / 8) | k <- [1 .. 8 :: Int] ]
        | (a, c1, c2, b) <- segsOf p0 rest ]
      CubicPath []      -> []
    segsOf cur (c1 : c2 : p3 : more) = (cur, c1, c2, p3) : segsOf p3 more
    segsOf _ _ = []
    bezPt (Point x0 y0) (Point x1 y1) (Point x2 y2) (Point x3 y3) t =
      let mt = 1 - t
          f a b c d' = mt*mt*mt*a + 3*mt*mt*t*b + 3*mt*t*t*c + t*t*t*d'
      in Point (f x0 x1 x2 x3) (f y0 y1 y2 y3)
    avgP ps = let n = fromIntegral (length ps)
              in Point (sum [ x | Point x _ <- ps ] / n) (sum [ y | Point _ y <- ps ] / n)
    ang (Point x y) = atan2 y x

-- | [日本語]: route の端点 (+cubic は隣接制御点も) を delta 平行移動する。
--   [English]: Translates a route's endpoint (and, for cubics, its adjacent
--   control point) by delta.
adjustEnd :: PortEnd -> Point -> EdgeRoute -> EdgeRoute
adjustEnd end d r = case (end, r) of
  (SrcEnd, StraightArrow a b)      -> StraightArrow (vadd a d) b
  (SnkEnd, StraightArrow a b)      -> StraightArrow a (vadd b d)
  (SrcEnd, SplinePath (p : rest))  -> SplinePath (vadd p d : rest)
  (SnkEnd, SplinePath ps)          -> SplinePath (mapLast (`vadd` d) ps)
  (SrcEnd, BezierPath (p : rest))  -> BezierPath (vadd p d : rest)
  (SnkEnd, BezierPath ps)          -> BezierPath (mapLast (`vadd` d) ps)
  (SrcEnd, CubicPath (p0 : c1 : rest)) -> CubicPath (vadd p0 d : vadd c1 d : rest)
  (SnkEnd, CubicPath ps)           -> CubicPath (mapLast2 (`vadd` d) ps)
  _                                -> r
  where
    mapLast f xs = case reverse xs of
      (z : zs) -> reverse (f z : zs)
      []       -> xs
    mapLast2 f xs = case reverse xs of
      (z : y : zs) -> reverse (f z : f y : zs)
      [z]          -> [f z]
      []           -> xs

-- | [日本語]: port cluster 判定の近接閾値 (pt)。 これ未満の port 対は「重なって見える」。
--   [English]: The proximity threshold (pt) for detecting a port cluster. A
--   pair of ports closer than this "looks overlapping".
portClusterEps :: Double
portClusterEps = 4.0

-- | [日本語]: 分散後の port 間隔 (境界弧距離、 pt)。 矢印幅 (~8pt) が重ならない程度。
--   [English]: The port spacing (pt, boundary arc distance) after fanning
--   out, chosen so arrowheads (~8pt wide) don't overlap.
portSep :: Double
portSep = 7.0

-- | [日本語]: 扇順序の「進入回廊」 を測る端からの弧長 (pt)。 局所接線より遠くで測ることで
--   近接方向 edge 対の順序逆転 (= 分散後クロス) を防ぐ。
--   [English]: The arc length (pt) from the endpoint used to measure the
--   "approach corridor" for fan ordering. Measuring farther out than the
--   local tangent prevents order reversal (crossing after fanning) for edge
--   pairs with similar directions.
portBackDist :: Double
portBackDist = 20.0

-- | [日本語]: 整列済みリストを隣接述語で連結 cluster に分割する。
--   [English]: Splits a sorted list into contiguous clusters using an
--   adjacency predicate.
clusterBy :: (a -> a -> Bool) -> [a] -> [[a]]
clusterBy _ [] = []
clusterBy eq (x : xs) = go [x] xs
  where
    go acc [] = [reverse acc]
    go acc@(prev : _) (y : ys)
      | eq prev y = go (y : acc) ys
      | otherwise = reverse acc : go [y] ys
    go [] _ = []

-- | [日本語]: 挿入ソート (射影キー・小規模用)。
--   [English]: Insertion sort on a projected key, for small lists.
sortOnD :: Ord b => (a -> b) -> [a] -> [a]
sortOnD f = foldr ins []
  where
    ins x [] = [x]
    ins x (z : zs) | f x <= f z = x : z : zs
                   | otherwise  = z : ins x zs

-- | [日本語]: Text の重複除去 (順序保持・小規模用)。
--   [English]: Deduplicates a list of Text values (order-preserving, for
--   small lists).
dedupTexts :: [Text] -> [Text]
dedupTexts = go []
  where
    go _ [] = []
    go seen (x : xs) | x `elem` seen = go seen xs
                     | otherwise     = x : go (x : seen) xs

-- | [日本語]: edge と node 形状の正確な交点を返す (= 矢印 port)。
--   @nodeAt@ = node 中心 (screen 座標)、 @target@ = edge 反対側 (= 方向決定用)、
--   @baseR@ = node の size scale。 楕円 / 矩形いずれも中心から target 方向へ伸ばし、
--   形状境界との交点を解析的に計算。
--   [English]: Returns the exact intersection point of an edge with a node's
--   shape (the arrowhead port). @nodeAt@ is the node center (screen
--   coordinates), @target@ is the edge's opposite end (used to determine
--   direction), and @baseR@ is the node's size scale. For both ellipses and
--   rectangles, it extends a ray from the center toward @target@ and computes
--   its intersection with the shape boundary analytically.
edgePortPoint :: DAGNode -> Point -> Point -> Double -> Point
edgePortPoint n (Point cx cy) (Point tx ty) baseR =
  let (rx, ry) = nodeExtent n baseR   -- ★A15-1: renderNode と同じ可変サイズを共有
      dx = tx - cx
      dy = ty - cy
      len = sqrt (dx * dx + dy * dy)
      (ux, uy) = if len > 1e-12 then (dx / len, dy / len) else (1, 0)
      t = case dnKind n of
        NodeLatent        -> ellipseT
        NodeObserved      -> ellipseT
        NodeDeterministic -> rectT
        NodeData          -> rectT
        NodeOther         -> rectT
      -- 楕円 (x/rx)^2 + (y/ry)^2 = 1 と方向 (ux, uy) の交点パラメタ
      ellipseT =
        let a = (ux / rx) ^ (2 :: Int) + (uy / ry) ^ (2 :: Int)
        in if a > 0 then 1 / sqrt a else baseR
      -- 矩形 |x| ≤ rx, |y| ≤ ry と方向の交点 (= 軸方向最小値)
      rectT =
        let txT = if abs ux > 1e-12 then rx / abs ux else 1 / 0
            tyT = if abs uy > 1e-12 then ry / abs uy else 1 / 0
        in min txT tyT
  in Point (cx + ux * t) (cy + uy * t)

-- | [日本語]: DAG ノードの半径 (rx, ry) を label 文字幅に合わせて算出する。
--   'Graphics.Hgg.Render.Special.renderNode' と 'edgePortPoint' が共有し、 形状端と edge port を一致させる。
--   deterministic は dist sublabel を出さない (= 1 行)。 @baseR@ は最小サイズの下限。
--
--   横半幅 rx の本体 (radius 非依存部) は layout と共有する
--   'Graphics.Hgg.Layout.dagNodeBaseHalfWidth' に一本化した。 ここでは render-time に既知の baseR
--   (= radius) を floor として被せるだけ。
--   [English]: Computes a DAG node's radii (rx, ry) to fit the label text
--   width. Shared by 'Graphics.Hgg.Render.Special.renderNode' and 'edgePortPoint' so the shape's edge and
--   the edge port agree. A deterministic node shows no dist sublabel (a
--   single line). @baseR@ is the lower bound on the minimum size.
--
--   The body of the horizontal half-width rx (the radius-independent part)
--   was consolidated into 'Graphics.Hgg.Layout.dagNodeBaseHalfWidth', which is shared with the
--   layout code. Here it simply applies the render-time-known baseR (the
--   radius) as a floor on top of that.
nodeExtent :: DAGNode -> Double -> (Double, Double)
nodeExtent n baseR =
  let showDist = nodeShowsDist n
      rx       = max baseR (dagNodeBaseHalfWidth n)
      nLines   = if showDist then 3 else 1 :: Int
      lineH    = dagLabelFs + 3
      ry       = max (baseR * 0.7) (fromIntegral nLines * lineH / 2 + 4)
  in (rx, ry)

-- | [日本語]: dist sublabel (@~ Dist@) を描くか。 deterministic は派生量ゆえ分布を持たず name のみ (PyMC 慣例)。
--   [English]: Whether to draw the dist sublabel (@~ Dist@). A deterministic
--   node is a derived quantity and has no distribution, so it shows only its
--   name (following PyMC convention).
nodeShowsDist :: DAGNode -> Bool
nodeShowsDist n = case dnKind n of
  NodeDeterministic -> False
  _                 -> case dnDist n of Just _ -> True; Nothing -> False

-- | [日本語]: plate 枠の __実 bbox (pt 空間)__ = (xlo, boxTop, xhi, yhi)。
--   label 帯を含む描画矩形そのもの。 'Graphics.Hgg.Render.Special.renderPlate' (描画) と pt 空間 edge router
--   (障害物判定) が共有する。 member が 1 つも無ければ Nothing。
--
--   nested plate の場合: graphviz の cluster bbox 計算 (= 子 cluster box ∪ 直接 member
--   glyph box を union し、 自身の margin を 1 段ぶん足す) を __再帰__ で忠実再現する。
--   これにより nested plate の親枠が子枠の外側 margin (graphviz @CL_OFFSET@ 相当) に出る
--   (= 旧実装は親も子も member 極値から flat margin で再計算し境界が一致していた)。
--   leaf plate (子無し) は @directIds = 全 member@ ・ @childBoxes = []@ で従来と完全同一
--   (= 図ビット不変)。 自身の直接子は 'plateChildrenOf' で @allPlates@ の包含関係から復元。
--   [English]: The plate frame's __actual bbox (pt space)__ = (xlo, boxTop,
--   xhi, yhi) — exactly the drawn rectangle, including the label band. Shared
--   by 'Graphics.Hgg.Render.Special.renderPlate' (drawing) and the pt-space edge router (obstacle
--   detection). Returns Nothing if the plate has no members at all.
--
--   For a nested plate: faithfully reproduces graphviz's cluster bbox
--   computation (union the child cluster boxes with the direct members'
--   glyph boxes, then add one level's own margin) __recursively__. This makes
--   a nested plate's parent frame sit outside its children's margin (the
--   equivalent of graphviz's @CL_OFFSET@) — the old implementation
--   recomputed both parent and child from member extrema with a flat margin,
--   so the boundaries happened to coincide. A leaf plate (no children) is
--   identical to before, with @directIds = all members@ and
--   @childBoxes = []@ (pixel-identical output). A plate's direct children are
--   recovered from the containment relation among @allPlates@ by
--   'plateChildrenOf'.
plateBoxPt :: (Double -> Double -> Point) -> Double
           -> [(Text, DAGNode)] -> [DAGPlate] -> DAGPlate
           -> Maybe (Double, Double, Double, Double)
plateBoxPt toScreen radius nodeMap allPlates plate =
  let children   = plateChildrenOf allPlates plate
      childIds   = concatMap dpNodeIds children
      directIds  = [ i | i <- dpNodeIds plate, i `notElem` childIds ]
      included   = mapMaybe (\i -> lookup i nodeMap) directIds
      -- 直接 member の glyph box (= left, right, top, bottom)
      memberBoxes = [ (x - gx, x + gx, y - gy, y + gy)
                    | n <- included
                    , let Point x y = toScreen (dnX n) (dnY n)
                    , let (gx, gy) = nodeExtent n radius ]
      -- 子 plate の box (= 既に各自の margin + label 帯込み)。 (xlo, boxTop, xhi, yhi)
      -- を本関数 tuple 規約 (left, right, top, bottom) に並べ替えて union 対象に混ぜる。
      childBoxes  = [ (xlo, xhi, boxTop, yhi)
                    | c <- children
                    , Just (xlo, boxTop, xhi, yhi)
                        <- [plateBoxPt toScreen radius nodeMap allPlates c] ]
      boxes = memberBoxes ++ childBoxes
  in case boxes of
       [] -> Nothing
       _  ->
         let margin = radius * 0.5
             labelH = 14
             xlo = minimum [a | (a, _, _, _) <- boxes] - margin
             ylo = minimum [b | (_, _, b, _) <- boxes] - margin
             xhi = maximum [a | (_, a, _, _) <- boxes] + margin
             yhi = maximum [b | (_, _, _, b) <- boxes] + margin
         -- label 帯は枠の **下** に確保する (graphviz labelloc=b 同型)。 box 下端を
         -- labelH ぶん広げ、 box 上端は member の margin のみ。
         in Just (xlo, ylo, xhi, yhi + labelH)

-- | [日本語]: plate @parent@ の __直接の子__ plate (= graphviz subcluster) 群。
--   子 = nodeIds が parent の真部分集合で、 間に別の plate を挟まない (= immediate) もの。
--   graphviz は cluster をネスト木として保持するが、 我々は plate list の包含関係から
--   復元する (= 内側 plate の member ⊊ 外側 plate の member、 という運用前提)。
--   [English]: The __direct children__ of plate @parent@ (i.e. graphviz
--   subclusters). A child is one whose nodeIds are a strict subset of
--   parent's, with no other plate interposed (immediate). graphviz keeps
--   clusters as a nesting tree, but we recover it from the containment
--   relation among the plate list (assuming the convention that an inner
--   plate's members are a strict subset of an outer plate's members).
plateChildrenOf :: [DAGPlate] -> DAGPlate -> [DAGPlate]
plateChildrenOf allPlates parent =
  let strictSub q p =
        let qs = dpNodeIds q; ps = dpNodeIds p
        in all (`elem` ps) qs && not (all (`elem` qs) ps)
      cands = [ q | q <- allPlates, strictSub q parent ]
      immediate q = not (any (\r -> strictSub q r && strictSub r parent) cands)
  in filter immediate cands

-- (routeAroundBoxes は Phase 52 A5 で削除: Phase 39 A2-8 の遺物で caller ゼロ、
--  channel/funnel + Proutespline (A-2/A-3/R3) に置換済。 実装は git 履歴参照。)

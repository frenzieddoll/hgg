-- |
-- Module      : Hgg.Plot.Render.EdgeRoute
-- Description : DAG edge の pt 空間 routing 幾何 (障害物回避・port・制御点列)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- Phase 39 B2: routing を描画 (Render.Special) から分離した純幾何 module。
-- pt 空間で toScreen・radius・plate bbox (障害物) を受け、 edge の制御点列と
-- 描画 style ('EdgeRoute') を返す。 Primitive 生成や ThemePalette には依存しない
-- (= 描画は呼出側 'renderEdge' の責務)。 B1 の段階型と対になる「routing 入力契約」。
--
-- node 形状幾何 ('nodeExtent' / 'edgePortPoint') も routing が依存するため本 module に
-- 置き、 描画側 (renderNode 等) は本 module から import する (= 下位 = 幾何、
-- 上位 = 描画 の層分け)。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Render.EdgeRoute
  ( -- * routing 結果
    EdgeRoute (..)
  , routeEdge
    -- * 障害物モデル (A-1)
  , Box (..)
  , Obstacles (..)
  , dagObstacles
  , plateBoxPt
  , plateChildrenOf
  , routeAroundBoxes
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

import           Hgg.Plot.Layout (dagLabelFs, dagNodeBaseHalfWidth)
import           Hgg.Plot.Render.Common (Point (..))
import           Hgg.Plot.Spec   (DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..))
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)

-- | edge routing の結果 = 制御点列 + 描画 style。 ThemePalette/Primitive 非依存。
--
--   * 'StraightArrow' = 単独 short edge (直線 + 矢印)
--   * 'SplinePath'    = 並列 short / 長 edge 非迂回 (Catmull-Rom・呼出側で平滑化)
--   * 'BezierPath'    = 長 edge の plate box 迂回 (箱角 waypoint を平滑化せず通す)
--   * 'CubicPath'     = R3 (Step6 P7a): graphviz Proutespline の box 拘束 cubic Bézier
--                       fit。 先頭 = 始点、 以後 3 点ずつ (制御点1, 制御点2, 終点) の
--                       cubic segment 列。
data EdgeRoute
  = StraightArrow Point Point
  | SplinePath [Point]
  | BezierPath [Point]
  | CubicPath [Point]
  deriving (Show, Eq)

-- | edge の制御点列と style を pt 空間で決定する純関数 (= 'renderEdge' から routing 部を抽出)。
-- 並列 edge は perpendicular に offset、 長 edge は graphviz routesplines:
-- 障害物 ('Obstacles') から box-channel を作り (A-2)、 funnel 最短折れ線 (A-3) を通す。
routeEdge
  :: (Double -> Double -> Point)
  -> Obstacles                            -- ^ A-1: node + plate 障害物 (pt 空間)
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
       Just chain | length chain >= 2 ->
         -- A-2/A-3: dummy chain を guide に box-channel を作り funnel で最短折れ線へ。
         -- 内部制御点は並列 spread 用に perpendicular bend を掛けてから guide にする。
         let inner    = [ toScreen x y | (x, y) <- take (length chain - 2) (drop 1 chain) ]
             innerOff = map offsetP inner
             guide    = fromCenter : innerOff ++ [toCenter]
             -- A-1: この edge が避ける障害物 (= 端点を含む箱は除外)。
             eboxes   = edgeBoxes obs from to fromCenter toCenter
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

-- | pt 空間の軸並行矩形 (xlo ≤ xhi, ylo ≤ yhi)。 routing の障害物 / channel box 共用。
data Box = Box !Double !Double !Double !Double  -- ^ xlo ylo xhi yhi
  deriving (Show, Eq)

-- | routing 用障害物集合。 node glyph box (= id 付き・端点除外用) と plate box を分けて保持。
data Obstacles = Obstacles
  { obNodes  :: [(Text, Box)]   -- ^ node id → glyph box (clearance margin 込み)
  , obPlates :: [Box]           -- ^ plate 枠 box (clearance margin 込み)
  } deriving (Show, Eq)

-- | clearance margin (= spline が箱に接しないための余白)。 graphviz: cluster 8pt。
obNodeMargin, obPlateMargin :: Double
obNodeMargin  = 4
obPlateMargin = 8

-- | 全 node glyph box (+margin) と plate box (+margin) を pt 空間で構築する (A-1)。
dagObstacles :: (Double -> Double -> Point) -> Double
             -> [DAGNode] -> [(Text, DAGNode)] -> [DAGPlate] -> Obstacles
dagObstacles toScreen radius nodes nodeMap plates =
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
    }

-- | この edge が避けるべき障害物 box 群。 端点 (from/to) の node box と、 端点中心を
-- 内側に含む box (= 端点が属する plate 等) は除外する (= edge は正規にそこへ接続する)。
edgeBoxes :: Obstacles -> DAGNode -> DAGNode -> Point -> Point -> [Box]
edgeBoxes obs from to srcC snkC =
  let nodeB = [ b | (i, b) <- obNodes obs, i /= dnId from, i /= dnId to ]
      allB  = nodeB ++ obPlates obs
  in [ b | b <- allB, not (boxContains b srcC), not (boxContains b snkC) ]

-- | 点が box の interior にあるか (境界は外側扱い)。
boxContains :: Box -> Point -> Bool
boxContains (Box xlo ylo xhi yhi) (Point x y) =
  x > xlo && x < xhi && y > ylo && y < yhi

-- ===========================================================================
-- A-2: box-channel 構築 (guide 折れ線 + 障害物 → portal 列)
-- ===========================================================================

-- | guide 折れ線 (端点含む・y 単調を想定) と障害物から funnel 用 portal 列を作る。
-- 各内部 guide 点の y 水平線上で、 guide の x を含む free 区間 (左右最寄り障害物に
-- clip) を求め、 (左点, 右点) の portal に。 端点 (src/snk) は退化 portal として両端に置く。
-- free 区間が退化/逆転したら guide 点をそのまま通す退化 portal にフォールバック
-- (= その点は funnel の強制通過点になる。 'funnel' の退化 portal 扱いを参照)。
--
-- ★ R1 (Step6 P7a・2026-06-24): 片側に障害物が無いときの壁を **graph bbox 端 (有限値)**
-- に clip する (旧: ±Infinity)。graphviz `maximal_bbox` (dotsplines.c) は隣 node が無ければ
-- cluster/graph 境界へ clip するため壁は常に有限。旧 ±Inf は funnel の 'tri' 外積を
-- Infinity 化して符号崩壊 → 直線 collapse を招いていた (correspondence doc §4-B)。
--
-- ★ R2-fix (2026-06-24): portal を free 区間**全幅**でなく **dummy x まわりの狭い窓**
-- ([gx-w, gx+w] を free 区間で clip) にする。graphviz `maximal_bbox` は virtual node 自身の
-- 細い幅 (lw≈1pt) 基準で box を作るため box は dummy に密着する。旧実装は free 区間全幅を
-- portal にしていたため、端点が片寄ると funnel が dummy lane を無視して chain 寄りへ
-- shortcut し L 字 (角 1 個) になり、R3 の cubic fit が暴走 (bulge) していた。狭い窓に
-- すると funnel が collinear な dummy lane に沿い、graphviz と同じ滑らかな bow になる。
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

-- | guide 折れ線 (y 単調を想定) の高さ @y@ における x を線形補間する。
-- box-stack portal の「側」 (どの障害物が左/右か) を決めるのに使う。
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

-- | 昇順ソート (挿入ソート・小規模 event 列向け)。
sortAsc :: [Double] -> [Double]
sortAsc = foldr ins []
  where
    ins x [] = [x]
    ins x (z : zs) | x <= z    = x : z : zs
                   | otherwise = z : ins x zs

-- | 近接した y を 1 つに畳む (portal の零高さセグメント除け)。
dedupNear :: [Double] -> [Double]
dedupNear [] = []
dedupNear (x : xs) = x : go x xs
  where
    go _ [] = []
    go prev (z : zs) | abs (z - prev) < epsY = go prev zs
                     | otherwise             = z : go z zs

-- | box 内側へ寄せて portal を sample する高さオフセット (pt)。 strict cross 判定に
-- 乗せ、box 上端・下端の角を確実に waypoint 化する。 近接 y の畳み込み閾値も兼ねる。
epsY :: Double
epsY = 0.75

-- | R1 フォールバック壁の余白 (pt)。 graph bbox 端からさらに外へ取る隙間。
channelMargin :: Double
channelMargin = 16

-- ===========================================================================
-- A-3: funnel (stringpulling) 最短折れ線
-- ===========================================================================

-- | portal 列 ((左点, 右点) の列・先頭=src 末尾=snk の退化 portal) を通る最短折れ線を
-- funnel アルゴリズムで求める。 戻り = src .. snk の折れ線 (端点含む)。
--
-- ★ R2 (Step6 P7a・2026-06-24): graphviz `Pshortestpath` (shortest.c の三角形分割 +
-- deque funnel + `ccw`) と **数学的に同一**な教科書的 Lee funnel
-- (Mononen "Simple Stupid Funnel Algorithm") に置換。box-stack polygon では三角形分割の
-- 対角線 = box 重なり portal なので portal-funnel = 三角形分割 funnel (= 新規アルゴでなく
-- Pshortestpath そのもの)。旧自前 apex-jump funnel は cone 不変条件違反で左右壁を交互
-- 往復する zigzag を生んでいた (correspondence doc §4-C)。
--
-- 規約: portal.left = 小 x 側 / portal.right = 大 x 側、path は下方向 (y 増加)。
-- 'triarea2' は canonical 定義 (bx*ay - ax*by)。right 壁が左へ寄ると triarea2 ≤ 0 で funnel が
-- 締まる (手計算検証済)。退化 portal (left==right) は 'vequal' 分岐で素通り。
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

-- | 連続する同一点 (vequal) を 1 つに畳む。 Mononen funnel は goal を末尾に必ず append
-- するため、 funnel が goal で collapse すると末尾が重複しうる。 R3 spline fit の零長
-- セグメント除けも兼ねる。
dedupConsec :: [Point] -> [Point]
dedupConsec [] = []
dedupConsec (x : xs) = x : go x xs
  where
    go _ [] = []
    go prev (y : ys)
      | vequal prev y = go prev ys
      | otherwise     = y : go y ys

-- | 三角形 (a,b,c) の符号付き面積 ×2 (Mononen canonical: bx*ay - ax*by)。
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

-- | portal 列から channel 境界の barrier 線分群を作る。 左鎖 (portal.left を上→下に連結)
-- と右鎖 (portal.right を連結) の各隣接ペア。 spline はこの内側に留まる。
channelBarriers :: [(Point, Point)] -> [(Point, Point)]
channelBarriers portals =
  let lefts  = map fst portals
      rights = map snd portals
      segs xs = filter (\(a, b) -> not (vequal a b)) (zip xs (drop 1 xs))
  in segs lefts ++ segs rights

-- | graphviz Proutespline 入口。 barriers (channel 境界線分) + taut 折れ線 (端点含む) +
-- 端点接線方向 (ev0=始点, ev1=終点・**単位ベクトル**) から cubic Bézier 制御点列を返す。
-- 戻り = [始点, c1, c2, 終点, c1, c2, 終点, ...] (= 先頭始点 + 3 点ずつの cubic segment)。
--
-- graphviz は endpoint slope を**呼出側 (dotsplines.c) が渡す**設計なので本 port も
-- ev0/ev1 を引数で受ける。 graphviz の @P->start.theta=-π/2 / P->end.theta=π/2 /
-- constrained@ は **内部の box-segment 境界** に適用される拘束で、 **実端点 (src/snk
-- port) の接線は port 方向 (斜め)** (一次実測: dot 14.1.5 gold は端点で斜め接線)。
-- 呼出側 (routeEdge A3.3) は taut の端 segment 方向 = 自然 port 方向を渡す。
-- (A3.1 で一時 rank 方向の垂直を渡したが、 これは narrow-portal 時代の symmetric V
--  taut への対症で、 A3.2 の y-sweep で taut が 4 点クリーン化した後は斜め近接 +
--  強制垂直の衝突で内側 S を生むため A3.3 で自然方向へ戻した。)
proutespline :: [(Point, Point)] -> [Point] -> Point -> Point -> [Point]
proutespline _ []  _   _   = []
proutespline _ [p] _   _   = [p]
proutespline barriers inps ev0 ev1 =
  head inps : reallyroutespline barriers inps (vnorm ev0) (vnorm ev1)

-- | route.c reallyroutespline。 1 本 fit を試み、 失敗なら最大偏差点で分割し再帰。
-- 戻り = 3 点ずつの cubic segment 列 (始点は含まない)。
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

-- | route.c mkspline。 input 折れ線 + 端点単位方向 ev0/ev1 から、 端点接線の scale を
-- 最小二乗で解く。 戻り = (始点, 始点接線ベクトル, 終点, 終点接線ベクトル)。
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

-- | route.c splinefits。 mkspline の接線を a/3 倍 (a=4 から半減) しつつ control 点を作り、
-- channel 内に収まる最大 (= 滑らかな) ものを採用。 inpn==2 は強制採用 (forceflag)。
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
           else if splineisinside barriers sps
             then Just [s1, s2, pb]
             else if a < 0.005
               then if forceflag then Just [s1, s2, pb] else Nothing
               else goA (if a > 0.01 then a / 2 else 0) False

-- | inps[0]..inps[n-1] の chord (始点-終点) から最も離れた内部点の index。
maxDevIndex :: [Point] -> Int
maxDevIndex inps =
  let p0 = head inps
      pn = last inps
      n  = length inps
      ds = [ (distToSeg (inps !! i) p0 pn, i) | i <- [1 .. n - 2] ]
  in if null ds then 1 else snd (maximum ds)

-- | 点 p から線分 (a,b) への距離。
distToSeg :: Point -> Point -> Point -> Double
distToSeg p a b =
  let ab = vsub b a
      l2 = vdot ab ab
  in if l2 < 1e-12 then vdist p a
     else let t = max 0 (min 1 (vdot (vsub p a) ab / l2))
          in vdist p (vadd a (vscale t ab))

polyLen :: [Point] -> Double
polyLen ps = sum (zipWith vdist ps (drop 1 ps))

-- | route.c splineisinside。 cubic (sps=[P0,c1,c2,P3]) が barrier 線分のいずれかを
-- 内部交差すれば外 (False)。
splineisinside :: [(Point, Point)] -> [Point] -> Bool
splineisinside barriers sps = not (any crosses barriers)
  where
    crosses bar = case splineIntersectsLine sps bar of
      Left ()    -> False                                 -- 退化 (4) は continue (= 非交差扱い)
      Right roots -> any (\t -> t > 1e-3 && t < 1 - 1e-3) roots

-- | route.c splineintersectsline。 cubic (sps) と線分 lps の交差 t (spline 側) を返す。
-- Left () = 退化 (graphviz の rootn==4 = 直線が spline 上に乗る/解無限)。
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

-- | Bézier control 値 (1D) → power-basis 係数 (c0 + c1 t + c2 t² + c3 t³)。
points2coeff :: Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
points2coeff p0 p1 p2 p3 =
  ( p0
  , 3 * (p1 - p0)
  , 3 * (p0 - 2 * p1 + p2)
  , p3 - 3 * p2 + 3 * p1 - p0 )

-- | power-basis cubic を t で評価。
evalCubic :: (Double, Double, Double, Double) -> Double -> Double
evalCubic (a, b, c, d) t = a + t * (b + t * (c + t * d))

-- | 実 cubic 求解 (solvers.c solve3 相当)。 戻り Right = 実根列、 Left () = 退化 (恒等0)。
-- 係数は power basis (c0 + c1 x + c2 x² + c3 x³)。
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

-- | Bernstein 基底 (b01 = B0+B1, b23 = B2+B3)。 mkspline 用。
b1, b2, b01, b23 :: Double -> Double
b1 t  = 3 * t * (1 - t) * (1 - t)
b2 t  = 3 * t * t * (1 - t)
b01 t = (1 - t) ** 3 + b1 t
b23 t = b2 t + t ** 3

-- | Phase 1 A7: edge と node 形状の正確な交点を返す (= 矢印 port)。
-- 'nodeAt' = node 中心 (screen 座標)、 'target' = edge 反対側 (= 方向決定用)、
-- 'baseR' = node の size scale。 楕円 / 矩形いずれも中心から target 方向へ伸ばし、
-- 形状境界との交点を解析的に計算。
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

-- | DAG ノードの半径 (rx, ry) を label 文字幅に合わせて算出 (Phase 52.A15-1)。
-- 'renderNode' と 'edgePortPoint' が共有し、 形状端と edge port を一致させる。
-- deterministic は dist sublabel を出さない (= 1 行)。 @baseR@ は最小サイズの下限。
--
-- Phase 39 P8 A4-2: 横半幅 rx の本体 (radius 非依存部) は layout と共有する
-- 'dagNodeBaseHalfWidth' に一本化した。 ここでは render-time に既知の baseR
-- (= radius) を floor として被せるだけ。
nodeExtent :: DAGNode -> Double -> (Double, Double)
nodeExtent n baseR =
  let showDist = nodeShowsDist n
      rx       = max baseR (dagNodeBaseHalfWidth n)
      nLines   = if showDist then 3 else 1 :: Int
      lineH    = dagLabelFs + 3
      ry       = max (baseR * 0.7) (fromIntegral nLines * lineH / 2 + 4)
  in (rx, ry)

-- | dist sublabel (@~ Dist@) を描くか。 deterministic は派生量ゆえ分布を持たず name のみ (PyMC 慣例)。
nodeShowsDist :: DAGNode -> Bool
nodeShowsDist n = case dnKind n of
  NodeDeterministic -> False
  _                 -> case dnDist n of Just _ -> True; Nothing -> False

-- | Phase 39 A2-8 / A4 (nested): plate 枠の **実 bbox (pt 空間)** = (xlo, boxTop, xhi, yhi)。
-- label 帯を含む描画矩形そのもの。 'renderPlate' (描画) と pt 空間 edge router
-- (障害物判定) が共有する。 member が 1 つも無ければ Nothing。
--
-- A4 (nested plate): graphviz の cluster bbox 計算 (= 子 cluster box ∪ 直接 member
-- glyph box を union し、 自身の margin を 1 段ぶん足す) を **再帰** で忠実再現する。
-- これにより nested plate の親枠が子枠の外側 margin (graphviz @CL_OFFSET@ 相当) に出る
-- (= 旧実装は親も子も member 極値から flat margin で再計算し境界が一致していた)。
-- leaf plate (子無し) は @directIds = 全 member@ ・ @childBoxes = []@ で従来と完全同一
-- (= 図ビット不変)。 自身の直接子は 'plateChildrenOf' で 'allPlates' の包含関係から復元。
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

-- | A4: plate 'parent' の **直接の子** plate (= graphviz subcluster) 群。
-- 子 = nodeIds が parent の真部分集合で、 間に別の plate を挟まない (= immediate) もの。
-- graphviz は cluster をネスト木として保持するが、 我々は plate list の包含関係から
-- 復元する (= 内側 plate の member ⊊ 外側 plate の member、 という運用前提)。
plateChildrenOf :: [DAGPlate] -> DAGPlate -> [DAGPlate]
plateChildrenOf allPlates parent =
  let strictSub q p =
        let qs = dpNodeIds q; ps = dpNodeIds p
        in all (`elem` ps) qs && not (all (`elem` qs) ps)
      cands = [ q | q <- allPlates, strictSub q parent ]
      immediate q = not (any (\r -> strictSub q r && strictSub r parent) cands)
  in filter immediate cands

-- | Phase 39 A2-8: pt 空間で plate box (障害物) を避ける内部制御点列を返す。
-- 戻り = (差し替え後の内部点, 迂回したか)。 both endpoints が箱外、 かつ元 polyline が
-- 箱 interior を横切る箱のみ対象とし、 その union を近い側で縦に迂回する角 waypoint
-- 2 点 (= 箱の実 bbox ± margin の上端/下端) に置換する (graphviz cluster 迂回相当)。
-- 端点が箱内 (= plate メンバへの正規 edge) は対象外。
routeAroundBoxes
  :: Double
  -> [(Double, Double, Double, Double)]
  -> Point -> Point -> [Point]
  -> ([Point], Bool)
routeAroundBoxes radius boxes srcC snkC innerPts =
  let poly = srcC : innerPts ++ [snkC]
      m = radius * 0.6
      pInside (Point x y) (xlo, ylo, xhi, yhi) =
        x > xlo && x < xhi && y > ylo && y < yhi
      lerpP (Point ax ay) (Point bx by) t =
        Point (ax + (bx - ax) * t) (ay + (by - ay) * t)
      segHits (a, b) box =
        any (\t -> pInside (lerpP a b t) box) [ fromIntegral i / 16 | i <- [1 .. 15 :: Int] ]
      polyHits box = any (`segHits` box) (zip poly (tail poly))
      crossed = [ box | box <- boxes
                , not (pInside srcC box), not (pInside snkC box)
                , polyHits box ]
  in case crossed of
       [] -> (innerPts, False)
       _  ->
         let bxlo = minimum [ a | (a, _, _, _) <- crossed ]
             bylo = minimum [ b | (_, b, _, _) <- crossed ]
             bxhi = maximum [ a | (_, _, a, _) <- crossed ]
             byhi = maximum [ b | (_, _, _, b) <- crossed ]
             cx   = (bxlo + bxhi) / 2
             Point sx _ = srcC
             Point tx _ = snkC
             refX  = (sx + tx) / 2
             sideX = if refX <= cx then bxlo - m else bxhi + m
         in ([ Point sideX (bylo - m), Point sideX (byhi + m) ], True)

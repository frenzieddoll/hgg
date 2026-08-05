-- |
-- Module      : Graphics.Hgg.Render.Special
-- Description : Special marks (pie, waterfall, parallel coordinates, text, DAG)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Render モノリス分割 (出力中立・純粋移動)。
--   [English]: Split off from the Render monolith (output-neutral, pure move).
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphics.Hgg.Render.Special where

import           Graphics.Hgg.Layout (numToText,
                                      Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, AxisPlacement (..),
                                      -- Phase 64 A4: 棒を投影層の形状 dispatcher へ
                                      BarShape (..), projectBar,
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical, textWidthEm)
import           Graphics.Hgg.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Graphics.Hgg.Spec   (Annotation (..), AxisFormat (..),
                                      ColData (..), ColRef,
                                      ColorEnc (..), ConnectSpec (..),
                                      DAGEdge (..), DAGLayoutAlgorithm (..),
                                      RoutedEdge (..), EdgeShapeKind (..),
                                      DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..), DAGSpec (..), Layer (..),
                                      LegendPosition (..), LegendSpec (..),
                                      Inset (..), MarginalSpec (..), MarkKind (..),
                                      MarkShape (..), ShapeMapEntry (..),
                                      LineType (..), lineTypeDash, lineTypeForIndex,
                                      ReferenceLine (..), Resolver,
                                      Position (..), Coord (..),
                                      FacetScales (..), freeScaleX, freeScaleY,
                                      FacetSpace (..), freeSpaceX, freeSpaceY,
                                      ThemeOverride (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axShowGrid,
                                      FontSpec (..),
                                      colRefName, resolveCol, resolveNum)
import           Data.Maybe          (mapMaybe, isJust)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Graphics.Hgg.Spec
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)

import           Graphics.Hgg.Primitive
import           Graphics.Hgg.Render.Common
import           Graphics.Hgg.Render.EdgeRoute (EdgeRoute (..), routeEdge,
                                      spreadPorts,
                                      Obstacles, dagObstacles,
                                      plateBoxPt,
                                      edgePortPoint, nodeExtent, nodeShowsDist,
                                      dagLabelFs)


-- | [日本語]: pt 空間への写像 @toScreen@ (= node 実寸から graphviz 風自然
--   アスペクトを算出)。 render と routing bake で共有する純関数。area 非依存。
--     * LayoutHierarchical: dnX = raw point x、 dnY = rank index。 x は 1:1、
--       y = rank index × rankPitch (= maxNodeH + ranksep)。 = 完全忠実 point pipeline。
--     * LayoutManual: dnX/dnY は正規化 [0,1]²。 各 rank 内の最小 x gap から横潰れしない
--       wpt を逆算し wpt/hpt で point 空間へ展開 (graphviz 風)。
--   最終 'fitPrimsToArea' が両経路ともアスペクト保持で area へ一様 fit。
--   [English]: Maps into pt space via @toScreen@ (computes a graphviz-style
--   natural aspect ratio from each node's actual size). A pure function
--   shared between rendering and routing bake; independent of area.
--     * LayoutHierarchical: dnX is the raw point x and dnY is the rank index.
--       x maps 1:1, and y = rank index times rankPitch (that is,
--       maxNodeH + ranksep) — a fully faithful point pipeline.
--     * LayoutManual: dnX/dnY are normalized to [0,1]^2. wpt is back-derived
--       from the minimum x gap within each rank so it does not collapse
--       horizontally, then expanded into pt space via wpt/hpt (graphviz-style).
--   The final 'fitPrimsToArea' uniformly fits either path into the area
--   while preserving the aspect ratio.
dagToScreen :: Double -> [DAGNode] -> DAGLayoutAlgorithm -> (Double -> Double -> Point)
dagToScreen radius nodes algo = toScreen
  where
    nodeW n = let (rx, _) = nodeExtent n radius in 2 * rx
    nodeH n = let (_, ry) = nodeExtent n radius in 2 * ry
    maxNodeW = maximum (2 * radius : map nodeW nodes)
    maxNodeH = maximum (2 * radius : map nodeH nodes)
    ranksep   = dagLabelFs * 2.0
    rankPitch = maxNodeH + ranksep
    isManual  = algo == LayoutManual
    nodesep   = maxNodeW * 0.25
    yLevels   = nub (sort (map dnY nodes))
    rankCount = length yLevels
    rowGaps   = [ g | lvl <- yLevels
                    , let xs = sort [ dnX n | n <- nodes, dnY n == lvl ]
                    , g <- zipWith (\a b -> b - a) xs (drop 1 xs)
                    , g > 1e-6 ]
    minGap    = if null rowGaps then 1 else minimum rowGaps
    wpt       = (maxNodeW + nodesep) / minGap
    hpt       = fromIntegral (max 1 (rankCount - 1)) * (maxNodeH + ranksep)
    toScreen x y
      | isManual  = Point (x * wpt) (y * hpt)
      | otherwise = Point x (y * rankPitch)

-- | [日本語]: layer の DAG edge に pt 空間 routing を焼き込む (= 'deRoute' 充填)。
--   'renderDAGStandalone' の edge routing pipeline (toScreen + obstacles + 並列 index +
--   routeEdge) と同一手順で計算するため、 baked route は live routing と byte-identical。
--   size は layer の 'lySize' (既定 径11mm)。 端点ノードが無い edge は 'Nothing' のまま。
--   結果 spec を JSON 化すると PS が同 routing を描ける (= HS/PS parity)。
--   [English]: Bakes pt-space routing into the layer's DAG edges (filling in
--   'deRoute'). Computed with the exact same steps as the edge routing
--   pipeline in 'renderDAGStandalone' (toScreen + obstacles + parallel index +
--   routeEdge), so the baked route is byte-identical to live routing. Size
--   comes from the layer's 'lySize' (default diameter 11 mm). An edge whose
--   endpoint node is missing is left as 'Nothing'. Once the resulting spec is
--   serialized to JSON, PS can draw the same routing (HS/PS parity).
dagBakeRoutes :: Layer -> Layer
dagBakeRoutes ly = case getLast (lyDAG ly) of
  Nothing -> ly
  Just (DAGSpec nodes es algo plates) ->
    let radius   = doubleOr (lySize ly) (mmPt 11) / 2
        toScreen = dagToScreen radius nodes algo
        nodeMap  = [ (dnId n, n) | n <- nodes ]
        lookup_ k = case [ n | (i, n) <- nodeMap, i == k ] of
          (n:_) -> Just n
          []    -> Nothing
        obs = dagObstacles toScreen radius nodes nodeMap plates es
        parTotal = foldl' (\m (DAGEdge f t _ _) -> Map.insertWith (+) (f, t) (1 :: Int) m)
                          Map.empty es
        -- 各 edge を (index, parIx) 付きで歩く (renderDAGStandalone と同順)。
        withIx = snd $ foldl'
          (\(seen, acc) e@(DAGEdge f t _ _) ->
              let k  = (f, t)
                  ix = Map.findWithDefault 0 k seen
              in (Map.insert k (ix + 1) seen, acc ++ [(e, ix)]))
          (Map.empty, []) es
        mkRoute (e@(DAGEdge f t _ _), ix) =
          case (lookup_ f, lookup_ t) of
            (Just from, Just to) ->
              let tot = Map.findWithDefault 1 (f, t) parTotal
              in Just (from, to, routeEdge toScreen obs from to (dePath e) radius ix tot)
            _ -> Nothing
        mrs = map mkRoute withIx
        -- ★ Phase 52 A6: 全 route 確定後に port 分散 post-pass (live 側と同順)
        rts = spreadPorts toScreen radius
                [ (f, t) | Just (f, t, _) <- mrs ]
                [ r | Just (_, _, r) <- mrs ]
        stitch ((e, _) : rest) (Nothing : ms) rs = e : stitch rest ms rs
        stitch ((e, _) : rest) (Just _ : ms) (r : rs) =
          e { deRoute = Just (edgeRouteToRouted r) } : stitch rest ms rs
        stitch _ _ _ = []
        es' = stitch withIx mrs rts
    in ly { lyDAG = Last (Just (DAGSpec nodes es' algo plates)) }

-- | [日本語]: 'Graphics.Hgg.Spec.VisualSpec' 内の全 DAG layer に 'dagBakeRoutes' を適用。
--   HS で図を生成し PS 用 JSON を吐く直前に呼ぶ (= routing を spec へ焼き込む境界)。
--   [English]: Applies 'dagBakeRoutes' to every DAG layer in a 'Graphics.Hgg.Spec.VisualSpec'.
--   Call this right before HS generates the figure and emits JSON for PS
--   (the boundary where routing gets baked into the spec).
bakeDAGRoutesInSpec :: VisualSpec -> VisualSpec
bakeDAGRoutesInSpec vs = vs { vsLayers = map dagBakeRoutes (vsLayers vs) }

-- | [日本語]: DAG 専用 (= axis 不要)。 0..1 domain 座標を area 内に直接 mapping。
--   描画順序: plate (= 背景) → edge → node。
--   [English]: DAG-only rendering (no axis needed). Maps 0..1 domain
--   coordinates directly into the area. Draw order: plate (background),
--   then edge, then node.
renderDAGStandalone :: Rect -> ThemePalette -> Layer -> [Primitive]
renderDAGStandalone area pal ly = case getLast (lyDAG ly) of
  Nothing -> []
  Just (DAGSpec nodes es algo plates) ->
    -- ★ Phase 39 A2-1: レイアウトは正規化 [0,1]² を返すが、 それを area の縦横へ
    -- 独立に引き伸ばすと rank 数が少ないとき縦に間延びする (アスペクト = area 比に
    -- 固定されてしまう)。 代わりに node の実寸 (pt) から graphviz 風の自然アスペクト
    -- (Wpt × Hpt) を算出して pt 空間へ写し、 最後に 'fitPrimsToArea' がアスペクトを
    -- 保ったまま area へフィット (= 縮小も拡大も) する。 これで rank 間隔/ノード間隔が
    -- ノード寸法に対し一定比になり間延びが解消する。
    let radius = doubleOr (lySize ly) (mmPt 11) / 2  -- ★ Phase 34 A3: size=直径 統一 (既定 径11mm=半径5.5mm)
        -- ★ Phase 42 sub B: pt 空間 toScreen は bake (dagBakeRoutes) と共有する純関数へ
        -- 抽出した。 toScreen の入力契約 (hierarchical=x 1:1/y=rank×pitch、 manual=wpt/hpt)
        -- は 'dagToScreen' を参照。
        toScreen = dagToScreen radius nodes algo
        nodeMap = [ (dnId n, n) | n <- nodes ]
        lookup_ k = case [n | (i, n) <- nodeMap, i == k] of
          (n:_) -> Just n
          []    -> Nothing
        platePrims = concatMap (renderPlate toScreen radius pal nodeMap plates) plates
        -- Phase 39 A-1: pt 空間の障害物 (node glyph box + plate box + A4 dummy lane)。
        -- long edge の box-channel + funnel routing に渡す。
        obs = dagObstacles toScreen radius nodes nodeMap plates es
        -- 並列 edge (= 同 (from, to) ペアが N 本) を検出。 各 edge に group 内 index と
        -- 総本数を付与して 'renderEdge' に渡し、 描画側で perpendicular にずらして
        -- 重なりを避ける (= graphviz dot 同等の表現)。
        parTotal = foldl' (\m (DAGEdge f t _ _) -> Map.insertWith (+) (f, t) (1 :: Int) m)
                          Map.empty es
        indexed  = snd $ foldl'
          (\(seen, acc) e@(DAGEdge f t _ _) ->
              let k  = (f, t)
                  ix = Map.findWithDefault 0 k seen
              in (Map.insert k (ix + 1) seen, acc ++ [(e, ix)]))
          (Map.empty, [])
          es
        -- ★ Phase 42 sub C: edge に baked 'deRoute' があればそれを描画 (HS/PS で同一)、
        -- 無ければ従来どおり live 'Graphics.Hgg.Render.EdgeRoute.routeEdge' で routing。 baked は同 pipeline 産なので
        -- HS 出力は byte-identical。
        -- ★ Phase 52 A6: 全 route 確定後に port 分散 ('spreadPorts')。 bake 済み route は
        -- bake 時に分散済 → cluster を成さず no-op (= 冪等) なので二重適用しない。
        edgeItems =
          [ (from, to, e, ix)
          | (e@(DAGEdge f t _ _), ix) <- indexed
          , Just from <- [lookup_ f]
          , Just to   <- [lookup_ t] ]
        rawRoutes =
          [ case deRoute e of
              Just re -> routedToEdgeRoute re
              Nothing ->
                let tot = Map.findWithDefault 1 (deFrom e, deTo e) parTotal
                in routeEdge toScreen obs from to (dePath e) radius ix tot
          | (from, to, e, ix) <- edgeItems ]
        routes = spreadPorts toScreen radius
                   [ (f, t) | (f, t, _, _) <- edgeItems ] rawRoutes
        edgePrims = concatMap (drawEdgeRoute pal) routes
        nodePrims = concatMap (renderNode toScreen radius pal) nodes
    -- Phase 39 A1: plate 枠 / ラベル / ノード / 矢印を含む実 bbox を求め、 指定 area
    -- 内に必ず収まるよう一括 scale+translate (graphviz `size`/`ratio=expand` 相当)。
    -- 縮小も拡大もし (一様スケール・中央寄せ)、 はみ出しゼロを最優先。
    in fitPrimsToArea area (platePrims <> edgePrims <> nodePrims)

-- | [日本語]: DAG プリミティブ全体の bounding box (xlo, ylo, xhi, yhi)。
--   'PText' は 'textWidthEm' × fontSize で幅、 fontSize で高さを見積もり anchor で
--   左右配分する (実フォント計測は不可ゆえ凡例/タイトルと同じ近似を流用)。
--   [English]: The bounding box (xlo, ylo, xhi, yhi) of the entire set of DAG
--   primitives. For 'PText', width is estimated as 'textWidthEm' times
--   fontSize and height as fontSize, then split left/right by anchor (actual
--   font measurement isn't available, so the same approximation used for the
--   legend/title is reused).
primsBBoxDAG :: [Primitive] -> Maybe (Double, Double, Double, Double)
primsBBoxDAG prims =
  case concatMap extents prims of
    []  -> Nothing
    bxs -> Just ( minimum [a | (a, _, _, _) <- bxs]
                , minimum [b | (_, b, _, _) <- bxs]
                , maximum [c | (_, _, c, _) <- bxs]
                , maximum [d | (_, _, _, d) <- bxs] )
  where
    segPts seg = case seg of
      MoveTo (Point x y) -> [(x, y)]
      LineTo (Point x y) -> [(x, y)]
      CurveTo (Point ax ay) (Point bx by) (Point cx cy) -> [(ax, ay), (bx, by), (cx, cy)]
      ClosePath          -> []
    boxOf ps = case ps of
      [] -> []
      _  -> let xs = map fst ps; ys = map snd ps
            in [(minimum xs, minimum ys, maximum xs, maximum ys)]
    extents p = case p of
      PLine (Point x1 y1) (Point x2 y2) _ ->
        [(min x1 x2, min y1 y2, max x1 x2, max y1 y2)]
      PRect (Rect x y w h) _ _ -> [(x, y, x + w, y + h)]
      PCircle (Point x y) rad _ _ _ -> [(x - rad, y - rad, x + rad, y + rad)]
      PPath segs _ _ -> boxOf (concatMap segPts segs)
      PText (Point x y) t ts ->
        let w   = textWidthEm t * tsSize ts
            asc = tsSize ts * 0.8
            dsc = tsSize ts * 0.25
            (xl, xr) = case tsAnchor ts of
              AnchorStart  -> (x, x + w)
              AnchorMiddle -> (x - w / 2, x + w / 2)
              AnchorEnd    -> (x - w, x)
        in [(xl, y - asc, xr, y + dsc)]
      _ -> []

-- | [日本語]: プリミティブ一式を指定 area 内に一括 scale+translate でフィット。
--   アスペクト比を保つ一様スケール (= min ratio・中央寄せ)。 figure が area より小さければ
--   拡大して余白を埋め (graphviz `ratio=expand` 相当)、 大きければ縮小する。 内側 pad を
--   取りストロークやラベル端が縁に触れないようにする。 フォント/線幅も s 倍。
--   [English]: Fits an entire set of primitives into the given area with a
--   single scale+translate. Uses a uniform, aspect-preserving scale (min
--   ratio, centered). If the figure is smaller than the area it is enlarged
--   to fill the space (equivalent to graphviz's `ratio=expand`); if larger,
--   it is shrunk. Takes an inner pad so strokes and label edges don't touch
--   the border. Font size and line width are scaled by the same factor s.
fitPrimsToArea :: Rect -> [Primitive] -> [Primitive]
fitPrimsToArea area prims = case primsBBoxDAG prims of
  Nothing -> prims
  Just (xlo, ylo, xhi, yhi) ->
    let w      = xhi - xlo
        h      = yhi - ylo
        pad    = 4
        availW = max 1 (rW area - 2 * pad)
        availH = max 1 (rH area - 2 * pad)
        s = if w <= 1e-9 || h <= 1e-9 then 1 else min (availW / w) (availH / h)
        newW = w * s
        newH = h * s
        tx = rX area + (rW area - newW) / 2 - xlo * s
        ty = rY area + (rH area - newH) / 2 - ylo * s
    in map (affinePrim s tx ty) prims

-- | [日本語]: x' = s·x + tx, y' = s·y + ty。 座標・半径・線幅・font size を一様に s 倍する
--   ('scalePrimitives' の dpi スケールに translate を加えた DAG fit 専用版)。
--   [English]: x' = s*x + tx, y' = s*y + ty. Uniformly scales coordinates,
--   radius, line width, and font size by s (a DAG-fit-only variant of
--   'scalePrimitives' with translation added to its dpi scaling).
affinePrim :: Double -> Double -> Double -> Primitive -> Primitive
affinePrim s tx ty = go
  where
    pt (Point x y)        = Point (s * x + tx) (s * y + ty)
    rect (Rect x y w h)   = Rect (s * x + tx) (s * y + ty) (s * w) (s * h)
    sst (StrokeStyle c w) = StrokeStyle c (w * s)
    sls (LineStyle c w d) = LineStyle c (w * s) (map (* s) d)
    sts t                 = t { tsSize = tsSize t * s }
    seg sg = case sg of
      MoveTo p      -> MoveTo (pt p)
      LineTo p      -> LineTo (pt p)
      CurveTo a b c -> CurveTo (pt a) (pt b) (pt c)
      ClosePath     -> ClosePath
    go p = case p of
      PLine a b ls           -> PLine (pt a) (pt b) (sls ls)
      PRect r fs mss         -> PRect (rect r) fs (fmap sst mss)
      PCircle c rad fs mss l -> PCircle (pt c) (rad * s) fs (fmap sst mss) l
      PPath segs fs mss      -> PPath (map seg segs) fs (fmap sst mss)
      PText q t ts           -> PText (pt q) t (sts ts)
      PClipPush r            -> PClipPush (rect r)
      PClipPop               -> PClipPop
      PTransformPush tr      -> PTransformPush tr
      PTransformPop          -> PTransformPop

-- | [日本語]: dePath で straight / spline 切替、 端点は node 形状との
--   正確な交点へ snap、 @parIx@ / @parCount@ で並列 edge の perpendicular bend を付与。
--
--     * @parCount@ = 1: 通常描画 (= bend 無し)
--     * @parCount@ > 1: 各 edge を ((parIx - (N-1)/2) * spacing) perpendicular ずらして
--       重ならない曲線群にする (= graphviz dot の parallel edge 表現)
--   [English]: Switches between straight and spline via dePath; endpoints are
--   snapped to the exact intersection with the node shape. @parIx@ /
--   @parCount@ add a perpendicular bend for parallel edges.
--
--     * @parCount@ = 1: normal drawing (no bend).
--     * @parCount@ > 1: each edge is offset perpendicular by
--       ((parIx - (N-1)/2) * spacing) so they form a set of non-overlapping
--       curves (the same representation graphviz dot uses for parallel edges).
renderEdge
  :: (Double -> Double -> Point)
  -> Obstacles                           -- ^ [日本語]: node + plate 障害物 (pt)。 [English]: Node and plate obstacles (pt).
  -> DAGNode -> DAGNode -> DAGEdge
  -> Double -> ThemePalette
  -> Int -> Int  -- ^ parIx, parCount
  -> [Primitive]
renderEdge toScreen obs from to e radius pal parIx parCount =
  -- Phase 39 B2 / 42 sub C: routing 幾何は baked 'deRoute' があればそれを使い、
  -- 無ければ live 'Graphics.Hgg.Render.EdgeRoute.routeEdge' (Render.EdgeRoute) で決定。 ここは制御点列 + style を
  -- ThemePalette 付きで描画 primitive へ落とすだけ。
  drawEdgeRoute pal $ case deRoute e of
    Just re -> routedToEdgeRoute re
    Nothing -> routeEdge toScreen obs from to (dePath e) radius parIx parCount

-- | [日本語]: 'EdgeRoute' (制御点列 + 形状) を描画 primitive へ。
--   [English]: Turns an 'EdgeRoute' (control points plus shape) into
--   rendering primitives.
drawEdgeRoute :: ThemePalette -> EdgeRoute -> [Primitive]
drawEdgeRoute pal route = case route of
  StraightArrow a b -> arrowEdgeFromPorts a b pal
  SplinePath pts    -> splineEdgeFromPorts pts pal
  -- 迂回経路は箱角 waypoint をそのまま通すため corner-cutting を掛けない
  -- (smoothInterior は棚緩和用で、 箱角を中央へ引き戻すと貫通する)。
  BezierPath pts    -> bezierThroughPorts pts pal
  -- R3 (Step6 P7a): graphviz Proutespline の cubic Bézier 制御点列 (始点 + 3 点ずつ)。
  CubicPath ctrl    -> cubicEdgeFromControls ctrl pal

-- | [日本語]: 焼き込んだ 'RoutedEdge' を 'EdgeRoute' へ復元 (pt 空間)。
--   [English]: Restores a baked 'RoutedEdge' back into an 'EdgeRoute' (pt space).
routedToEdgeRoute :: RoutedEdge -> EdgeRoute
routedToEdgeRoute (RoutedEdge k ps) =
  let pts = [ Point x y | (x, y) <- ps ]
  in case k of
       EShStraight -> case pts of
         (a : b : _) -> StraightArrow a b
         _           -> SplinePath pts   -- 退化時の安全側
       EShSpline   -> SplinePath pts
       EShBezier   -> BezierPath pts
       EShCubic    -> CubicPath pts

-- | [日本語]: 'EdgeRoute' を spec 焼き込み用 'RoutedEdge' (pt 空間) へ。
--   [English]: Converts an 'EdgeRoute' into a 'RoutedEdge' (pt space) for
--   baking into a spec.
edgeRouteToRouted :: EdgeRoute -> RoutedEdge
edgeRouteToRouted route = case route of
  StraightArrow a b -> RoutedEdge EShStraight (map p2 [a, b])
  SplinePath pts    -> RoutedEdge EShSpline (map p2 pts)
  BezierPath pts    -> RoutedEdge EShBezier (map p2 pts)
  CubicPath ctrl    -> RoutedEdge EShCubic (map p2 ctrl)
  where p2 (Point x y) = (x, y)

-- | [日本語]: 1 node を kind に応じた形状で描画 + label (+ 分布名 sub-label)。
--   ★ サイズは label に合わせ可変 ('Graphics.Hgg.Render.EdgeRoute.nodeExtent')。 形状は PyMC 慣例 = latent/observed は楕円、
--   deterministic/data/other は四角。 deterministic は name のみ (dist 非表示)。
--   [English]: Draws a single node with a kind-appropriate shape plus its
--   label (and an optional distribution sub-label). Size varies to fit the
--   label ('Graphics.Hgg.Render.EdgeRoute.nodeExtent'). Shape follows PyMC convention: latent/observed use
--   an ellipse, deterministic/data/other use a rectangle. A deterministic
--   node shows only its name (no distribution).
renderNode :: (Double -> Double -> Point) -> Double -> ThemePalette
           -> DAGNode -> [Primitive]
renderNode toScreen radius pal n =
  let Point cx cy = toScreen (dnX n) (dnY n)
      (rx, ry)    = nodeExtent n radius
      showDist    = nodeShowsDist n
      ts = mkFontTS Nothing pal TickF AnchorMiddle 0
      tsSmall = ts { tsSize = 9 }
      fill = case dnKind n of
        NodeLatent        -> "#ffffff"
        NodeObserved      -> "#cfcfcf"   -- 灰色 fill = observed (PyMC 慣例)
        NodeDeterministic -> "#ffffff"   -- 白四角 = deterministic (PyMC 慣例)
        NodeData          -> "#cfcfcf"   -- 灰 = ConstantData (PyMC 慣例)
        NodeOther         -> "#dddddd"
      stroke_ = StrokeStyle (tpAxis pal) 1.2
      -- shape: latent/observed = 楕円、 deterministic/data/other = 四角 (PyMC 慣例)
      ellipseShape = PPath (ellipsePath cx cy rx ry) (FillStyle fill 1.0) (Just stroke_)
      rectShape    = PRect (Rect (cx - rx) (cy - ry) (2 * rx) (2 * ry))
                           (FillStyle fill 1.0) (Just stroke_)
      shape = case dnKind n of
        NodeLatent        -> ellipseShape
        NodeObserved      -> ellipseShape
        NodeDeterministic -> rectShape
        NodeData          -> rectShape
        NodeOther         -> rectShape
      -- label: dist を持つ stochastic は 3 行 (name / ~ / dist)、 それ以外は name のみ 1 行。
      -- Phase 39 A2-7: 行を cy 中心に対称配置。 baseAdj は baseline→glyph 視覚中心の
      -- 補正 (≈ 0.34×fs)。 旧 (-8/+4/+16) は補正なしで block が約 2px 下寄りだった。
      baseAdj  = tsSize ts * 0.34
      lineGap  = 12
      textPrims = case dnDist n of
        Just dist | showDist ->
          [ PText (Point cx (cy - lineGap + baseAdj)) (dnLabel n) ts
          , PText (Point cx (cy + baseAdj))           (T.pack "~") tsSmall
          , PText (Point cx (cy + lineGap + baseAdj)) dist tsSmall
          ]
        _ ->
          [ PText (Point cx (cy + baseAdj)) (dnLabel n) ts ]
  in shape : textPrims

-- | [日本語]: 楕円を Bezier 近似で path に。
--   [English]: Turns an ellipse into a path via Bezier approximation.
ellipsePath :: Double -> Double -> Double -> Double -> [PathSegment]
ellipsePath cx cy rx ry =
  let k = 0.5522847498  -- magic for circle approximation
      kx = rx * k
      ky = ry * k
  in [ MoveTo  (Point (cx - rx) cy)
     , CurveTo (Point (cx - rx) (cy - ky)) (Point (cx - kx) (cy - ry)) (Point cx (cy - ry))
     , CurveTo (Point (cx + kx) (cy - ry)) (Point (cx + rx) (cy - ky)) (Point (cx + rx) cy)
     , CurveTo (Point (cx + rx) (cy + ky)) (Point (cx + kx) (cy + ry)) (Point cx (cy + ry))
     , CurveTo (Point (cx - kx) (cy + ry)) (Point (cx - rx) (cy + ky)) (Point (cx - rx) cy)
     , ClosePath
     ]

-- | [日本語]: plate を node 群の bounding box + label で描画。
--
--   bbox はノード中心でなく __glyph bbox (中心 ± 'Graphics.Hgg.Render.EdgeRoute.nodeExtent')__。
--   固定 pad (radius*1.6) だと label の長いノード (rx > pad) が plate の
--   水平端で枠を超える (実測で確定)。
--   [English]: Draws a plate as the bounding box of its node group plus a
--   label.
--
--   The bbox is not the node centers but the
--   __glyph bbox (center plus/minus 'Graphics.Hgg.Render.EdgeRoute.nodeExtent')__. With a fixed pad
--   (radius*1.6), a node with a long label (rx > pad) would overflow the
--   plate's horizontal edge (confirmed by measurement).
renderPlate :: (Double -> Double -> Point) -> Double -> ThemePalette
            -> [(Text, DAGNode)] -> [DAGPlate] -> DAGPlate -> [Primitive]
renderPlate toScreen radius pal nodeMap allPlates plate =
  case plateBoxPt toScreen radius nodeMap allPlates plate of
    Nothing -> []
    Just (xlo, boxTop, xhi, yhi) ->
      let rw = xhi - xlo
          -- label を枠の **下端・右寄せ** に置く (graphviz labelloc=b labeljust=r 同型)。
          -- label 帯は 'Graphics.Hgg.Render.EdgeRoute.plateBoxPt' が box 下端に labelH ぶん確保済。
          rh = yhi - boxTop
          labelTS = mkFontTS Nothing pal LegendItemF AnchorEnd 0
      in [ PRect (Rect xlo boxTop rw rh)
                 (FillStyle "#ffffff" 0)
                 (Just (StrokeStyle (tpAxis pal) 1.0))
         , PText (Point (xhi - 5) (yhi - 4))
                 (dpLabel plate) labelTS
         ]

-- | [日本語]: 端点が既に node 形状端に snap 済の前提で直線 + 矢印ヘッド描画。
--   [English]: Draws a straight line plus arrowhead, assuming the endpoints
--   are already snapped to the node shape's edge.
arrowEdgeFromPorts :: Point -> Point -> ThemePalette -> [Primitive]
arrowEdgeFromPorts (Point sx sy) (Point ex ey) pal =
  let dx = ex - sx; dy = ey - sy
      len = sqrt (dx * dx + dy * dy)
      ux = if len > 0 then dx / len else 0
      uy = if len > 0 then dy / len else 0
      -- 矢じり: graphviz 較正 (長 10 × 底辺 7 = headWid*2)。 旧 headWid=5 は底辺 10 で
      -- graphviz より太かった (Phase1A5 の未較正定数・Phase 39 で較正)。
      headLen = 10.0
      headWid = 3.5
      px = -uy; py = ux
      bx = ex - ux * headLen
      by = ey - uy * headLen
      -- ★ Phase 44.8: 線は **鏃の底辺 (bx,by) で止める** (tip まで引かない)。 線を tip
      -- まで引くと塗り三角の内部を線が貫通し「線＋三角を重ねた」見た目になり矢印に
      -- 見えない (ユーザ指摘 2026-06-26)。 graphviz も線を鏃底辺で止め tip は鏃が担う。
      line_ = PLine (Point sx sy) (Point bx by) (solid (tpAxis pal) 1.5)
      h1 = Point (bx + px * headWid) (by + py * headWid)
      h2 = Point (bx - px * headWid) (by - py * headWid)
      headPath = PPath
        [ MoveTo (Point ex ey), LineTo h1, LineTo h2, ClosePath ]
        (FillStyle (tpAxis pal) 1.0) Nothing
  in [line_, headPath]

-- | [日本語]: 始終点 snap 済 control 点列を Catmull-Rom spline + 矢印で描画。
--   中間制御点には corner-cutting smoothing (= 内部点を隣接 3 点の (1,2,1)/4 平均で置換) を
--   2 pass 適用してから Catmull-Rom に渡す。 これで dummy 経由の「棚 / 2 山」 を緩和し、
--   真の B-spline に近い視覚を直線パスのまま得る。 端点は保持されるので port snap は崩れない。
--
--   ただし内部点が __1 個だけ__ (= 制御点 3 個、 dummy 1 個の短い skip)
--   の場合は smoothing を掛けない。 2-pass 平均は唯一の内部点を始終点の中点へ強く
--   引き戻すため、 routeLongEdgeDummies が plate 箱の外へ出した bulge が潰れて edge が
--   箱へ再侵入してしまう。 棚は内部点 2 個以上 (長い chain) でしか生じないので、
--   短い chain では bulge をそのまま活かす。
--   [English]: Draws a Catmull-Rom spline plus arrowhead from a control-point
--   list whose start/end points are already snapped. Applies two passes of
--   corner-cutting smoothing to the interior control points (each interior
--   point is replaced by the (1,2,1)/4 average of itself and its two
--   neighbors) before handing them to Catmull-Rom. This softens the
--   "shelf / double-hump" artifact that comes from routing through dummy
--   points, yielding a look close to a true B-spline while staying a
--   straight-line path. Endpoints are preserved, so port snapping is not
--   disturbed.
--
--   However, when there is only __one__ interior point (three control points
--   total — a short skip with a single dummy), smoothing is skipped. The
--   two-pass average would pull that lone interior point strongly toward the
--   midpoint of the start and end, collapsing the bulge that
--   routeLongEdgeDummies pushed outside the plate box and letting the edge
--   re-enter the box. The shelf artifact only occurs with two or more
--   interior points (a long chain), so short chains keep the bulge as-is.
-- | [日本語]: 制御点列を __そのまま__ Catmull-Rom で通す (= 平滑化なし)。
--   箱角 waypoint を中央へ引き戻さないため、 迂回経路の描画に使う。
--   [English]: Passes the control-point list straight through Catmull-Rom,
--   unchanged (no smoothing). Used for drawing detour routes, since it must
--   not pull box-corner waypoints back toward the center.
bezierThroughPorts :: [Point] -> ThemePalette -> [Primitive]
bezierThroughPorts = drawCatmullRom

splineEdgeFromPorts :: [Point] -> ThemePalette -> [Primitive]
splineEdgeFromPorts ptsRaw pal =
  let pts = if length ptsRaw >= 4 then smoothInterior 2 ptsRaw else ptsRaw
  in drawCatmullRom pts pal

-- | [日本語]: Catmull-Rom spline + 矢印ヘッドを制御点列から描画 (平滑化は呼出側責務)。
--   [English]: Draws a Catmull-Rom spline plus arrowhead from a control-point
--   list (smoothing is the caller's responsibility).
drawCatmullRom :: [Point] -> ThemePalette -> [Primitive]
drawCatmullRom pts pal =
  let n = length pts
  in if n < 2 then [] else
  let p0 = head pts
      (trimmed, basePt, apex, u) = trimLastCubic dagHeadLen p0 (catmullRomToBezier pts)
      edgePath = PPath (MoveTo p0 : trimmed)
                       (FillStyle "#000000" 0)
                       (Just (StrokeStyle (tpAxis pal) 1.5))
  in [edgePath, arrowHeadPrim basePt apex u pal]

-- | [日本語]: 矢じり寸法 (graphviz 較正: 長 10 × 底辺 7 = headWid*2)。 全 edge 描画で共有。
--   [English]: Arrowhead dimensions (calibrated against graphviz: length 10,
--   base 7 = headWid*2). Shared across all edge rendering.
dagHeadLen, dagHeadWid :: Double
dagHeadLen = 10.0
dagHeadWid = 3.5

-- | [日本語]: 鏃 (塗り三角) primitive。 base = 底辺中心 (= 線の終端・曲線上)、 apex = 元終点
--   (= ノード port = tip)、 u = tip 方向単位ベクトル。
--   [English]: The arrowhead (filled triangle) primitive. base is the center
--   of its base edge (the line's endpoint, on the curve), apex is the
--   original endpoint (the node port, i.e. the tip), and u is the unit
--   vector pointing toward the tip.
arrowHeadPrim :: Point -> Point -> (Double, Double) -> ThemePalette -> Primitive
arrowHeadPrim (Point bx by) apex (ux, uy) pal =
  let (perpx, perpy) = (-uy, ux)
      h1 = Point (bx + perpx * dagHeadWid) (by + perpy * dagHeadWid)
      h2 = Point (bx - perpx * dagHeadWid) (by - perpy * dagHeadWid)
  in PPath [ MoveTo apex, LineTo h1, LineTo h2, ClosePath ]
           (FillStyle (tpAxis pal) 1.0) Nothing

-- | [日本語]: 描画パス末尾の cubic セグメントを __終点側へ弧長 ~headLen 分 de Casteljau 分割__し、
--   線を曲線上の base 点で滑らかに止める (= 鏃が tip を担う)。 終点だけ差し替えると
--   制御点据え置きで曲線が変形し base で折れるため、 正しく分割して曲線形状を保つ
--   (graphviz の arrow clip と同型)。 戻り = (分割後セグ列, base 点(曲線上),
--   apex(=元終点), 単位 tip 方向)。
--   [English]: Splits the last cubic segment of the drawn path via de
--   Casteljau, __trimming arc-length ~headLen back from the endpoint__, so
--   the line stops smoothly at a base point on the curve (the arrowhead
--   then covers the tip). Simply replacing the endpoint would deform the
--   curve while leaving the control points fixed, producing a kink at base;
--   splitting it properly preserves the curve shape (the same technique as
--   graphviz's arrow clip). Returns (the split segment list, the base point
--   on the curve, the apex — the original endpoint, and the unit tip
--   direction).
trimLastCubic
  :: Double -> Point -> [PathSegment]
  -> ([PathSegment], Point, Point, (Double, Double))
trimLastCubic headLen p0 segs = case reverse segs of
  (CurveTo c1 c2 e : restR) ->
    let ini      = reverse restR
        segStart = if null ini then p0 else segEndOf (last ini)
        cub      = (segStart, c1, c2, e)
        t        = trimParamForLen headLen cub
        unit m   = let Point mx my = m; Point ex ey = e
                       dx = ex - mx; dy = ey - my
                       l = sqrt (dx * dx + dy * dy)
                   in if l > 1e-9 then (dx / l, dy / l) else (0, 1)
    in if t <= 1e-6
         then (ini, segStart, e, unit segStart)       -- 末尾セグ全体が鏃より短い
         else let (_, a, d, m) = splitCubicLeft t cub
              in (ini ++ [CurveTo a d m], m, e, unit m)
  _ -> (segs, p0, p0, (0, 1))
  where
    segEndOf (LineTo q)      = q
    segEndOf (CurveTo _ _ q) = q
    segEndOf (MoveTo q)      = q
    segEndOf ClosePath       = p0

-- | [日本語]: cubic (p0,c1,c2,p3) を媒介変数 t で de Casteljau 分割し、 左半分の制御点を返す。
--   [English]: Splits the cubic (p0,c1,c2,p3) at parameter t via de
--   Casteljau, returning the control points of the left half.
splitCubicLeft :: Double -> (Point, Point, Point, Point) -> (Point, Point, Point, Point)
splitCubicLeft t (p0, c1, c2, p3) =
  let lp (Point ax ay) (Point bx by) = Point (ax + (bx - ax) * t) (ay + (by - ay) * t)
      a = lp p0 c1; b = lp c1 c2; c = lp c2 p3
      d = lp a b;   e = lp b c
      m = lp d e
  in (p0, a, d, m)

-- | [日本語]: cubic 上の点 B(t)。
--   [English]: The point B(t) on the cubic.
cubicAt :: Double -> (Point, Point, Point, Point) -> Point
cubicAt t cub = let (_, _, _, m) = splitCubicLeft t cub in m

-- | [日本語]: 終点 p3 から弧長 ~target だけ手前の媒介変数 t を二分法で求める
--   (chord 近似)。 距離 |B(t) - p3| は t→1 で 0 へ単調減少する。 末尾セグ全長が
--   target 未満なら 0 を返す。
--   [English]: Binary-searches for the parameter t that is arc-length ~target
--   back from the endpoint p3 (chord approximation). The distance |B(t) -
--   p3| decreases monotonically to 0 as t approaches 1. Returns 0 if the
--   entire final segment is shorter than target.
trimParamForLen :: Double -> (Point, Point, Point, Point) -> Double
trimParamForLen target cub@(_, _, _, p3) =
  let dist t = let Point mx my = cubicAt t cub; Point px py = p3
               in sqrt ((px - mx) ** 2 + (py - my) ** 2)
      go lo hi 0 = (lo + hi) / 2
      go lo hi k = let mid = (lo + hi) / 2
                   in if dist mid > target then go mid hi (k - 1 :: Int)
                                           else go lo mid (k - 1)
  in if dist 0 <= target then 0 else go 0 1 32

-- | [日本語]: graphviz Proutespline の制御点列 ([始点, c1, c2, 終点, c1, c2, ...])
--   を cubic Bézier path + 矢印で描画。 矢印方向は最終 segment の (c2→終点) 接線。
--   [English]: Draws graphviz's Proutespline control-point list
--   ([start, c1, c2, end, c1, c2, ...]) as a cubic Bezier path plus
--   arrowhead. The arrow direction is the tangent of the final segment's
--   (c2 to end) leg.
cubicEdgeFromControls :: [Point] -> ThemePalette -> [Primitive]
cubicEdgeFromControls ctrl pal
  | length ctrl < 4 = case ctrl of
      [a, b] -> arrowEdgeFromPorts a b pal
      _      -> []
  | otherwise =
      let p0        = head ctrl
          segs      = chunk3 (tail ctrl)
          curveSegs = [ CurveTo c1 c2 e | [c1, c2, e] <- segs ]
          -- ★ Phase 44.8: 末尾 cubic を de Casteljau で弧長 ~headLen 分手前へ分割し
          -- 線を曲線上の base 点で止める (折れ無し)。 tip は鏃が担う。
          (trimmed, basePt, apex, u) = trimLastCubic dagHeadLen p0 curveSegs
          edgePath  = PPath (MoveTo p0 : trimmed)
                            (FillStyle "#000000" 0)
                            (Just (StrokeStyle (tpAxis pal) 1.5))
      in [edgePath, arrowHeadPrim basePt apex u pal]
  where
    chunk3 (a : b : c : rest) = [a, b, c] : chunk3 rest
    chunk3 _                  = []

-- | [日本語]: Corner-cutting smoothing: 内部点 P[i] (i ∉ {0, n-1}) を
--   (P[i-1] + 2 P[i] + P[i+1]) / 4 で置換し @k@ 回繰り返す。 端点は不変。
--   'splineEdgeFromPorts' で dummy 経由制御点列の「棚」 を緩和するために使う。
--   [English]: Corner-cutting smoothing: replaces each interior point P[i]
--   (i not in {0, n-1}) with (P[i-1] + 2 P[i] + P[i+1]) / 4, repeated @k@
--   times. Endpoints are left unchanged. Used by 'splineEdgeFromPorts' to
--   soften the "shelf" artifact of dummy-routed control-point lists.
smoothInterior :: Int -> [Point] -> [Point]
smoothInterior k ps
  | k <= 0 || length ps < 3 = ps
  | otherwise = smoothInterior (k - 1) (onePass ps)
  where
    avg3 (Point ax ay) (Point bx by) (Point cx cy) =
      Point ((ax + 2 * bx + cx) / 4) ((ay + 2 * by + cy) / 4)
    onePass xs =
      let middle = zipWith3 avg3 xs (drop 1 xs) (drop 2 xs)
      in head xs : middle ++ [last xs]

-- | [日本語]: Catmull-Rom control 列 → cubic Bezier segments。 端点は ghost (= 自分自身)
--   で扱う (= natural spline、 端で直線に近づく)。
--
--   ★ 制御点オフセットを __セグメント長でクランプ__ する。
--   knot 間隔が極端に不均一だと (= 例: 迂回 waypoint 不足で長 edge が 3 点になる場合)、
--   tangent (b-prev)/6 が遠い prev に引っ張られ制御点が segment 外へ大きく overshoot し、
--   末端に「フック」が出ていた。 均等間隔での標準オフセットは segLen/3 ゆえ上限 0.5·segLen
--   なら通常曲線は不変、 過大時のみ抑制される (graphviz が box 内拘束で防ぐのと同趣旨)。
--   [English]: Converts a Catmull-Rom control-point list into cubic Bezier
--   segments. Endpoints are handled as their own ghost points (a natural
--   spline that approaches a straight line at the ends).
--
--   __Clamps the control-point offset to the segment length__. When knot
--   spacing is extremely uneven (for example, a long edge reduced to three
--   points due to insufficient detour waypoints), the tangent (b-prev)/6
--   gets pulled by a distant prev and the control point overshoots far
--   outside the segment, producing a "hook" at the end. Since the standard
--   offset for evenly spaced knots is segLen/3, clamping at 0.5*segLen
--   leaves ordinary curves unchanged and only suppresses the excessive case
--   (the same idea graphviz uses when it constrains points inside the box).
catmullRomToBezier :: [Point] -> [PathSegment]
catmullRomToBezier ps = go (0 :: Int) ps
  where
    n = length ps
    at i = ps !! i
    -- ベクトル (dx,dy) を最大長 maxL にクランプ。
    clampLen maxL dx dy =
      let l = sqrt (dx * dx + dy * dy)
      in if l > maxL && l > 1e-12 then (dx * maxL / l, dy * maxL / l) else (dx, dy)
    go _ [_]            = []
    go _ []             = []
    go i (a : b : rest) =
      let prev = if i == 0      then a else at (i - 1)
          next = case rest of
                   (c : _) -> c
                   []      -> b
          Point ax ay = a
          Point bx by = b
          Point px py = prev
          Point qx qy = next
          segLen   = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
          maxOff   = 0.5 * segLen
          (o1x, o1y) = clampLen maxOff ((bx - px) / 6) ((by - py) / 6)
          (o2x, o2y) = clampLen maxOff ((qx - ax) / 6) ((qy - ay) / 6)
          c1 = Point (ax + o1x) (ay + o1y)
          c2 = Point (bx - o2x) (by - o2y)
      in CurveTo c1 c2 b : go (i + 1) (b : rest)
      where
        _unused = n  -- silence unused if any

-- | [日本語]: geom_text / geom_label。 各 (x,y) 点に lyLabel 列の文字を描く。
--   withBox=True (= geom_label) は文字の背後に角丸風の矩形を敷く。 色は static color
--   指定 (= 固定色 color) があればそれ、 無ければ tpText。 font サイズは lySize (default 11)。
--   [English]: geom_text / geom_label. Draws the text from the lyLabel column
--   at each (x,y) point. withBox=True (geom_label) lays a rounded-rect-like
--   background behind the text. Color uses a static color (a fixed color) if
--   one is specified, otherwise falls back to tpText. Font size is lySize
--   (default 11).
renderText :: Resolver -> Layout -> ThemePalette -> Layer -> Bool -> [Primitive]
renderText r layout pal ly withBox =
  let xs   = V.toList (vecOr (lyEncX ly) r)
      ys   = V.toList (vecOr (lyEncY ly) r)
      labs = case getLast (lyLabel ly) of
        Just cr -> case resolveCol r cr of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> V.toList (V.map numToText v)
          Nothing          -> []
        Nothing -> []
      n  = minimum [length xs, length ys, length labs]
      -- 固定色 (color) 明示時のみ採用 (= geom_text(color=))、 それ以外は theme text 色。
      txtCol = case getLast (lyColor ly) of
        Just (ColorStatic c) -> c
        _                    -> tpText pal
      fontSz = doubleOr (lySize ly) 11
      coord  = lpCoord layout
      ts = TextStyle txtCol fontSz "sans-serif" AnchorMiddle 0 "normal" False
      mkOne i =
        let Point px py = projectPoint coord layout (xs !! i) (ys !! i)
            lab = labs !! i
            -- geom_label の背景矩形 (文字幅を 0.6×fontSize×文字数 で概算 + padding)。
            boxW = fontSz * 0.6 * fromIntegral (T.length lab) + 8
            boxH = fontSz + 6
            box = [ PRect (Rect (px - boxW / 2) (py - boxH / 2) boxW boxH)
                          (FillStyle (tpBackground pal) 0.9)
                          (Just (StrokeStyle (tpAxis pal) 0.5)) ]
            -- 文字 baseline を矩形中央に合わせる (= py + fontSize*0.35)。
            textP = [ PText (Point px (py + fontSz * 0.35)) lab ts ]
        in (if withBox then box else []) <> textP
  in if n <= 0 then [] else concatMap mkOne [0 .. n - 1]

-- | [日本語]: HBM ModelGraph DAG 描画。
--   node 位置 (dnX, dnY) は domain 座標として scale 適用、 node = PCircle +
--   PText、 edge = PLine。 layout 計算は外部 (= hanalyze / frontend) で。
--   [English]: Draws an HBM ModelGraph DAG. Node positions (dnX, dnY) are
--   scaled as domain coordinates; a node becomes PCircle + PText, an edge
--   becomes PLine. Layout computation happens externally (in hanalyze
--   / the frontend).
-- | [日本語]: embedded DAG (= MDAG レイヤを他 geom と同一軸に重ねた退化ケース)。
--   ★ 旧実装は node 座標を [0,1] に潰す @nrm@ shim + 軸 scale で
--   直線のみ (矢印/plate 箱/迂回 routing 無し) を描く間に合わせだった。 本格
--   'renderDAGStandalone' (矢印・plate・routing・fit 完備) が landing 済のため、
--   shim を撤去して standalone を panel 矩形 ('lpPlotArea') 上で呼ぶ委譲に統一する。
--   これで mixed ケースでも DAG 専用経路 (renderDAGOnly) と同一品質で描画される。
--   [English]: An embedded DAG (the degenerate case where an MDAG layer is
--   overlaid on the same axes as other geoms).
--   The old implementation was a stopgap that collapsed node coordinates
--   into [0,1] via an @nrm@ shim plus axis scaling, drawing only straight
--   lines (no arrowheads, plate boxes, or detour routing). Now that the
--   full-featured 'renderDAGStandalone' (arrowheads, plates, routing, and
--   fitting all included) has landed, the shim is removed and delegation is
--   unified to call standalone on the panel rectangle ('lpPlotArea'). As a
--   result, the mixed case now renders at the same quality as the dedicated
--   DAG path (renderDAGOnly).
renderDAG :: Layout -> ThemePalette -> Layer -> [Primitive]
renderDAG layout = renderDAGStandalone (lpPlotArea layout)

-- | [日本語]: parallel coordinates。 lyHover で渡された N 列を
--   等間隔の縦軸として並べ、 row 毎に折線を引く。 placeholder 実装: data の
--   各列を [0, 1] に正規化、 polyline で描画。
--   [English]: Parallel coordinates. Lays out the N columns passed via
--   lyHover as equally spaced vertical axes and draws a polyline per row.
--   Placeholder implementation: normalizes each column of data to [0, 1]
--   and draws it as a polyline.
renderParallel :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderParallel r layout pal ly =
  let cols = lyHover ly
      nCols = length cols
      a = lpPlotArea layout
      vecs = [V.toList (vecOr (Last (Just c)) r) | c <- cols]
      n = case vecs of
        [] -> 0
        _  -> minimum (map length vecs)
  in if nCols < 2 || n == 0 then [] else
    let -- 各 column の min/max
        extents = [ (minimum xs, maximum xs) | xs <- vecs, not (null xs) ]
        norm vx (lo, hi) = if hi <= lo then 0.5 else (vx - lo) / (hi - lo)
        -- 軸 x 座標
        axisX i = rX a + rW a * fromIntegral i / fromIntegral (nCols - 1)
        -- 軸名 (inline 列は名前を持たないので placeholder のときは空 = ラベル無し)
        labelTextOf i = let nm = colRefName (cols !! i)
                        in if nm == "<inline-num>" || nm == "<inline-txt>" then "" else nm
        anyLabel = any (\i -> labelTextOf i /= "") [0 .. nCols - 1]
        -- ラベルがあるときだけ上端に帯を確保 (= タイトルと被らないよう領域内に置く)。
        -- ラベルが無ければ topPad=0 で詰める。
        topPad = if anyLabel then 16 else 0
        yTop = rY a + topPad
        -- 値 → 画面 y (反転: 0 が下、 1 が上)。 データは yTop..下端 に収める。
        valY ny = yTop + (rH a - topPad) * (1 - ny)
        -- 軸を縦線で描画 (データ域 yTop..下端)
        axisLines = [ PLine (Point (axisX i) yTop) (Point (axisX i) (rY a + rH a))
                            (solid (tpAxis pal) 1.0)
                    | i <- [0 .. nCols - 1] ]
        -- 各 row の折線
        rowSegs row =
          let pts = [ Point (axisX i) (valY (norm (vecs !! i !! row) (extents !! i)))
                    | i <- [0 .. nCols - 1] ]
              pairs = zip pts (drop 1 pts)
              c = tpDefault pal
          in [ PLine pa pb (solid c 0.6) | (pa, pb) <- pairs ]
        -- col label (領域内上端・空ラベルは描かない)
        labels_ =
          [ PText (Point (axisX i) (yTop - 4))
                  (labelTextOf i)
                  (mkFontTS Nothing pal TickF AnchorMiddle 0)
          | i <- [0 .. nCols - 1], labelTextOf i /= "" ]
    in axisLines <> concatMap rowSegs [0 .. n - 1] <> labels_

-- | [日本語]: Pie chart: lyEncX = categorical labels、 lyEncY = values。
--   plotArea 中央に円描画、 各 slice は categorical palette で着色。 軸 / tick 非表示前提。
--   [English]: Pie chart: lyEncX gives the categorical labels, lyEncY gives
--   the values. Draws the circle centered in the plotArea, coloring each
--   slice from the categorical palette. Assumes axes and ticks are hidden.
--
--   [日本語]: __投影層 (projectXY 等) を通さず自前で cos/sin を回すのは意図的__。
--   pie はそもそも極座標専用の図で、 cross 軸 / value 軸という直交の役割分担を
--   持たない (角度が値そのもの)。 座標系の切替対象ではないため投影層に載せる
--   意味が薄い。
--   [English]: __Deliberately computes cos/sin itself rather than going through
--   the projection layer__ (projectXY and friends). A pie is inherently a
--   polar-only chart with no cross-axis / value-axis split — the angle __is__
--   the value — so it is not a target for coordinate switching and would gain
--   nothing from the projection layer.
renderPie :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderPie r layout thePal ly =
  let area    = lpPlotArea layout
      values  = V.toList (vecOr (lyEncY ly) r)
      labels  = catLabelsOf r ly
      total   = sum values
      cx      = rX area + rW area / 2
      cy      = rY area + rH area / 2
      radius  = min (rW area) (rH area) * 0.4
      pal     = lpCategoricalPalette layout
      a       = doubleOr (lyAlpha ly) 0.9
      tsLabel = mkFontTS Nothing thePal TickF AnchorMiddle 0
      -- 各 slice を path (= moveTo center + arc approximation by 60 line segs + close)
      mkSlice i (v, lbl) startA =
        let endA = startA + 2 * pi * v / max total 1
            segments = 60 :: Int
            sample k = let t = startA + (endA - startA) * fromIntegral k / fromIntegral segments
                       in Point (cx + radius * cos t) (cy + radius * sin t)
            color = pal !! (i `mod` length pal)
            slicePath = PPath
              ( MoveTo (Point cx cy)
              : LineTo (sample 0)
              : [ LineTo (sample k) | k <- [1 .. segments] ]
              ++ [ClosePath]
              )
              (FillStyle color a) (Just (StrokeStyle "#ffffff" 1.0))
            -- Phase 8 B1: 扇の重心方向 (= 半径 0.7) に「項目名 (n%)」 ラベル (PS と同型)
            midA = (startA + endA) / 2
            lx   = cx + radius * 0.7 * cos midA
            ly_  = cy + radius * 0.7 * sin midA
            pct  = if total > 0 then v / total * 100 else 0
            lblTxt = if T.null lbl then "" else lbl <> " (" <> numToText pct <> "%)"
            labelPrim = [ PText (Point lx ly_) lblTxt tsLabel | not (T.null lblTxt) ]
        in slicePath : labelPrim
      n = length values
      paddedLabels = labels ++ replicate (n - length labels) ""
      starts = scanl (\acc v -> acc + 2 * pi * v / max total 1) (-pi / 2) values
  in concat [ mkSlice i (v, lbl) s
            | (i, (v, lbl, s)) <- zip [0..] (zip3 values paddedLabels starts) ]

-- | [日本語]: Waterfall chart: lyEncX = categorical labels、 lyEncY = delta values。
--   各 bar は前 bar の累積値から start、 + delta だけ移動。
--   正 = positive 色、 負 = negative 色。
--   [English]: Waterfall chart: lyEncX gives the categorical labels, lyEncY
--   gives the delta values. Each bar starts from the previous bar's
--   cumulative value and moves by + delta. Positive deltas use the positive
--   color, negative deltas use the negative color.
renderWaterfall :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderWaterfall r layout pal ly =
  let xCats = lpXCategoryLabels layout
      isCat = not (null xCats)
      deltas = V.toList (vecOr (lyEncY ly) r)
      n = length deltas
      -- 累積位置 (= 前 bar 終了 = 次 bar 開始)
      cumulative = scanl (+) 0 deltas
      -- bar 配置: x position は 0..n-1
      a   = doubleOr (lyAlpha ly) 0.85
      posC = "#16a34a"  -- 緑 (positive)
      negC = "#dc2626"  -- 赤 (negative)
      sx = scaleApply (lpXScale layout)
      sy = scaleApply (lpYScale layout)
      area = lpPlotArea layout
      -- Phase 8 A2 Step4c: bar 幅 = 1 スロット (unit = sx 1 - sx 0) の 0.6。 旧 rW/n (Total
      -- スロットを数えず個数ベース) を unit ベースに。 PS renderWaterfallLayer と同値に統一
      -- (従来 HS=rW/n*0.6 / PS=rW/(n+1)*0.7 で不一致だった)。 ±0.6 expansion にも追従。
      -- ★ Phase 64 A4: flipOnly を撤去し projectBar (形状 dispatcher) へ。 直線座標系は
      --   projectBarRect と同式なので px は bit 一致、 極座標では扇形 (wedge) になる。
      --   halfWidthD 0.3 = 厚み 0.6 スロットの半分 (catUnitPx は 1 data 単位の px 幅)。
      coord = lpCoord layout
      bw = catUnitPx coord layout * 0.6   -- Phase 10 A4-fix: flip では縦スロット幅
      mkBar i d =
        let xp = if isCat then fromIntegral i else fromIntegral i
            yStart = cumulative !! i
            yEnd   = yStart + d
            c = if d >= 0 then posC else negC
        -- Phase 10 A4: data x=xp、 yStart..yEnd を data 値で、 厚み bw px (flip 追従)。
        in case projectBar coord layout xp yStart yEnd 0.3 bw of
             BarRect  rect -> PRect rect (FillStyle c a) (Just (StrokeStyle c 1.0))
             BarWedge segs -> PPath segs (FillStyle c a) (Just (StrokeStyle c 1.0))
      -- Phase 7 A6: 末尾に合計 (Total) バー (= base 0 から累積到達値)。 デフォルト出す
      -- (フラグ切替の Spec API は後追い)。 中立灰で増減バーと区別。 x = n (Layout で
      -- category を "Total" 1 つ拡張済み)。
      totalC = "#6b7280"
      total  = sum deltas
      totalBar =
        let xp = fromIntegral n
        in case projectBar coord layout xp 0 total 0.3 bw of
             BarRect  rect -> PRect rect (FillStyle totalC a) (Just (StrokeStyle totalC 1.0))
             BarWedge segs -> PPath segs (FillStyle totalC a) (Just (StrokeStyle totalC 1.0))
  in [ mkBar i d | (i, d) <- zip [0..] deltas ] ++ [totalBar]

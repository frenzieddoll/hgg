-- | Phase 52 A1: DAG エッジ kink の再現 + routing 計測 probe。
--
-- @
-- cabal run phase52-kink-probe
-- @
-- → design/phase52-kink/ に before SVG 3 枚 + routes CSV を書き出す。
--
--   * corr6:   analyze Phase 77 の相関グラフ (6 node・12 辺) 再現。
--              由来図 corr-graph.svg で genetics→chd / diet→chd / diet→bp に
--              hairpin (単一 cubic の異常曲率) が実測された構図。
--   * lingam6: 同 LiNGAM DAG (7 辺・kink 軽微) = 対照。
--   * dense15: 15 node + 長 skip 辺の合成 DAG (完了条件の 6〜15 node 帯の上限)。
--
-- CSV は bake 済み 'RoutedEdge' の (from, to, kind, 制御点列) を素で吐く。
-- kink 角の解析 (junction 接線差 / 単一 cubic 内の最大回転) は
-- design/phase52-kink/analyze-kinks.py が行う (= 由来図の実測と同じ metric)。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hgg.Plot.Backend.SVG    (saveSVG)
import           Hgg.Plot.DAG            (dagPlot, (~>))
import           Hgg.Plot.Easy
import           Hgg.Plot.Primitive      (Point (..))
import           Hgg.Plot.Render.EdgeRoute (Box (..), Obstacles (..),
                                              buildChannel, dagObstacles,
                                              edgePortPoint, proutespline)
import qualified Hgg.Plot.Render.EdgeRoute as ER
import           Hgg.Plot.Render.Special (bakeDAGRoutesInSpec, dagToScreen)
import           Hgg.Plot.Spec           (DAGEdge (..), DAGNode (..),
                                              DAGSpec (..), Layer,
                                              RoutedEdge (..),
                                              VisualSpec (..), lyDAG)
import           Hgg.Plot.Unit           (px, (*~))
import           Data.Monoid                 (Last (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO

-- ===========================================================================
-- ケース定義
-- ===========================================================================

-- | analyze Phase 77 相関グラフ (|r|>0.3・12 辺) の実測エッジ集合。
corr6 :: Layer
corr6 = dagPlot $
     g ~> "exercise" <> g ~> "bp"  <> g ~> "chd"
  <> d ~> "bmi"      <> d ~> "bp"  <> d ~> "chd"
  <> e ~> "bmi"      <> e ~> "bp"  <> e ~> "chd"
  <> b ~> "bp"       <> b ~> "chd"
  <> p ~> "chd"
  where
    g = "genetics" :: Text
    d = "diet" :: Text
    e = "exercise" :: Text
    b = "bmi" :: Text
    p = "bp" :: Text

-- | 同 LiNGAM DAG (直接因果 7 辺) = kink 軽微の対照。
lingam6 :: Layer
lingam6 = dagPlot $
     ("genetics" :: Text) ~> "exercise"
  <> ("genetics" :: Text) ~> "bp"
  <> ("diet"     :: Text) ~> "bmi"
  <> ("exercise" :: Text) ~> "bmi"
  <> ("bmi"      :: Text) ~> "bp"
  <> ("bmi"      :: Text) ~> "chd"
  <> ("bp"       :: Text) ~> "chd"

-- | 15 node + rank を 3 つ以上跨ぐ skip 辺を複数持つ合成 DAG。
-- 長 skip 辺は経路上の node/plate を迂回する CubicPath になり、 kink が出やすい。
dense15 :: Layer
dense15 = dagPlot $
     x 1 ~> x 4  <> x 1 ~> x 5  <> x 2 ~> x 5  <> x 2 ~> x 6  <> x 3 ~> x 6
  <> x 4 ~> x 7  <> x 5 ~> x 7  <> x 5 ~> x 8  <> x 6 ~> x 9
  <> x 7 ~> x 10 <> x 8 ~> x 10 <> x 8 ~> x 11 <> x 9 ~> x 12
  <> x 10 ~> x 13 <> x 11 ~> x 13 <> x 12 ~> x 14
  <> x 13 ~> x 15 <> x 14 ~> x 15
  -- 長 skip (3+ rank 跨ぎ = 迂回 routing を強制)
  <> x 1 ~> x 13 <> x 2 ~> x 14 <> x 3 ~> x 15 <> x 4 ~> x 15 <> x 6 ~> x 15
  where
    x :: Int -> Text
    x i = T.pack ("x" <> show i)

-- ===========================================================================
-- 計測 dump
-- ===========================================================================

-- | spec を bake し、 全 DAG edge の RoutedEdge を CSV 行へ。
-- 形式: case,from,to,kind,pts ("x y;x y;..")
dumpRoutes :: Text -> VisualSpec -> Text
dumpRoutes caseName spec =
  let baked = bakeDAGRoutesInSpec spec
      dagsOf l = case getLast (lyDAG l) of
        Just ds -> dsEdges ds
        Nothing -> []
      es = concatMap dagsOf (vsLayers baked)
      row e = case deRoute e of
        Just (RoutedEdge k pts) ->
          [ T.intercalate ","
              [ caseName, deFrom e, deTo e, T.pack (show k)
              , T.intercalate ";"
                  [ T.pack (show px_ <> " " <> show py) | (px_, py) <- pts ] ] ]
        Nothing -> []
  in T.unlines (concatMap row es)

mkSpec :: Text -> Layer -> VisualSpec
mkSpec t ly = purePlot
  <> layer (ly <> size 22)
  <> title t
  <> theme ThemeLight
  <> widthUnit (640 *~ px) <> heightUnit (560 *~ px)

-- ===========================================================================
-- A2 予備計測: kink 辺の routing 内部 (guide/portal/taut/barrier/fit) を dump
-- ===========================================================================

-- | routeEdge の chain 分岐 (parCount=1) を probe 内で再現し、 中間産物を印字する。
-- 実装は EdgeRoute.routeEdge と同一手順 (= 差異が出たら probe 側のバグ)。
debugEdge :: Text -> Layer -> Text -> Text -> IO ()
debugEdge caseName ly f t = case getLast (lyDAG ly) of
  Nothing -> putStrLn "  (no DAG layer)"
  Just (DAGSpec nodes es algo plates) -> do
    let radius   = 11  -- probe の size 22 と同値
        toScreen = dagToScreen radius nodes algo
        nodeMap  = [ (dnId n, n) | n <- nodes ]
        obs      = dagObstacles toScreen radius nodes nodeMap plates es
        lookup_ k = case [ n | (i, n) <- nodeMap, i == k ] of
          (n:_) -> Just n; [] -> Nothing
        Just from = lookup_ f
        Just to   = lookup_ t
        Just e    = case [ ed | ed <- es, deFrom ed == f, deTo ed == t ] of
          (x:_) -> Just x; [] -> Nothing
        fromCenter@(Point _ _) = toScreen (dnX from) (dnY from)
        toCenter               = toScreen (dnX to)   (dnY to)
        Just chain = dePath e
        inner    = [ toScreen x y | (x, y) <- take (length chain - 2) (drop 1 chain) ]
        guide    = fromCenter : inner ++ [toCenter]
        -- edgeBoxes 相当 (EdgeRoute 内部と同ロジック)
        boxContains (Box xlo ylo xhi yhi) (Point x y) =
          x > xlo && x < xhi && y > ylo && y < yhi
        nodeB = [ b | (i, b) <- obNodes obs, i /= f, i /= t ]
        laneB = [ b | ((lf, lt), bs) <- obLanes obs
                    , not (lf == f && lt == t), not (lf == t && lt == f)
                    , b <- bs ]
        allB  = nodeB ++ obPlates obs ++ laneB
        eboxes = [ b | b <- allB, not (boxContains b fromCenter)
                     , not (boxContains b toCenter) ]
        portals  = buildChannel eboxes guide
        taut     = ER.funnel portals
        interior = drop 1 (init taut)
        firstDir = case interior of (p:_) -> p; [] -> toCenter
        lastDir  = case reverse interior of (p:_) -> p; [] -> fromCenter
        fromPortS = edgePortPoint from fromCenter firstDir radius
        toPortS   = edgePortPoint to   toCenter   lastDir  radius
        tautPorts = fromPortS : interior ++ [toPortS]
        -- channelBarriers 相当
        vequal (Point ax ay) (Point bx by) =
          abs (ax - bx) < 1e-9 && abs (ay - by) < 1e-9
        segs xs = filter (\(a, b) -> not (vequal a b)) (zip xs (drop 1 xs))
        barriers = segs (map fst portals) ++ segs (map snd portals)
        vnorm p@(Point x y) =
          let l = sqrt (x*x + y*y)
          in if l > 1e-12 then Point (x/l) (y/l) else p
        vsub (Point ax ay) (Point bx by) = Point (ax-bx) (ay-by)
        ev0 = case tautPorts of (a:b:_) -> vnorm (vsub b a); _ -> Point 0 1
        ev1 = case reverse tautPorts of (a:b:_) -> vnorm (vsub a b); _ -> Point 0 1
        ctrl = proutespline barriers tautPorts ev0 ev1
        shP (Point x y) = "(" <> r1 x <> "," <> r1 y <> ")"
        r1 v = show (fromIntegral (round (v * 10) :: Int) / 10 :: Double)
    putStrLn $ "=== " <> T.unpack caseName <> " " <> T.unpack f <> "->" <> T.unpack t
    putStrLn $ "  guide     = " <> unwords (map shP guide)
    putStrLn $ "  portals   = " <> unwords [ "[" <> shP a <> "~" <> shP b <> "]" | (a,b) <- portals ]
    putStrLn $ "  taut      = " <> unwords (map shP taut)
    putStrLn $ "  tautPorts = " <> unwords (map shP tautPorts)
    putStrLn $ "  barriers  = " <> unwords [ shP a <> "-" <> shP b | (a,b) <- barriers ]
    putStrLn $ "  ev0/ev1   = " <> shP ev0 <> " " <> shP ev1
    putStrLn $ "  ctrl      = " <> unwords (map shP ctrl)
    putStrLn $ "  eboxes    = " <> unwords
      [ "[" <> r1 xlo <> "," <> r1 ylo <> ".." <> r1 xhi <> "," <> r1 yhi <> "]"
      | Box xlo ylo xhi yhi <- eboxes ]

main :: IO ()
main = do
  -- 出力名は現 HEAD の状態を指す (A1 の before 記録は git 管理済の *-before.*)。
  let cases =
        [ ("corr6",   mkSpec "corr6: correlation graph (12 edges)" corr6)
        , ("lingam6", mkSpec "lingam6: LiNGAM DAG (7 edges)" lingam6)
        , ("dense15", mkSpec "dense15: 15-node + long skips" dense15)
        ]
  mapM_ (\(nm, spec) -> saveSVG ("design/phase52-kink/" <> T.unpack nm <> "-after.svg") spec) cases
  let csvHeader = "case,from,to,kind,pts\n"
      csvBody   = T.concat [ dumpRoutes nm spec | (nm, spec) <- cases ]
  TIO.writeFile "design/phase52-kink/routes-after.csv" (csvHeader <> csvBody)
  putStrLn "wrote design/phase52-kink/{corr6,lingam6,dense15}-after.svg + routes-after.csv"
  -- A2 予備計測: kink 辺 (A1 flag) の routing 内部
  debugEdge "corr6"   corr6   "genetics" "chd"
  debugEdge "corr6"   corr6   "diet"     "bp"
  debugEdge "dense15" dense15 "x3"       "x15"
  debugEdge "dense15" dense15 "x4"       "x15"

-- | Phase 53 A1: DAG x 座標割当の実測 probe (braid 真因の layout/render 切り分け)。
--
-- @
-- cabal run phase53-x-probe
-- @
-- → design/phase53-x-coord/ に layout-before.csv + routes-before.csv +
--   {corr6,dense15}.dot (graphviz gold 入力) を書き出す。
--
-- 計測意図 (Phase 53 A1):
--   * layout-before.csv = 全 real node + 長 edge dummy waypoint の rank / x
--     (raw 座標と screen px の両方)。 「braid (corr6 genetics->chd ×
--     exercise->chd) が layout 空間で既に交差しているか、 routing (funnel) で
--     潰れて交差するか」 を切り分ける一次データ。
--   * routes-before.csv = bake 済み spline (Phase 52 probe と同形式) = render 側。
--   * *.dot = 同 topology の graphviz gold 入力 (dot -Tplain 突合は analyze-x.py)。
--
-- ケース定義 (corr6 / dense15) は Phase52KinkProbe.hs と同一 (executable 間は
-- import 不可のため複製。 変更時は両方を同期すること)。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.DAG            (dagPlot, dagPlotWithRankGroups,
                                              (~>))
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Primitive      (Point (..))
import           Graphics.Hgg.Render.Special (bakeDAGRoutesInSpec, dagToScreen)
import           Graphics.Hgg.Spec           (DAGEdge (..), DAGNode (..),
                                              DAGSpec (..), Layer,
                                              RoutedEdge (..),
                                              VisualSpec (..), lyDAG)
import           Graphics.Hgg.Unit           (px, (*~))
import           Data.List                   (sortOn)
import           Data.Monoid                 (Last (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.Directory            (createDirectoryIfMissing)

-- ===========================================================================
-- ケース定義 (= Phase52KinkProbe と同一)
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

-- | 15 node + rank を 3 つ以上跨ぐ skip 辺を複数持つ合成 DAG。
dense15 :: Layer
dense15 = dagPlot $
     x 1 ~> x 4  <> x 1 ~> x 5  <> x 2 ~> x 5  <> x 2 ~> x 6  <> x 3 ~> x 6
  <> x 4 ~> x 7  <> x 5 ~> x 7  <> x 5 ~> x 8  <> x 6 ~> x 9
  <> x 7 ~> x 10 <> x 8 ~> x 10 <> x 8 ~> x 11 <> x 9 ~> x 12
  <> x 10 ~> x 13 <> x 11 ~> x 13 <> x 12 ~> x 14
  <> x 13 ~> x 15 <> x 14 ~> x 15
  <> x 1 ~> x 13 <> x 2 ~> x 14 <> x 3 ~> x 15 <> x 4 ~> x 15 <> x 6 ~> x 15
  where
    x :: Int -> Text
    x i = T.pack ("x" <> show i)

-- | Phase 53 A3 flat-edge fixture: rank group {a,m,b} で flat edge 3 本
-- (隣接 a→m / m→b = 水平直線、 非隣接 a→b = 迂回 spline)。 graphviz gold は
-- {rank=same} で同 topology を再現する。
flat5 :: Layer
flat5 = dagPlotWithRankGroups
  (    r ~> "a" <> r ~> "m" <> r ~> "b"
    <> "a" ~> "m" <> "m" ~> "b" <> "a" ~> "b"
    <> "a" ~> "t" <> "b" ~> "t" )
  [["a", "m", "b"]]
  where
    r = "r" :: Text

-- | flat5 の graphviz gold 用 rank group (= dumpDot で {rank=same} を出す)。
flat5Groups :: [[Text]]
flat5Groups = [["a", "m", "b"]]

mkSpec :: Text -> Layer -> VisualSpec
mkSpec t ly = purePlot
  <> layer (ly <> size 22)
  <> title t
  <> theme ThemeLight
  <> widthUnit (640 *~ px) <> heightUnit (560 *~ px)

-- | probe の size 22 に対応する node 半径 (Phase 52 probe と同値)。
probeRadius :: Double
probeRadius = 11

-- ===========================================================================
-- layout dump: real node + dummy waypoint の rank / x
-- ===========================================================================

dagOf :: Layer -> Maybe DAGSpec
dagOf ly = getLast (lyDAG ly)

-- | 形式: case,type,id,edge,rank,xRaw,xScr,yScr
--   * real  = DAGNode (layout 済 dnX/dnY。 A4-3 point pipeline で x = raw point、
--             y = rank index)
--   * dummy = dePath (chain waypoint) の内部点。 edge 列は "from->to"、
--             id は "from->to#<k>" (k = 上流から 1 始まり)。
dumpLayout :: Text -> Layer -> Text
dumpLayout caseName ly = case dagOf ly of
  Nothing -> ""
  Just (DAGSpec nodes es algo _plates) ->
    let toScreen = dagToScreen probeRadius nodes algo
        shD v = T.pack (show (fromIntegral (round (v * 100) :: Int) / 100 :: Double))
        scr x y = let Point sx sy = toScreen x y in (shD sx, shD sy)
        nodeRow n =
          let (sx, sy) = scr (dnX n) (dnY n)
          in T.intercalate ","
               [ caseName, "real", dnId n, ""
               , T.pack (show (round (dnY n) :: Int)), shD (dnX n), sx, sy ]
        dummyRows e = case dePath e of
          Just ch | length ch > 2 ->
            [ let (sx, sy) = scr x y
              in T.intercalate ","
                   [ caseName, "dummy"
                   , deFrom e <> "->" <> deTo e <> "#" <> T.pack (show k)
                   , deFrom e <> "->" <> deTo e
                   , T.pack (show (round y :: Int)), shD x, sx, sy ]
            | (k, (x, y)) <- zip [1 :: Int ..]
                                 (take (length ch - 2) (drop 1 ch)) ]
          _ -> []
    in T.unlines (map nodeRow nodes ++ concatMap dummyRows es)

-- | console 用: rank ごとに x 昇順で real/dummy を並べる (目視突合用)。
printRankTable :: Text -> Layer -> IO ()
printRankTable caseName ly = case dagOf ly of
  Nothing -> pure ()
  Just (DAGSpec nodes es _ _) -> do
    let reals = [ (round (dnY n) :: Int, dnX n, dnId n, "real" :: Text) | n <- nodes ]
        dums  = concat
          [ [ (round y :: Int, x, deFrom e <> "->" <> deTo e, "dummy")
            | (x, y) <- take (length ch - 2) (drop 1 ch) ]
          | e <- es, Just ch <- [dePath e], length ch > 2 ]
        ranks = [ minimum rs .. maximum rs ]
          where rs = [ r | (r, _, _, _) <- reals ++ dums ]
        row (r_, x, i, ty) = "(" <> T.unpack i <> " " <> T.unpack ty
          <> " x=" <> show (fromIntegral (round (x * 10) :: Int) / 10 :: Double) <> ")"
          where _ = r_ :: Int
    putStrLn $ "=== " <> T.unpack caseName <> " rank table (x 昇順) ==="
    mapM_ (\r -> putStrLn $ "  rank " <> show r <> ": " <> unwords
             [ row e | e@(r', _, _, _) <- sortOn (\(_, x, _, _) -> x) (reals ++ dums)
             , r' == r ])
          ranks

-- ===========================================================================
-- routes dump (Phase 52 probe と同形式) + graphviz gold 入力
-- ===========================================================================

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

-- | 同 topology の graphviz 入力 (.dot)。 構造一致比較用 (既定属性のまま)。
-- Phase 53 A3: rank group があれば {rank=same} subgraph を出す。
dumpDot :: [[Text]] -> Layer -> Text
dumpDot groups ly = case dagOf ly of
  Nothing -> ""
  Just (DAGSpec _ es _ _) -> T.unlines $
       [ "digraph g {" ]
    ++ [ "  { rank=same; " <> T.concat [ "\"" <> m <> "\"; " | m <- g ] <> "}"
       | g <- groups ]
    ++ [ "  \"" <> deFrom e <> "\" -> \"" <> deTo e <> "\";" | e <- es ]
    ++ [ "}" ]

main :: IO ()
main = do
  let outDir = "design/phase53-x-coord/"
      cases  = [ ("corr6", corr6), ("dense15", dense15) ]
  createDirectoryIfMissing True outDir
  -- layout dump (probe の主目的)
  let layoutHeader = "case,type,id,edge,rank,xRaw,xScr,yScr\n"
      layoutBody   = T.concat [ dumpLayout nm ly | (nm, ly) <- cases ]
  TIO.writeFile (outDir <> "layout-before.csv") (layoutHeader <> layoutBody)
  -- routes dump (render 側の before 固定)
  let csvHeader = "case,from,to,kind,pts\n"
      csvBody   = T.concat [ dumpRoutes nm (mkSpec nm ly) | (nm, ly) <- cases ]
  TIO.writeFile (outDir <> "routes-before.csv") (csvHeader <> csvBody)
  -- graphviz gold 入力
  mapM_ (\(nm, ly) -> TIO.writeFile (outDir <> T.unpack nm <> ".dot")
                        (dumpDot [] ly)) cases
  -- Phase 53 A3: flat-edge fixture (= 別ファイル。 corr6/dense15 の before 固定は
  -- 触らない = 上の 2 CSV が byte 不変であることが A3 の回帰チェックを兼ねる)
  TIO.writeFile (outDir <> "layout-flat.csv")
    (layoutHeader <> dumpLayout "flat5" flat5)
  TIO.writeFile (outDir <> "routes-flat.csv")
    (csvHeader <> dumpRoutes "flat5" (mkSpec "flat5" flat5))
  TIO.writeFile (outDir <> "flat5.dot") (dumpDot flat5Groups flat5)
  putStrLn ("wrote " <> outDir
            <> "{layout-before.csv, routes-before.csv, corr6.dot, dense15.dot,"
            <> " layout-flat.csv, routes-flat.csv, flat5.dot}")
  mapM_ (uncurry printRankTable) (cases ++ [("flat5", flat5)])

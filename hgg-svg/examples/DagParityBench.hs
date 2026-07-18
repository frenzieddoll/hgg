-- | Phase 1 A9 — graphviz CLI との parity check 用 bench。
--
-- @
-- cabal run dag-parity-bench
-- @
-- → design/dag-parity/parity/{small,medium,large}/ に
--   hgg.svg + input.dot + crossings.txt を出力。
--
-- 後段 'scripts/dag-parity-check.sh' が dot CLI で input.dot を SVG 化 (= graphviz.svg)、
-- side-by-side HTML を生成する。 dot が無ければ hgg 出力のみ。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Backend.SVG (saveSVG)
import qualified Graphics.Hgg.DAG
import           Graphics.Hgg.DAG         ((~>))
import           Graphics.Hgg.DAG.Internal.Sugiyama
                                          (assignOrderFull, assignRanks,
                                           buildLayoutGraph, countCrossings)
import           Graphics.Hgg.Easy
import qualified Graphics.Hgg.Spec        as Spec
import           Graphics.Hgg.Unit        (px, (*~))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           System.Directory         (createDirectoryIfMissing)

main :: IO ()
main = do
  -- canvas サイズは dot の見た目に合わせる: 各 case の rank 数 (= 縦) と
  -- rank あたり最大幅 (= 横) を見て、 dot 既定の出力 aspect に近づける。
  --   small (rank 5 程度、 幅 3-4): 400x500 (= 縦長気味)
  --   medium (rank ~30、 幅 1-2):   400x1400 (= かなり縦長、 dot chain と同形)
  --   large (rank ~12、 幅 5):     800x1100 (= dot の portrait に近い)
  mapM_ runCase
    [ ("small",  buildSmall,  "N=10、 単純な階層 HBM 様",
        400,  500)
    , ("medium", buildMedium, "N=30、 chain + skip edges",
        400, 1400)
    , ("large",  buildLarge,  "N=60、 5 chain 並列 + cross + long",
        800, 1500)
    , ("isolated", buildIsolated,
        "孤立 4 node + chain 6 = 10、 孤立が上部に横並びになるか確認",
        600, 400)
    ]
  putStrLn ""
  putStrLn "Done. 次に scripts/dag-parity-check.sh を実行して dot 比較 + HTML 生成。"

-- ===========================================================================
-- Test case 1: small (N=10)
-- ===========================================================================

buildSmall :: ([Text], [(Text, Text)])
buildSmall =
  let ns = [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j" ]
      es = [ ("a", "c"), ("b", "c"), ("c", "d"), ("c", "e")
           , ("d", "f"), ("e", "f"), ("d", "g"), ("e", "h")
           , ("f", "i"), ("g", "j"), ("h", "j"), ("i", "j")
           , ("a", "j")  -- long skip edge
           ]
  in (ns, es)

buildMedium :: ([Text], [(Text, Text)])
buildMedium =
  let ns = [ T.pack ("n" <> show k) | k <- [0 .. 29 :: Int] ]
      -- chain n0 → n1 → n2 → ... → n29
      chain = [ (ns !! k, ns !! (k + 1)) | k <- [0 .. 28] ]
      -- 横方向 skip edges
      skips = [ (ns !! 0, ns !! 5), (ns !! 3, ns !! 10)
              , (ns !! 7, ns !! 15), (ns !! 12, ns !! 22)
              , (ns !! 18, ns !! 28)
              ]
  in (ns, chain <> skips)

-- | 孤立 node が含まれるケース。 isoX (= 4 個) は edge を持たず、 chain a-f (= 5 edge)
-- とは独立。 dot は孤立 4 個を上部に横並びで配置し、 hgg も同じ挙動 (= 'mkGraph'
-- が nodeIds 全てを 'vertex' で先に登録するため) を取る。
buildIsolated :: ([Text], [(Text, Text)])
buildIsolated =
  let ns = [ "a", "b", "c", "d", "e", "f"
           , "iso1", "iso2", "iso3", "iso4" ]
      es = [ ("a","b"), ("b","c"), ("c","d"), ("d","e"), ("e","f") ]
  in (ns, es)

buildLarge :: ([Text], [(Text, Text)])
buildLarge =
  let ns = [ T.pack ("v" <> show k) | k <- [0 .. 59 :: Int] ]
      -- 5 disjoint chain × 12 node = 60 node、 全 node が edge を持つ。
      -- chain c は node [12c .. 12c+11] を使う (= 旧実装の indexing 重複 bug を修正、
      -- 孤立 22 node が rank 0 に山積みになるのを防ぐ)。
      chains = [ (ns !! (12 * c + k), ns !! (12 * c + k + 1))
               | c <- [0 .. 4], k <- [0 .. 10] ]
      -- 同 rank 間の cross-link (= chain c の rank k → chain c+1 の rank k)
      cross  = [ (ns !! (12 * c + k), ns !! (12 * (c + 1) + k))
               | c <- [0 .. 3], k <- [0, 4, 8] ]
      -- chain 内の長 skip edge
      longs  = [ (ns !! 0, ns !! 11), (ns !! 12, ns !! 22)
               , (ns !! 24, ns !! 35) ]
  in (ns, chains <> cross <> longs)

-- ===========================================================================
-- Case runner
-- ===========================================================================

runCase :: (FilePath, ([Text], [(Text, Text)]), String, Double, Double) -> IO ()
runCase (name, (nodeIds, edges), desc, w, h) = do
  let dir = "design/dag-parity/parity/" <> name
  createDirectoryIfMissing True dir

  -- hgg SVG 生成 (canvas は case ごとに調整、 dot の auto sizing に合わせる)
  let g = mkGraph nodeIds edges
      spec = purePlot
        <> layer (Graphics.Hgg.DAG.dagPlot g <> size 11)
        <> title (T.pack (name <> " (" <> desc <> ")"))
        <> theme  ThemeLight
        <> widthUnit (w *~ px) <> heightUnit (h *~ px)
  saveSVG (dir <> "/hgg.svg") spec

  -- DOT 出力 (= graphviz CLI 用入力)
  TIO.writeFile (dir <> "/input.dot") (toDot name nodeIds edges)

  -- hgg 内部の crossings 数を計測 (= LayoutGraph 経由)
  let lg0 = assignRanks (buildLayoutGraph nodeIds edges)
      (lg1, om, _) = assignOrderFull lg0
      crossings = countCrossings lg1 om
  writeFile (dir <> "/crossings.txt")
    ("hgg crossings: " <> show crossings <> "\n"
       <> "N nodes: " <> show (length nodeIds) <> "\n"
       <> "N edges: " <> show (length edges) <> "\n")

  putStrLn $ "  " <> name <> ": " <> show (length nodeIds)
          <> " nodes, " <> show (length edges) <> " edges, "
          <> "hgg crossings = " <> show crossings

-- ===========================================================================
-- Helpers
-- ===========================================================================

-- | Text 列 + edge 列を Graph に。
-- 全 nodeIds を 'vertex' で先に登録してから edge を overlay する。 これで edge に
-- 含まれない孤立 node も Graph に残り、 dot の DOT (= 全 node 宣言) と挙動が揃う。
mkGraph :: [Text] -> [(Text, Text)] -> Graphics.Hgg.DAG.Graph Text
mkGraph nodeIds edges =
  let vs = foldr (\v acc -> acc <> Graphics.Hgg.DAG.vertex v) mempty nodeIds
      es = foldr (\(f, t) acc -> acc <> (f ~> t)) mempty edges
  in vs <> es

-- | DOT 文字列を生成。 graphviz CLI の `dot -Tsvg input.dot` で SVG 化可。
toDot :: String -> [Text] -> [(Text, Text)] -> Text
toDot name ns es = T.unlines $
  [ T.pack ("digraph " <> name <> " {")
  , "  rankdir=TB;"
  , "  node [shape=ellipse, fontsize=10];"
  ] <> [ "  " <> sanitize n <> ";" | n <- ns ]
    <> [ "  " <> sanitize f <> " -> " <> sanitize t <> ";" | (f, t) <- es ]
    <> [ "}" ]
  where
    sanitize = T.replace "-" "_"

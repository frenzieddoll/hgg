-- | Phase 2 A6: 3 ルート並列出力 demo。
--
-- 同じ 'ModelGraph' を 3 種類の出力ルート (Mermaid HTML / Graphviz DOT /
-- hgg SVG) で同時生成し、 ユーザが用途別に比較できるようにする。
--
-- @
-- cabal run three-routes-demo
-- @
-- → @design/three-routes/@ に下記 3 形式 × 3 model (= 計 9 ファイル) を出力。
--
-- Demo モデル: 単純 LM / 階層 LM (= 2 plate) / nested plate (= 多重所属) の 3 種。
--
-- 注: 本 demo は analyze 自体に置けない (= analyze は hgg に依存しない、
-- 計画 §4.1 依存方向規律)。 bridge package 内 demo として配置。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Directory    (createDirectoryIfMissing)

import           Hanalyze.Model.HBM        (ModelGraph (..), Node (..),
                                            NodeKind (..))
import qualified Hanalyze.Viz.ModelGraph   as Mermaid
import qualified Hanalyze.Viz.ModelGraphDot as Dot

import           Hgg.Plot.Bridge.Analyze
                                           (renderModelGraphSVG)

-- ===========================================================================
-- Demo モデル (= A2 test fixture と同じ、 再利用)
-- ===========================================================================

simpleLM :: ModelGraph
simpleLM = ModelGraph
  { mgNodes =
      [ Node "alpha" LatentN          "Normal"   Set.empty []
      , Node "beta"  LatentN          "Normal"   Set.empty []
      , Node "y"     (ObservedN 100)  "Normal"   Set.empty []
      ]
  , mgEdges  = [("alpha", "y"), ("beta", "y")]
  , mgPlates = Map.empty
  }

hierLM :: ModelGraph
hierLM = ModelGraph
  { mgNodes =
      [ Node "mu_g"  LatentN           "Normal"     Set.empty []
      , Node "sig_g" LatentN           "HalfCauchy" Set.empty []
      , Node "mu"    LatentN           "Normal"     Set.empty ["group"]
      , Node "y"     (ObservedN 200)   "Normal"     Set.empty ["record"]
      ]
  , mgEdges  = [("mu_g", "mu"), ("sig_g", "mu"), ("mu", "y")]
  , mgPlates = Map.fromList [("group", 5), ("record", 200)]
  }

nestedPlate :: ModelGraph
nestedPlate = ModelGraph
  { mgNodes =
      [ Node "mu"   LatentN          "Normal" Set.empty []
      , Node "mu_c" LatentN          "Normal" Set.empty ["condition"]
      , Node "y"    (ObservedN 200)  "Normal" Set.empty ["record", "condition"]
      ]
  , mgEdges  = [("mu", "mu_c"), ("mu_c", "y")]
  , mgPlates = Map.fromList [("record", 200), ("condition", 3)]
  }

-- ===========================================================================
-- Runner
-- ===========================================================================

main :: IO ()
main = do
  let outDir = "design/three-routes"
  createDirectoryIfMissing True outDir
  mapM_ (renderAllRoutes outDir)
    [ ("simple-lm",   "Simple LM (= 3 node、 plate 無し)",   simpleLM)
    , ("hier-lm",     "Hierarchical LM (= 2 plate、 disjoint)", hierLM)
    , ("nested-plate","Nested plate (= y が 2 plate に属す)",  nestedPlate)
    ]
  putStrLn ""
  putStrLn $ "Wrote 9 files to " <> outDir <> "/"
  putStrLn "  *-mermaid.html  (= Mermaid CDN 経由ブラウザ描画)"
  putStrLn "  *-graphviz.dot  (= dot CLI で SVG/PNG 化用)"
  putStrLn "  *-hgg.svg   (= hgg 直 SVG、 依存ゼロ)"

renderAllRoutes :: FilePath -> (String, T.Text, ModelGraph) -> IO ()
renderAllRoutes outDir (slug, titleTxt, mg) = do
  let base = outDir <> "/" <> slug

  -- Route 1: Mermaid HTML
  Mermaid.renderModelGraph (base <> "-mermaid.html") titleTxt mg

  -- Route 2: Graphviz DOT (= .dot 中間 text)
  TIO.writeFile (base <> "-graphviz.dot") (Dot.renderModelGraphDot mg)

  -- Route 3: hgg SVG (= 依存ゼロの直描画)
  renderModelGraphSVG (base <> "-hgg.svg") titleTxt mg

  putStrLn $ "  " <> slug <> ": 3 形式 出力 (mermaid.html / graphviz.dot / hgg.svg)"

-- |
-- Module      : Graphics.Hgg.Bridge.Analyze.Internal
-- Description : ModelGraph → DAGSpec 変換 (= 公開 API は Bridge.Analyze)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 本 module は internal。 直接利用は推奨しない (= API 安定保証なし)。
-- 公開 helper は @Graphics.Hgg.Bridge.Analyze@ に。
--
-- Phase 2 A2: 'toDAGNodes' / 'toDAGEdges' / 'toDAGPlates' / 'toDAGTriple' の
-- 変換関数群。 NodeKind / Distribution / Plate stack の mapping を担当。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Bridge.Analyze.Internal
  ( -- * 変換
    toDAGNodes
  , toDAGEdges
  , toDAGPlates
  , toDAGTriple
    -- * 内部 helper (= test 用に export)
  , mapNodeKind
  , plateLabel
  ) where

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text           as T

import qualified Graphics.Hgg.Spec   as Spec
import           Hanalyze.Model.HBM  (ModelGraph (..), Node (..),
                                      NodeKind (..))

-- ===========================================================================
-- 変換層
-- ===========================================================================

-- | 全体変換: 'ModelGraph' → ('[DAGNode]', '[DAGEdge]', '[DAGPlate]')。
-- 公開 API 側で 'Graphics.Hgg.DAG.dagPlotWithPlates' に渡す形に成形する。
toDAGTriple :: ModelGraph -> ([Spec.DAGNode], [Spec.DAGEdge], [Spec.DAGPlate])
toDAGTriple mg =
  ( toDAGNodes  mg
  , toDAGEdges  mg
  , toDAGPlates mg
  )

-- | 'mgNodes' を 'DAGNode' に変換。 dnX / dnY は 0 (= layout 計算で埋まる)、
-- dnDist には分布名を 'Just' で入れる (= 空文字列なら 'Nothing' に正規化)。
toDAGNodes :: ModelGraph -> [Spec.DAGNode]
toDAGNodes mg =
  [ Spec.DAGNode
      { Spec.dnId    = nodeName n
      , Spec.dnLabel = nodeName n
      , Spec.dnKind  = mapNodeKind (nodeKind n)
      , Spec.dnDist  = nonEmpty (nodeDist n)
      , Spec.dnX     = 0
      , Spec.dnY     = 0
      }
  | n <- mgNodes mg
  ]
  where
    nonEmpty t = if T.null t then Nothing else Just t

-- | 'mgEdges' (= (parent, child) 列) を 'DAGEdge' に。 dePath = Nothing
-- (= layout 計算で routing される)。 deRoute = Nothing (= 未 bake・layout で確定)。
toDAGEdges :: ModelGraph -> [Spec.DAGEdge]
toDAGEdges mg = [ Spec.DAGEdge p c Nothing Nothing | (p, c) <- mgEdges mg ]

-- | 'mgPlates' (= plate 名 → サイズ N) を 'DAGPlate' に。
-- 各 plate の dpNodeIds は @nodePlates@ に当該 plate 名を含む node を列挙。
-- dpLabel は @"\<plate 名\> (N=\<size\>)"@ の形式 (= dot / PyMC 慣例に近い)。
toDAGPlates :: ModelGraph -> [Spec.DAGPlate]
toDAGPlates mg =
  let nodesInPlate :: Text -> [Text]
      nodesInPlate pname =
        [ nodeName n | n <- mgNodes mg, pname `elem` nodePlates n ]
  in [ Spec.DAGPlate
         { Spec.dpLabel   = plateLabel pname size
         , Spec.dpNodeIds = nodesInPlate pname
         }
     | (pname, size) <- Map.toAscList (mgPlates mg)
     ]

-- ===========================================================================
-- Helpers
-- ===========================================================================

-- | analyze の 'NodeKind' を hgg の 'DAGNodeKind' に。
--
--   * 'LatentN'        → 'Spec.NodeLatent'    (= 楕円、 stochastic latent)
--   * 'ObservedN _'    → 'Spec.NodeObserved'  (= 楕円 + 灰塗、 観測)
mapNodeKind :: NodeKind -> Spec.DAGNodeKind
mapNodeKind LatentN       = Spec.NodeLatent
mapNodeKind (ObservedN _) = Spec.NodeObserved

-- | plate label を @"\<name\> (N=\<size\>)"@ の形式で生成。
-- analyze の Mermaid / Graphviz DOT 出力と同じ形式。
plateLabel :: Text -> Int -> Text
plateLabel name size = name <> " (N=" <> T.pack (show size) <> ")"

-- ===========================================================================
-- 未使用警告抑止
-- ===========================================================================

_unused :: Map Text Int -> Int
_unused = Map.size

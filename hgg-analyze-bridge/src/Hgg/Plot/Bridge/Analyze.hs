-- |
-- Module      : Hgg.Plot.Bridge.Analyze
-- Description : hanalyze ModelGraph → hgg SVG/PNG/PDF 直描画 bridge
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hanalyze の 'Hanalyze.Model.HBM.ModelGraph' を、 hgg 経由で
-- 直接 SVG / PNG / PDF に描画する公開 API。 graphviz CLI / Mermaid CDN 依存なし。
--
-- == 3 ルートの選び方
--
-- 同じ 'ModelGraph' を出力する 3 種類のルートがある。 用途に応じて使い分け:
--
-- +---------------+---------------------------------------------+--------------------+-----------------------+
-- | ルート        | 場所                                        | 出力 / 描画依存    | 推奨用途              |
-- +===============+=============================================+====================+=======================+
-- | Mermaid HTML  | @Hanalyze.Viz.ModelGraph.renderModelGraph@  | .html + CDN script | GitHub README、 ノート |
-- +---------------+---------------------------------------------+--------------------+-----------------------+
-- | Graphviz DOT  | @Hanalyze.Viz.ModelGraphDot.toDot@          | .dot text + dot CLI| graphviz 連携、 加工   |
-- +---------------+---------------------------------------------+--------------------+-----------------------+
-- | 本 module     | 'renderModelGraphSVG' (= A3 完了)           | 依存ゼロ           | production、 offline   |
-- |               | 'renderModelGraphPNG' / 'renderModelGraphPDF' (= A4 stub、 backend 待ち) |             |             |
-- +---------------+---------------------------------------------+--------------------+-----------------------+
--
-- 3 ルートとも同じ 'ModelGraph' 構造 (= node / edge / plate) を表現する。
-- visual layout は実装ごとに異なる: 本ルートは graphviz dot 70-80% 同等品質
-- (Phase 1 §10.1)。
--
-- == 使用例 (= A3 で 'renderModelGraphSVG' 公開予定)
--
-- @
-- import Hgg.Plot.Bridge.Analyze (modelGraphToDAGSpec)
-- import Hanalyze.Model.HBM (buildModelGraph)
--
-- main = do
--   let mg = buildModelGraph myModel
--       (nodes, edges, plates) = modelGraphToDAGSpec mg
--   -- → A3 で renderModelGraphSVG file title mg として 1 行で完結予定
-- @
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Bridge.Analyze
  ( -- * ModelGraph → DAGSpec 変換 (Phase 2 A2)
    modelGraphToDAGSpec
  , modelGraphToDAGNodes
  , modelGraphToDAGEdges
  , modelGraphToDAGPlates
    -- * 直描画 API (Phase 2 A3 SVG)
  , renderModelGraphSVG
  , renderModelGraphSVGBytes
  , modelGraphToVisualSpec
    -- * 直描画 API (Phase 2 A4 PNG / PDF、 backend 未実装の stub)
  , renderModelGraphPNG
  , renderModelGraphPDF
  ) where

import           Data.Text                (Text)
import qualified Data.Text.IO             as TIO

import qualified Hgg.Plot.Backend.SVG as SVGBackend
import qualified Hgg.Plot.DAG         as DAG
import qualified Hgg.Plot.Easy        as Easy
import qualified Hgg.Plot.Spec        as Spec
import           Hanalyze.Model.HBM       (ModelGraph)

import qualified Hgg.Plot.Bridge.Analyze.Internal as I

-- | 'ModelGraph' を ('[DAGNode]', '[DAGEdge]', '[DAGPlate]') の triple に変換。
-- 各要素は 'Hgg.Plot.Spec.dagFromListsWithPlates' に渡せる形になっている。
modelGraphToDAGSpec
  :: ModelGraph -> ([Spec.DAGNode], [Spec.DAGEdge], [Spec.DAGPlate])
modelGraphToDAGSpec = I.toDAGTriple

-- | 'modelGraphToDAGSpec' の node 部分のみ。
modelGraphToDAGNodes :: ModelGraph -> [Spec.DAGNode]
modelGraphToDAGNodes = I.toDAGNodes

-- | 'modelGraphToDAGSpec' の edge 部分のみ (= dePath は 'Nothing'、 layout で埋まる)。
modelGraphToDAGEdges :: ModelGraph -> [Spec.DAGEdge]
modelGraphToDAGEdges = I.toDAGEdges

-- | 'modelGraphToDAGSpec' の plate 部分のみ (= plate label は @"\<name\> (N=\<size\>)"@)。
modelGraphToDAGPlates :: ModelGraph -> [Spec.DAGPlate]
modelGraphToDAGPlates = I.toDAGPlates

-- ===========================================================================
-- Phase 2 A3: SVG 直描画 API
-- ===========================================================================

-- | 'ModelGraph' を Phase 1 完了の DAG layout (= Sugiyama framework + plate-aware
-- ordering + Catmull-Rom spline + port snap) でレンダリングし、 'VisualSpec' に
-- 包んで返す。 ユーザは title / theme / size 等を追加合成可能。
--
-- @
-- let spec = modelGraphToVisualSpec mg
--          \<\> title \"My HBM\"
--          \<\> theme ThemeDark
--          \<\> widthMm 1200 \<\> heightMm 800
-- @
modelGraphToVisualSpec :: ModelGraph -> Spec.VisualSpec
modelGraphToVisualSpec mg =
  let (nodes, edges, plates) = modelGraphToDAGSpec mg
      -- Phase 1 layout pipeline を直接適用 (= Graph rebuild ではなく [DAGNode] + [DAGEdge] 経由)
      -- これで isolated node + 多重所属 plate も保たれ、 O(N) で済む
      (positioned, routed) =
        DAG.layoutHierarchicalFullWithPlates nodes edges plates
      dagSpec = Spec.dagFromListsWithPlates
                  positioned routed Spec.LayoutHierarchical plates
  in Easy.purePlot <> Easy.layer (dagSpec <> Easy.size 22)

-- | 'ModelGraph' を SVG ファイルに直描画。 title は plot 上部に表示。
-- size / theme 等を細かく指定したい場合は 'modelGraphToVisualSpec' + 'plot' を使う。
--
-- @
-- renderModelGraphSVG \"out\/dag.svg\" \"My HBM\" mg
-- @
renderModelGraphSVG :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraphSVG path titleTxt mg =
  let spec = modelGraphToVisualSpec mg
           <> Easy.title titleTxt
           <> Easy.theme Easy.ThemeLight
           <> Easy.widthMm 900
           <> Easy.heightMm 700
  in SVGBackend.saveSVG path spec

-- | 'renderModelGraphSVG' の ByteString 版 (= ファイル書き出さず Text で返す)。
-- web server / pipeline で SVG を直接他経路に流したいときに使う。
renderModelGraphSVGBytes :: Text -> ModelGraph -> Text
renderModelGraphSVGBytes titleTxt mg =
  let spec = modelGraphToVisualSpec mg
           <> Easy.title titleTxt
           <> Easy.theme Easy.ThemeLight
           <> Easy.widthMm 900
           <> Easy.heightMm 700
  in SVGBackend.renderSVG spec

-- 上記 helper 群は OverloadedStrings + qualified imports で標準的に書ける形。
-- 内部実装の細かい調整 (= サイズ default / theme) は今後の利用で feedback ベースで変える。
_unusedTextIO :: FilePath -> Text -> IO ()
_unusedTextIO = TIO.writeFile

-- ===========================================================================
-- Phase 2 A4: PNG / PDF 直描画 API (= backend 未実装の stub)
-- ===========================================================================

-- | __現状 stub__: hgg-rasterific backend は未実装の placeholder
-- (= 実行すると @error \"not implemented yet\"@)。 本 API は signature を先に
-- 公開しておき、 backend 実装後に自動的に動くようにする。
--
-- 暫定的に PNG が必要なら 'renderModelGraphSVG' で SVG を出力し、 別 tool
-- (= @inkscape@ / @rsvg-convert@ 等) で PNG 化する経路を推奨。
renderModelGraphPNG :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraphPNG _path _titleTxt _mg =
  error $ unlines
    [ "renderModelGraphPNG: hgg-rasterific backend が未実装の placeholder です。"
    , "  暫定回避: renderModelGraphSVG で SVG を出力し、 別 tool で PNG 化してください。"
    , "    例: rsvg-convert input.svg -o output.png"
    , "        inkscape input.svg --export-png=output.png"
    , "  本 API は backend 実装後に自動的に動作します (= signature 安定)。"
    ]

-- | __現状 stub__: hgg-pdf backend は未実装の placeholder
-- (= 実行すると @error \"not implemented yet\"@)。 本 API は signature を先に
-- 公開しておき、 backend 実装後に自動的に動くようにする。
--
-- 暫定的に PDF が必要なら 'renderModelGraphSVG' で SVG を出力し、 別 tool
-- (= @rsvg-convert -f pdf@ / @inkscape --export-pdf@ 等) で PDF 化する経路を推奨。
renderModelGraphPDF :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraphPDF _path _titleTxt _mg =
  error $ unlines
    [ "renderModelGraphPDF: hgg-pdf backend が未実装の placeholder です。"
    , "  暫定回避: renderModelGraphSVG で SVG を出力し、 別 tool で PDF 化してください。"
    , "    例: rsvg-convert -f pdf input.svg -o output.pdf"
    , "        inkscape input.svg --export-pdf=output.pdf"
    , "  本 API は backend 実装後に自動的に動作します (= signature 安定)。"
    ]

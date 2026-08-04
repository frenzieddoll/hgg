-- |
-- Module      : Graphics.Hgg.Bridge.Analyze
-- Description : hanalyze ModelGraph → hgg SVG/PNG/PDF 直描画 bridge
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: hanalyze の 'Hanalyze.Model.HBM.ModelGraph' を、 hgg 経由で
-- 直接 SVG / PNG / PDF に描画する公開 API。 graphviz CLI / Mermaid CDN 依存なし。
--
-- [English]: The public API that renders hanalyze's
-- 'Hanalyze.Model.HBM.ModelGraph' directly to SVG / PNG / PDF via
-- hgg, with no dependency on the graphviz CLI or the Mermaid CDN.
--
-- == 3 ルートの選び方
--
-- [日本語]: 同じ 'ModelGraph' を出力する 3 種類のルートがある。 用途に応じて使い分け:
--
-- [Mermaid HTML]: @Hanalyze.Viz.ModelGraph.renderModelGraph@ —
--   出力は @.html@ で描画に CDN script が要る。 GitHub README やノート向け。
-- [Graphviz DOT]: @Hanalyze.Viz.ModelGraphDot.toDot@ —
--   出力は @.dot@ テキストで描画に @dot@ CLI が要る。 graphviz 連携・加工向け。
-- [本 module]: 'renderModelGraphSVG' は依存ゼロで production / offline 向け。
--   @renderModelGraphPNG@ / @renderModelGraphPDF@ は backend 待ちの stub。
--
-- [English]: There are 3 routes that all output the same 'ModelGraph'.
-- Choose according to the use case:
--
-- [Mermaid HTML]: @Hanalyze.Viz.ModelGraph.renderModelGraph@ —
--   outputs @.html@, which needs a CDN script to render. For GitHub READMEs
--   and notebooks.
-- [Graphviz DOT]: @Hanalyze.Viz.ModelGraphDot.toDot@ — outputs
--   @.dot@ text, which needs the @dot@ CLI to render. For graphviz
--   integration and post-processing.
-- [This module]: 'renderModelGraphSVG' has zero dependencies, for
--   production / offline use. @renderModelGraphPNG@ / @renderModelGraphPDF@
--   are stubs pending their backends.
--
-- 3 ルートとも同じ 'ModelGraph' 構造 (= node / edge / plate) を表現する。
-- visual layout は実装ごとに異なる: 本ルートは graphviz dot の 70-80% 同等品質。
--
-- [English]: All 3 routes represent the same 'ModelGraph' structure (node /
-- edge / plate). The visual layout differs between implementations: this
-- route achieves roughly 70-80% of graphviz dot's quality.
--
-- == 使用例
--
-- [日本語]: 使用例 ('renderModelGraphSVG' は今後公開予定)。
-- [English]: A usage example ('renderModelGraphSVG' is planned for future
-- release).
--
-- @
-- import Graphics.Hgg.Bridge.Analyze (modelGraphToDAGSpec)
-- import Hanalyze.Model.HBM (buildModelGraph)
--
-- main = do
--   let mg = buildModelGraph myModel
--       (nodes, edges, plates) = modelGraphToDAGSpec mg
--   -- → 将来的に renderModelGraphSVG file title mg として 1 行で完結予定
-- @
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Bridge.Analyze
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

import qualified Graphics.Hgg.Backend.SVG as SVGBackend
import qualified Graphics.Hgg.DAG         as DAG
import qualified Graphics.Hgg.Easy        as Easy
import qualified Graphics.Hgg.Spec        as Spec
import           Hanalyze.Model.HBM       (ModelGraph)

import qualified Graphics.Hgg.Bridge.Analyze.Internal as I

-- | [日本語]: 'ModelGraph' を ('[DAGNode]', '[DAGEdge]', '[DAGPlate]') の triple に変換。
--   各要素は 'Graphics.Hgg.Spec.dagFromListsWithPlates' に渡せる形になっている。
--   [English]: Converts a 'ModelGraph' to a triple of ('[DAGNode]',
--   '[DAGEdge]', '[DAGPlate]'). Each element is already in the form that
--   'Graphics.Hgg.Spec.dagFromListsWithPlates' accepts.
modelGraphToDAGSpec
  :: ModelGraph -> ([Spec.DAGNode], [Spec.DAGEdge], [Spec.DAGPlate])
modelGraphToDAGSpec = I.toDAGTriple

-- | [日本語]: 'modelGraphToDAGSpec' の node 部分のみ。
--   [English]: Just the node part of 'modelGraphToDAGSpec'.
modelGraphToDAGNodes :: ModelGraph -> [Spec.DAGNode]
modelGraphToDAGNodes = I.toDAGNodes

-- | [日本語]: 'modelGraphToDAGSpec' の edge 部分のみ (= dePath は 'Nothing'、 layout で埋まる)。
--   [English]: Just the edge part of 'modelGraphToDAGSpec' (dePath is
--   'Nothing', filled in later by layout).
modelGraphToDAGEdges :: ModelGraph -> [Spec.DAGEdge]
modelGraphToDAGEdges = I.toDAGEdges

-- | [日本語]: 'modelGraphToDAGSpec' の plate 部分のみ (= plate label は @"\<name\> (N=\<size\>)"@)。
--   [English]: Just the plate part of 'modelGraphToDAGSpec' (plate labels
--   take the form @"\<name\> (N=\<size\>)"@).
modelGraphToDAGPlates :: ModelGraph -> [Spec.DAGPlate]
modelGraphToDAGPlates = I.toDAGPlates

-- ===========================================================================
-- SVG 直描画 API
-- ===========================================================================

-- | [日本語]: 'ModelGraph' を Sugiyama framework + plate-aware
--   ordering + Catmull-Rom spline + port snap の DAG layout でレンダリングし、
--   @VisualSpec@ に包んで返す。 ユーザは title / theme / size 等を追加合成可能。
--   [English]: Renders a 'ModelGraph' with a DAG layout (Sugiyama framework
--   + plate-aware ordering + Catmull-Rom splines + port snapping) and
--   returns it wrapped in a @VisualSpec@. Callers can further compose in
--   title / theme / size and the like.
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
      -- layout pipeline を直接適用 (= Graph rebuild ではなく [DAGNode] + [DAGEdge] 経由)
      -- これで isolated node + 多重所属 plate も保たれ、 O(N) で済む
      (positioned, routed) =
        DAG.layoutHierarchicalFullWithPlates nodes edges plates
      dagSpec = Spec.dagFromListsWithPlates
                  positioned routed Spec.LayoutHierarchical plates
  in Easy.purePlot <> Easy.layer (dagSpec <> Easy.size 22)

-- | [日本語]: 'ModelGraph' を SVG ファイルに直描画。 title は plot 上部に表示。
--   size / theme 等を細かく指定したい場合は 'modelGraphToVisualSpec' + @plot@ を使う。
--   [English]: Renders a 'ModelGraph' directly to an SVG file. The title is
--   shown at the top of the plot. For finer control over size / theme and
--   the like, use 'modelGraphToVisualSpec' with @plot@ instead.
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

-- | [日本語]: 'renderModelGraphSVG' の ByteString 版 (= ファイル書き出さず Text で返す)。
--   web server / pipeline で SVG を直接他経路に流したいときに使う。
--   [English]: The ByteString variant of 'renderModelGraphSVG' (returns
--   Text instead of writing a file). Useful when a web server or pipeline
--   needs to route the SVG elsewhere directly.
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
-- PNG / PDF 直描画 API (= backend 未実装の stub)
-- ===========================================================================

-- | [日本語]: __現状 stub__: hgg-rasterific backend は未実装の placeholder
--   (= 実行すると @error \"not implemented yet\"@)。 本 API は signature を先に
--   公開しておき、 backend 実装後に自動的に動くようにする。
--
--   暫定的に PNG が必要なら 'renderModelGraphSVG' で SVG を出力し、 別 tool
--   (= @inkscape@ / @rsvg-convert@ 等) で PNG 化する経路を推奨。
--   [English]: __Currently a stub__: a placeholder pending the
--   hgg-rasterific backend implementation (calling it raises
--   @error "not implemented yet"@). This API publishes its signature ahead
--   of time so it will start working automatically once the backend lands.
--
--   In the meantime, if PNG is needed, the recommended path is to output
--   SVG with 'renderModelGraphSVG' and convert it with an external tool
--   (e.g. @inkscape@ / @rsvg-convert@).
renderModelGraphPNG :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraphPNG _path _titleTxt _mg =
  error $ unlines
    [ "renderModelGraphPNG: hgg-rasterific backend が未実装の placeholder です。"
    , "  暫定回避: renderModelGraphSVG で SVG を出力し、 別 tool で PNG 化してください。"
    , "    例: rsvg-convert input.svg -o output.png"
    , "        inkscape input.svg --export-png=output.png"
    , "  本 API は backend 実装後に自動的に動作します (= signature 安定)。"
    ]

-- | [日本語]: __現状 stub__: hgg-pdf backend は未実装の placeholder
--   (= 実行すると @error \"not implemented yet\"@)。 本 API は signature を先に
--   公開しておき、 backend 実装後に自動的に動くようにする。
--
--   暫定的に PDF が必要なら 'renderModelGraphSVG' で SVG を出力し、 別 tool
--   (= @rsvg-convert -f pdf@ / @inkscape --export-pdf@ 等) で PDF 化する経路を推奨。
--   [English]: __Currently a stub__: a placeholder pending the
--   hgg-pdf backend implementation (calling it raises @error "not
--   implemented yet"@). This API publishes its signature ahead of time so
--   it will start working automatically once the backend lands.
--
--   In the meantime, if PDF is needed, the recommended path is to output
--   SVG with 'renderModelGraphSVG' and convert it with an external tool
--   (e.g. @rsvg-convert -f pdf@ / @inkscape --export-pdf@).
renderModelGraphPDF :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraphPDF _path _titleTxt _mg =
  error $ unlines
    [ "renderModelGraphPDF: hgg-pdf backend が未実装の placeholder です。"
    , "  暫定回避: renderModelGraphSVG で SVG を出力し、 別 tool で PDF 化してください。"
    , "    例: rsvg-convert -f pdf input.svg -o output.pdf"
    , "        inkscape input.svg --export-pdf=output.pdf"
    , "  本 API は backend 実装後に自動的に動作します (= signature 安定)。"
    ]

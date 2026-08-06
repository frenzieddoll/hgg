-- |
-- Module      : Graphics.Hgg.Render.Layer
-- Description : orchestration + renderLayer dispatch + facet/subplot/legend/annotation/inset/marginal
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Render モノリス分割 (出力中立・純粋移動)。
--   [English]: Split out of the render monolith (an output-neutral, purely
--   mechanical move).
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphics.Hgg.Render.Layer where

import           Graphics.Hgg.Layout (Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      UCtx (..), resolvePosX, resolvePosY,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, extendedBreaks, scaleApply,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      polarClipPath,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical,
                                      effectiveHalfLine, effectiveLegendBaseSize,
                                      effectiveLegendKeyW, effectiveLegendKeyPitch,
                                      textWidthEm, legendGuideWidth,
                                      numToText, nubKeep, findColorEnc,
                                      effectiveLegendTitle, legendOrder,
                                      allColorCategories,
                                      LegendGuide(..), collectGuides)
import           Graphics.Hgg.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Graphics.Hgg.Layout.Grid    (GridCell (..), GridPlacement (..),
                                              flattenSubplots)  -- Phase 37 A3
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Graphics.Hgg.Spec   (Annotation (..), AxisFormat (..),
                                      ColData (..), ColRef,
                                      ColorEnc (..), ConnectSpec (..),
                                      DAGEdge (..), DAGLayoutAlgorithm (..),
                                      DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..), DAGSpec (..), Layer (..),
                                      RenderCtx (..), CustomMark (..),
                                      LegendPosition (..), LegendSpec (..),
                                      Inset (..), MarginalSpec (..), MarkKind (..),
                                      MarkShape (..), ShapeMapEntry (..),
                                      LineType (..), lineTypeDash, lineTypeForIndex,
                                      ReferenceLine (..), Resolver,
                                      Position (..), Coord (..),
                                      FacetScales (..), freeScaleX, freeScaleY,
                                      FacetSpace (..), freeSpaceX, freeSpaceY,
                                      ThemeOverride (..), TagStyle (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      applyDiscreteLimits, ridgeAutoFlip, selectedSubplots,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axisTextAngleXOf, axisTextAngleYOf,
                                      axShowGrid,
                                      FontSpec (..), orderedCats,
                                      colRefName, resolveCol, resolveNum,
                                      compositeLanes, inlineCat, reindexLayer)
import           Data.Char           (chr, ord)
import           Data.Maybe          (mapMaybe, isJust, listToMaybe)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Graphics.Hgg.Spec
import           Graphics.Hgg.Unit   (Length (..), LUnit (..))
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)

import           Graphics.Hgg.Primitive
import           Graphics.Hgg.Render.Common
import           Graphics.Hgg.Render.Basic
import           Graphics.Hgg.Render.Distribution
import           Graphics.Hgg.Render.Statistical
import           Graphics.Hgg.Render.MCMC
import           Graphics.Hgg.Render.Special


-- | [日本語]: spec → primitive 列。 layer ごとに mark kind に応じた変換 + 背景 +
--   title / xLabel / yLabel + 軸 + tick + facet panel grid。
--   [English]: Converts a spec into a list of primitives. For each layer,
--   dispatches on mark kind, then adds the background, title / xLabel /
--   yLabel, axes, ticks, and facet panel grid lines.
renderToPrimitives :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderToPrimitives r layout spec0
  | isDAGOnly spec        = renderDAGOnly layout spec    -- ★ §E-6 DAG 専用 path
  | isPieOnly spec        = renderPieStandalone r layout spec  -- ★ Phase 8 B1: 軸なし pie
  -- ★ Phase 36 B1c: ridge は標準軸 (連続 x = 値・カテゴリ y = 群) に載せ替え、 専用 path を廃止。
  | isEssOnly spec        = renderEssStandalone r layout spec   -- ★ Phase 8 B13: ess 専用
  | isAutocorrOnly spec   = renderAutocorrStandalone r layout spec -- ★ Phase 8 B12: autocorr 専用
  | hasSubplots           = renderSubplots r layout spec
  | hasFacetGrid          = renderFacetGrid r layout spec
  | otherwise = case getLast (vsFacet spec) of
      Just facetCol -> renderFaceted r layout spec facetCol
      Nothing       -> renderSingle r layout spec
  where
    -- ★ Phase 18 A2: 離散 limits を解決 (computeLayout と同じ・冪等・未指定 no-op)
    -- ★ Phase 19 A1: 凡例正本 (全 layer union) を glyph 側 lyColorCats へ注入
    spec = injectColorCats r (ridgeAutoFlip (applyDiscreteLimits r spec0))  -- ★ B1c: ridge→coord_flip
    hasSubplots  = not (null (vsSubplots spec))
    hasFacetGrid = isJust (getLast (vsFacetRow spec))
                || isJust (getLast (vsFacetCol spec))

-- | [日本語]: Pie 専用 spec か判定 (= 全 layer が MPie)。
--   [English]: Checks whether a spec is pie-only (all layers are 'MPie').
isPieOnly :: VisualSpec -> Bool
isPieOnly spec = case vsLayers spec of
  [] -> False
  ls -> all (\l -> case getFirst (lyKind l) of
                     Just MPie -> True
                     _         -> False) ls

-- | [日本語]: pie 専用描画 (= 軸 / tick / grid 無し、 PS renderPieOnly と同型)。
--   背景 + title だけ描き、 扇形 + 項目名ラベルは renderPie に委譲。
--   [English]: Draws pie charts standalone (no axes / ticks / grid, matching
--   PS renderPieOnly). Draws only the background and title; wedges and
--   category-name labels are delegated to 'renderPie'.
renderPieStandalone :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderPieStandalone r layout spec =
  let pal = specThemePalette spec
  in background layout pal
       <> labels layout spec pal
       <> concatMap (renderPie r layout pal) (vsLayers spec)

-- | [日本語]: Ess 専用 spec か判定 (= 全 layer が MEss)。
--   [English]: Checks whether a spec is ESS-only (all layers are 'MEss').
isEssOnly :: VisualSpec -> Bool
isEssOnly spec = case vsLayers spec of
  [] -> False
  ls -> all (\l -> case getFirst (lyKind l) of
                     Just MEss -> True
                     _         -> False) ls

-- | [日本語]: ess 専用描画。 x = 名前 (categorical) / y = ESS 値で軸が転置するため
--   Layout の tickMarks (x=値前提) を使わず、 renderESS が自前で軸 + y 目盛り + 名前を描く。
--   [English]: Draws ESS plots standalone. Since x = name (categorical) and
--   y = ESS value transposes the axes, this bypasses Layout's tickMarks
--   (which assume x = value); 'renderESS' draws its own axis plus y ticks
--   and names.
renderEssStandalone :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderEssStandalone r layout spec =
  let pal = specThemePalette spec
  in background layout pal
       <> labels layout spec pal
       <> concatMap (renderESS r layout pal) (vsLayers spec)

-- | [日本語]: Autocorr 専用 spec か判定 (= 全 layer が MAutocorr)。
--   [English]: Checks whether a spec is autocorr-only (all layers are
--   'MAutocorr').
isAutocorrOnly :: VisualSpec -> Bool
isAutocorrOnly spec = case vsLayers spec of
  [] -> False
  ls -> all (\l -> case getFirst (lyKind l) of
                     Just MAutocorr -> True
                     _              -> False) ls

-- | [日本語]: autocorr 専用描画。 x = lag / y = 相関で軸が転置するため Layout の
--   tickMarks (x=値前提) を使わず、 renderAutocorr が自前で軸 + lag/相関 を描く。
--   [English]: Draws autocorrelation plots standalone. Since x = lag and
--   y = correlation transposes the axes, this bypasses Layout's tickMarks
--   (which assume x = value); 'renderAutocorr' draws its own axis plus
--   lag/correlation ticks.
renderAutocorrStandalone :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderAutocorrStandalone r layout spec =
  let pal = specThemePalette spec
  in background layout pal
       <> labels layout spec pal
       <> concatMap (renderAutocorr r layout pal) (vsLayers spec)

-- | [日本語]: subplots layout (= 任意 spec を grid 並列)。
--   ★ (統一グリッド): vsSubplots / @<->@ / @<:>@ のネストを
--   'flattenSubplots' で __単一グリッド__へ平坦化し、 各 leaf パネルに
--   @(rowStart,rowSpan,colStart,colSpan)@ を割り当てる。 旧実装は各 subplots レベルが
--   独立に grid を組み、 ネストは renderToPrimitives で再帰描画していたため、 ネスト境界を
--   またいだパネル本体が整列しなかった。 平坦化により描画はこのグリッド 1 枚に対して
--   「列ごと左右帯・行ごと上下帯」 を 1 回だけ確保するだけになり、 任意の深さで本体が整列する。
--
--   gtable 配置 (patchwork 流): 各 leaf を span を含む推定セル寸法で独立 computeLayout し、
--   必要マージンを得る。 列ごとに左右帯 = 最大マージン (始まり列 / 終わり列で集約)、 行ごとに
--   上下帯 = 最大マージンを 1 回確保し、 残りをパネル本体として列/行で均等割り。 span パネルの
--   本体はまたぐ列/行の本体 + 内側帯 + pad を内包する。 container 自身の phantom 軸マージンは
--   既に除去済 (Layout の isContainer 分岐)。
--
--   ★既知の制約: 平坦化は leaf のみを残すため、 ネスト中間の subplots ノードに付けた
--   title/theme は描かれない (operator チェーンの中間ノードは純粋な構造なので通常問題ない。
--   全体 theme は top spec から themeCtx で全 leaf に伝播する)。
--   [English]: Lays out subplots (tiles arbitrary specs into a grid).
--   (unified grid): flattens the nesting of vsSubplots / @<->@ / @<:>@ via
--   'flattenSubplots' into a __single grid__, assigning each leaf panel
--   @(rowStart,rowSpan,colStart,colSpan)@. The previous implementation had
--   each subplots level build its own independent grid and recursed through
--   nesting in renderToPrimitives, so panel bodies did not align across
--   nesting boundaries. With flattening, rendering only needs to reserve
--   "left/right bands per column, top/bottom bands per row" once for this
--   single grid, so bodies align regardless of nesting depth.
--
--   gtable placement (patchwork-style): each leaf is independently run
--   through computeLayout at its estimated cell size (including span) to
--   obtain its required margins. Per column, the left/right band is the
--   maximum margin (aggregated over the starting/ending column); per row,
--   the top/bottom band is likewise the maximum margin, reserved once; the
--   remainder is split evenly across columns/rows as the panel body. A
--   spanning panel's body encloses the bodies of the columns/rows it spans
--   plus the inner bands and padding. The container's own phantom axis
--   margin has already been removed (the isContainer branch in Layout).
--
--   __Known limitation__: since flattening keeps only leaves, a
--   title/theme set on an intermediate subplots node in the nesting is not
--   drawn (intermediate nodes in the operator chain are purely structural,
--   so this is usually not an issue; the overall theme propagates from the
--   top spec to every leaf via themeCtx).
renderSubplots :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderSubplots r parentLayout spec =
  let pal    = specThemePalette spec
      -- 外側 theme を各 panel に伝播 (panel 自身の設定が優先 = 右辺勝ち)。
      -- これが無いと panel の文字色等が既定のままで、 暗テーマ panel に黒文字が乗る。
      themeCtx = mempty { vsTheme         = vsTheme spec
                        , vsThemeOverride = vsThemeOverride spec
                        , vsTitleFont     = vsTitleFont spec
                        , vsAxisLabelFont = vsAxisLabelFont spec
                        , vsTickFont      = vsTickFont spec
                        , vsLegendFont    = vsLegendFont spec }
      -- ★ Phase 37 A3: 入れ子も含め単一の統一グリッドへ平坦化 (vsPanelSel も toPTree 内で適用)。
      gp     = flattenSubplots spec
      gcols  = gpCols gp
      grows  = gpRows gp
      panels0 = gpPanels gp               -- [(leaf spec, GridCell)]
      -- ★ Phase 63 A7: panel 自動タグ (subplotTags = cowplot plot_grid labels="AUTO")。
      --   panel 列挙順に vsTag を注入する。 panel 自身の vsTag 明示指定が優先
      --   (Last の右勝ち = 個別 > 一括)。 注入位置を panels にすることで、
      --   computeLayout の margin 予約 (labsTagExtra) と labels の tag 描画が
      --   同じ tagged spec を見る (= 予約と描画の単一情報源)。
      panels = case getLast (vsSubplotTags spec) of
        Nothing  -> panels0
        Just sty -> [ (sub { vsTag = Last (Just (tagTextFor sty i)) <> vsTag sub }, c)
                    | (i, (sub, c)) <- zip [0 ..] panels0 ]
      area   = lpPlotArea parentLayout
      -- Phase 8 A2 Step3 (design §A-5): panel 間 spacing = ggplot panel.spacing 既定 = half_line。
      -- ★ Phase 63 A13: half_line = base/2 派生へ (既定 11 で従来と bit 同値)。
      pad    = effectiveHalfLine spec * lpMarginScale parentLayout
      -- ★ Phase 63 A6: 列/行の相対サイズ (subplotWidths/Heights = cowplot rel_widths/
      --   rel_heights)。 グリッド数に対して不足分は 1 で埋める。 未指定 = 全て 1 で
      --   従来の等分と同値。 推定寸法 (margin 算出用) と本体分割の両方を同じ重みで割る。
      weightsFor mws n = take n (maybe [] id (getLast mws) ++ repeat 1)
      colWs = weightsFor (vsSubplotWidths spec) gcols
      rowWs = weightsFor (vsSubplotHeights spec) grows
      colWSum = sum colWs
      rowWSum = sum rowWs
      -- 重み付きの単一セル寸法 (帯算出前の margin 推定用)。
      estColWOf j = (rW area - pad * fromIntegral (gcols - 1)) * (colWs !! j) / colWSum
      estRowHOf i = (rH area - pad * fromIntegral (grows - 1)) * (rowWs !! i) / rowWSum
      -- span を含む panel の推定寸法 (= またぐ列/行の本体 + 内側 pad)。
      estWOf c = sum [ estColWOf k | k <- [gcCol c .. gcCol c + gcColSpan c - 1] ]
                   + pad * fromIntegral (gcColSpan c - 1)
      estHOf c = sum [ estRowHOf k | k <- [gcRow c .. gcRow c + gcRowSpan c - 1] ]
                   + pad * fromIntegral (gcRowSpan c - 1)
      computed = [ (sub, c, computeLayout r (themeCtx <> sub
                       { vsWidth  = Last (Just (Length (estWOf c) Pt))
                       , vsHeight = Last (Just (Length (estHOf c) Pt)) }))
                 | (sub, c) <- panels ]
      mTopOf cl     = lpMarginTop cl
      mLeftOf cl    = lpMarginLeft cl
      mBotOf cl     = lpMarginBottom cl
      -- 右マージンは plotArea から導出 (lpMarginRight は保持されないため)。panel 推定幅基準。
      mRightOf c cl = estWOf c - lpMarginLeft cl - rW (lpPlotArea cl)
      -- 列 j の左帯 = 列 j 始まりの panel の mLeft 最大、 右帯 = 列 j 終わりの panel の mRight 最大。
      -- span パネルは始まり列に左帯・終わり列に右帯だけ寄与 (またぐ内側列には軸が無い)。
      colLeft j  = maximum (0 : [ mLeftOf cl    | (_, c, cl) <- computed, gcCol c == j ])
      colRight j = maximum (0 : [ mRightOf c cl | (_, c, cl) <- computed, gcCol c + gcColSpan c - 1 == j ])
      rowTop i   = maximum (0 : [ mTopOf cl     | (_, c, cl) <- computed, gcRow c == i ])
      rowBot i   = maximum (0 : [ mBotOf cl     | (_, c, cl) <- computed, gcRow c + gcRowSpan c - 1 == i ])
      sumLR = sum [ colLeft j + colRight j | j <- [0 .. gcols - 1] ]
      sumTB = sum [ rowTop i + rowBot i    | i <- [0 .. grows - 1] ]
      -- ★ Phase 63 A6: 本体を重みで分割 (全重み 1 なら旧 等分 bodyW/bodyH と同値)。
      bodyTotalW = rW area - sumLR - pad * fromIntegral (gcols - 1)
      bodyTotalH = rH area - sumTB - pad * fromIntegral (grows - 1)
      colBodyW j = max 1 (bodyTotalW * (colWs !! j) / colWSum)
      rowBodyH i = max 1 (bodyTotalH * (rowWs !! i) / rowWSum)
      colBodyX j = rX area + sum [ colLeft k + colBodyW k + colRight k + pad | k <- [0 .. j - 1] ] + colLeft j
      rowBodyY i = rY area + sum [ rowTop k + rowBodyH k + rowBot k + pad | k <- [0 .. i - 1] ] + rowTop i
      -- span panel の本体矩形: 開始列の本体左 〜 終了列の本体右 (内側帯 + pad を内包)。
      panelRectOf c =
        let c0 = gcCol c; c1 = c0 + gcColSpan c - 1
            r0 = gcRow c; r1 = r0 + gcRowSpan c - 1
            x0 = colBodyX c0;  x1 = colBodyX c1 + colBodyW c1
            y0 = rowBodyY r0;  y1 = rowBodyY r1 + rowBodyH r1
        in Rect x0 y0 (x1 - x0) (y1 - y0)
      bg = background parentLayout pal
      -- Phase 11 A5-a: subtitle/caption/tag も labels で描く (= title 同様に root レベル)。
      -- ★ 全体タイトル/subtitle/caption/tag は panel グリッドの bounding box (左端 =
      --   第1列 panel の y 軸 'colBodyX 0'、 右端 = 最終列本体右) に揃える。 親 plotArea の
      --   左端 (= キャンバス左マージン) 基準だと全体タイトルが各 panel の y 軸より左へ
      --   飛び出す (subplots / hbm 図のタイトル不揃い)。 通常図の「タイトル = y 軸線に揃う」
      --   と統一。 rY/rH は親のまま (= タイトルは上端・caption は下端のまま)。
      gridLeft  = colBodyX 0
      gridRight = colBodyX (gcols - 1) + colBodyW (gcols - 1)
      titleLayout = parentLayout
        { lpPlotArea = (lpPlotArea parentLayout) { rX = gridLeft, rW = gridRight - gridLeft } }
      title = case ( getLast (vsTitle spec), getLast (vsSubtitle spec)
                   , getLast (vsCaption spec), getLast (vsTag spec) ) of
        (Nothing, Nothing, Nothing, Nothing) -> []
        _                                    -> labels titleLayout spec pal
      subPrims = concat
        [ let panelRect = panelRectOf c
              subSized = themeCtx <> sub { vsWidth  = Last (Just (Length (estWOf c) Pt))
                                         , vsHeight = Last (Just (Length (estHOf c) Pt)) }
              finalLayout = subLayoutComputed
                { lpPlotArea = panelRect
                , lpXScale = scaleRetargetX (lpXScale subLayoutComputed) panelRect
                , lpYScale = scaleRetargetY (lpYScale subLayoutComputed) panelRect
                -- Phase 10 A5 (罠5): flip scale も panel に retarget しないと flip 時
                -- 全 panel が親 baseArea 基準で重なる。
                , lpXScaleFlipped = scaleRetargetY (lpXScaleFlipped subLayoutComputed) panelRect
                , lpYScaleFlipped = scaleRetargetX (lpYScaleFlipped subLayoutComputed) panelRect
                -- Phase 8 B16: viewport を 0 にして panel の background (全画面塗り) を抑制。
                , lpViewport = ViewportSize 0 0
                }
          -- leaf パネルを描画 (平坦化済みなので vsSubplots は空 = 再帰しない。 facet/mark はあり得る)。
          in renderToPrimitives r finalLayout subSized
        | (sub, c, subLayoutComputed) <- computed
        ]
  in bg <> title <> subPrims

-- | [日本語]: 'TagStyle' と panel index (0 始まり) から自動タグ文字列を作る。
--   26 panel 超は spreadsheet 流 bijective 26 進 (\"Z\" の次は \"AA\") で総関数にする
--   (cowplot は LETTERS 超過で NA だが、 「エラーにしない」 方針に合わせる)。
--   [English]: Builds the auto-tag string from a 'TagStyle' and a
--   zero-based panel index. Past 26 panels, this uses spreadsheet-style
--   bijective base-26 (after \"Z\" comes \"AA\") so the function stays
--   total (cowplot yields NA past LETTERS, but this follows the "never
--   error" policy instead).
tagTextFor :: TagStyle -> Int -> Text
tagTextFor TagNumeric i = T.pack (show (i + 1))
tagTextFor TagUpper   i = alphaTagFor 'A' i
tagTextFor TagLower   i = alphaTagFor 'a' i

alphaTagFor :: Char -> Int -> Text
alphaTagFor base = T.pack . reverse . go
  where
    go n = let (q, rest) = n `divMod` 26
           in chr (ord base + rest) : if q == 0 then [] else go (q - 1)

-- | [日本語]: DAG 専用 spec か判定 (= 全 layer が MDAG)。
--   [English]: Checks whether a spec is DAG-only (all layers are 'MDAG').
isDAGOnly :: VisualSpec -> Bool
isDAGOnly spec = case vsLayers spec of
  [] -> False
  ls -> all (\l -> case getFirst (lyKind l) of
                     Just MDAG -> True
                     _         -> False) ls

-- | [日本語]: DAG 専用描画。 ★他のプロット ('renderSingle') と__同じ枠組み__に統一した。
--   'Graphics.Hgg.Layout.computeLayout' が確保した 'lpPlotArea' (= title 帯 + 軸目盛りマージンを引いた軸内領域) に
--   DAG を描き、 title も 'labels' で標準位置 (他パネルと同じ高さ) に描く。 ただし
--   __軸・グリッド・枠・目盛り・軸タイトルは一切描かない__ (= DAG では常に非表示)。
--   'labels' は title のみ描く (DAG spec は xLabel/yLabel を持たないので軸タイトルは出ない)。
--
--   これにより DAG パネルの title 位置・plot area 枠が他パネルと揃う (subplot セルでも同じ:
--   'labels' は viewport でなく lpPlotArea ± margin 基準で配置するため・'lpPlotArea' は親が
--   panelRect に retarget 済)。 旧実装は viewport±pad で独自に area/title を作っており、 DAG
--   だけ title がずれ、 入れ子セルから漏れていた (旧来の viewport 特例も本統一で不要に)。
--   [English]: Draws DAGs standalone. Unified with the __same framework__
--   as other plots ('renderSingle'). Draws the DAG inside the 'lpPlotArea'
--   reserved by 'Graphics.Hgg.Layout.computeLayout' (the in-axis region after subtracting the
--   title band and axis-tick margins), and draws the title at the standard
--   position via 'labels' (the same height as other panels). However,
--   __axes, grid, frame, ticks, and axis titles are never drawn__ (always
--   hidden for DAGs). 'labels' draws only the title (a DAG spec has no
--   xLabel/yLabel, so no axis title appears).
--
--   This keeps a DAG panel's title position and plot-area frame aligned
--   with other panels (the same holds inside subplot cells: 'labels'
--   positions relative to lpPlotArea ± margin rather than the viewport, and
--   the parent has already retargeted 'lpPlotArea' to the panelRect). The
--   previous implementation built its own area/title from viewport±pad,
--   which made only the DAG title drift and leak out of nested cells (the
--   old viewport special-case is no longer needed under this unification).
renderDAGOnly :: Layout -> VisualSpec -> [Primitive]
renderDAGOnly layout spec =
  let pal = specThemePalette spec
  in background layout pal
       <> labels layout spec pal
       <> concatMap (renderDAGStandalone (lpPlotArea layout) pal) (vsLayers spec)

renderSingle :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderSingle r layout spec =
  let pal = specThemePalette spec
      fmtX = axisFormatOf (vsXAxis spec)
      fmtY = axisFormatOf (vsYAxis spec)
      rotX = resolveAxisAngle (vsXAxis spec) (axisTextAngleXOf (vsThemeOverride spec))
      rotY = resolveAxisAngle (vsYAxis spec) (axisTextAngleYOf (vsThemeOverride spec))
      showX = axisShowTicksOf (vsXAxis spec)
      showY = axisShowTicksOf (vsYAxis spec)
      -- Phase 26 §C-2 #10: marginal histogram のため plot area を縮める
      (mainLayout, marginalPrims) = applyMarginal r pal layout spec
      -- ★ Phase 33 B6: 注釈/参照線の Pos 解決に dpi (PAbs Px 用)。layout は pt なので
      --   dpi は px 入力の pt 化にだけ使う (既定 96)。
      annotDpi = maybe 96 id (getLast (Graphics.Hgg.Spec.vsDpi spec))
      -- Phase 11 A7-a: coord_cartesian zoom が有効なときだけ glyph を panel に clip
      --   (= 範囲外データが panel 外にはみ出すのを防ぐ)。 未指定では従来同一 (ゼロ diff)。
      hasCoordLim = getLast (vsCoordXLim spec) /= Nothing
                 || getLast (vsCoordYLim spec) /= Nothing
      -- ★ Phase 64 A8 (B-2): polar は常に外周円で clip する。 それまでは
      --   coordXLim/YLim 指定時の矩形 clip しか無く、 半径が範囲外の点が
      --   外周円の外にはみ出して描かれていた。
      coordClip prims
        | polar       = PClipPath [ Point x y | (x, y) <- polarClipPath mainLayout ]
                        : prims ++ [PClipPop]
        | hasCoordLim = PClipPush (lpPlotArea mainLayout) : prims ++ [PClipPop]
        | otherwise   = prims
      -- Phase 11 A7-c: 極座標は直交 grid/枠/tick の代わりに polarGrid (同心円 + スポーク)。
      polar = isPolar (coordOf spec)
      gridAxisPrims
        | polar     = polarGrid spec mainLayout pal
        | otherwise = gridLines mainLayout spec pal
                   <> axisFrame mainLayout pal
                   <> tickMarks (Just spec) mainLayout pal fmtX fmtY rotX rotY showX showY
  in background mainLayout pal
       -- Phase 9 A-1: plot bg の上に panel 背景 (theme_grey/ブランド) → その上に grid。
       <> panelBackground mainLayout pal
       -- TODO-3b (2026-05-29): grid line を axisFrame 直後 (= layer の下) に
       -- 描いて、 mark / refLine が grid の上に乗る順にする (= PS と同順序)。
       <> gridAxisPrims
       -- Phase 8 C (marginal fix): title/軸ラベルは marginal で縮小する前の元 layout 基準で
       -- 描く (= PS と同一)。 mainLayout (縮小後) だと title が marginal 帯に食い込む。
       -- marginal は上端/右端のみ縮小し下端・左端は不変なので xLabel/yLabel も整合。
       <> labels    layout spec pal
       -- Phase 8 B22: 右 Y 軸対象 layer は yScale を右軸 scale に swap して描画。
       <> coordClip (concatMap (renderLayerDual r mainLayout pal) (vsLayers spec))
       <> renderRightYAxis mainLayout pal (axisFormatOf (vsYAxisRight spec))
       <> concatMap (renderRefLine annotDpi mainLayout pal) (vsRefLines spec)
       <> concatMap (renderAnnotation annotDpi mainLayout pal) (vsAnnotations spec)
       <> renderLegend r mainLayout pal spec
       <> marginalPrims
       -- Phase 8 B21: inset (図中図)。 HS は従来 vsInsets を全く描いていなかった。
       <> concatMap (renderInset r layout pal) (vsInsets spec)

-- | [日本語]: scatter の周辺に X/Y histogram を sub-plot として配置。
--   main plot area を縮めて余白に小さな histogram を描く。
--   [English]: Places X/Y histograms as sub-plots around a scatter plot.
--   Shrinks the main plot area and draws the small histograms into the
--   freed margin.
applyMarginal :: Resolver -> ThemePalette -> Layout -> VisualSpec -> (Layout, [Primitive])
applyMarginal r pal layout spec = case getLast (vsMarginal spec) of
  Nothing -> (layout, [])
  Just ms ->
    let baseArea = lpPlotArea layout
        topH  = if msShowX ms then 60 else 0
        rightW = if msShowY ms then 60 else 0
        gutter = if topH > 0 || rightW > 0 then 8 else 0
        mainArea = Rect (rX baseArea)
                        (rY baseArea + topH + (if topH > 0 then gutter else 0))
                        (rW baseArea - rightW - (if rightW > 0 then gutter else 0))
                        (rH baseArea - topH - (if topH > 0 then gutter else 0))
        -- ★ Phase 8 B18: plotArea を縮めるだけでなく xScale/yScale の range も mainArea に
        -- 再ターゲットする。 さもないと mark (散布図点など) や marginalHist が元の大きい枠
        -- 基準で位置決めされ、 縮小した軸枠からはみ出す (= scale の range = 位置決め基準)。
        mainLayout = layout
          { lpPlotArea = mainArea
          , lpXScale = scaleRetargetX (lpXScale layout) mainArea
          , lpYScale = scaleRetargetY (lpYScale layout) mainArea
          -- Phase 10 A5 (罠5): flip scale も追従 (XFlipped=縦 range, YFlipped=横 range)。
          , lpXScaleFlipped = scaleRetargetY (lpXScaleFlipped layout) mainArea
          , lpYScaleFlipped = scaleRetargetX (lpYScaleFlipped layout) mainArea
          }
        nBins = msBins ms
        -- 最初の layer の encX/encY を marginal source として使う
        firstLayer = case vsLayers spec of
          (l:_) -> Just l
          []    -> Nothing
        xPrims = if msShowX ms
          then case firstLayer >>= getLast . lyEncX of
                 Just cr ->
                   let topArea = Rect (rX mainArea) (rY baseArea) (rW mainArea) topH
                       sx = lpXScale mainLayout
                   in marginalHist r pal cr topArea sx nBins False
                 Nothing -> []
          else []
        yPrims = if msShowY ms
          then case firstLayer >>= getLast . lyEncY of
                 Just cr ->
                   let rightArea = Rect (rX mainArea + rW mainArea + gutter)
                                        (rY mainArea) rightW (rH mainArea)
                       sy = lpYScale mainLayout
                   in marginalHist r pal cr rightArea sy nBins True
                 Nothing -> []
          else []
    in (mainLayout, xPrims <> yPrims)

-- | [日本語]: 単一 ColRef を histogram として与えられた area に描画。
--   isVertical=False (= X marginal、 上に置く、 bar は縦)、
--   isVertical=True  (= Y marginal、 右に置く、 bar は横)。
--   [English]: Draws a single 'ColRef' as a histogram into the given area.
--   isVertical=False draws the X marginal (placed above, vertical bars);
--   isVertical=True draws the Y marginal (placed to the right, horizontal
--   bars).
marginalHist :: Resolver -> ThemePalette -> ColRef -> Rect -> Graphics.Hgg.Layout.Scale -> Int -> Bool -> [Primitive]
marginalHist r pal cr area scaleAlong nBins isVertical =
  case resolveNum r cr of
    Nothing -> []
    Just v | V.null v -> []
    Just v ->
      let xs = V.toList v
          lo = minimum xs
          hi = maximum xs
          binW = (hi - lo) / fromIntegral nBins
          binIx vx = min (nBins - 1) (max 0 (floor ((vx - lo) / binW)))
          counts = foldl (\acc vx ->
                          let i = binIx vx
                              cur = acc !! i
                          in take i acc <> [cur + 1] <> drop (i + 1) acc)
                         (replicate nBins (0 :: Int)) xs
          maxC = max 1 (maximum counts)
          c = tpDefault pal
          a = 0.6
      in [ let xPos = scaleApply scaleAlong (lo + fromIntegral i * binW)
               xPos' = scaleApply scaleAlong (lo + fromIntegral (i + 1) * binW)
               normH = fromIntegral cnt / fromIntegral maxC
           in if isVertical
                then -- Y marginal: 右領域、 bar は横 (left → right)
                  let barW = normH * rW area
                  in PRect (Rect (rX area) (min xPos xPos') barW (abs (xPos - xPos')))
                           (FillStyle c a) (Just (StrokeStyle c 0.5))
                else -- X marginal: 上領域、 bar は縦 (bottom → top)
                  let barH = normH * rH area
                  in PRect (Rect (min xPos xPos') (rY area + rH area - barH)
                                 (abs (xPos - xPos')) barH)
                           (FillStyle c a) (Just (StrokeStyle c 0.5))
         | (i, cnt) <- zip [0 .. nBins - 1] counts ]

-- | [日本語]: facet 列の distinct 値ごとに plot area を grid 分割し、
--   各セルに「その facet 値だけの sub-resolver」 で sub-spec を描画。
--   簡易実装: 1 行 N 列 (= horizontal flow)、 各 panel は独立 axis (= shared
--   軸の縮尺対応は後続)。
--   [English]: Splits the plot area into a grid for each distinct value of
--   the facet column, drawing the sub-spec in each cell with a sub-resolver
--   restricted to that facet value's rows. Simple implementation: 1 row × N
--   columns (horizontal flow); each panel has an independent axis (shared
--   axis scaling support comes later).
renderFaceted :: Resolver -> Layout -> VisualSpec -> ColRef -> [Primitive]
renderFaceted r layout spec facetCol =
  let pal = specThemePalette spec
      -- Phase 62 A2: facet 行 vec は facetVecOf に一元化 (subsetInlineSpec と共有)。
      facetVec = facetVecOf r facetCol
      nFacet   = length facetVec
      -- Phase 28: facet panel 順も ggplot 同様アルファベット順 (= R4DS facet_wrap)。
      facetVals = orderedCats facetVec
      nPanels = length facetVals
  in if nPanels == 0 then renderSingle r layout spec
     else
       let baseArea = lpPlotArea layout
           -- Phase 8 A2 Step3 (design §A-5): panel.spacing = half_line (sc 縮小)。
           gutter   = effectiveHalfLine spec * lpMarginScale layout   -- ★ A13: base 派生
           headerH  = 18    -- panel 上の strip label (群名) 用
           -- Phase 8 C G7: facet_wrap 複数行。 vsFacetNcol 未指定 = 1 行 N 列 (非破壊)、
           -- 指定 = n 列で nRows = ceil(nPanels/n) 行に折り返す。 軸 drop は ggplot 流に
           -- 「同列の下に panel が無い (= 最下) panel のみ x 軸」「左端列のみ y 軸」。
           nCols = case getLast (vsFacetNcol spec) of
                     Just n | n > 0 -> min n nPanels
                     _              -> nPanels
           nRows = (nPanels + nCols - 1) `div` nCols
           -- Phase 8 C (gtable §E-3): 列/行を solveTracks トラックで割付。 列 = nCols 個の
           -- Null + (nCols-1) gutter、 行 = 各行 [Fixed headerH(strip), Null panel] を gutter
           -- 区切り。 Null トラックがパネル本体 (= 従来の cellW/cellH 均等割りと数値同値)。
           -- ★ A6: free y は各 panel が独立 y 軸を出すため、 panel 間 gutter に y 軸帯
           --   (lpMarginLeft 相当) を加算し、 目盛りラベルが左隣 panel に被るのを防ぐ。
           --   fixed (左端列のみ y 軸) は yBand=0 で従来不変。
           yBand   = if freeY then lpMarginLeft layout else 0
           hTracks = concat [ if c == 0 then [Null 1] else [Fixed (gutter + yBand), Null 1]
                            | c <- [0 .. nCols - 1] ]
           vTracks = concat [ (if rr == 0 then [] else [Fixed gutter])
                              ++ [Fixed (fromIntegral headerH), Null 1]
                            | rr <- [0 .. nRows - 1] ]
           nullSlots ts solved = [ sl | (Null _, sl) <- zip ts solved ]
           hPanels = nullSlots hTracks (solveTracks (rX baseArea) (rW baseArea) hTracks)
           vPanels = nullSlots vTracks (solveTracks (rY baseArea) (rH baseArea) vTracks)
           fmtX = axisFormatOf (vsXAxis spec)
           fmtY = axisFormatOf (vsYAxis spec)
           rotX = resolveAxisAngle (vsXAxis spec) (toAxisTextAngle (vsThemeOverride spec))
           rotY = resolveAxisAngle (vsYAxis spec) (toAxisTextAngle (vsThemeOverride spec))
           showXt = axisShowTicksOf (vsXAxis spec)
           showYt = axisShowTicksOf (vsYAxis spec)
           -- Phase 8 B19: panel には title/軸ラベルを持たせない (= 全体で 1 回だけ描く)。
           specPanel = spec { vsFacet = Last Nothing, vsTitle = Last Nothing
                            , vsXLabel = Last Nothing, vsYLabel = Last Nothing
                            , vsSubtitle = Last Nothing, vsCaption = Last Nothing
                            , vsTag = Last Nothing }
           (stripBg, showStrip) = themeStripStyle spec   -- Phase 9 A-4
           -- ★ Phase 11 A7-b: facet free scales。 free な軸は panel ごとに sub-resolver で
           --   computeLayout して独立 domain を得る (= subplots と同手法)、 fixed は従来通り
           --   parent layout の scale を range だけ retarget。
           facetSc  = maybe FacetFixed id (getLast (vsFacetScales spec))
           freeX    = freeScaleX facetSc
           freeY    = freeScaleY facetSc
           panelFor (idx, val) =
             let col = idx `mod` nCols
                 row = idx `div` nCols
                 (cellX, cellW)     = hPanels !! col
                 (panelTop, panelH) = vPanels !! row
                 -- ★ Phase 8 B19: plotArea だけでなく scale range も panelArea に再ターゲット
                 -- (= B18 marginal と同根。 これをしないと点が baseArea 全幅基準で描かれ、
                 -- 全 panel が重なって左/中央が空に見える)。
                 panelArea = Rect cellX panelTop cellW panelH
                 subResolver = filterResolver r facetCol val
                 -- Phase 62 A2: inline encoding は Resolver を通らないため、 spec 側も
                 -- 同じ keepIdx で部分列化する (基準の一元化 = facetVecOf/facetKeepIdx)。
                 specSub = subsetInlineSpec nFacet (facetKeepIdx facetVec val) specPanel
                 -- free な軸の domain は panel 自身のデータで再計算 (fixed は parent layout)。
                 panelDomLayout
                   | freeX || freeY = computeLayout subResolver specSub
                   | otherwise      = layout
                 xSrc = if freeX then panelDomLayout else layout
                 ySrc = if freeY then panelDomLayout else layout
                 subLayout = layout
                   { lpPlotArea = panelArea
                   , lpXScale = scaleRetargetX (lpXScale xSrc) panelArea
                   , lpYScale = scaleRetargetY (lpYScale ySrc) panelArea
                   -- Phase 10 A5 (罠5): flip scale も panel に retarget (XFlipped=縦, YFlipped=横)。
                   , lpXScaleFlipped = scaleRetargetY (lpXScaleFlipped xSrc) panelArea
                   , lpYScaleFlipped = scaleRetargetX (lpYScaleFlipped ySrc) panelArea
                   -- free 軸は panel 固有の tick / categorical label / 明示 label を採用。
                   , lpXTicks = lpXTicks xSrc
                   , lpYTicks = lpYTicks ySrc
                   , lpXCategoryLabels = lpXCategoryLabels xSrc
                   , lpYCategoryLabels = lpYCategoryLabels ySrc
                   , lpXTickLabels = lpXTickLabels xSrc
                   , lpYTickLabels = lpYTickLabels ySrc }
                 isLeft   = col == 0                  -- y tick は左端列のみ
                 isBottom = idx + nCols >= nPanels    -- 同列の下に panel が無い = x 軸あり
                 -- Phase 10 A5 ②-fix: flip では tickMarks がデータ x を縦軸(左)・データ y を
                 -- 横軸(下)に転置するので、 内側軸 drop の gating も入替える (gateX=データ x 軸の
                 -- 可視判定、 gateY=データ y 軸の可視判定)。 入替えないと縦軸が全 panel に出て重なり、
                 -- 横軸が左端 panel しか出ない (= ユーザ browser 報告と一致したバグ)。
                 -- ★ A7-b: free 軸は各 panel が独立 scale を持つので全 panel に軸を表示。
                 (baseGX, baseGY) = case lpCoord layout of
                                    CoordFlip -> (isLeft, isBottom)
                                    _         -> (isBottom, isLeft)
                 gateX = freeX || baseGX
                 gateY = freeY || baseGY
                 -- Phase 9 A-4: strip 背景帯 (灰矩形) を文字の背後に。 帯 = panel 上の headerH 分。
                 stripRect = [ PRect (Rect cellX (panelTop - fromIntegral headerH) cellW (fromIntegral headerH))
                                     (FillStyle stripBg 1.0) Nothing
                             | showStrip ]
                 header = stripRect <>
                          [ PText
                              (Point (cellX + cellW / 2) (panelTop - 4))
                              val
                              (mkFontTS (Just spec) pal TickF AnchorMiddle 0) ]
             in header
                  <> panelBackground subLayout pal
                  <> axisFrame subLayout pal
                  <> gridLines subLayout specPanel pal
                  <> tickMarks (Just specPanel) subLayout pal fmtX fmtY rotX rotY
                               (showXt && gateX) (showYt && gateY)
                  <> concatMap (renderLayer subResolver subLayout pal) (vsLayers specSub)
       in background layout pal
            <> labels layout spec pal
            <> concatMap panelFor (zip [0..] facetVals)
            -- ★ Phase 34: facet でも凡例を描画 (color/shape encoding 用)。予約は
            --   computeLayout 側で行うので baseArea は既に凡例ぶん縮んでいる。
            <> renderLegend r layout pal spec

-- | [日本語]: facet_grid(row ~ col)。 2 変数 cross 配置。
--   row 変数の distinct levels で行、 col 変数の distinct levels で列を作り、 panel(r,c) は
--   両条件 (row==rowVal && col==colVal) を満たす行のみで描く。 strip は上 (col 名・各列頭)・
--   右 (row 名・各行端、 縦書き)、 軸は最下行 x・左端列 y のみ (ggplot facet_grid 既定の内側
--   軸 drop)。 片方のみ指定なら 1 行 (col のみ) / 1 列 (row のみ) の grid。
--   全 panel は共通スケール (= renderFaceted 同様、 値比較可)。
--   [English]: Implements facet_grid(row ~ col) — a two-variable cross
--   layout. Rows come from the distinct levels of the row variable, columns
--   from the distinct levels of the col variable; panel(r,c) is drawn only
--   from rows that satisfy both conditions (row==rowVal && col==colVal).
--   Strips appear on top (col name, once per column head) and on the right
--   (row name, once per row end, drawn vertically); axes appear only on the
--   bottom row (x) and leftmost column (y) — ggplot's default inner-axis
--   drop for facet_grid. If only one of row/col is given, the grid becomes
--   a single row (col only) or a single column (row only). All panels share
--   the same scale (as with 'renderFaceted', values remain comparable).
renderFacetGrid :: Resolver -> Layout -> VisualSpec -> [Primitive]
renderFacetGrid r layout spec =
  let pal = specThemePalette spec
      mRowCol = getLast (vsFacetRow spec)
      mColCol = getLast (vsFacetCol spec)
      -- Phase 62 A2: 行 vec は facetVecOf に一元化 (subsetInlineSpec と共有)。
      rowVec  = maybe [] (facetVecOf r) mRowCol
      colVec  = maybe [] (facetVecOf r) mColCol
      -- Phase 28: facet_grid もアルファベット順
      rowVals = if isJust mRowCol then orderedCats rowVec else [""]
      colVals = if isJust mColCol then orderedCats colVec else [""]
      nRows = length rowVals
      nCols = length colVals
  in if nRows == 0 || nCols == 0 then renderSingle r layout spec
     else
       let baseArea = lpPlotArea layout
           gutter   = effectiveHalfLine spec * lpMarginScale layout   -- ★ A13: base 派生
           hasRowStrip = isJust mRowCol
           hasColStrip = isJust mColCol
           stripTopH   = if hasColStrip then 18 else 0
           stripRightW = if hasRowStrip then 18 else 0
           -- ★ Phase 11 A7-b: facet_grid free scales + space。 free_x は **列ごと共有 x
           --   domain** (列内の全行データ)、 free_y は **行ごと共有 y domain** (行内の全列
           --   データ)。 space free は track 重みを各列/行の data 範囲に比例配分する。
           facetSc = maybe FacetFixed id (getLast (vsFacetScales spec))
           freeX   = freeScaleX facetSc
           freeY   = freeScaleY facetSc
           facetSp = maybe SpaceFixed id (getLast (vsFacetSpace spec))
           spX     = freeSpaceX facetSp
           spY     = freeSpaceY facetSp
           -- Phase 62 A2: grid の facet 列長 (行/列とも同じ元データ行数。 片方のみ指定
           -- なら在る方)。 inline 部分列化の対象判定 n に使う。
           nFacetG = max (length rowVec) (length colVec)
           -- 片軸のみの keepIdx (free-scale domain 用) と、 row×col 交差の keepIdx。
           keepIdxCol cv = if null colVec then [0 .. nFacetG - 1]
                           else facetKeepIdx colVec cv
           keepIdxRow rv = if null rowVec then [0 .. nFacetG - 1]
                           else facetKeepIdx rowVec rv
           colDomLayout c = let cv  = colVals !! c
                                res = maybe r (\cc -> filterResolver r cc cv) mColCol
                            in computeLayout res (subsetInlineSpec nFacetG (keepIdxCol cv) specPanel)
           rowDomLayout rr = let rv  = rowVals !! rr
                                 res = maybe r (\rc -> filterResolver r rc rv) mRowCol
                             in computeLayout res (subsetInlineSpec nFacetG (keepIdxRow rv) specPanel)
           colXLayouts = [ colDomLayout c | c <- [0 .. nCols - 1] ]   -- memoize
           rowYLayouts = [ rowDomLayout rr | rr <- [0 .. nRows - 1] ]
           spanX lay = abs (lsDomainHi (lpXScale lay) - lsDomainLo (lpXScale lay))
           spanY lay = abs (lsDomainHi (lpYScale lay) - lsDomainLo (lpYScale lay))
           colWeight c  = if spX then max 1e-9 (spanX (colXLayouts !! c)) else 1
           rowWeight rr = if spY then max 1e-9 (spanY (rowYLayouts !! rr)) else 1
           -- gtable §E-3: 列 = nCols 個 Null (gutter 区切り) + 右 strip 帯、
           --   行 = 上 strip 帯 + nRows 個 Null (gutter 区切り)。 space free 時は Null 重みを
           --   data 範囲に比例 (= ggplot facet_grid(space=) と同じく単位長を揃える)。
           hTracks = concat [ [Null (colWeight c)] ++ (if c < nCols - 1 then [Fixed gutter] else [])
                            | c <- [0 .. nCols - 1] ]
                     ++ [Fixed stripRightW]
           vTracks = [Fixed stripTopH]
                     ++ concat [ (if rr > 0 then [Fixed gutter] else []) ++ [Null (rowWeight rr)]
                               | rr <- [0 .. nRows - 1] ]
           nullSlots ts solved = [ sl | (Null _, sl) <- zip ts solved ]
           hPanels = nullSlots hTracks (solveTracks (rX baseArea) (rW baseArea) hTracks)
           vPanels = nullSlots vTracks (solveTracks (rY baseArea) (rH baseArea) vTracks)
           fmtX = axisFormatOf (vsXAxis spec)
           fmtY = axisFormatOf (vsYAxis spec)
           rotX = resolveAxisAngle (vsXAxis spec) (toAxisTextAngle (vsThemeOverride spec))
           rotY = resolveAxisAngle (vsYAxis spec) (toAxisTextAngle (vsThemeOverride spec))
           showXt = axisShowTicksOf (vsXAxis spec)
           showYt = axisShowTicksOf (vsYAxis spec)
           specPanel = spec { vsFacetRow = Last Nothing, vsFacetCol = Last Nothing
                            , vsTitle = Last Nothing
                            , vsXLabel = Last Nothing, vsYLabel = Last Nothing
                            , vsSubtitle = Last Nothing, vsCaption = Last Nothing
                            , vsTag = Last Nothing }
           (stripBg, showStrip) = themeStripStyle spec   -- Phase 9 A-4
           -- 上 strip (col 名): 各列頭に 1 つ。 中央 = panel 中心 x、 y = 上帯中央。 背景帯を背後に。
           colStrips = concat
                       [ [ PRect (Rect cellX (rY baseArea) cellW stripTopH) (FillStyle stripBg 1.0) Nothing
                         | showStrip ]
                         <> [ PText (Point (cellX + cellW / 2) (rY baseArea + stripTopH / 2))
                                    cv (mkFontTS (Just spec) pal TickF AnchorMiddle 0) ]
                       | hasColStrip
                       , (c, cv) <- zip [0..] colVals
                       , let (cellX, cellW) = hPanels !! c ]
           -- 右 strip (row 名): 各行端に 1 つ。 縦書き (rotate 90)。 背景帯を背後に。
           rowStrips = concat
                       [ [ PRect (Rect (rX baseArea + rW baseArea - stripRightW) panelTop stripRightW panelH)
                                 (FillStyle stripBg 1.0) Nothing
                         | showStrip ]
                         <> [ PText (Point (rX baseArea + rW baseArea - stripRightW / 2)
                                          (panelTop + panelH / 2))
                                    rv (mkFontTS (Just spec) pal TickF AnchorMiddle (-90)) ]  -- Phase 50 A1: CCW canonical (facet row strip 縦書き)
                       | hasRowStrip
                       , (rr, rv) <- zip [0..] rowVals
                       , let (panelTop, panelH) = vPanels !! rr ]
           panelFor row col =
             let rowVal = rowVals !! row
                 colVal = colVals !! col
                 (cellX, cellW)     = hPanels !! col
                 (panelTop, panelH) = vPanels !! row
                 panelArea = Rect cellX panelTop cellW panelH
                 -- ★ A7-b: free_x は列 domain layout、 free_y は行 domain layout を scale 源に。
                 xSrc = if freeX then colXLayouts !! col else layout
                 ySrc = if freeY then rowYLayouts !! row else layout
                 subLayout = layout
                   { lpPlotArea = panelArea
                   , lpXScale = scaleRetargetX (lpXScale xSrc) panelArea
                   , lpYScale = scaleRetargetY (lpYScale ySrc) panelArea
                   -- Phase 10 A5 (罠5): flip scale も panel に retarget (XFlipped=縦, YFlipped=横)。
                   , lpXScaleFlipped = scaleRetargetY (lpXScaleFlipped xSrc) panelArea
                   , lpYScaleFlipped = scaleRetargetX (lpYScaleFlipped ySrc) panelArea
                   -- free 軸は列/行 固有の tick / categorical / 明示 label。
                   , lpXTicks = lpXTicks xSrc
                   , lpYTicks = lpYTicks ySrc
                   , lpXCategoryLabels = lpXCategoryLabels xSrc
                   , lpYCategoryLabels = lpYCategoryLabels ySrc
                   , lpXTickLabels = lpXTickLabels xSrc
                   , lpYTickLabels = lpYTickLabels ySrc }
                 applyRow res = maybe res (\rc -> filterResolver res rc rowVal) mRowCol
                 applyCol res = maybe res (\cc -> filterResolver res cc colVal) mColCol
                 subResolver = applyCol (applyRow r)
                 -- Phase 62 A2: inline encoding 用の spec 側部分列化 (row∩col の交差)。
                 keepIdxRC = [ i | i <- [0 .. nFacetG - 1]
                                 , null rowVec || rowVec !! i == rowVal
                                 , null colVec || colVec !! i == colVal ]
                 specSub = subsetInlineSpec nFacetG keepIdxRC specPanel
                 isLeft   = col == 0           -- y tick は左端列のみ
                 isBottom = row == nRows - 1   -- x tick は最下行のみ
                 -- Phase 10 A5 ②-fix: flip で軸転置に合わせ内側軸 drop の gating を入替 (renderFaceted と同様)。
                 (gateX, gateY) = case lpCoord layout of
                                    CoordFlip -> (isLeft, isBottom)
                                    _         -> (isBottom, isLeft)
             in panelBackground subLayout pal
                  <> axisFrame subLayout pal
                  <> gridLines subLayout specPanel pal
                  <> tickMarks (Just specPanel) subLayout pal fmtX fmtY rotX rotY
                               (showXt && gateX) (showYt && gateY)
                  <> concatMap (renderLayer subResolver subLayout pal) (vsLayers specSub)
       in background layout pal
            <> labels layout spec pal
            <> colStrips
            <> rowStrips
            <> concat [ panelFor row col | row <- [0 .. nRows - 1], col <- [0 .. nCols - 1] ]

-- | [日本語]: facet 列を Text 行ベクタへ (keepIdx 算出の唯一の源)。
--   'filterResolver' (ColByName 経路) と 'subsetInlineSpec' (inline 経路) の
--   分割基準がずれないよう、 両者ともここを通す。
--   [English]: Converts the facet column to a Text row vector (the single
--   source used to compute keepIdx). Both 'filterResolver' (the ColByName
--   path) and 'subsetInlineSpec' (the inline path) go through here, so
--   their splitting criteria never diverge.
facetVecOf :: Resolver -> ColRef -> [Text]
facetVecOf r cr = case resolveCol r cr of
  Just (TxtData v) -> V.toList v
  Just (NumData v) -> map (T.pack . show) (V.toList v)
  Nothing          -> []

-- | [日本語]: facet 値 @val@ に一致する行 index。
--   [English]: The row indices that match the facet value @val@.
facetKeepIdx :: [Text] -> Text -> [Int]
facetKeepIdx vec val = [i | (i, v) <- zip [0 ..] vec, v == val]

-- | [日本語]: resolver wrap: facet 列が val に一致する行のみ通すフィルタ。
--   他列も同じ index で抽出。
--   [English]: A resolver wrapper that filters to rows where the facet
--   column equals val; other columns are extracted using the same indices.
filterResolver :: Resolver -> ColRef -> Text -> Resolver
filterResolver base facetCol val = \name ->
  let keepIdx = facetKeepIdx (facetVecOf base facetCol) val
      pickFrom vs = [vs !! i | i <- keepIdx, i < length vs]
  in case base name of
       Just (NumData v) -> Just (NumData (V.fromList (pickFrom (V.toList v))))
       Just (TxtData v) -> Just (TxtData (V.fromList (pickFrom (V.toList v))))
       Nothing          -> Nothing

-- | [日本語]: (§1) 'filterResolver' と対になる __spec 側の部分列化__。
--   inline (ColNum/ColTxt) の encoding は Resolver を通らないため resolver wrap では
--   絞れない (= facet が全 panel に同一データを描くバグの root)。 panel の keepIdx で
--   spec 直下 layer の inline 列を部分ベクタへ差し替える。 ColByName は従来通り
--   subResolver 側で絞られるので触らない ('reindexLayer' がその区別を持つ)。
--   長さが facet 列長 @n@ と一致する inline のみ対象 — 不一致は黙って切り詰めず
--   据え置く (§3 = 検出して警告する側の対象)。
--   [English]: (§1) The __spec-side counterpart__ to 'filterResolver'.
--   Inline (ColNum/ColTxt) encodings never go through a Resolver, so a
--   resolver wrap cannot restrict them (this was the root cause of the bug
--   where facets drew identical data in every panel). Using the panel's
--   keepIdx, this substitutes a partial vector for the inline columns of
--   layers directly under the spec. ColByName is left untouched, since it
--   is restricted on the subResolver side as before ('reindexLayer' knows
--   the distinction). Only inline columns whose length matches the facet
--   column length @n@ are affected — mismatches are left as-is rather than
--   silently truncated (§3 covers detecting and warning about those).
subsetInlineSpec :: Int -> [Int] -> VisualSpec -> VisualSpec
subsetInlineSpec n keepIdx sp =
  sp { vsLayers = map (reindexLayer n (V.fromList keepIdx)) (vsLayers sp) }

-- ---------------------------------------------------------------------------
-- Layer 別 render
-- ---------------------------------------------------------------------------

-- | [日本語]: dual Y 軸対応の layer 描画。 layer が右軸 (lyYAxisSide = YAxisRight)
--   かつ右軸 scale が存在する場合のみ、 lpYScale を右軸 scale に差し替えて描画する
--   (= 右軸系列を独立 domain で位置決め)。 それ以外は通常の renderLayer。
--   [English]: Draws a layer with dual-Y-axis support. Only when the layer
--   targets the right axis (lyYAxisSide = YAxisRight) and a right-axis
--   scale exists does this swap lpYScale for the right-axis scale before
--   drawing (positioning the right-axis series on its own independent
--   domain). Otherwise it behaves like ordinary 'renderLayer'.
renderLayerDual :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderLayerDual r layout pal ly =
  let effLayout = case (getLast (lyYAxisSide ly), lpYScaleRight layout) of
        (Just YAxisRight, Just sR) -> layout { lpYScale = sR }
        _                          -> layout
  in renderLayer r effLayout pal ly

-- | [日本語]: ★ 1 layer = 1 base mark + 任意個の重畳 sub-mark ('lyOverlay')。
--   base を描いた後、 各 sub-mark を「親の群 (encX)・色 (colorBy)・値 (encY) 等を継承し、
--   自前の kind/nudge/markWidth/side で」 描く (= raincloud / 自作 composite)。 overlay が
--   空 (= 既存の単一 mark layer) なら base のみ・出力は従来と byte 一致。
--   [English]: One layer = one base mark plus any number of overlaid
--   sub-marks ('lyOverlay'). After drawing the base, each sub-mark is drawn
--   "inheriting the parent's group (encX), color (colorBy), value (encY),
--   etc., but with its own kind/nudge/markWidth/side" (used for raincloud
--   plots / custom composites). If overlay is empty (an ordinary
--   single-mark layer), only the base is drawn and output stays byte-for-
--   byte identical to before.
renderLayer :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderLayer r layout pal ly
  -- ★ Phase 36 D3: 合成が複数の値列にまたがる (= distCols) ときは、 各マークを「自分の値列名」
  --   スロットに置く。 単一列 (raincloud 等) は従来 (D2) どおり同 slot に重畳 (byte 不変)。
  | length (compositeLanes ly) > 1 =
      concatMap (renderLaneMark r layout pal) (ly { lyOverlay = [] } : lyOverlay ly)
  | otherwise =
      renderLayerBase r layout pal ly
      ++ concatMap (renderLayerBase r layout pal . inheritShared ly) (lyOverlay ly)

-- | [日本語]: distCols のレーン 1 マーク。 自分の値列名を inline カテゴリとして encX に与え、
--   分布 renderer がそれを「列名スロット」 として大域 index に置く。 ① 非分布 mark は描画 skip。
--   [English]: Draws one distCols lane mark. Passes its own value-column
--   name as an inline category on encX, which the distribution renderer
--   places at a global index acting as a "column-name slot". ① Non-
--   distribution marks are skipped.
renderLaneMark :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderLaneMark r layout pal m
  | getFirst (lyKind m) `notElem`
      map Just [MBox, MViolin, MStrip, MSwarm, MRaincloud] = []   -- ① render skip (案 A)
  | otherwise =
      let n  = V.length (vecOr (lyEncY m) r)
          nm = maybe "" colRefName (getLast (lyEncY m))
          m' = m { lyEncX = Last (Just (inlineCat (replicate n nm))) }
      in renderLayerBase r layout pal m'

-- | [日本語]: 親 layer の共有属性 (群・色・値・alpha 等) を sub-mark に継承させる。 kind と
--   位置決めつまみ (nudge/markWidth/side) と overlay 自身は親から引き継がず、 sub 側の
--   指定を使う。
--   [English]: Has a sub-mark inherit the parent layer's shared attributes
--   (group, color, value, alpha, etc.). The kind, the positioning knobs
--   (nudge/markWidth/side), and overlay itself are not inherited from the
--   parent; the sub-mark's own values are used instead.
inheritShared :: Layer -> Layer -> Layer
inheritShared parent sub =
  let cleared = parent { lyKind      = First Nothing
                       , lyNudge     = mempty
                       , lyMarkWidth = mempty
                       , lySide      = mempty
                       , lyOverlay   = [] }
  in cleared <> sub   -- Last 系は sub が指定あれば勝ち・無ければ親を継承。 kind は sub (First)。

renderLayerBase :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderLayerBase r layout pal ly =
  case getFirst (lyKind ly) of
    Just MScatter    -> renderScatter   r layout pal ly
    Just MQuiver     -> renderQuiver    r layout pal ly  -- Phase 26 A2 vector field
    Just MLine       -> renderLine      r layout pal ly
    Just MTrace      -> renderLine      r layout pal ly  -- = MLine 同等 + (Phase 26 §E-1)
    Just MBar        -> renderBar       r layout pal ly
    Just MHistogram  -> renderHistogram r layout pal ly
    Just MBox        -> renderBox       r layout pal ly
    Just MDensity    -> renderDensity   r layout pal ly
    Just MFreqPoly   -> renderFreqPoly  r layout pal ly  -- Ch10 EDA (Phase 28) geom_freqpoly
    Just MStatMean   -> renderStatLine  r layout pal ly mean
    Just MStatMedian -> renderStatLine  r layout pal ly median
    Just MParallel   -> renderParallel   r layout pal ly
    Just MDAG        -> renderDAG        layout pal ly
    Just MBand       -> renderBand       r layout pal ly  -- TODO-11
    Just MStream     -> renderStream     r layout pal ly  -- Phase 52.D2
    Just MAutocorr   -> renderAutocorr   r layout pal ly  -- Phase 6 A4
    Just MEss        -> renderESS        r layout pal ly  -- Phase 6 A5
    Just MForest     -> renderForest     r layout pal ly  -- Phase 6 A2
    Just MFunnel     -> renderFunnel     r layout pal ly  -- Phase 6 A3
    Just MPie        -> renderPie        r layout pal ly  -- Phase 6+ C-2
    Just MWaterfall  -> renderWaterfall  r layout pal ly  -- Phase 6+ C-2
    Just MStep       -> renderStep       r layout pal ly  -- Phase 6+ C-3
    Just MStem       -> renderStem       r layout pal ly  -- Phase 6+ C-3
    Just MViolin     -> renderViolin     r layout pal ly  -- Phase 6+ C-4
    Just MStrip      -> renderStrip      r layout pal ly  -- Phase 6+ C-4
    Just MSwarm      -> renderSwarm      r layout pal ly  -- Phase 6+ C-4
    Just MRaincloud  -> renderRaincloud  r layout pal ly  -- Phase 6+ C-5
    Just MRidge      -> renderRidge      r layout pal ly  -- Phase 6+ C-5
    Just MScatter3D  -> []  -- ★ Phase 26 §C-2 #15 placeholder (実装は hgg-3d 別 Phase)
    Just MText       -> renderText r layout pal ly False  -- Phase 11 A6 geom_text
    Just MLabel      -> renderText r layout pal ly True   -- Phase 11 A6 geom_label (bg box)
    Just MQQ         -> renderQQ   r layout pal ly        -- Phase 11 A6-2 geom_qq
    Just MHeatmap    -> renderHeatmap r layout pal ly     -- Phase 11 A6-3 heatmap
    Just MCount      -> renderCount r layout pal ly       -- Phase 28 geom_count (stat_sum)
    Just MContour    -> renderContour r layout pal ly     -- 等高線 (marching squares)
    Just MContourFilled -> renderContourFilled r layout pal ly  -- 等値帯塗り (Phase 24 A4)
    Just MBin2d      -> renderBin2d   r layout pal ly     -- binned heatmap (geom_bin2d)
    Just MTile       -> renderTile    r layout pal ly     -- Phase 60: 連続軸タイル塗り (geom_tile)
    Just MHexbin     -> renderHexbin  r layout pal ly     -- Phase 40 六角ビニング (geom_hex)
    Just MEcdf       -> renderEcdf   r layout pal ly       -- Phase 11 A6-4 stat_ecdf
    Just MLineRange  -> renderRangeBar r layout pal ly False False  -- Phase 11 A6-4b
    Just MPointRange -> renderRangeBar r layout pal ly True  False  -- 〃 + 中心点
    Just MCrossbar   -> renderRangeBar r layout pal ly False True   -- 〃 幅付き箱
    Just MStatLM     -> []  -- Phase 16: 未解決 stat は描かない (bridge resolveStats が band+line に展開)
    Just MStatSmooth -> []  -- 〃
    Just MStatPoly   -> []  -- 〃 (B3)
    Just MStatResid  -> []  -- 〃 (B3)
    Just MCustom     -> renderCustom r layout pal ly  -- ★ Phase 51: custom mark (closure)
    _                -> []  -- 他 mark は §A-5 続きで段階追加

-- | [日本語]: ★ custom mark を描く。 'Graphics.Hgg.Spec.Layer.lyCustom' の draw closure に 'RenderCtx' を渡し、
--   返った 'Primitive' 列をそのまま emit する (HS は registry 不要 = closure が源)。
--   'Graphics.Hgg.Spec.Layer.lyCustom' が空なら no-op。 RenderCtx は scale 適用済 projection・plot 領域・resolver・
--   theme 既定色を提供する (authoring API)。
--   [English]: Draws a custom mark. Passes a 'RenderCtx' to the draw
--   closure stored in 'Graphics.Hgg.Spec.Layer.lyCustom' and emits the returned 'Primitive' list
--   as-is (no registry is needed on the Haskell side — the closure is the
--   source). A no-op when 'Graphics.Hgg.Spec.Layer.lyCustom' is empty. RenderCtx provides the
--   scale-applied projection, plot area, resolver, and theme default
--   colors (the authoring API).
renderCustom :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderCustom r layout pal ly =
  case getLast (lyCustom ly) of
    Nothing -> []
    Just cm ->
      let coord = lpCoord layout
          ctx = RenderCtx
            { rcProjectXY = projectXY coord layout
            , rcPlotArea  = lpPlotArea layout
            , rcResolver  = r
            , rcColor     = tpDefault pal
            , rcFill      = tpDefaultFill pal
            , rcTextColor = tpText pal
            , rcAxisColor = tpAxis pal
            }
      in cmDraw cm ctx

-- ===========================================================================
-- Phase 6+ C-8: Legend render (= 簡略実装、 categorical color encoding 限定)
-- ===========================================================================

-- | [日本語]: 凡例 (legend chip) を描画。 vsLegend が None なら空。
--   各 layer の lyColor が ColorByCol なら、 その列の distinct 値を chip として並べる。
--   位置は LegendPosition、 inside の場合は plotArea 内、 right/bottom は外側。
--   [English]: Draws the legend (legend chips). Empty when vsLegend is
--   None. If a layer's lyColor is ColorByCol, that column's distinct
--   values are laid out as chips. Position is given by LegendPosition:
--   "inside" is placed within the plot area, while right/bottom are
--   placed outside it.
-- | [日本語]: legend を PS と同一ロジックで描画 (配置を ggplot に揃える)。
--   gating は 'needsLegend' (= color encoding があれば 'Graphics.Hgg.Spec.Setters.legend' 明示なしでも auto)。 位置別に
--   Right / Bottom / Inside の sub-renderer に dispatch。 Right/Bottom は予約域 (Layout legendW/H)
--   に収まり、 Inside は panel 内に bg box 付きで描く。 色/文字は theme 連動 (mkFontTS / pal)。
--   [English]: Draws the legend with the same logic as PS (positioning
--   matched to ggplot). Gating is via 'needsLegend' (auto-shown whenever a
--   color encoding exists, even without an explicit 'Graphics.Hgg.Spec.Setters.legend'). Dispatches
--   to a sub-renderer per position — Right / Bottom / Inside. Right/Bottom
--   fit within the reserved area (Layout's legendW/H), while Inside is
--   drawn inside the panel with a background box. Colors/text follow the
--   theme (mkFontTS / pal).
renderLegend :: Resolver -> Layout -> ThemePalette -> VisualSpec -> [Primitive]
renderLegend r layout pal spec =
  let pos = needsLegend spec (effectiveLegendPos spec)
  -- ★ Phase 35: LegendRight は collectGuides 経路 (色/形の複数 guide・色無し形のみも可)。
  --   Bottom/Inside は未 guide 化なので従来の単一 color enc 経路を維持。
  in case pos of
       LegendNone        -> []
       LegendRight       -> renderLegendRight spec r layout pal False (ColorStatic "")  -- enc は未使用
       LegendRightCenter -> renderLegendRight spec r layout pal True  (ColorStatic "")  -- 縦中央寄せ
       _ -> case findColorEnc (vsLayers spec) of
              Nothing  -> []
              Just enc -> case pos of
                LegendBottom            -> renderLegendBottom spec r layout pal enc
                LegendInsideTopRight    -> renderLegendInside spec r layout pal enc 1 0
                LegendInsideTopLeft     -> renderLegendInside spec r layout pal enc 0 0
                LegendInsideBottomRight -> renderLegendInside spec r layout pal enc 1 1
                LegendInsideBottomLeft  -> renderLegendInside spec r layout pal enc 0 1
                _                       -> []

-- | [日本語]: 最初に見つけた color encoding を凡例化 (= PS findColorEnc)。
--   ★ findColorEnc / allColorCategories / effectiveLegendTitle / nubKeep /
--   LegendGuide / collectGuides は Layout へ集約 (予約と描画の単一情報源)。
--   ここでは Layout から import して使う。
--   [English]: Turns the first color encoding found into a legend (matches
--   PS findColorEnc). findColorEnc / allColorCategories /
--   effectiveLegendTitle / nubKeep / LegendGuide / collectGuides are all
--   consolidated into Layout (a single source shared by reservation and
--   drawing); here they are simply imported from Layout and used.

-- | [日本語]: 凡例の正本 ('allColorCategories' union) を glyph 側へ注入する。
--   'lyColorCats' が空の ColorByCol レイヤにだけ union を詰める (ユーザ明示の
--   'Graphics.Hgg.Spec.Constructors.colorCats' は非空なので上書きしない・冪等)。 'Graphics.Hgg.Render.Common.colorVector' (TODO-3d 機構) が
--   この順序で palette index を引くため、 glyph と凡例 swatch が同じ正本を参照し
--   `<>` 重畳・facet panel でズレない。 単一 layer では union = layer 内 nub
--   (どちらも初出順) なので従来配色と一致する。
--   [English]: Injects the legend's source of truth (the union from
--   'allColorCategories') into the glyph side. Only fills the union into
--   ColorByCol layers whose 'lyColorCats' is empty (a user-supplied
--   'Graphics.Hgg.Spec.Constructors.colorCats' is non-empty, so it is never overwritten — idempotent).
--   Since 'Graphics.Hgg.Render.Common.colorVector' (the TODO-3d mechanism) looks up the palette index
--   by this ordering, glyphs and legend swatches reference the same source
--   of truth and never drift apart across @\<\>@ overlays or facet panels.
--   For a single layer, union == the layer's own nub (both in first-seen
--   order), so this matches the previous coloring.
injectColorCats :: Resolver -> VisualSpec -> VisualSpec
injectColorCats r spec =
  case allColorCategories r (vsLayers spec) of
    []   -> spec
    cats -> spec { vsLayers = map inj (vsLayers spec) }
      where
        inj ly = case getLast (lyColor ly) of
          Just (ColorByCol _) | null (lyColorCats ly) -> ly { lyColorCats = cats }
          _ -> ly

-- | [日本語]: 凡例タイトル (= 変数名) は常に非表示。 gallery 等で spec を JSON 化
--   (bakeSpec) すると color 列が inline 化され列名が失われ、 PS は構造的にタイトルを出せない。
--   HS だけ live 名 ("group") を出すと HS/PS が食い違う (= ユーザ報告)。 両方 "" に揃える
--   (legend 項目ラベル自体が自己説明的)。 将来 name 保持 bake を入れたら復活させる。
--   [English]: The legend title (the variable name) is always hidden. When
--   a spec is serialized to JSON (bakeSpec) for the gallery and similar
--   uses, the color column becomes inline and its name is lost, so PS
--   cannot structurally show a title. If HS alone showed its live name
--   ("group"), HS and PS would disagree (a user-reported issue). Both are
--   aligned to "" instead (legend item labels are self-explanatory on
--   their own). This can be revived once a name-preserving bake exists.
legendHeaderText :: ColRef -> Text
legendHeaderText _ = ""

-- ★ Phase 38: effectiveLegendTitle / nubKeep は Layout へ集約 (import 済)。

-- ★ Phase 63 A17: legendOrder は Layout へ移設 (auto-wrap の列幅計算と共有・import 済)。

-- | [日本語]: 縦凡例の ncol (>=1)。
--   [English]: The ncol of a vertical legend (>=1).
legendNcolOf :: VisualSpec -> Int
legendNcolOf spec = max 1 (maybe 1 id (getLast (vsLegendNcol spec)))

-- ★ Phase 63 A17: legendNrowOf/legendGridH は撤去 (bottom 凡例の列数は Layout の
--   lpLegendNCol = 予約と同一の単一情報源へ。 grid 位置は nc から直接 (mod/div))。

-- | [日本語]: 縦凡例グリッド: 表示 index k → (col, row)。 列優先 (column-major)、 nrows=ceil(n/ncol)。
--   ncol=1 なら (0, k) で従来の単一列と一致。
--   [English]: The vertical-legend grid: display index k -> (col, row).
--   Column-major, with nrows=ceil(n/ncol). At ncol=1 this reduces to
--   (0, k), matching the previous single-column layout.
legendGridV :: Int -> Int -> Int -> (Int, Int)
legendGridV ncol n k = let nr = (n + ncol - 1) `div` ncol in (k `div` nr, k `mod` nr)

-- | [日本語]: i 番目の categorical 色 (palette 長で wrap、 空なら default)。
--   [English]: The categorical color at index i (wraps at palette length;
--   the default when the palette is empty).
legendColorAt :: Layout -> ThemePalette -> Int -> Text
legendColorAt layout pal i =
  let catPal = lpCategoricalPalette layout
  in if null catPal then tpDefault pal else catPal !! (i `mod` length catPal)

-- | [日本語]: legend chip 色。 scale_color_manual の辞書に該当ラベルがあれば優先 (= 凡例と
--   panel の色を一致させる)。 未登録は index ベースの 'legendColorAt'。
--   [English]: The legend chip color. Prefers a matching label in the
--   scale_color_manual dictionary, if any (keeping the legend and panel
--   colors in sync). Falls back to the index-based 'legendColorAt' for
--   unregistered labels.
legendColorFor :: Layout -> ThemePalette -> Int -> Text -> Text
legendColorFor layout pal i label =
  case lookup label (lpColorManual layout) of
    Just c  -> c
    Nothing -> legendColorAt layout pal i

-- | [日本語]: legend の color 凡例が point geom (= scatter) かどうか。 true なら
--   色見本を panel と同じ円で描く (ggplot legend key は geom 形状に従う)。 それ以外は矩形。
--   [English]: Whether the legend's color legend is a point geom
--   (scatter). When true, the color swatch is drawn as the same circle
--   used in the panel (ggplot legend keys follow the geom's shape);
--   otherwise a rectangle is used.
legendUsesPoint :: VisualSpec -> Bool
legendUsesPoint spec = case filter (\l -> case getLast (lyColor l) of
                                            Just _ -> True; Nothing -> False) (vsLayers spec) of
  (l : _) -> getFirst (lyKind l) == Just MScatter
  []      -> False

-- | [日本語]: legend の色見本 (left,top,key-size 指定)。 ★ ggplot @legend.key@ 同様
--   各キーに背景四角を敷き、 その上にマーカーを描く。 point geom は円
--   (shapeBy が color と同列なら per-category の ●▲■)、 他は色付き矩形。 マーカー径は
--   __プロット中の点と同径__ (markerDiam = 解決済 lySize / 既定 1.65mm) にして凡例だけ
--   大きくならないようにする。
--   ★ キー背景は 'legendKeyPrim' と同じ theme 口 tpLegendKeyBg
--   ("" = 塗らない) に一本化。 旧 grey95 ハードコードは bottom/top 凡例だけ
--   一本化から漏れていた取り残し。
--   [English]: The legend's color swatch (given left, top, key size).
--   Like ggplot's @legend.key@, lays a background square behind each key
--   before drawing the marker on top. Point geoms are drawn as circles
--   (per-category ●▲■ when shapeBy maps to the same column as color);
--   others are drawn as colored rectangles. The marker diameter is set to
--   __the same diameter as points in the plot__ (markerDiam = the resolved
--   lySize, or the 1.65mm default) so the legend markers are not enlarged.
--   The key background is consolidated onto the same theme knob as
--   'legendKeyPrim', tpLegendKeyBg ("" = unfilled). The old hardcoded
--   grey95 was a leftover that had been missed when the bottom/top legend
--   was consolidated onto this knob.
legendSwatch :: Maybe Layer -> Bool -> Maybe MarkShape -> Double -> ThemePalette
             -> Double -> Double -> Double -> Text -> [Primitive]
legendSwatch mLayer usePoint mShape markerDiam pal left top sz col =
  let keyBg
        | tpLegendKeyBg pal == "" = []
        | otherwise = [ PRect (Rect left top sz sz)
                              (FillStyle (tpLegendKeyBg pal) 1.0) Nothing ]
  in (keyBg ++) $ if usePoint
       then let ctr = Point (left + sz / 2) (top + sz / 2)
                r   = markerDiam / 2
                -- plot 点と同じ装飾 (既定縁なし)。 旧 1pt 縁ハードコードを廃止。
                (fs, ms) = case mLayer of
                  Just ly -> (markerFillFor ly col 1.0, markerStrokeFor ly col)
                  Nothing -> (FillStyle col 1.0, Nothing)
                marker = case mShape of
                  Just sh | sh /= MShCircle -> shapeToPrim sh ctr r fs ms Nothing
                  _                         -> PCircle ctr r fs ms Nothing
            in [ marker ]
       else [ PRect (Rect left top sz sz) (FillStyle col 1.0) (Just (StrokeStyle (tpAxis pal) 0.5)) ]

-- ★ Phase 38: legendBaseSize / legendKeyW / legendKeyPitch は Layout へ集約 (単一情報源)。
--   ここでは Layout から import して使う (定義は Graphics.Hgg.Layout)。

-- | [日本語]: top-align 凡例ブロックの上余白 (pt)。 ggplot は右凡例を縦中央寄せするため
--   直接の対応 metric は無い。 ユーザ好み (上揃え) ゆえ half_line の倍数で定義 (= 11pt ≈ 10)。
--   ★ half_line = base/2 派生へ (既定 11 で従来 2×5.5 と bit 同値)。
--   [English]: The top margin (pt) of a top-aligned legend block. ggplot
--   vertically centers the right legend, so there is no directly
--   corresponding metric; since this is a user preference (top-aligned),
--   it is defined as a multiple of half_line (11pt ≈ 10). half_line is
--   now derived as base/2 (bit-identical to the previous 2×5.5 at the
--   default of 11).
legendTopInset :: VisualSpec -> Double
legendTopInset spec = 2 * effectiveHalfLine spec

-- | [日本語]: 凡例キーの描画スタイル (= ggplot draw_key 同型・geom 種で変わる)。
--   [English]: The legend key's drawing style (mirrors ggplot's draw_key;
--   varies with the geom kind).
data LegendKeyStyle
  = KeyPoint !(Maybe MarkShape)   -- scatter: point glyph (色塗り)
  | KeyFilled                     -- bar/histogram: 色ベタ塗り矩形
  | KeyOutline !(Maybe Double)    -- density/line: 色枠線矩形 (Just a = 内部を色@a 塗り / Nothing = 透明=灰背景が見える)

-- | [日本語]: 凡例キー 1 個を (cx, cy) 中心に描く (キー灰背景は別途連続ブロックで描く)。
--   [English]: Draws a single legend key centered at (cx, cy) (the key's
--   grey background is drawn separately as one contiguous block).
-- | [日本語]: ★ 凡例キーの装飾は plot 点と揃える (@mLayer@ = 当該 point レイヤ)。 KeyPoint の塗り・
--   縁は 'markerFillFor'/'markerStrokeFor' に一本化 (既定縁なし)。 旧実装は塗り同色の
--   1pt 縁をハードコードしており、 精緻なスーツ形の凹みを潰していた (= plot と不一致)。
--   ★ キー 1 辺 kw は呼び手が実効値 ('effectiveLegendKeyW') で渡す
--   (pitch = keyW ゆえ引数 1 つ。 本関数は spec を持たないため)。
--   [English]: The legend key's decoration is kept in sync with the
--   plot's points (@mLayer@ = the corresponding point layer). KeyPoint's
--   fill and stroke are consolidated onto 'markerFillFor'/'markerStrokeFor'
--   (no stroke by default). The previous implementation hardcoded a 1pt
--   stroke of the same color as the fill, which flattened fine shape
--   details such as suit-symbol notches (a mismatch with the plot). The
--   key's side length kw is passed by the caller as the effective value
--   ('effectiveLegendKeyW') — a single argument, since pitch = keyW and
--   this function itself does not hold the spec.
legendKeyPrim :: Double -> Maybe Layer -> LegendKeyStyle -> Double -> ThemePalette -> Double -> Double -> Text -> [Primitive]
legendKeyPrim kw mLayer style markerDiam pal cx cy col =
  -- ★ 矩形キー (bar/density) はセルより線幅 (lwd mm) 分**内側**に縮める
  --   (= ggplot draw_key_polygon: rectGrob width = unit(1,"npc") - unit(lwd,"mm"))。
  --   隣接セルとの間に lwd mm の隙間ができ、 ggplot 同様「隣接するが接しない」。
  let lwInset = mmPt 0.5                              -- ggplot 既定 linewidth = 0.5mm
      keyRect = Rect (cx - (kw - lwInset) / 2) (cy - (kw - lwInset) / 2)
                     (kw - lwInset) (kw - lwInset)
      -- ★ Phase 32 (re-apply): legend.key 背景 (ggplot theme_grey = grey95 #F2F2F2)。
      --   symbol の背後にキーセル全体 (keyW × keyW) を塗る。 tpLegendKeyBg が
      --   空文字なら描かない (= 従来挙動)。 全 guide variant が legendKeyPrim 経由ゆえ 1 箇所で網羅。
      keyBg
        | tpLegendKeyBg pal == "" = []
        | otherwise = [ PRect (Rect (cx - kw / 2) (cy - kw / 2) kw kw)
                              (FillStyle (tpLegendKeyBg pal) 1.0) Nothing ]
  in (keyBg ++) $ case style of
       KeyPoint mShape ->
         let ctr = Point cx cy
             r   = markerDiam / 2
             -- plot 点と同じ装飾 (markerFillFor/markerStrokeFor)。 レイヤ不明時は縁なし既定。
             (fs, ms) = case mLayer of
               Just ly -> (markerFillFor ly col 1.0, markerStrokeFor ly col)
               Nothing -> (FillStyle col 1.0, Nothing)
         in case mShape of
              Just sh | sh /= MShCircle -> [ shapeToPrim sh ctr r fs ms Nothing ]
              _                         -> [ PCircle ctr r fs ms Nothing ]
       KeyFilled ->
         -- ★ 枠なしの塗り潰しチップ (bar/box/violin の見た目に合わせる)。 旧実装は
         --   tpAxis の固定枠 (黒) で mark の塗りと不一致だった。
         [ PRect keyRect (FillStyle col 1.0) Nothing ]
       KeyOutline mAlpha ->
         let fs = case mAlpha of { Just a -> FillStyle col a; Nothing -> FillStyle col 0.0 }
         in [ PRect keyRect fs (Just (StrokeStyle col 1.0)) ]

-- | [日本語]: 凡例マーカーの径 (pt)。 最初の scatter レイヤの解決済 'lySize' (= プロット点と
--   同径)、 無ければ既定 'defaultMarkerDiameter'。
--   [English]: The legend marker's diameter (pt): the resolved 'lySize' of
--   the first scatter layer (the same diameter as points in the plot), or
--   the 'defaultMarkerDiameter' if there is none.
legendMarkerDiam :: VisualSpec -> Double
legendMarkerDiam spec =
  case [ doubleOr (lySize l) defaultMarkerDiameter
       | l <- vsLayers spec, getFirst (lyKind l) == Just MScatter ] of
    (d : _) -> d
    []      -> defaultMarkerDiameter

-- | [日本語]: ★ 凡例エントリ k (= カテゴリ index) のマーカー形。 scatter レイヤが
--   color と shape を __同じ列__ にマップしているとき (ggplot の統合凡例) のみ、 自動
--   shape scale ('shapePalette') を k で巡回して返す。 色のみ・shape 別列 (= ggplot は
--   2 凡例) のときは Nothing (= 従来の円) にして単一 color 凡例を保つ。
--   [English]: The marker shape for legend entry k (a category index).
--   Only when a scatter layer maps color and shape to __the same column__
--   (ggplot's combined legend) does this cycle through the automatic shape
--   scale ('shapePalette') by k. When color alone is mapped, or shape maps
--   to a different column (ggplot then shows 2 legends), returns Nothing
--   (the previous circle), keeping a single color legend.
legendShapeFor :: VisualSpec -> Int -> Maybe MarkShape
legendShapeFor spec k =
  case [ () | ly <- vsLayers spec
            , getFirst (lyKind ly) == Just MScatter
            , Just (ColorByCol cc) <- [getLast (lyColor ly)]
            , Just sc <- [getLast (lyShapeBy ly)]
            , cc == sc ] of
    (_ : _) -> Just (shapePalette !! (k `mod` length shapePalette))
    []      -> Nothing

-- | [日本語]: 凡例 guide (= ggplot guides)。 aesthetic ごとに 1 guide、 同一列に
--   マップされた色+形は色 guide に統合 ('legendShapeFor' 経由) するので形 guide は作らない。
--   [English]: A legend guide (mirrors ggplot's guides). One guide per
--   aesthetic; color and shape mapped to the same column are merged into
--   the color guide (via 'legendShapeFor'), so no separate shape guide is
--   created.

-- | [日本語]: 1 guide を原点 (ox, oy) から描き、 (prims, ブロック高さ) を返す。
--   ブロック = [タイトル行 (凡例列名あり時)] + [エントリ行…]。 内部レイアウトは原点相対。
--   [English]: Draws one guide starting from the origin (ox, oy), returning
--   (prims, block height). A block consists of [the title row, if a legend
--   column name exists] + [entry rows...]. The internal layout is relative
--   to the origin.
renderGuideBlock :: VisualSpec -> Resolver -> Layout -> ThemePalette
                 -> Double -> Double -> Text -> LegendGuide -> ([Primitive], Double)
renderGuideBlock spec r layout pal ox oy title guide =
  let tsTitle = mkFontTS (Just spec) pal LegendTitleF AnchorStart 0
      tsItem  = mkFontTS (Just spec) pal LegendItemF  AnchorStart 0
      -- pt メトリクス (ggplot 同型・マジック数を排す)。 半行 = half_line。
      -- ★ Phase 63 A13: 凡例メトリクスを base 派生の実効値へ (既定 11 で従来と bit 同値)。
      hl  = effectiveHalfLine spec
      lbs = effectiveLegendBaseSize spec
      kw  = effectiveLegendKeyW spec
      kp  = effectiveLegendKeyPitch spec
      itemDy  = lbs * 0.8 * 0.32                                 -- item 文字をキー中心に縦揃え
      titleH  = if title == "" then 0 else lbs + hl              -- title 行高 (文字 + 下マージン)
      header  = if title == "" then [] else [ PText (Point ox (oy + lbs)) title tsTitle ]
      firstCy = oy + titleH + kp / 2                             -- 最初のキー中心
      labelX  = ox + kw + hl / 2                                 -- key → label gap = half_line/2
      cyAt k  = firstCy + fromIntegral k * kp
      -- ★ Phase 63 A19.5: 旧 Phase 35 の「tpPanelBg 連続帯 (bgRect)」 を撤去。 ggplot に
      --   キー列の連続帯という要素は無く、 凡例キー背景は legend.key = 'legendKeyPrim' の
      --   keyBg (tpLegendKeyBg、 "" = 塗らない) が唯一の口。 旧帯は白背景 theme では
      --   不可視だったが、 A18 の背景透過で不透過白帯として顕在化した (root:
      --   after-map 実測 RGBA(255,255,255,255))。 ThemeGrey も grey95 キーとの二重塗りを解消。
  in case guide of
       ColorGuide (ColorByCol _cr) ->
         let vals     = allColorCategories r (vsLayers spec)  -- Phase 52.A10: 全レイヤ union
             n        = length vals
             items    = legendOrder spec vals
             -- ★ Phase 35: 凡例キー形は色 aesthetic を持つレイヤの geom 種で決まる (ggplot draw_key)。
             colorLayer = listToMaybe [ l | l <- vsLayers spec, isColorMapLayer l ]
             keyKind    = maybe MScatter id (colorLayer >>= getFirst . lyKind)
             -- density 等の塗り (densityFill) 有無 → 枠線キーの内部塗り alpha。
             mFillAlpha = case colorLayer of
               Just l | getLast (lyDensityFill l) == Just True -> Just (doubleOr (lyAlpha l) 0.5)
               _                                               -> Nothing
             -- ★ Phase 36 C: hollow (= fill=NA) の塗り系 mark (box 等) は塗らないので枠線キー
             --   (= density と同じ KeyOutline)。 凡例キーを「mark 種」でなく「塗るか否か」で選ぶ。
             colorLayerHollow = case colorLayer of
               Just l -> getLast (lyHollow l) == Just True
               _      -> False
             styleFor origI = case keyKind of
               MScatter                                  -> KeyPoint (legendShapeFor spec origI)
               k | k `elem` [MDensity, MLine, MFreqPoly, MStep] -> KeyOutline mFillAlpha
               _ | colorLayerHollow                      -> KeyOutline Nothing
               _                                         -> KeyFilled
             chipFor k (origI, label) =
               let cy = cyAt k
                   col = legendColorFor layout pal origI label
               in legendKeyPrim kw colorLayer (styleFor origI) (legendMarkerDiam spec) pal (ox + kw / 2) cy col
                  <> [ PText (Point labelX (cy + itemDy)) label tsItem ]
         in ( header <> concat (zipWith chipFor [0 :: Int ..] items)
            , titleH + fromIntegral n * kp )
       ColorGuide (ColorByContinuous cr) -> case resolveNum r cr of
         Nothing   -> ([], 0)
         Just nums | V.null nums -> ([], 0)
                   | otherwise ->
           let vMin = V.minimum nums
               vMax = V.maximum nums
               barW = kw; barH = 11 * lbs; barX = ox; barY = oy + titleH
               nStop = 40 :: Int
               step = barH / fromIntegral nStop
               -- ★ A4-e: gradient2 指定時は発散 3-stop を bar に反映 (= 凡例も diverging palette)。
               legendPal = case lpColorGradient2 layout of
                 Just (cLo, cMid, cHi, _) -> [cLo, cMid, cHi]
                 Nothing                  -> lpContinuousPalette layout
               stops = [ let t = fromIntegral i / fromIntegral (nStop - 1)
                             sy = barY + barH - fromIntegral i * step
                         in PRect (Rect barX (sy - step) barW (step + 0.5))
                                  (FillStyle (continuousColor legendPal t) 1.0) Nothing
                       | i <- [0 .. nStop - 1] ]
               tickX = barX + barW + hl / 2
               -- ggplot 同型: 連続凡例の目盛りは生 min/mid/max でなく Wilkinson extended
               -- breaks (= 軸と同じ nice 値) を範囲内に置く。 生値の長大桁を避けラベルが短くなる。
               legBreaks = case filter (\b -> b >= vMin && b <= vMax) (extendedBreaks 5 vMin vMax) of
                 [] -> [vMin, vMax]
                 bs -> bs
               yOfV v = if vMax > vMin then barY + barH * (vMax - v) / (vMax - vMin)
                                       else barY + barH / 2
               ticks = [ PText (Point tickX (yOfV b + itemDy)) (numToText b) tsItem | b <- legBreaks ]
           in (header <> stops <> ticks, titleH + barH)
       -- ★ Phase 40: hexbin の件数 colorbar (列でなく集計値ゆえ域 lo/hi を直に持つ)。
       --   ColorByContinuous と同型の gradient bar + extended breaks 目盛り、 タイトルは "count"。
       CountBarGuide lo hi ->
         let barTitle = "count"
             titleH'  = lbs + hl
             header'  = [ PText (Point ox (oy + lbs)) barTitle tsTitle ]
             vMin = lo; vMax = hi
             barW = kw; barH = 11 * lbs; barX = ox; barY = oy + titleH'
             nStop = 40 :: Int
             step = barH / fromIntegral nStop
             legendPal = lpContinuousPalette layout
             stops = [ let t = fromIntegral i / fromIntegral (nStop - 1)
                           sy = barY + barH - fromIntegral i * step
                       in PRect (Rect barX (sy - step) barW (step + 0.5))
                                (FillStyle (continuousColor legendPal t) 1.0) Nothing
                     | i <- [0 .. nStop - 1] ]
             tickX = barX + barW + hl / 2
             legBreaks = case filter (\b -> b >= vMin && b <= vMax) (extendedBreaks 5 vMin vMax) of
               [] -> [vMin, vMax]
               bs -> bs
             yOfV v = if vMax > vMin then barY + barH * (vMax - v) / (vMax - vMin)
                                     else barY + barH / 2
             ticks = [ PText (Point tickX (yOfV b + itemDy)) (numToText b) tsItem | b <- legBreaks ]
         in (header' <> stops <> ticks, titleH' + barH)
       ColorGuide (ColorStatic _) -> ([], 0)
       ShapeGuide scr ->
         -- 形 guide。 色は tpDefault (= 既定の ink・色未指定点の
         --   フォールバック色)、 形は plot と同じ規則 (orderedCats の index で shapePalette 巡回)。
         let vals = case resolveCol r scr of
               Just (TxtData v) -> orderedCats (V.toList v)
               Just (NumData v) -> orderedCats (map numToText (V.toList v))
               _                -> []
             n      = length vals
             inkCol = tpDefault pal
             chipFor k label =
               let cy = cyAt k
                   sh = shapePalette !! (k `mod` length shapePalette)
               in legendKeyPrim kw (legendPointLayer spec) (KeyPoint (Just sh)) (legendMarkerDiam spec) pal (ox + kw / 2) cy inkCol
                  <> [ PText (Point labelX (cy + itemDy)) label tsItem ]
         in ( header <> concat (zipWith chipFor [0..] vals)
            , titleH + fromIntegral n * kp )

-- | [日本語]: レイヤが色マップ (ColorByCol/ColorByContinuous) を持つか (= 凡例を駆動)。
--   [English]: Whether a layer carries a color mapping (ColorByCol/
--   ColorByContinuous), which drives the legend.
isColorMapLayer :: Layer -> Bool
isColorMapLayer l = case getLast (lyColor l) of
  Just (ColorByCol _)        -> True
  Just (ColorByContinuous _) -> True
  _                          -> False

-- | [日本語]: 凡例キーの装飾 (縁・hollow) を決める「代表 point レイヤ」。 色マップ層を
--   優先し、 無ければ最初の scatter 層。 これを 'legendKeyPrim'/'legendSwatch' に渡し、
--   plot 点と同じ 'markerStrokeFor'/'markerFillFor' を凡例にも適用する。
--   [English]: The "representative point layer" that decides the legend
--   key's decoration (stroke / hollow). Prefers a color-mapped layer,
--   falling back to the first scatter layer. Passed to 'legendKeyPrim'/
--   'legendSwatch' so that the same 'markerStrokeFor'/'markerFillFor' used
--   for plot points is also applied to the legend.
legendPointLayer :: VisualSpec -> Maybe Layer
legendPointLayer spec = listToMaybe
  (  [ l | l <- vsLayers spec, isColorMapLayer l ]
  ++ [ l | l <- vsLayers spec, getFirst (lyKind l) == Just MScatter ] )

-- | [日本語]: LegendRight: panel 右の予約域に guide を縦スタック (= PS renderLegendRight)。
--   centered=True (LegendRightCenter) なら guide スタック全体を panel 高の縦中央に揃える
--   (ggplot 既定の legend.position="right")。 False は従来の上揃え (legendTopInset 起点)。
--   [English]: LegendRight: stacks guides vertically in the reserved area
--   to the right of the panel (matches PS renderLegendRight). When
--   centered=True (LegendRightCenter), the whole guide stack is vertically
--   centered on the panel height (ggplot's default legend.position=
--   "right"); False keeps the previous top alignment (anchored at
--   legendTopInset).
renderLegendRight :: VisualSpec -> Resolver -> Layout -> ThemePalette -> Bool -> ColorEnc -> [Primitive]
renderLegendRight spec r layout pal centered _enc =
  let area = lpPlotArea layout
      hl = effectiveHalfLine spec            -- ★ A13: base 派生 (既定 11 で従来と同値)
      x0 = rX area + rW area + 2 * hl        -- panel→凡例 gap = ggplot legend.box.spacing = 1 line
      guideGap = 2 * hl                      -- guide 間スペース = 1 line
      guides = collectGuides r spec
      -- shape 凡例の見出しは列名。 inline data に resolve され名前が失われた場合は
      -- sentinel ("<inline-txt>" / "<inline-num>") を出さず空に潰す (= color 凡例と同じ規律)。
      titleOf g = case g of
        ColorGuide _  -> effectiveLegendTitle spec
        ShapeGuide cr -> let nm = colRefName cr
                         in if nm == "<inline-num>" || nm == "<inline-txt>" then "" else nm
        CountBarGuide _ _ -> ""   -- title は guide 側で "count" を出すので空 (= PS titleOf と同一)
      -- ★ Phase 32 (re-apply): 縦中央寄せ用に guide スタックの総高を見積る (renderGuideBlock
      --   は純粋ゆえ oy=0 で高さのみ取り出す)。 総高 = Σ block高 + gap*(n-1)。
      blockH g = snd (renderGuideBlock spec r layout pal x0 0 (titleOf g) g)
      totalH = sum (map blockH guides) + guideGap * fromIntegral (max 0 (length guides - 1))
      -- ★ Phase 35 #1: 上揃え時はブロック上端を panel 上端 + legendTopInset に下げ、 凡例
      --   タイトルが panel/キャンバス上端に詰まるのを防ぐ (グラフタイトルの有無に依らない)。
      y0 | centered  = rY area + max (legendTopInset spec) ((rH area - totalH) / 2)
         | otherwise = rY area + legendTopInset spec
      go _  []       = []
      go oy (g : gs) =
        let (prims, h) = renderGuideBlock spec r layout pal x0 oy (titleOf g) g
        in prims <> go (oy + h + guideGap) gs
  in go y0 guides

-- | [日本語]: LegendBottom: panel 下の予約域に横並び (= PS renderLegendBottom)。
--   ★ 実位置 = panel 下端 + lpLegendYOff (Layout の bM 予約 stack と単一情報源
--   = ticks→labels→title→legend の最外)。 旧 +50 固定は軸タイトルと逆順だった。
--   行 pitch も予約と同じ effectiveLegendKeyPitch。 +7 は swatch 上端 (cy-7) を
--   ブロック上端に一致させる内部 anchor (swatch/text の描画式は従来のまま)。
--   [English]: LegendBottom: lays out entries horizontally in the reserved
--   area below the panel (matches PS renderLegendBottom). The actual
--   position is panel-bottom + lpLegendYOff (a single source shared with
--   Layout's bM reservation stack — the outermost of
--   ticks→labels→title→legend). The old hardcoded +50 was in the reverse
--   order from the axis title. The row pitch also matches the reservation
--   (effectiveLegendKeyPitch); the +7 is an internal anchor that aligns the
--   swatch's top edge (cy-7) with the block's top edge (the swatch/text
--   drawing formulas are unchanged).
renderLegendBottom :: VisualSpec -> Resolver -> Layout -> ThemePalette -> ColorEnc -> [Primitive]
renderLegendBottom spec r layout pal enc =
  let area = lpPlotArea layout
      y0 = rY area + rH area + lpLegendYOff layout + 7
      tsItem  = mkFontTS (Just spec) pal LegendItemF  AnchorStart 0
      tsTitle = mkFontTS (Just spec) pal LegendTitleF AnchorStart 0
      -- Phase 11 A4-c: タイトル指定時のみ先頭に表示し chip を右へずらす (未指定はゼロ diff)。
      titleStr = effectiveLegendTitle spec
      -- ★ Phase 38: タイトル幅も字種別 'textWidthEm' で算出 (旧 0.6*len)。
      titleW   = if titleStr == "" then 0 else tsSize tsTitle * textWidthEm titleStr + 12
      titlePrim = if titleStr == "" then []
                  else [ PText (Point (rX area) (y0 + 2)) titleStr tsTitle ]
  in case enc of
       ColorByCol _cr ->
         let vals = allColorCategories r (vsLayers spec)  -- Phase 52.A10: 全レイヤ union
             items = legendOrder spec vals
             n     = length items
             -- ★ Phase 63 A17: 列数は Layout の auto-wrap 結果 (明示 legendNrow も
             --   Layout 側で解決済 = 予約 legendH と同一の単一情報源)。
             nc    = max 1 (lpLegendNCol layout)
             rowH  = effectiveLegendKeyPitch spec
             -- ★ Phase 38: 各アイテムの横送りをラベル内容で算出 (旧 chipW=80 固定 → content-based)。
             --   item 横幅 = swatch→label gap(14) + ラベル幅 + 列間 gap(ggHalfLine)。
             --   列 (k mod nc) ごとに、 その列に入る全行アイテムの最大幅を採る
             --   (Layout の legFits 列幅と同式)。
             itemAdv lbl = 14 + tsSize tsItem * textWidthEm lbl + effectiveHalfLine spec
             labelAt k   = snd (items !! k)
             colWidth c  = maximum (0 : [ itemAdv (labelAt k) | k <- [0 .. n - 1], k `mod` nc == c ])
             -- colXs !! c = 第 c 列の左端 x (title 後を起点に列幅を累積)。
             colXs = scanl (+) (rX area + titleW) (map colWidth [0 .. nc - 1])
             chipFor k (origI, label) =
               let (col, row) = (k `mod` nc, k `div` nc)  -- 行優先 (旧 legendGridH と同式)
                   cx = colXs !! col
                   cy = y0 + fromIntegral row * rowH
               in legendSwatch (legendPointLayer spec) (legendUsesPoint spec) (legendShapeFor spec origI) (legendMarkerDiam spec) pal cx (cy - 7) 10 (legendColorFor layout pal origI label)
                  <> [ PText (Point (cx + 14) (cy + 2)) label tsItem ]
         in titlePrim <> concat (zipWith chipFor [0..] items)
       _ -> []

-- | [日本語]: LegendInside: panel 内に bg box 付きで描く (= PS renderLegendInside)。 fracX/fracY は
--   0=左/上、 1=右/下。
--   [English]: LegendInside: draws the legend inside the panel with a
--   background box (matches PS renderLegendInside). fracX/fracY: 0 = left/
--   top, 1 = right/bottom.
renderLegendInside :: VisualSpec -> Resolver -> Layout -> ThemePalette -> ColorEnc
                   -> Double -> Double -> [Primitive]
renderLegendInside spec r layout pal enc fracX fracY =
  let area = lpPlotArea layout
      padB = 8
      tsTitle = mkFontTS (Just spec) pal LegendTitleF AnchorStart 0
      tsItem  = mkFontTS (Just spec) pal LegendItemF  AnchorStart 0
      itemH    = max 16 (tsSize tsItem + 6)
      chipSize = max 10 (tsSize tsItem * 0.75)
      chipGap  = 6
  in case enc of
       ColorByCol _cr ->
         let vals = allColorCategories r (vsLayers spec)  -- Phase 52.A10: 全レイヤ union
             nItems = length vals
             titleStr = effectiveLegendTitle spec
             hasTitleL = titleStr /= ""
             titleH = if hasTitleL then tsSize tsTitle + 6 else 0
             maxItemLen = maximum (0 : map T.length vals)
             titleLen   = T.length titleStr
             -- Phase 11 A5-c: ncol グリッド + reverse。 ncol=1・非 reverse で従来とゼロ diff。
             ncol  = legendNcolOf spec
             nrows = (nItems + ncol - 1) `div` ncol
             colW  = chipSize + chipGap + tsSize tsItem * 0.6 * fromIntegral maxItemLen + 12
             contentW = max (tsSize tsTitle * 0.6 * fromIntegral titleLen)
                            (fromIntegral ncol * colW - 12)
             boxW = max 80 (contentW + 14)
             boxH = titleH + 4 + fromIntegral nrows * itemH + 4
             x0 = if fracX >= 0.5 then rX area + rW area - boxW - padB else rX area + padB
             y0 = if fracY >= 0.5 then rY area + rH area - boxH - padB else rY area + padB
             bg = [ PRect (Rect (x0 - 4) (y0 - 4) (boxW + 8) (boxH + 4))
                          (FillStyle (tpBackground pal) 0.85)
                          (Just (StrokeStyle (tpAxis pal) 0.5)) ]
             header = if hasTitleL
                        then [ PText (Point x0 (y0 + tsSize tsTitle * 0.8)) titleStr tsTitle ]
                        else []
             items = legendOrder spec vals
             chipFor k (origI, label) =
               let (col, row) = legendGridV ncol nItems k
                   cx = x0 + fromIntegral col * colW
                   cy = y0 + titleH + 4 + fromIntegral row * itemH + itemH * 0.5
               in legendSwatch (legendPointLayer spec) (legendUsesPoint spec) (legendShapeFor spec origI) (legendMarkerDiam spec) pal cx (cy - chipSize / 2) chipSize (legendColorFor layout pal origI label)
                  <> [ PText (Point (cx + chipSize + chipGap) (cy + tsSize tsItem * 0.35)) label tsItem ]
         in bg <> header <> concat (zipWith chipFor [0..] items)
       _ -> []

-- ===========================================================================
-- Phase 6+ C-8: Annotation render
-- ===========================================================================

-- | [日本語]: annotation 1 個を Primitive に変換。 ★ 座標は 'Graphics.Hgg.Unit.Pos' で、
--   'resolvePosX'/'resolvePosY' (= UCtx 経由) で pt 化する。native/npc/絶対長を軸
--   ごとに混在できる。dpi は PAbs Px 解決にのみ使う (layout は pt)。
--   [English]: Converts a single annotation into primitives. Coordinates
--   are given as 'Graphics.Hgg.Unit.Pos' and converted to pt via 'resolvePosX'/'resolvePosY'
--   (through UCtx). native/npc/absolute-length units can be mixed per
--   axis. dpi is used only to resolve PAbs Px (layout itself is in pt).
renderAnnotation :: Double -> Layout -> ThemePalette -> Annotation -> [Primitive]
renderAnnotation dpi layout pal ann =
  let uc = UCtx dpi (lpPlotArea layout) (lpXScale layout) (lpYScale layout)
      rx = resolvePosX uc
      ry = resolvePosY uc
      _  = pal
  in case ann of
    AnnText x y t col sz ->
      let ts = TextStyle col sz "sans-serif" AnchorMiddle 0 "normal" False
      in [ PText (Point (rx x) (ry y)) t ts ]
    AnnArrow x1 y1 x2 y2 col w ->
      -- Phase 8 B20: 終点に矢じり (= 2 本の短線) を付ける。 旧実装はただの線で
      -- AnnLine と区別が付かなかった (PS は矢じりを描いており parity を取る)。
      let px1 = rx x1; py1 = ry y1; px2 = rx x2; py2 = ry y2
          dx = px2 - px1; dy = py2 - py1
          len = sqrt (dx * dx + dy * dy)
          (ux, uy) = if len == 0 then (0, 0) else (dx / len, dy / len)
          ah = mmPt 2.5   -- 矢じりの長さ (2.5mm)
          aw = 0.5         -- 開き (直交方向の比率)
          bx = px2 - ux * ah; by = py2 - uy * ah
          lx = bx - uy * ah * aw; ly = by + ux * ah * aw
          rx' = bx + uy * ah * aw; ry' = by - ux * ah * aw
          ls = solid col w
      in [ PLine (Point px1 py1) (Point px2 py2) ls
         , PLine (Point px2 py2) (Point lx ly) ls
         , PLine (Point px2 py2) (Point rx' ry') ls ]
    AnnRect x1 y1 x2 y2 fill stroke sw fillOp ->
      -- 2 隅の Pos を pt 化し、min/abs で正規矩形に (y は device 下向きで反転するため
      -- min/abs 必須・旧 (x,y,w,h) 実装と bit 一致)。
      let ax1 = rx x1; ay1 = ry y1; ax2 = rx x2; ay2 = ry y2
      in [ PRect (Rect (min ax1 ax2) (min ay1 ay2) (abs (ax2 - ax1)) (abs (ay2 - ay1)))
                 (FillStyle fill fillOp)
                 (Just (StrokeStyle stroke sw)) ]
    AnnLine x1 y1 x2 y2 col w ->
      [ PLine (Point (rx x1) (ry y1)) (Point (rx x2) (ry y2)) (solid col w) ]

-- | [日本語]: inset (図中図)。 子 spec を inset サイズの sub-viewport で描画し、
--   offsetPrim で plotArea 内の (inX, inY) 位置へシフトする (= PS renderInset と同方式)。
--   inX/inY/inW/inH は plotArea に対する 0..1 の比率。
--   [English]: An inset (a figure within a figure). Draws the child spec
--   in a sub-viewport at the inset's size and shifts it via offsetPrim to
--   position (inX, inY) within the plot area (matches PS renderInset).
--   inX/inY/inW/inH are 0..1 fractions of the plot area.
renderInset :: Resolver -> Layout -> ThemePalette -> Inset -> [Primitive]
renderInset r layout pal ins =
  let a  = lpPlotArea layout
      px = rX a + inX ins * rW a
      py = rY a + inY ins * rH a
      pw = inW ins * rW a
      ph = inH ins * rH a
      subSpec = (inSpec ins) { vsWidth  = Last (Just (Length pw Pt))
                             , vsHeight = Last (Just (Length ph Pt)) }
      subLayout = computeLayout r subSpec
      inner = map (offsetPrim px py) (renderToPrimitives r subLayout subSpec)
      frame = [ PRect (Rect px py pw ph)
                      (FillStyle (tpBackground pal) 0.95)
                      (Just (StrokeStyle (tpAxis pal) 0.8)) ]
  in frame <> inner

-- | [日本語]: primitive を (dx, dy) 平行移動 (inset 配置用)。
--   [English]: Translates a primitive by (dx, dy), used to position insets.
offsetPrim :: Double -> Double -> Primitive -> Primitive
offsetPrim dx dy p = case p of
  PLine (Point x1 y1) (Point x2 y2) ls ->
    PLine (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy)) ls
  PRect (Rect x y w h) f s -> PRect (Rect (x + dx) (y + dy) w h) f s
  PCircle (Point x y) rr f s t -> PCircle (Point (x + dx) (y + dy)) rr f s t
  PPath segs f s -> PPath (map offsetSeg segs) f s
  PText (Point x y) t ts -> PText (Point (x + dx) (y + dy)) t ts
  PClipPush (Rect x y w h) -> PClipPush (Rect (x + dx) (y + dy) w h)
  other -> other
  where
    offsetSeg seg = case seg of
      MoveTo (Point x y) -> MoveTo (Point (x + dx) (y + dy))
      LineTo (Point x y) -> LineTo (Point (x + dx) (y + dy))
      CurveTo (Point x1 y1) (Point x2 y2) (Point x3 y3) ->
        CurveTo (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))
                (Point (x3 + dx) (y3 + dy))
      ClosePath -> ClosePath

-- |
-- Module      : Graphics.Hgg.Render.Common
-- Description : Core types, theme, projection, axis/grid/tick, color, shape, and stat helpers
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Render モノリス分割 (出力中立・純粋移動)。
-- [English]: Split out from the Render monolith (output-neutral, pure relocation only).
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphics.Hgg.Render.Common where

import           Graphics.Hgg.Layout (numToText,
                                      Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      formatTicksGG,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      effectiveTickLength, effectiveTickDir,
                                      tickOutwardLen, effectivePlotMargin,
                                      effectiveBaseFontSize, effectiveHalfLine,
                                      effectiveAxTextMar,
                                      effectiveSubtitleSize, effectiveCaptionSize,
                                      effectiveTagSize,
                                      effectiveShowAxisText, effectiveShowAxisTitle,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      polarOuterFrac,
                                      isTernary, ternaryCenter, ternaryVertices,
                                      ternaryPoint, normalizeTernary,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical,
                                      -- Phase 64 A4: categorical-cross 投影口
                                      CrossLoc (..), BarShape (..),
                                      projectCrossPoint, projectCrossSpan,
                                      projectCrossBar,
                                      UCtx (..), resolvePosX, resolvePosY)
import           Graphics.Hgg.Unit   (Pos (..), mmToPt)
import           Graphics.Hgg.Primitive  -- Phase 51: Point/Rect/style/Primitive/scalePrimitives (leaf)
import           Graphics.Hgg.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Graphics.Hgg.Spec   (Annotation (..), AxisFormat (..),
                                      ColData (..), ColRef,
                                      ColorEnc (..), ConnectSpec (..),
                                      DAGEdge (..), DAGLayoutAlgorithm (..),
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
                                      ThemeOverride (..), TickDir (..), Margin (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axisTextAngleXOf, axisTextAngleYOf,
                                      axShowGrid,
                                      FontSpec (..), orderedCats,
                                      colRefName, distGroupRef, distDodgeRef,
                                      resolveCol, resolveNum)
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


-- Phase 51: Point/Rect/style/PathSegment/Transform/Primitive/solid/scalePrimitives は
-- 'Graphics.Hgg.Primitive' (leaf) へ移設 (循環回避)。 本 module は import 済 (下記)。

-- | [日本語]: TODO-10 (2026-05-29) PS port: どの font slot を引くか
--   (= spec の titleFont / axisLabelFont / tickFont / legendFont)。
--   [English]: TODO-10 (2026-05-29) PS port: which font slot to pull (that is,
--   the spec's titleFont / axisLabelFont / tickFont / legendFont).
data FontKind = TitleF | AxisLabelF | TickF | LegendTitleF | LegendItemF
  deriving (Show, Eq)

-- | [日本語]: TODO-10 (2026-05-29) PS port: spec の font 設定 + theme default を merge して TextStyle を生成。
--   spec を取れない場所 (= layer helper 内など) では Nothing を渡すと slot default に fallback。
--   [English]: TODO-10 (2026-05-29) PS port: merges the spec's font settings with
--   the theme default to build a 'TextStyle'. Where the spec is unavailable
--   (for example, inside a layer helper), pass 'Nothing' to fall back to the
--   slot default.
mkFontTS :: Maybe VisualSpec -> ThemePalette -> FontKind -> TextAnchor -> Double -> TextStyle
mkFontTS mSpec pal fk anchor rot =
  let -- Phase 34: ggplot theme_grey の base_size + 相対比に較正 (R theme_grey() 実測)。
      -- 旧値 (Title16/Axis12/Tick11/LegTitle11/LegItem10) は ggplot より系統的に大きく、
      -- 特に目盛が base 11pt のままだった (ggplot は axis.text = base×0.8 = 8.8pt)。
      -- ★ Phase 63 A12: 固定 11 を theme (toBaseFontSize) の実効値へ。 spec を取れない
      --   場所 (mSpec = Nothing) は従来どおり 11。 予約 (Layout) と同一情報源。
      baseSize = maybe 11 effectiveBaseFontSize mSpec
      defSize = case fk of
        TitleF       -> baseSize * 1.2   -- plot.title  rel(1.2) = 13.2pt
        AxisLabelF   -> baseSize         -- axis.title  = base    = 11pt
        TickF        -> baseSize * 0.8   -- axis.text   rel(0.8) = 8.8pt
        LegendTitleF -> baseSize         -- legend.title= base    = 11pt
        LegendItemF  -> baseSize * 0.8   -- legend.text rel(0.8) = 8.8pt
      -- Phase 9 A-3: font setter (vs*Font) に theme override (to*Font) を上書き合成。
      --   `setter <> override` (Maybe FontSpec の Monoid) で override の Just field が優先
      --   → 優先順位は override > font setter > preset 既定 (= 下の defSize/tpText fallback)。
      mFont = case mSpec of
        Nothing   -> Nothing
        Just spec ->
          let setterF = case fk of
                TitleF       -> getLast (vsTitleFont spec)
                AxisLabelF   -> getLast (vsAxisLabelFont spec)
                TickF        -> getLast (vsTickFont spec)
                LegendTitleF -> getLast (vsLegendFont spec)
                LegendItemF  -> getLast (vsLegendFont spec)
              overrideF = case fk of
                TitleF       -> getLast (toTitleFont     (vsThemeOverride spec))
                AxisLabelF   -> getLast (toAxisLabelFont (vsThemeOverride spec))
                TickF        -> getLast (toTickFont      (vsThemeOverride spec))
                LegendTitleF -> getLast (toLegendFont    (vsThemeOverride spec))
                LegendItemF  -> getLast (toLegendFont    (vsThemeOverride spec))
          in setterF <> overrideF
      orElse l d = case getLast l of
        Just v  -> v
        Nothing -> d
      -- Phase 32 (re-apply): plot.title / axis.title は tpTitleColor を既定色に
      --   (ggplot theme_grey は black)。 その他の文字 (tick/legend) は従来 tpText。
      defColor = case fk of
        TitleF     -> tpTitleColor pal
        AxisLabelF -> tpTitleColor pal
        _          -> tpText pal
      -- ★ Phase 63 A20.5: 全 slot 共通の family fallback (themeFontFamily)。
      --   優先順位: slot 別 FontSpec の fsFamily > toFontFamily > "sans-serif"
      defFamily = case mSpec of
        Nothing   -> "sans-serif"
        Just spec -> orElse (toFontFamily (vsThemeOverride spec)) "sans-serif"
  in case mFont of
       Nothing -> TextStyle defColor defSize defFamily anchor rot "normal" False
       Just fs -> TextStyle
         { tsColor  = orElse (Graphics.Hgg.Spec.fsColor  fs) defColor
         , tsSize   = orElse (Graphics.Hgg.Spec.fsSize   fs) defSize
         , tsFamily = orElse (Graphics.Hgg.Spec.fsFamily fs) defFamily
         , tsAnchor = anchor
         , tsRotate = rot
         , tsWeight = orElse (Graphics.Hgg.Spec.fsWeight fs) "normal"
         , tsItalic = orElse (Graphics.Hgg.Spec.fsItalic fs) False
         }

-- Phase 51: Transform / PathSegment / Primitive は 'Graphics.Hgg.Primitive' へ移設。

-- | [日本語]: mm → pt 変換。 mark 既定 (point/line/半径/cap/矢じり) を物理 mm で
--   書くためのヘルパ。 layout/Primitive は純 pt なので、 既定もここで pt に解決する。
--   backend が dpi 係数 (k) を最後に一律適用する ('scalePrimitives')。
--   [English]: Converts mm to pt. A helper for writing mark defaults
--   (point/line radius, cap, arrowhead) in physical mm. Since layout/Primitive
--   are in pure pt, defaults are resolved to pt here too; the backend applies
--   the dpi factor (k) uniformly at the end ('scalePrimitives').
mmPt :: Double -> Double
mmPt mm = mm * mmToPt

-- | [日本語]: scatter / point マーカーの既定__直径__ (pt)。ggplot
--   @geom_point@ 既定を実測した 1.65mm (= 半径 2.34pt) に較正
--   (`phase-34-measurements/A1-results.md`)。size 意味論は「外接円の直径」。
--   [English]: The default __diameter__ (pt) of a scatter/point marker.
--   Calibrated to the measured 1.65mm (radius 2.34pt) default of ggplot's
--   @geom_point@ (see `phase-34-measurements/A1-results.md`). The "size"
--   semantics is "the diameter of the bounding circle".
defaultMarkerDiameter :: Double
defaultMarkerDiameter = mmPt 1.65

-- | [日本語]: 線 (geom_line/path/step/segment) の既定__線幅__ (pt)。ggplot
--   @linewidth 0.5@ の実描画幅 0.376mm に較正 (解析式 @nominal × .pt/96 × 25.4@ を
--   太線 bbox 実測で検証)。
--   [English]: The default __line width__ (pt) for lines (geom_line/path/step/
--   segment). Calibrated to the measured 0.376mm rendered width of ggplot's
--   @linewidth 0.5@ (the formula @nominal × .pt/96 × 25.4@ was verified
--   against a thick-line bounding-box measurement).
defaultLineWidth :: Double
defaultLineWidth = mmPt 0.376

-- | [日本語]: geom_smooth 線の既定線幅 (pt)。ggplot は @linewidth = 2 × 既定@ なので line の
--   2倍 (0.753mm)。
--   [English]: The default line width (pt) for geom_smooth lines. ggplot uses
--   @linewidth = 2 × default@, so this is twice the plain line width (0.753mm).
defaultSmoothWidth :: Double
defaultSmoothWidth = mmPt 0.753

-- | [日本語]: Theme 色 palette (= JSON serialize しないので Render module 内に閉じる)。
--   色に加え「panel 背景塗り / grid / border 有無フラグ」 を持つ。
--     * tpPanelBg    = panel (plotArea) 背景色。 tpShowPanel が True のとき塗る。
--     * tpShowPanel  = panel 矩形を塗るか (theme_grey / ブランドは True、 従来 preset は False)。
--     * tpShowGrid   = theme レベルの grid master (False で全 grid 抑制。 軸ごと axShowGrid と AND)。
--     * tpShowBorder = axisFrame の 4 辺枠を描くか (従来 preset True、 panel 塗り系は False)。
--     * tpShowBackground = plot 全面背景 (tpBackground) を塗るか。
--       False = 塗らない = 透過。 tpBackground の色自体は geom_label 箱や bar 縁取り等の
--       「背景色」 参照用に残る。
--   [English]: The theme color palette (kept inside the Render module because
--   it is not JSON-serialized). Besides the colors themselves, it carries
--   on/off flags for panel background fill, grid, and border:
--     * tpPanelBg    = the panel (plot area) background color, painted when
--       tpShowPanel is True.
--     * tpShowPanel  = whether to paint the panel rectangle (True for
--       theme_grey / brand themes, False for the legacy presets).
--     * tpShowGrid   = the theme-level grid master switch (False suppresses
--       all grid lines; ANDed with the per-axis axShowGrid).
--     * tpShowBorder = whether to draw the 4-sided axisFrame border (True for
--       the legacy presets, False for panel-filled themes).
--     * tpShowBackground = whether to paint the plot's full background
--       (tpBackground). False means no fill (transparent); the tpBackground
--       color itself is still kept for other uses, such as the geom_label box
--       or bar outline "background color".
data ThemePalette = ThemePalette
  { tpBackground :: !Text
  , tpShowBackground :: !Bool
  , tpAxis       :: !Text
  , tpText       :: !Text
  , tpGrid       :: !Text
  , tpDefault    :: !Text   -- layer default color (point/line/density 等の線・点)
  , tpDefaultFill :: !Text  -- ★ Phase 34: fill geom (bar/histogram/area) の既定塗り色。
                            --   ggplot は geom ごとに既定が違う (point/line=black, bar=grey35)
                            --   ので 2 値持つ。 brand/従来テーマは tpDefault と同値 (挙動不変)。
  , tpPanelBg    :: !Text   -- panel (plotArea) 背景色
  , tpShowPanel  :: !Bool   -- panel 矩形を塗るか
  , tpShowGrid   :: !Bool   -- theme レベルの grid master
    -- ★ Phase 63 A2: major/minor の個別 flag。 preset 定義では書かず 'themePalette' の
    --   出口で tpShowGrid と同値に初期化 (= 既定挙動不変)。 'resolveTheme' が
    --   個別 override > 一括 toShowGrid > preset の順で解決する。
  , tpShowGridMajor :: !Bool -- panel.grid.major
  , tpShowGridMinor :: !Bool -- panel.grid.minor
  , tpShowBorder :: !Bool   -- axisFrame の 4 辺枠を描くか
  , tpShowAxisLine :: !Bool -- 下辺(x軸)+左辺(y軸)の 2 本軸線を描くか (theme_classic)
  -- ★ Phase 32 (re-apply): ggplot theme_grey fidelity 用の追加 field。
  --   既存テーマは既定値で挙動不変、 ThemeGrey のみ ggplot 厳密値を入れる。
  , tpTitleColor    :: !Text   -- plot.title / axis.title の文字色 (ggplot=black、 既定= tpText)
  , tpTitleHjust    :: !Double -- plot.title の水平揃え (0=左、 0.5=中央。 ggplot theme_grey=0)
  , tpTickLineColor :: !Text   -- 軸目盛線 (tick mark) の色 (ggplot=grey20、 既定= tpAxis)
  , tpLegendKeyBg   :: !Text   -- legend.key 背景塗り色 ("" なら塗らない。 ggplot=grey95)
  } deriving (Show, Eq)

themePalette :: Graphics.Hgg.Spec.ThemeName -> ThemePalette
themePalette t = case t of
  -- 従来 4 preset: 白(灰)背景・panel 塗り無し・4 辺枠あり (= G4 の白背景+薄 grid を温存)。
  Graphics.Hgg.Spec.ThemeDefault -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#444444", tpText = "#333333", tpGrid = "#dddddd"
    , tpDefault = "#1f77b4", tpDefaultFill = "#1f77b4", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#333333", tpTitleHjust = 0.0, tpTickLineColor = "#444444", tpLegendKeyBg = "" }
  Graphics.Hgg.Spec.ThemeMinimal -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#333333", tpText = "#333333", tpGrid = "#eeeeee"
    , tpDefault = "#1f77b4", tpDefaultFill = "#1f77b4", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#333333", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "" }
  Graphics.Hgg.Spec.ThemeLight -> ThemePalette
    { tpBackground = "#fafafa", tpAxis = "#666666", tpText = "#444444", tpGrid = "#e0e0e0"
    , tpDefault = "#3498db", tpDefaultFill = "#3498db", tpPanelBg = "#fafafa"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#444444", tpTitleHjust = 0.0, tpTickLineColor = "#666666", tpLegendKeyBg = "" }
  Graphics.Hgg.Spec.ThemeDark -> ThemePalette
    { tpBackground = "#222222", tpAxis = "#cccccc", tpText = "#eeeeee", tpGrid = "#444444"
    , tpDefault = "#5dade2", tpDefaultFill = "#5dade2", tpPanelBg = "#222222"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#eeeeee", tpTitleHjust = 0.0, tpTickLineColor = "#cccccc", tpLegendKeyBg = "" }
  -- ggplot 既定 theme_grey: 白 plot bg・灰 panel #EBEBEB・白 grid・枠なし・軸線なし。
  -- ★ Phase 34: geom 既定色を ggplot 厳密値に (point/line = black、 bar/hist = grey35)。
  Graphics.Hgg.Spec.ThemeGrey -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#4d4d4d", tpText = "#4d4d4d", tpGrid = "#ffffff"
    , tpDefault = "#000000", tpDefaultFill = "#595959", tpPanelBg = "#ebebeb"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    -- ★ Phase 32: ggplot theme_grey 厳密値。 title=black/左寄せ・tick=grey20・legend.key=grey95。
    , tpTitleColor = "#000000", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "#f2f2f2" }
  -- ブランド (panel 塗りあり・grid あり・枠なし、 series は themeSeriesPalette)。
  Graphics.Hgg.Spec.ThemeNoir -> ThemePalette
    { tpBackground = "#16161e", tpAxis = "#5a6080", tpText = "#c8ccda", tpGrid = "#2a2e45"
    , tpDefault = "#7aa2f7", tpDefaultFill = "#7aa2f7", tpPanelBg = "#1e2030"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#c8ccda", tpTitleHjust = 0.0, tpTickLineColor = "#5a6080", tpLegendKeyBg = "" }
  Graphics.Hgg.Spec.ThemeLumen -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#8a857e", tpText = "#2b2b33", tpGrid = "#e7e3db"
    , tpDefault = "#4c5bd4", tpDefaultFill = "#4c5bd4", tpPanelBg = "#f7f5f1"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#2b2b33", tpTitleHjust = 0.0, tpTickLineColor = "#8a857e", tpLegendKeyBg = "" }
  -- Parchment 正式テーマ (明)。 panel=羊皮紙 cream-light #F8F5EE は据え置き、
  --   外周 plot bg は白 #FFFFFF にして軸内 panel を額装的に強調 (2026-06-02 ユーザ確定)。
  Graphics.Hgg.Spec.ThemeParchment -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#8b6f3a", tpText = "#1a1620", tpGrid = "#e0d6c0"
    , tpDefault = "#f0a5a0", tpDefaultFill = "#f0a5a0", tpPanelBg = "#f8f5ee"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#1a1620", tpTitleHjust = 0.0, tpTickLineColor = "#8b6f3a", tpLegendKeyBg = "" }
  -- 暗版 = Charcoal (中性炭、 Red Queen §4.8 Charcoal #2B2B2E 由来。 焦茶から変更 2026-06-02)。
  Graphics.Hgg.Spec.ThemeParchmentDark -> ThemePalette
    { tpBackground = "#1e1e22", tpAxis = "#9aa0a8", tpText = "#d6d8dd", tpGrid = "#42424a"
    , tpDefault = "#f0a5a0", tpDefaultFill = "#f0a5a0", tpPanelBg = "#2a2a30"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#d6d8dd", tpTitleHjust = 0.0, tpTickLineColor = "#9aa0a8", tpLegendKeyBg = "" }
  -- ggplot theme_bw: 白背景・薄グレー grid・黒灰の 4 辺枠 (軸線なし)。
  Graphics.Hgg.Spec.ThemeBW -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#333333", tpText = "#4d4d4d", tpGrid = "#ebebeb"
    , tpDefault = "#353535", tpDefaultFill = "#353535", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#4d4d4d", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "" }
  -- ggplot theme_classic: 白背景・grid なし・枠なし・下/左の 2 軸線あり。
  Graphics.Hgg.Spec.ThemeClassic -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#333333", tpText = "#4d4d4d", tpGrid = "#ffffff"
    , tpDefault = "#353535", tpDefaultFill = "#353535", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = False, tpShowBorder = False, tpShowAxisLine = True
    , tpShowGridMajor = False, tpShowGridMinor = False, tpShowBackground = True
    , tpTitleColor = "#4d4d4d", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "" }
  -- ggplot theme_void: 背景・grid・枠・軸線すべてなし (データのみ)。
  Graphics.Hgg.Spec.ThemeVoid -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#4d4d4d", tpText = "#4d4d4d", tpGrid = "#ffffff"
    , tpDefault = "#353535", tpDefaultFill = "#353535", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = False, tpShowBorder = False, tpShowAxisLine = False
    , tpShowGridMajor = False, tpShowGridMinor = False, tpShowBackground = True
    , tpTitleColor = "#4d4d4d", tpTitleHjust = 0.0, tpTickLineColor = "#4d4d4d", tpLegendKeyBg = "" }
  -- ggplot theme_linedraw: 白背景・黒寄り細 grid・黒の 4 辺枠。
  Graphics.Hgg.Spec.ThemeLinedraw -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#000000", tpText = "#1a1a1a", tpGrid = "#b3b3b3"
    , tpDefault = "#000000", tpDefaultFill = "#000000", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpShowGridMajor = True, tpShowGridMinor = True, tpShowBackground = True
    , tpTitleColor = "#1a1a1a", tpTitleHjust = 0.0, tpTickLineColor = "#000000", tpLegendKeyBg = "" }

-- | [日本語]: preset palette に ThemeOverride を合成 (element 単位上書き)。
--   各 override field が Just なら preset 値を差し替える。 描画は合成後の値のみ参照。
--   [English]: Merges a 'ThemeOverride' onto the preset palette (per-element
--   override). Each Just override field replaces the corresponding preset
--   value; rendering only ever consults the merged result.
resolveTheme :: Graphics.Hgg.Spec.ThemeName -> ThemeOverride -> ThemePalette
resolveTheme name ov =
  let base = themePalette name
  in base
       { tpBackground   = ovT toPlotBg       (tpBackground base)
         -- ★ Phase 63 A18: plot.background 塗り on/off (False = 透過)。
       , tpShowBackground = ovB toShowBackground (tpShowBackground base)
       , tpPanelBg      = ovT toPanelBg      (tpPanelBg base)
       , tpShowPanel    = ovB toShowPanel    (tpShowPanel base)
       , tpGrid         = ovT toGridColor    (tpGrid base)
       , tpShowGrid     = ovB toShowGrid     (tpShowGrid base)
         -- ★ Phase 63 A2: 個別 flag > 一括 toShowGrid > preset の順。 toShowGrid は
         --   major/minor 両方を設定する糖衣なので、 一括値を既定に個別値で上書きする。
       , tpShowGridMajor = ovB toShowGridMajor (ovB toShowGrid (tpShowGridMajor base))
       , tpShowGridMinor = ovB toShowGridMinor (ovB toShowGrid (tpShowGridMinor base))
       , tpShowBorder   = ovB toShowBorder   (tpShowBorder base)
       , tpShowAxisLine = ovB toShowAxisLine (tpShowAxisLine base)
       , tpAxis         = ovT toAxisColor    (tpAxis base)
       , tpText         = ovT toTextColor    (tpText base)
         -- ★ Phase 43 A4: プリセット専用だった 4 項目の上書き合成。
       , tpTitleHjust    = ovD toTitleHjust    (tpTitleHjust base)
       , tpTitleColor    = ovT toTitleColor    (tpTitleColor base)
       , tpTickLineColor = ovT toTickLineColor (tpTickLineColor base)
       , tpLegendKeyBg   = ovT toLegendKeyBg   (tpLegendKeyBg base)
       }
  where
    ovT f d = fromMaybe d (getLast (f ov))
    ovB f d = fromMaybe d (getLast (f ov))
    ovD f d = fromMaybe d (getLast (f ov))

-- | [日本語]: spec の theme + override を解決して ThemePalette を得る (全 render 経路の入口)。
--   [English]: Resolves the spec's theme plus its override into a
--   'ThemePalette' (the entry point used by every render path).
specThemePalette :: VisualSpec -> ThemePalette
specThemePalette spec =
  resolveTheme (fromMaybe Graphics.Hgg.Spec.ThemeDefault (getLast (vsTheme spec)))
               (vsThemeOverride spec)

-- | [日本語]: facet strip.background の (塗り色, 表示) を解決。 ggplot は殆どの preset で
--   灰矩形 (grey85 #d9d9d9)、 theme_minimal / theme_void は strip 矩形なし。 panel 塗り系
--   (dark/noir/canvas-dark) は panel より少し明るい/暗い帯。 override (toStripBg/toShowStrip) 優先。
--   [English]: Resolves the facet strip.background (fill color, visibility).
--   Most ggplot presets use a grey rectangle (grey85 #d9d9d9); theme_minimal /
--   theme_void draw no strip rectangle at all. Panel-filled themes
--   (dark/noir/canvas-dark) use a band slightly lighter/darker than the panel.
--   The override (toStripBg/toShowStrip) takes priority when present.
themeStripStyle :: VisualSpec -> (Text, Bool)
themeStripStyle spec =
  let name = fromMaybe Graphics.Hgg.Spec.ThemeDefault (getLast (vsTheme spec))
      ov   = vsThemeOverride spec
      (dbg, dshow) = case name of
        Graphics.Hgg.Spec.ThemeMinimal          -> ("#d9d9d9", False)
        Graphics.Hgg.Spec.ThemeVoid             -> ("#ffffff", False)
        Graphics.Hgg.Spec.ThemeLight            -> ("#e0e0e0", True)
        Graphics.Hgg.Spec.ThemeDark             -> ("#3a3a3a", True)
        Graphics.Hgg.Spec.ThemeNoir      -> ("#2a2e45", True)
        Graphics.Hgg.Spec.ThemeLumen     -> ("#ece8e0", True)
        Graphics.Hgg.Spec.ThemeParchment    -> ("#ece0c8", True)
        Graphics.Hgg.Spec.ThemeParchmentDark -> ("#3a3a42", True)
        _                                        -> ("#d9d9d9", True)  -- default/grey/bw/classic/linedraw
      bg  = fromMaybe dbg   (getLast (toStripBg ov))
      shw = fromMaybe dshow (getLast (toShowStrip ov))
  in (bg, shw)

-- | [日本語]: Scale の range (= pixel 出力域) を別 Rect に合わせて作り直す。 domain は不変。
--   plot area を縮める時 (subplot / marginal) は plotArea だけでなく scale の range も
--   必ず合わせないと、 mark の位置が古い枠基準のまま描かれて軸枠からはみ出す
--   (= ggplot で panel が動けば座標変換も追従するのと同じ原則)。
--   [English]: Rebuilds a scale's range (the pixel output extent) to match a
--   different Rect; the domain is unchanged. When shrinking the plot area
--   (subplots / marginal panels), the scale's range must be re-matched along
--   with plotArea — otherwise marks keep the old frame's positions and spill
--   outside the axis frame (the same principle as ggplot's coordinate
--   transform following the panel whenever it moves).
scaleRetargetX :: Graphics.Hgg.Layout.Scale -> Rect -> Graphics.Hgg.Layout.Scale
scaleRetargetX scale rect = case scale of
  LinearScale lo hi _ _ -> LinearScale lo hi (rX rect) (rX rect + rW rect)
  LogScale lo hi _ _    -> LogScale    lo hi (rX rect) (rX rect + rW rect)
  SqrtScale lo hi _ _   -> SqrtScale   lo hi (rX rect) (rX rect + rW rect)
  TimeScale lo hi _ _   -> TimeScale   lo hi (rX rect) (rX rect + rW rect)

scaleRetargetY :: Graphics.Hgg.Layout.Scale -> Rect -> Graphics.Hgg.Layout.Scale
scaleRetargetY scale rect = case scale of
  LinearScale lo hi _ _ -> LinearScale lo hi (rY rect + rH rect) (rY rect)
  LogScale lo hi _ _    -> LogScale    lo hi (rY rect + rH rect) (rY rect)
  SqrtScale lo hi _ _   -> SqrtScale   lo hi (rY rect + rH rect) (rY rect)
  TimeScale lo hi _ _   -> TimeScale   lo hi (rY rect + rH rect) (rY rect)

-- ===========================================================================
-- ★ Phase 68: grid / 軸線 線幅の実効値解決 (単一情報源)
--   ThemeOverride の線幅 field (未指定=各 role の現状決め打ち) を解決する。
--   未指定時は既存 golden をゼロ diff で保つため現状リテラルへ fallback。
--   線幅は panel 面積に波及しないので Layout 予約は無く、 Render 側のみで閉じる。
--   [English]: Phase 68 — effective line widths for grid / axis lines (single
--   source of truth). Resolves the ThemeOverride width fields, falling back to
--   each role's current literal when unspecified (zero golden diff). Widths do
--   not affect panel area, so this is closed on the Render side (no Layout
--   reservation).
-- ===========================================================================

-- | Cartesian grid major の線幅。 既定 1.0。
effectiveGridWidth :: ThemeOverride -> Double
effectiveGridWidth ov = fromMaybe 1.0 (getLast (toGridWidth ov))

-- | Cartesian grid minor の線幅。 未指定は major × 0.5 (ggplot @panel.grid.minor
--   = rel(0.5)@)、 'toGridMinorWidth' 明示時はそれを優先。
effectiveGridMinorWidth :: ThemeOverride -> Double
effectiveGridMinorWidth ov =
  fromMaybe (effectiveGridWidth ov * 0.5) (getLast (toGridMinorWidth ov))

-- | polar / ternary grid の線幅。 現状は座標系別の決め打ち 0.5。 'toGridWidth' を
--   指定した場合は Cartesian major と統一する (ggplot panel.grid は座標系非依存)、
--   未指定は現状 0.5 を維持 (golden 保存)。 polar・ternary は同挙動なので共用。
effectiveNonCartesianGridWidth :: ThemeOverride -> Double
effectiveNonCartesianGridWidth ov = fromMaybe 0.5 (getLast (toGridWidth ov))

-- | axis.line / panel.border / ternary edge / 右 Y 軸線 の線幅。 既定 1.0。
--   (tick mark は ggplot @axis.ticks@ = 別 element なので本 Phase scope 外・1.0 維持。)
effectiveAxisLineWidth :: ThemeOverride -> Double
effectiveAxisLineWidth ov = fromMaybe 1.0 (getLast (toAxisLineWidth ov))

-- | [日本語]: TODO-3b (2026-05-29): C-5 grid line 描画。 PS Render.purs:gridLines を
--   HS に port。 vsXAxis / vsYAxis の axShowGrid が True なら x/y tick 位置に
--   薄い grid line を描く。 default false (= 旧 HS 挙動と互換)。
--   [English]: TODO-3b (2026-05-29): draws C-5 grid lines. Ported from PS
--   Render.purs:gridLines to Haskell. When axShowGrid is True on vsXAxis /
--   vsYAxis, faint grid lines are drawn at the x/y tick positions. Defaults to
--   false (compatible with the previous Haskell behavior).
gridLines :: Layout -> VisualSpec -> ThemePalette -> [Primitive]
gridLines layout spec pal =
  let area = lpPlotArea layout
      coord = coordOf spec
      ov = vsThemeOverride spec   -- ★ Phase 68: grid 線幅 override
      sx = scaleApply (lpXScale layout)
      sy = scaleApply (lpYScale layout)
      -- Phase 9 C: flip 時はデータ x が縦 px・データ y が横 px に写る。
      sxF = scaleApply (lpXScaleFlipped layout)
      syF = scaleApply (lpYScaleFlipped layout)
      -- ★ Phase 68: 決め打ち 1.0/0.5 を theme (toGridWidth/toGridMinorWidth) 実効値へ。
      --   未指定=現状値 (major 1.0 / minor = major×0.5 = 0.5) で golden ゼロ diff。
      majorStyle = solid (tpGrid pal) (effectiveGridWidth ov)
      minorStyle = solid (tpGrid pal) (effectiveGridMinorWidth ov)   -- G4: minor は major の半分 (ggplot 準拠)
      -- Phase 8 C G4: ggplot theme は既定で grid 表示。 axShowGrid 未指定 (Nothing) は
      -- 旧 False → True に (白背景 + 薄グレー major+minor grid = theme_bw/minimal 風)。
      showXGrid = case getLast (axShowGrid (axisOrDef (vsXAxis spec))) of
        Just b  -> b
        Nothing -> True
      showYGrid = case getLast (axShowGrid (axisOrDef (vsYAxis spec))) of
        Just b  -> b
        Nothing -> True
      xLo = rX area; xHi = rX area + rW area
      yLo = rY area; yHi = rY area + rH area
      withinX p = p >= min xLo xHi - 1e-6 && p <= max xLo xHi + 1e-6
      withinY p = p >= min yLo yHi - 1e-6 && p <= max yLo yHi + 1e-6
      -- categorical 軸は minor grid 無し (= ggplot 離散軸は major のみ)。
      catX = not (null (lpXCategoryLabels layout))
      catY = not (null (lpYCategoryLabels layout))
      -- Cartesian は現行と bit 一致。 Flip はデータ x grid を横線・データ y grid を縦線に。
      (majorX, minorX, majorY, minorY) = case coord of
        CoordCartesian ->
          ( [ PLine (Point (sx v) yLo) (Point (sx v) yHi) majorStyle | v <- lpXTicks layout ]
          , if catX then []
            else [ PLine (Point (sx v) yLo) (Point (sx v) yHi) minorStyle
                 | v <- minorBreaksFor (lpXScale layout) (lpXTicks layout), withinX (sx v) ]
          , [ PLine (Point xLo (sy v)) (Point xHi (sy v)) majorStyle | v <- lpYTicks layout ]
          , if catY then []
            else [ PLine (Point xLo (sy v)) (Point xHi (sy v)) minorStyle
                 | v <- minorBreaksFor (lpYScale layout) (lpYTicks layout), withinY (sy v) ] )
        CoordFlip ->
          ( [ PLine (Point xLo (sxF v)) (Point xHi (sxF v)) majorStyle | v <- lpXTicks layout ]
          , if catX then []
            else [ PLine (Point xLo (sxF v)) (Point xHi (sxF v)) minorStyle
                 | v <- minorBreaksFor (lpXScale layout) (lpXTicks layout), withinY (sxF v) ]
          , [ PLine (Point (syF v) yLo) (Point (syF v) yHi) majorStyle | v <- lpYTicks layout ]
          , if catY then []
            else [ PLine (Point (syF v) yLo) (Point (syF v) yHi) minorStyle
                 | v <- minorBreaksFor (lpYScale layout) (lpYTicks layout), withinX (syF v) ] )
        -- Phase 11 A7-c: 極座標の grid は polarGrid (= 同心円 + 放射スポーク) が描く。
        _ -> ([], [], [], [])
      -- minor を先に (= major が上に乗る)。 grid 全体は layer の下 (描画順は呼出側)。
      -- Phase 63 A2: major/minor を個別 flag で描き分け ('resolveTheme' が一括
      -- toShowGrid / preset との優先を解決済)。 軸ごと axShowGrid は従来通り AND。
      gx = pick showXGrid minorX majorX
      gy = pick showYGrid minorY majorY
      pick axOn minor major =
        (if tpShowGridMinor pal && axOn then minor else [])
          ++ (if tpShowGridMajor pal && axOn then major else [])
  in gx <> gy
  where
    axisOrDef la = case getLast la of
      Just sp -> sp
      Nothing -> mempty
    -- Phase 9 A-5 fix: minor breaks を scale 別に算出。 旧実装は一様 step (b-a) 前提で、
    -- log/sqrt の非一様 major tick に当てると変な位置に線が出ていた。
    --   * Linear/Time : major tick 間の中点 + 両端 half-step (ggplot 既定 minor)。
    --   * Log         : 各 decade の 2..9 ×10^n (= ggplot log minor grid)。
    --   * Sqrt        : 連続 major 間の sqrt 空間中点 (((√a+√b)/2)²)。
    -- panel 外は呼出側 within* で除外。
    minorBreaksFor :: Scale -> [Double] -> [Double]
    minorBreaksFor scale ts = case scale of
      LogScale dLo dHi _ _ | dLo > 0, dHi > 0 ->
        let lo = min dLo dHi; hi = max dLo dHi
        in [ val | e <- [floor (logBase 10 lo) .. ceiling (logBase 10 hi)]
                 , k <- [2,3,4,5,6,7,8,9] :: [Double]
                 , let val = k * 10 ** fromIntegral (e :: Int)
                 , val >= lo, val <= hi ]
      SqrtScale dLo dHi _ _ | dLo >= 0, dHi >= 0 ->
        [ ((sqrt a + sqrt b) / 2) ** 2 | (a, b) <- zip ts (drop 1 ts) ]
      _ -> uniformMid ts
    uniformMid ts = case ts of
      (a : b : _) -> let step = b - a in [ t - step / 2 | t <- ts ] ++ [ last ts + step / 2 ]
      _           -> []

-- | [日本語]: 極座標の grid + 軸 (= 直交 gridLines/axisFrame/tickMarks の代わり)。
--   半径方向 = 同心円 (rad tick ごと) + 中心からの r 軸ラベル (上スポーク沿い)。
--   角度方向 = 放射スポーク (theta tick ごと) + 外周の角度ラベル。
--   theta 軸は PolarX なら x、 PolarY なら y。
--   [English]: Polar-coordinate grid + axes (replacing the Cartesian
--   gridLines/axisFrame/tickMarks). The radial direction draws concentric
--   circles (one per radius tick) plus r-axis labels along the top spoke; the
--   angular direction draws radial spokes (one per theta tick) plus angle
--   labels around the perimeter. The theta axis is x for PolarX and y for
--   PolarY.
polarGrid :: VisualSpec -> Layout -> ThemePalette -> [Primitive]
polarGrid spec layout pal =
  let coord = coordOf spec
      (cx, cy, maxR) = polarCenter layout
      gridCol = tpGrid pal
      -- ★ Phase 68: polar grid (同心円 + 外周円 + スポーク) の線幅を theme 実効値へ。
      --   未指定 0.5 維持。 toGridWidth 指定時は Cartesian major と統一。 ('ovT' は下で定義)
      polarGridW = effectiveNonCartesianGridWidth ovT
      circleStyle = Just (StrokeStyle gridCol polarGridW)
      noFill = FillStyle gridCol 0.0
      spokeStyle = solid gridCol polarGridW
      -- theta / radius を担う scale と tick / category ラベルを coord で選ぶ。
      (thetaScale, thetaTicks, thetaCats, radScale, radTicks) = case coord of
        CoordPolarY _ -> ( lpYScale layout, lpYTicks layout, lpYCategoryLabels layout
                         , lpXScale layout, lpXTicks layout )
        _           -> ( lpXScale layout, lpXTicks layout, lpXCategoryLabels layout
                       , lpYScale layout, lpYTicks layout )
      inUnit f = f >= -1e-9 && f <= 1 + 1e-9
      -- 同心円 (半径 grid)。 domFrac が [0,1] のものだけ。
      circles = [ PCircle (Point cx cy) (domFrac radScale v * maxR) noFill circleStyle Nothing
                | v <- radTicks, inUnit (domFrac radScale v) ]
      -- 外周円。 ★ Phase 64 A8: ggplot2 に完全準拠させた (user 判断 2026-08-06)。
      --   旧実装は Phase 11 A7-c 由来の**軸色の濃い円** (ggplot2 に対応物が無い独自
      --   要素) をデータ最大半径に描いていた。 ggplot2 は radial grid の最外周を
      --   θ ラベルと同じ npc 0.45 に、 **grid 線として**置く
      --   (coord-polar.R の @rfine <- c(r_rescale(...), 0.45)@)。 これに合わせたので
      --   θ ラベルが線に重なっても読める (細い grid 色) し、 grid off の theme では
      --   ggplot2 と同じく円自体が消える。
      boundary = [ PCircle (Point cx cy) (maxR * polarOuterFrac) noFill circleStyle Nothing ]
      -- 放射スポーク (角度 grid)。 中心→外周円 (ggplot2 も中心→npc 0.45 =
      --   coord-polar.R render_bg の @vec_interleave(0, 0.45 * sin(theta))@)。
      spokes = [ PLine (Point cx cy)
                       (uncurry Point (polarPointXY (domFrac thetaScale v) polarOuterFrac))
                       spokeStyle
               | v <- thetaTicks ]
      -- r 軸ラベル (上スポーク θ=0 沿い、 各 rad tick)。
      tsR = mkFontTS (Just spec) pal TickF AnchorEnd 0
      radLabels = [ PText (Point (cx - 4) (cy - domFrac radScale v * maxR + 4)) (numToText v) tsR
                  | v <- radTicks, inUnit (domFrac radScale v), domFrac radScale v > 1e-6 ]
      -- θ 軸ラベル (外周円の上、 各 theta tick)。 categorical なら群名、 でなければ値。
      --   ★ Phase 64 A8: 旧実装は根拠の無い @1.12@ 倍で、 maxR が panel 内接円
      --   (= panel の縁) だったため必ず panel の外へ出てタイトルと重なっていた。
      --   maxR を ggplot2 の npc 0.4 に合わせた今は 'polarOuterFrac' (0.45/0.4) が
      --   そのまま ggplot2 の θ ラベル半径 npc 0.45 に一致し、 panel 内に収まる。
      -- ★ Phase 64 A18: θ 軸ラベルの回転を theme の axis.text 角に従わせる
      --   (ggplot axis.text.x = element_text(angle=))。 θ を担う軸は coord で変わる
      --   (PolarX=x / PolarY=y) ので、 その軸の 'axisTextAngleXOf'/'axisTextAngleYOf'
      --   を解決する (Cartesian tick と同じ resolveAxisAngle 経路 = CCW 正 canonical)。
      --   polar の θ ラベルは npc 0.45 の panel 内配置なので、 Cartesian と違い
      --   回転マージンの予約は不要 (Layout 側は無改造)。 接線方向への自動回転は
      --   ggplot に無いので入れない (plan §4-3)。
      --   [English]: Make the theta-axis label rotation follow the theme's
      --   axis.text angle (ggplot axis.text.x = element_text(angle=)). Which
      --   spec axis drives theta depends on the coord (PolarX=x / PolarY=y), so
      --   resolve that axis's angle via the same resolveAxisAngle path as
      --   Cartesian ticks (CCW-positive canonical). Polar theta labels sit
      --   inside the panel at npc 0.45, so unlike Cartesian no rotation-margin
      --   reservation is needed (Layout untouched). No automatic tangential
      --   rotation (ggplot has none; plan §4-3).
      ovT = vsThemeOverride spec
      thetaRot = case coord of
        CoordPolarY _ -> resolveAxisAngle (vsYAxis spec) (axisTextAngleYOf ovT)
        _             -> resolveAxisAngle (vsXAxis spec) (axisTextAngleXOf ovT)
      tsT = mkFontTS (Just spec) pal TickF AnchorMiddle thetaRot
      thetaLabelFor i v = if not (null thetaCats) && i < length thetaCats
                            then thetaCats !! i else numToText v
      thetaLabels = [ let (lx, ly) = polarPointXY (domFrac thetaScale v) polarOuterFrac
                      in PText (Point lx (ly + 4)) (thetaLabelFor i v) tsT
                    | (i, v) <- zip [0 ..] thetaTicks ]
      polarPointXY tf rf = polarPoint layout tf rf
  -- ★ Phase 64 A8: 外周円は grid の一部になったので、 grid off の theme では
  --   ggplot2 と同じく描かない (旧実装は軸要素扱いで常に描いていた)。
  in if tpShowGrid pal then circles <> spokes <> boundary <> radLabels <> thetaLabels
     else radLabels <> thetaLabels

-- | [日本語]: 三角座標 (ternary) の grid + 軸 (= Phase 64 A12、 'polarGrid' の対)。
--   直交 gridLines/axisFrame/tickMarks の代わりに、 正三角形の外周 3 辺 + 3 方向の
--   格子線 + 三辺の数値 tick ラベル + 3 頂点の軸タイトルを描く。 成分 ↔ 頂点は
--   'ternaryVertices' に従う (a=上・b=左下・c=右下)。 grid off の theme では格子線を
--   落とし、 外周 3 辺 + tick/タイトルは残す ('polarGrid' の外周円と同方針)。
--   [English]: The ternary grid + axes (Phase 64 A12; the counterpart of
--   'polarGrid'). Instead of the Cartesian gridLines/axisFrame/tickMarks, it
--   draws the equilateral triangle's three outer edges, three families of grid
--   lines, numeric tick labels along the three edges, and the three vertex
--   axis titles. Component ↔ vertex follows 'ternaryVertices' (a=top,
--   b=bottom-left, c=bottom-right). Under a grid-off theme the grid lines are
--   dropped while the outer edges, ticks, and titles remain (matching how
--   'polarGrid' keeps its boundary).
ternaryGrid :: VisualSpec -> Layout -> ThemePalette -> [Primitive]
ternaryGrid spec layout pal =
  let (ctrX, ctrY, _) = ternaryCenter layout
      ov        = vsThemeOverride spec   -- ★ Phase 68: edge/grid 線幅 override
      gridCol   = tpGrid pal
      axisCol   = tpAxis pal
      -- ★ Phase 68: 三角形の 3 辺 = axis.line role (toAxisLineWidth・既定 1.0)、
      --   内部格子 = grid role (toGridWidth 指定で統一・未指定 0.5)。
      edgeStyle = solid axisCol (effectiveAxisLineWidth ov)
      gridStyle = solid gridCol (effectiveNonCartesianGridWidth ov)
      tp abc = uncurry Point (ternaryPoint layout abc)
      -- 外周 3 辺 (a=上 A, b=左下 B, c=右下 C)。
      edges = [ PLine (tp (1, 0, 0)) (tp (0, 1, 0)) edgeStyle    -- A→B (c=0)
              , PLine (tp (0, 1, 0)) (tp (0, 0, 1)) edgeStyle    -- B→C (a=0)
              , PLine (tp (0, 0, 1)) (tp (1, 0, 0)) edgeStyle ]  -- C→A (b=0)
      -- 3 方向の格子線 (内部 tick fraction のみ)。 a=const は a=0 辺に平行、 以下同様。
      inner = [ t | t <- lpZTicks layout, t > 1e-9, t < 1 - 1e-9 ]
      gridA = [ PLine (tp (t, 1 - t, 0)) (tp (t, 0, 1 - t)) gridStyle | t <- inner ]
      gridB = [ PLine (tp (1 - t, t, 0)) (tp (0, t, 1 - t)) gridStyle | t <- inner ]
      gridC = [ PLine (tp (1 - t, 0, t)) (tp (0, 1 - t, t)) gridStyle | t <- inner ]
      -- 中心から外向きへ d px 押し出す (頂点タイトル用。 頂点は 1 点なので放射方向で正しい)。
      outward (px, py) d =
        let dx = px - ctrX; dy = py - ctrY; m = sqrt (dx * dx + dy * dy)
        in if m < 1e-9 then (px, py) else (px + dx / m * d, py + dy / m * d)
      -- ★ Phase 69 A2: 辺の外向き法線 (辺 P→Q に垂直・重心から外向き)。 全 tick を
      --   同一方向へ一定 px 押すことで、 辺上のラベルが辺に平行に整列する。
      --   旧実装 (重心放射 'outward') は辺中央の tick ほど押し出しが垂直になり、
      --   底辺の 0.4/0.6 が 0.2/0.8 より下にずれていた (Phase 69 起票の user 指摘)。
      edgeNormal (x1, y1) (x2, y2) =
        let ex = x2 - x1; ey = y2 - y1
            (nx, ny) = (-ey, ex)                 -- 辺に垂直
            mx = (x1 + x2) / 2; my = (y1 + y2) / 2
            s  = if nx * (mx - ctrX) + ny * (my - ctrY) < 0 then -1 else 1  -- 外向きへ符号
            m  = sqrt (nx * nx + ny * ny)
        in if m < 1e-9 then (0, 0) else (s * nx / m, s * ny / m)
      vA = ternaryPoint layout (1, 0, 0)
      vB = ternaryPoint layout (0, 1, 0)
      vC = ternaryPoint layout (0, 0, 1)
      -- 三辺の数値 tick ラベル。 a 列の左辺 A-B / b 列の下辺 B-C / c 列の右辺 C-A。
      tsTick = mkFontTS (Just spec) pal TickF AnchorMiddle 0
      -- 端点 (0/1 = 三角形の頂点) は 2 軸の tick が重なる上に頂点タイトルと被るので
      --   除き、 内部 tick (0.2..0.8) のみラベルする。
      edgeLabels = inner
      mkLabelOn (nx, ny) abc t =
        let (px, py) = ternaryPoint layout abc
        in PText (Point (px + nx * 12) (py + ny * 12 + 3)) (numToText t) tsTick
      tickA = [ mkLabelOn (edgeNormal vA vB) (t, 1 - t, 0) t | t <- edgeLabels ]
      tickB = [ mkLabelOn (edgeNormal vB vC) (0, t, 1 - t) t | t <- edgeLabels ]
      tickC = [ mkLabelOn (edgeNormal vC vA) (1 - t, 0, t) t | t <- edgeLabels ]
      -- 3 頂点の軸タイトル (vsXLabel/vsYLabel/vsZLabel、 無ければ encX/encY/encZ 列名)。
      tsTitle = mkFontTS (Just spec) pal AxisLabelF AnchorMiddle 0
      firstLay = case vsLayers spec of (l0 : _) -> Just l0; [] -> Nothing
      -- ★ Phase 64 A13: 無名 inline 列の sentinel ("<inline-num>"/"<inline-txt>") は
      --   頂点タイトルに出さず Nothing に潰す (= 他の軸タイトル経路 Layout.hs/Special.hs/
      --   Layer.hs と同じ規律。 A12 で潰し漏れていた)。 ラベルは vsX/Y/ZLabel か名前付き列で。
      titleFor lbl enc = case getLast lbl of
        Just t  -> Just t
        Nothing -> case fmap colRefName (firstLay >>= getLast . enc) of
          Just nm | nm /= "<inline-num>" && nm /= "<inline-txt>" -> Just nm
          _                                                      -> Nothing
      vertexTitle abc mtxt = case mtxt of
        Nothing  -> []
        Just txt -> let (lx, ly) = outward (ternaryPoint layout abc) 22
                    in [ PText (Point lx (ly + 3)) txt tsTitle ]
      titles = vertexTitle (1, 0, 0) (titleFor (vsXLabel spec) lyEncX)
            <> vertexTitle (0, 1, 0) (titleFor (vsYLabel spec) lyEncY)
            <> vertexTitle (0, 0, 1) (titleFor (vsZLabel spec) lyEncZ)
  in if tpShowGrid pal
       then gridA <> gridB <> gridC <> edges <> tickA <> tickB <> tickC <> titles
       else edges <> tickA <> tickB <> tickC <> titles

-- ★ Phase 64 A2: wedgeSegments (Phase 11 A7-c の扇形 path) は投影層 (Layout.hs) へ
--   移設 (projectBar が共有するため)。

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just v) = v

-- ---------------------------------------------------------------------------
-- 軸 / tick
-- ---------------------------------------------------------------------------

-- | [日本語]: plot 全面背景。 tpShowBackground が False なら塗らない (= 透過。
--   SVG/PDF は背景 rect 自体が消えて自然に透過、 raster は backend が init 色を切り替える)。
--   [English]: The plot's full background. When tpShowBackground is False,
--   nothing is painted (transparent): for SVG/PDF the background rect simply
--   disappears, naturally leaving it transparent; for raster output the
--   backend switches its init color instead.
background :: Layout -> ThemePalette -> [Primitive]
background layout pal
  | not (tpShowBackground pal) = []
  | otherwise =
      let ViewportSize w h = lpViewport layout
      in [ PRect (Rect 0 0 (fromIntegral w) (fromIntegral h))
                 (FillStyle (tpBackground pal) 1.0)
                 Nothing ]

-- | [日本語]: panel (plotArea) 背景の塗り経路。 theme_grey / ブランドは灰/暗の
--   panel 矩形を塗り、 その上に白/淡色 grid を重ねる (ggplot theme_grey 構造)。
--   tpShowPanel が False の preset では何も描かない (= 従来の白背景挙動を温存)。
--   [English]: The paint path for the panel (plot area) background.
--   theme_grey and brand themes paint a grey/dark panel rectangle and layer
--   white/pale grid lines on top of it (the ggplot theme_grey structure).
--   Presets with tpShowPanel False draw nothing, preserving the legacy white
--   background behavior.
panelBackground :: Layout -> ThemePalette -> [Primitive]
panelBackground layout pal
  | tpShowPanel pal = [ PRect (lpPlotArea layout) (FillStyle (tpPanelBg pal) 1.0) Nothing ]
  | otherwise       = []

-- | [日本語]: axisFrame: panel の 4 辺枠。 tpShowBorder が False の theme (grey / ブランド) では
--   枠を描かない (= ggplot theme_grey は border なし)。
--   axisLine (下辺=x軸 + 左辺=y軸 の 2 本) は theme_classic 用に tpShowAxisLine で出す。
--   border と axisLine は排他ではないが、 classic は border なし + axisLine ありの組合せ。
--   [English]: axisFrame: the panel's 4-sided border. Themes with
--   tpShowBorder False (grey / brand) draw no border (ggplot theme_grey has
--   none). The axisLine (2 lines: bottom = x axis, left = y axis) is emitted
--   via tpShowAxisLine for theme_classic. border and axisLine are not
--   mutually exclusive, but classic uses the combination of no border plus an
--   axisLine.
-- ★ Phase 68: axisW = axis.line / panel.border の実効線幅 (呼び元が
--   'effectiveAxisLineWidth' で解決。 theme override 非対応の呼び元 (MCMC) は現状値 1.0)。
axisFrame :: Double -> Layout -> ThemePalette -> [Primitive]
axisFrame axisW layout pal = border ++ axisLine
  where
    a = lpPlotArea layout
    border | tpShowBorder pal =
               [ PRect a (FillStyle (tpBackground pal) 0) (Just (StrokeStyle (tpAxis pal) axisW)) ]
           | otherwise = []
    axisLine | tpShowAxisLine pal =
                 [ PLine (Point (rX a) (rY a + rH a)) (Point (rX a + rW a) (rY a + rH a)) (solid (tpAxis pal) axisW)
                 , PLine (Point (rX a) (rY a)) (Point (rX a) (rY a + rH a)) (solid (tpAxis pal) axisW) ]
             | otherwise = []

-- | [日本語]: TODO-3 (2026-05-29): axRotate / axShowTicks 対応 (= PS Render.tickMarksWithShow port)。
--   TODO-10 (2026-05-29): mSpec を thread して tick font (= spec.tickFont) を反映。
--   rotX/rotY は度数 (0 = 水平、 90 = 縦)。 showX/showY が False の軸は tick line + label を省略。
--   [English]: TODO-3 (2026-05-29): supports axRotate / axShowTicks (ported
--   from PS Render.tickMarksWithShow). TODO-10 (2026-05-29): threads mSpec
--   through to apply the tick font (spec.tickFont). rotX/rotY are in degrees
--   (0 = horizontal, 90 = vertical). Axes with showX/showY False omit both
--   the tick line and the label.
tickMarks :: Maybe VisualSpec -> Layout -> ThemePalette
          -> Maybe AxisFormat -> Maybe AxisFormat
          -> Double -> Double -> Bool -> Bool -> [Primitive]
tickMarks mSpec layout pal fmtX fmtY rotX rotY showX showY =
  let a  = lpPlotArea layout
      sx = lpXScale layout
      sy = lpYScale layout
      ts = mkFontTS mSpec pal TickF AnchorMiddle 0
      tsY = ts { tsAnchor = AnchorEnd }
      -- ★ 回転 x ラベルの anchor は回転符号で決める (pivot = 軸直下)。 rotX は **CCW 正 canonical**。
      --   rotX>0 (CCW・下→上読み) = AnchorEnd で軸下へ垂れる (y 軸タイトルと同じ向き)。
      --   rotX<0 (CCW 負 = CW・上→下読み) = AnchorStart。 (Phase 50 A1: CCW 化で符号条件を反転)
      tsXrot = ts { tsAnchor = if rotX < 0 then AnchorStart else AnchorEnd
                  , tsRotate = rotX }
      tsYrot = tsY { tsRotate = rotY }
      -- ★ Phase 8 A2 Step1: tick 線長 / ラベル位置を computeLayout と同じ sc・定数で
      --   算出 (design §D-3)。 tickSize は実フォント値、 gap は sc 倍してマージン予約に整合。
      sc       = lpMarginScale layout
      tickSize = tsSize ts
      -- ★ Phase 63 A4: tick 長・向きは theme 実効値 (Layout の margin 予約と単一情報源)。
      --   outLen = panel 外向き分 / inLen = panel 内向き分。 ラベル offset (tkGap) は
      --   外向き分にのみ追従 (TickIn はラベルが軸に寄る)。 既定 (TickOut・ggTickLen)
      --   では従来式 tkLen = ggTickLen*sc / tkGap = (ggTickLen+ggAxTextMar)*sc と同値。
      tkLen    = maybe ggTickLen effectiveTickLength mSpec * sc
      tickDir  = maybe TickOut effectiveTickDir mSpec
      outLen   = case tickDir of TickIn  -> 0; _ -> tkLen
      inLen    = case tickDir of TickOut -> 0; _ -> tkLen
      -- ★ Phase 63 A13: axis.text margin も base 派生の実効値 (spec 不在時は従来定数)。
      tkGap    = outLen + maybe ggAxTextMar effectiveAxTextMar mSpec * sc
      -- Phase 32 (re-apply): 目盛線 (tick mark) は tpTickLineColor (ggplot=grey20)。
      --   軸線/枠 (axisFrame) は tpAxis のままで別物。
      tickStyle = solid (tpTickLineColor pal) 1.0
      -- ★ Phase 63 A19: axis.text (目盛ラベル文字) の表示。 Layout の margin 予約
      --   (effectiveShowAxisText) と単一情報源。 False は文字のみ落とし tick 線は
      --   長さ (effectiveTickLength) と独立に残す。 mSpec 無し経路は従来どおり表示。
      showText = maybe True effectiveShowAxisText mSpec
      textPrims ps = if showText then ps else []
      xCats = lpXCategoryLabels layout
      yCats = lpYCategoryLabels layout
      -- ★ Phase 11 A4-d: 明示ラベル override。 lpXTicks と 1:1 対応 (computeLayout で censor
      --   済) なので tick 値で lookup (exact Eq、 値は lpXTicks 由来で同一)。
      xLabsOv = lpXTickLabels layout
      yLabsOv = lpYTickLabels layout
      -- ★ Phase 34: AxisFormat 未指定の数値軸は break ベクトル全体を ggplot/base-R
      --   format() 準拠で整形 (formatTicksGG)。 単値の formatTick (numToText) では
      --   小数桁が揃わず "0.5"/"0.50" 不整合・指数選択も無かった。
      xDefMap = zip (lpXTicks layout) (formatTicksGG (lpXTicks layout))
      yDefMap = zip (lpYTicks layout) (formatTicksGG (lpYTicks layout))
      defLabelX v = case fmtX of
        Nothing -> maybe (formatTick fmtX v) id (lookup v xDefMap)
        Just _  -> formatTick fmtX v
      defLabelY v = case fmtY of
        Nothing -> maybe (formatTick fmtY v) id (lookup v yDefMap)
        Just _  -> formatTick fmtY v
      -- categorical なら integer position v を category label にマップ。 override 優先。
      xLabel v
        | not (null xLabsOv) = case lookup v (zip (lpXTicks layout) xLabsOv) of
            Just l  -> l
            Nothing -> defLabelX v
        | null xCats = defLabelX v
        | otherwise  = case lookup (round v :: Int) (zip [0..] xCats) of
            Just l  -> l
            Nothing -> defLabelX v
      yLabel v
        | not (null yLabsOv) = case lookup v (zip (lpYTicks layout) yLabsOv) of
            Just l  -> l
            Nothing -> defLabelY v
        | null yCats = defLabelY v
        | otherwise  = case lookup (round v :: Int) (zip [0..] yCats) of
            Just l  -> l
            Nothing -> defLabelY v
      xMark v =
        let px = scaleApply sx v
            yb = rY a + rH a
        in [ PLine (Point px (yb - inLen)) (Point px (yb + outLen)) tickStyle ]
           -- Phase 8 C (small-viewport text fix): フォント由来オフセット (tickSize*k) は
           -- 等倍 (tkGap = sc*間隔 のみ scale)。 旧 *sc で小パネル時に数値が軸に被っていた。
           <> textPrims
           [ if rotX == 0
               then PText (Point px (yb + tkGap + tickSize * 0.8)) (xLabel v) ts
               else PText (Point px (yb + tkGap + tickSize * 0.4)) (xLabel v) tsXrot
           ]
      yMark v =
        let py = scaleApply sy v
            xl = rX a
        in [ PLine (Point (xl + inLen) py) (Point (xl - outLen) py) tickStyle ]
           <> textPrims [ PText (Point (xl - tkGap) (py + tickSize * 0.35)) (yLabel v) tsYrot ]
      -- Phase 9 C flip: データ x 軸を左辺に (= yMark 風)、 データ y 軸を下辺に (= xMark 風)。
      --   ラベルは水平のまま (anchor のみ placement に対応)。 sxF=データ x→縦 px、 syF=データ y→横 px。
      coord = maybe CoordCartesian coordOf mSpec
      sxF = lpXScaleFlipped layout
      syF = lpYScaleFlipped layout
      xMarkFlip v =
        let py = scaleApply sxF v
            xl = rX a
        in [ PLine (Point (xl + inLen) py) (Point (xl - outLen) py) tickStyle ]
           <> textPrims [ PText (Point (xl - tkGap) (py + tickSize * 0.35)) (xLabel v) tsY ]
      yMarkFlip v =
        let px = scaleApply syF v
            yb = rY a + rH a
        in [ PLine (Point px (yb - inLen)) (Point px (yb + outLen)) tickStyle ]
           <> textPrims [ PText (Point px (yb + tkGap + tickSize * 0.8)) (yLabel v) ts ]
      (xMarkF, yMarkF) = case coord of
        CoordFlip -> (xMarkFlip, yMarkFlip)
        _         -> (xMark, yMark)
      -- Phase 11 A7-c: 極座標の tick は polarGrid が描く (= 直交辺の tick は出さない)。
      xPrims = if showX && not (isPolar coord) then concatMap xMarkF (lpXTicks layout) else []
      yPrims = if showY && not (isPolar coord) then concatMap yMarkF (lpYTicks layout) else []
  in xPrims <> yPrims

-- | [日本語]: AxisFormat に応じて Double を表示文字列に。 Nothing = auto。
--   'AxisTimeFmt' は Double を unix epoch (= seconds since 1970 UTC) と
--   解釈し、 Data.Time.formatTime で format 文字列を適用。
--   [English]: Formats a Double as a display string according to the
--   'AxisFormat'. 'Nothing' means auto. 'AxisTimeFmt' interprets the Double
--   as a Unix epoch (seconds since 1970 UTC) and applies the format string
--   via Data.Time.formatTime.
formatTick :: Maybe AxisFormat -> Double -> Text
formatTick fmt v = case fmt of
  Nothing                  -> numToText v
  Just AxisIntegerFmt      -> T.pack (show (round v :: Int))
  Just (AxisDecimalFmt n)  -> T.pack (showFFloat (Just n) v "")
  Just (AxisExponentFmt n) -> T.pack (showEFloat (Just n) v "")
  Just (AxisTimeFmt pat)   ->
    let utc = posixSecondsToUTCTime (realToFrac v)
        defLocale = Data.Time.Format.defaultTimeLocale
    in T.pack (Data.Time.Format.formatTime defLocale (T.unpack pat) utc)

labels :: Layout -> VisualSpec -> ThemePalette -> [Primitive]
labels layout spec pal =
  let a = lpPlotArea layout
      tsTitle  = mkFontTS (Just spec) pal TitleF     AnchorMiddle 0
      tsLabel  = mkFontTS (Just spec) pal AxisLabelF AnchorMiddle 0
      tsLabelV = tsLabel { tsRotate = 90 }   -- Phase 50 A1: CCW 正 canonical (y 軸タイトル = CCW 90 = 下→上)
      cx = rX a + rW a / 2
      cy = rY a + rH a / 2
      -- ★ Phase 8 A2 Step1 (design §D-3): title/軸タイトルを viewport 端基準で配置。
      --   title は plot.margin(上) + ascent。 旧実装は plotArea 相対 (rY a -12) で、
      --   タイトルフォントを大きくすると上にはみ出していた (= A2 本丸)。 viewport 上端基準
      --   なので marginal-X 帯にも被らず B18 特例も不要に。
      sc        = lpMarginScale layout
      titleSize = tsSize tsTitle
      labelSize = tsSize tsLabel
      -- plotArea を基準にマージン箱の各辺を求める (subplots は viewport=0 でも panel 端で
      -- 配置できるよう vpH/vpW ではなく plotArea ± lpMargin* を使う)。
      boxTop    = rY a - lpMarginTop layout
      boxBottom = rY a + rH a + lpMarginBottom layout
      boxLeft   = rX a - lpMarginLeft layout
      -- ★ Phase 63 A5: 外周余白は theme 実効値 (Layout の margin 予約と単一情報源)。
      --   既定 (各辺 half_line) では従来式と同値。
      -- ★ Phase 63 A13: title→subtitle gap も half_line = base/2 派生へ。
      pm = effectivePlotMargin spec
      hasTitle = case getLast (vsTitle spec) of Just _ -> True; _ -> False
      titleBaseY = boxTop + sc * (marTop pm + titleSize * 0.8)
      -- Phase 32 (re-apply): plot.title の水平揃え。 hjust=0 (ggplot theme_grey) は
      --   panel 左端にアンカー開始、 それ以外 (既定 0.5) は従来通り中央。
      (titleX, tsTitle') = if tpTitleHjust pal <= 0.0
                             then (rX a, tsTitle { tsAnchor = AnchorStart })
                             else (cx,   tsTitle)
      titleP = case getLast (vsTitle spec) of
        Just t  -> [ PText (Point titleX titleBaseY) t tsTitle' ]
        Nothing -> []
      -- ★ Phase 63 A15: 軸タイトルは「軸 text 直下 + axis.title margin」 基準 (= ggplot
      --   方式)。 offset は Layout の margin 予約と同じ stack (lpXTitleOff/lpYTitleOff =
      --   単一情報源)。 旧 boxBottom/boxLeft 最外端 pin は LegendBottom/caption 時に
      --   タイトルが凡例の外側 (最下端) へ出ていた (J2/J5 root)。
      -- ★ Phase 63 A19: axis.title の表示 (ThemeVoid 既定 False = element_blank)。
      --   Layout の margin 予約 (hasXLabel/hasYLabel gating) と単一情報源。
      showAxTitle = effectiveShowAxisTitle spec
      -- ★ Phase 64 A12: ternary は 3 頂点の軸タイトルを ternaryGrid が描くので、
      --   直交 (bottom/left) の x/y タイトルは抑止して二重描画を防ぐ。
      isTern = isTernary (coordOf spec)
      -- x 軸タイトル: baseline = panel 下端 + offset + ascent。
      xLP = case getLast (vsXLabel spec) of
        Just t | showAxTitle && not isTern -> [ PText (Point cx (rY a + rH a + lpXTitleOff layout + labelSize * 0.8)) t tsLabel ]
        _ -> []
      -- y 軸タイトル (rot 90 CCW = ascent が -x 側): baseline = panel 左端 - offset - descent。
      yLP = case getLast (vsYLabel spec) of
        Just t | showAxTitle && not isTern -> [ PText (Point (rX a - lpYTitleOff layout - labelSize * 0.2) cy) t tsLabelV ]
        _ -> []
      -- ★ Phase 11 A5-a: subtitle (title 直下、 小フォント) / caption (図右下・
      --   小フォント・右寄せ) / tag (左上隅・やや大・左寄せ太字)。 Layout の margin 予約
      --   ('hasSubtitle'/'hasCaption'/'hasTag') と座標を揃える。
      --   ★ subtitle の水平揃えは plot.title と同じ ('tpTitleHjust'): theme_grey は左寄せ。
      --   ★ Phase 63 A14: 固定 11/9/13 を base 派生 (×1 / ×0.8 / ×1.2 = ggplot
      --   theme_grey 倍率) へ。 Layout の labs 予約と単一情報源。
      subSize  = effectiveSubtitleSize spec
      capSize  = effectiveCaptionSize spec
      tagSize  = effectiveTagSize spec
      tsSub = (mkFontTS (Just spec) pal AxisLabelF AnchorMiddle 0) { tsSize = subSize }
      tsCap = (mkFontTS (Just spec) pal AxisLabelF AnchorEnd    0) { tsSize = capSize }
      tsTag = (mkFontTS (Just spec) pal TitleF     AnchorStart  0) { tsSize = tagSize, tsWeight = "bold" }
      boxRight = rX a + rW a
      -- subtitle baseline: title があればその下、 無ければ title 位置に置く。
      subBaseY = (if hasTitle then titleBaseY + sc * effectiveHalfLine spec + subSize * 0.8
                              else boxTop + sc * (marTop pm + subSize * 0.8))
      -- plot.title と同じ hjust 規則: hjust=0 (theme_grey) は panel 左端アンカー開始。
      (subX, tsSub') = if tpTitleHjust pal <= 0.0
                         then (rX a, tsSub { tsAnchor = AnchorStart })
                         else (cx,   tsSub)
      subP = case getLast (vsSubtitle spec) of
        Just t  -> [ PText (Point subX subBaseY) t tsSub' ]
        Nothing -> []
      capP = case getLast (vsCaption spec) of
        Just t  -> [ PText (Point boxRight (boxBottom - sc * marBottom pm)) t tsCap ]
        Nothing -> []
      tagP = case getLast (vsTag spec) of
        Just t  -> [ PText (Point boxLeft (boxTop + sc * marTop pm + tagSize * 0.8)) t tsTag ]
        Nothing -> []
  in titleP <> subP <> xLP <> yLP <> capP <> tagP

-- ★ Phase 38: numToText は Layout へ集約 (Layout import 経由で使用)。

-- | [日本語]: lpYScaleRight が Just のとき plotArea 右端に Y 軸線 + tick を描画
--   (= PS renderRightYAxis と同方式)。 Nothing なら何も描かない。
--   [English]: When lpYScaleRight is Just, draws a Y axis line plus ticks at
--   the right edge of the plot area (same approach as PS renderRightYAxis).
--   Draws nothing when Nothing.
-- ★ Phase 68: axisW = 軸線 (右 Y 軸) の実効線幅 (呼び元が 'effectiveAxisLineWidth'
--   で解決)。 tick mark は scope 外 = 主軸 tick と同じ 1.0 固定 (axis.line と axis.ticks
--   を分離して統一挙動にする)。
renderRightYAxis :: Double -> Layout -> ThemePalette -> Maybe AxisFormat -> [Primitive]
renderRightYAxis axisW layout pal fmtYR = case lpYScaleRight layout of
  Nothing -> []
  Just sR ->
    let a   = lpPlotArea layout
        xR  = rX a + rW a
        ts  = mkFontTS Nothing pal TickF AnchorStart 0
        axisStyle = solid (tpAxis pal) axisW   -- 軸線 = toAxisLineWidth
        tickStyle = solid (tpAxis pal) 1.0     -- tick mark = scope 外 (1.0 固定)
        axisLine  = [ PLine (Point xR (rY a)) (Point xR (rY a + rH a)) axisStyle ]
        tickPrim v =
          let py = scaleApply sR v
          in [ PLine (Point xR py) (Point (xR + 5) py) tickStyle
             , PText (Point (xR + 8) (py + 4)) (formatTick fmtYR v) ts ]
    in axisLine <> concatMap tickPrim (lpYTicksRight layout)

-- | [日本語]: データ空間 (dx, dy) を coord に従い px の 'Point' に写す薄いラッパ。
--   projectXY は生 tuple を返す (Layout は Render の Point に依存できない) ので、
--   mark renderer 側はこのラッパで Point に包む。 coord = lpCoord layout を渡す前提で、
--   Cartesian では `Point (scaleApply (lpXScale l) dx) (scaleApply (lpYScale l) dy)` と
--   bit 一致する (= 従来の `Point (sx x) (sy y)` と同値 → ゼロ diff)。
--   [English]: A thin wrapper that maps data space (dx, dy) to a pixel-space
--   'Point' according to the coordinate system. projectXY returns a raw
--   tuple (Layout cannot depend on Render's Point), so mark renderers wrap it
--   into a Point via this helper. Given coord = lpCoord layout, the Cartesian
--   case is bit-identical to
--   `Point (scaleApply (lpXScale l) dx) (scaleApply (lpYScale l) dy)` (the
--   same as the previous `Point (sx x) (sy y)`, so it produces zero diff).
projectPoint :: Coord -> Layout -> Double -> Double -> Point
projectPoint c l dx dy = let (px, py) = projectXY c l dx dy in Point px py

-- | [日本語]: ★ Phase 64 A13: 三角座標 (ternary) の geom 前処理。 各行の (x,y) を
--   encZ 列と合わせて 'normalizeTernary' で成分和 1 の fraction (a',b') へ写す。
--   __第 3 成分 c は @projectXY CoordTernary@ が @c = 1-a'-b'@ で補完する__ので、
--   正規化後の (a',b') をそのまま既存の 'projectXY' / 'projectPoint' に渡せば
--   'ternaryPoint' と一致する (geom 側の投影呼出は無改造で 3 列を正しく使える)。
--   退化行 (負値 / 合計≤0、 および NaN) は @(NaN, NaN)@ にして既存の NA 除外
--   (点 skip / 線の詰め) に乗せる (= 色/サイズ vector との行整列を保ったまま脱落)。
--   encZ 未指定時は @c = 1-x-y@ で補完 (A12 の 2 引数挙動と互換)。
--   __非 ternary 座標系では (xs, ys) を素通し (byte 不変)__。
--   [English]: Phase 64 A13. Ternary geom preprocessing: normalizes each row's
--   (x, y) together with the encZ column into fractions summing to 1 via
--   'normalizeTernary', returning (a', b'). Since @projectXY CoordTernary@ fills
--   the third component as @c = 1 - a' - b'@, passing the normalized (a', b')
--   straight to the existing 'projectXY' / 'projectPoint' reproduces
--   'ternaryPoint' — so a geom's projection call sites need no change to use the
--   three columns correctly. Degenerate rows (negative component / non-positive
--   sum / NaN) become @(NaN, NaN)@ so the existing NA handling (point skip /
--   line contraction) drops them while preserving row alignment with the
--   color/size vectors. A missing encZ falls back to @c = 1 - x - y@ (compatible
--   with A12's two-argument behavior). For non-ternary coords, passes (xs, ys)
--   through unchanged (byte-identical).
ternaryRemap :: Resolver -> Layout -> Layer
             -> V.Vector Double -> V.Vector Double
             -> (V.Vector Double, V.Vector Double)
ternaryRemap r layout ly xs ys
  | not (isTernary (lpCoord layout)) = (xs, ys)
  | otherwise =
      let zs = vecOrFull (lyEncZ ly) r
          n  = min (V.length xs) (V.length ys)
          nan = 0 / 0 :: Double
          remap i =
            let x = xs V.! i
                y = ys V.! i
                z = if i < V.length zs then zs V.! i else 1 - x - y
            in case normalizeTernary (x, y, z) of
                 Just (a, b, _) -> (a, b)
                 Nothing        -> (nan, nan)
          pairs = V.generate n remap
      in (V.map fst pairs, V.map snd pairs)

-- | [日本語]: 極座標を解さない standalone renderer (ess/autocorr/forest/funnel/
--   box/violin/strip/swarm/waterfall = 直交/flip 専用 2-way 分岐) 用に coord を
--   {Cartesian, Flip} に正規化する (= polar はそれらの mark では Cartesian 扱い)。
--   polar は座標系として点/線/扇形 bar に意味があり、 これらの統計 mark には適用しない。
--   [English]: Normalizes coord to {Cartesian, Flip} for standalone renderers
--   that don't understand polar coordinates (ess/autocorr/forest/funnel/
--   box/violin/strip/swarm/waterfall, which only branch two ways between
--   Cartesian and Flip); polar is treated as Cartesian for these marks. The
--   polar coordinate system is meaningful for points/lines/sector bars, but
--   is not applicable to these statistical marks.
flipOnly :: Coord -> Coord
flipOnly CoordFlip = CoordFlip
flipOnly _         = CoordCartesian

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

median :: [Double] -> Double
median [] = 0
median xs =
  let s = sort xs
      n = length xs
  in if odd n then s !! (n `div` 2)
     else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2

-- | [日本語]: categorical 列を群キー列 [Text] に解決 (linetypeBy 用)。
--   [English]: Resolves a categorical column into a list of group keys
--   [Text] (used by linetypeBy).
groupKeysOf :: Resolver -> ColRef -> Maybe [Text]
groupKeysOf r cr = case resolveCol r cr of
  Just (TxtData v) -> Just (V.toList v)
  Just (NumData v) -> Just (map (T.pack . show) (V.toList v))
  _                -> Nothing

-- | [日本語]: キー列と値列を zip し、 キー初出順を保ったまま群ごとにまとめる (= group split)。
--   [English]: Zips a key column with a value column and groups values by
--   key, preserving the key's first-occurrence order (a group split).
orderedGroups :: Eq a => [a] -> [b] -> [(a, [b])]
orderedGroups keys vals =
  let paired = zip keys vals
  in [ (k, [ v | (k', v) <- paired, k' == k ]) | k <- nub keys ]

-- | [日本語]: scatter 上に「点を結ぶ線」 を生成。 group 列があれば
--   group 内のみで連結、 order 列があればソート後に連結。
--   [English]: Generates "lines connecting the points" on top of a scatter.
--   When a group column is present, points are connected only within their
--   group; when an order column is present, points are connected after
--   sorting by it.
renderConnect :: Resolver -> Layout -> ThemePalette -> Layer -> ConnectSpec
              -> V.Vector Double -> V.Vector Double -> Int -> [Primitive]
renderConnect r layout pal ly cs xs ys n =
  let color = case getLast (csColor cs) of
        Just c  -> c
        Nothing -> staticColorOr ly (tpDefault pal)
      width = case getLast (csWidth cs) of
        Just w  -> w
        Nothing -> 1.5
      ls = solid color width
      coord = lpCoord layout
      pp = projectPoint coord layout
      -- order: 指定があれば 数値順 sort、 無ければ index 順
      orderKey i = case getLast (csOrder cs) >>= resolveNum r of
        Just v  -> v V.!? i
        Nothing -> Just (fromIntegral i)
      -- group: 文字列列で partition、 無ければ全点 1 group
      groupKey i = case getLast (csGroup cs) >>= resolveTxt r of
        Just v  -> v V.!? i
        Nothing -> Just (T.pack "")
        where
          resolveTxt res cr = case resolveCol res cr of
            Just (TxtData v) -> Just v
            _                -> Nothing
      idxs = [0 .. n - 1]
      groupedSorted =
        let withKey = [(g, o, i) | i <- idxs
                                 , Just g <- [groupKey i]
                                 , Just o <- [orderKey i]]
            byGroup = groupBy (\(g1,_,_) (g2,_,_) -> g1 == g2)
                              (sortOn (\(g,_,_) -> g) withKey)
        in [ map (\(_,_,i) -> i) (sortOn (\(_,o,_) -> o) grp)
           | grp <- byGroup ]
      segsForGroup is =
        [ PLine (pp (xs V.! a) (ys V.! a))
                (pp (xs V.! b) (ys V.! b)) ls
        | (a, b) <- zip is (drop 1 is) ]
  in concatMap segsForGroup groupedSorted

-- | [日本語]: plot area 内に参照線 1 本を描画。
--   domain (= scale の dLo/dHi) を直接見て 2 端点を計算。
--   [English]: Draws a single reference line inside the plot area. The two
--   endpoints are computed by reading the domain (the scale's dLo/dHi)
--   directly.
-- | [日本語]: ★ 参照線も 'resolvePosX'/'resolvePosY' (UCtx) 経由に統一。
--   値は PNative、panel 端は PNpc 0/1 で表す (出力は旧実装と bit 一致)。dpi は
--   PAbs Px 用 (参照線は使わないが UCtx 一貫のため受ける)。
--   [English]: Reference lines are also unified to go through
--   'resolvePosX'/'resolvePosY' (UCtx). Values use PNative and panel edges
--   use PNpc 0/1 (the output is bit-identical to the previous
--   implementation). dpi is for PAbs Px (reference lines don't use it, but it
--   is accepted for consistency with UCtx).
renderRefLine :: Double -> Layout -> ThemePalette -> ReferenceLine -> [Primitive]
renderRefLine dpi layout pal rl =
  let uc = UCtx dpi (lpPlotArea layout) (lpXScale layout) (lpYScale layout)
      rxp = resolvePosX uc
      ryp = resolvePosY uc
      ls = solid (tpAxis pal) 1.5
      (xLo, xHi) = case lpXScale layout of
        LinearScale lo hi _ _ -> (lo, hi)
        LogScale    lo hi _ _ -> (lo, hi)
      (yLo, yHi) = case lpYScale layout of
        LinearScale lo hi _ _ -> (lo, hi)
        LogScale    lo hi _ _ -> (lo, hi)
  in case rl of
       RefIdentity ->
         -- y = x ─ 交差範囲 [max xLo yLo, min xHi yHi] の対角線
         let lo = max xLo yLo; hi = min xHi yHi
         in [ PLine (Point (rxp (PNative lo)) (ryp (PNative lo)))
                    (Point (rxp (PNative hi)) (ryp (PNative hi))) ls ]
       RefHorizontalAt y ->
         [ PLine (Point (rxp (PNpc 0)) (ryp (PNative y)))
                 (Point (rxp (PNpc 1)) (ryp (PNative y))) ls ]
       RefVerticalAt x ->
         [ PLine (Point (rxp (PNative x)) (ryp (PNpc 1)))
                 (Point (rxp (PNative x)) (ryp (PNpc 0))) ls ]
       RefLinear sl ic ->
         [ PLine (Point (rxp (PNative xLo)) (ryp (PNative (sl * xLo + ic))))
                 (Point (rxp (PNative xHi)) (ryp (PNative (sl * xHi + ic)))) ls ]

-- ---------------------------------------------------------------------------
-- 共通 helper
-- ---------------------------------------------------------------------------

-- | [日本語]: 列を数値 Vector に解決。 __NA (NaN) を落とす__ (nullable 列対応・ggplot na.rm
--   相当)。 単一列 geom (histogram/freqpoly/density/box/ecdf 等) はこれで欠損を内部処理。
--   非 NULL 列 (NaN を含まない) には no-op なので従来挙動と同一。
--   [English]: Resolves a column to a numeric Vector, __dropping NA (NaN)__
--   (supports nullable columns; equivalent to ggplot's na.rm). Single-column
--   geoms (histogram/freqpoly/density/box/ecdf, etc.) handle missing values
--   internally this way. It is a no-op on non-null columns (containing no
--   NaN), so behavior is unchanged from before.
vecOr :: Last ColRef -> Resolver -> V.Vector Double
vecOr lc = V.filter (not . isNaN) . vecOrFull lc

-- | [日本語]: 'vecOr' の NaN 保持版 (= 長さを保つ)。
--   __多列 geom (scatter/line) が行整列を保って欠損対を落とす__ために使う
--   (per-column drop だと x/y がズレるため)。
--   [English]: The NaN-preserving variant of 'vecOr' (keeps the length
--   unchanged). Used so that
--   __multi-column geoms (scatter/line) can drop missing pairs while keeping x/y row-aligned__
--   (dropping per-column would throw x/y out of sync).
vecOrFull :: Last ColRef -> Resolver -> V.Vector Double
vecOrFull lc r = case getLast lc of
  Nothing -> V.empty
  Just cr -> maybe V.empty id (resolveNum r cr)

-- | [日本語]: Okabe-Ito 8 色 categorical palette (= 色覚多様性配慮)。
--   [English]: The 8-color Okabe-Ito categorical palette (chosen for color-
--   vision-deficiency accessibility).
okabeIto :: [Text]
okabeIto =
  [ "#E69F00", "#56B4E9", "#009E73", "#F0E442"
  , "#0072B2", "#D55E00", "#CC79A7", "#000000" ]

-- | [日本語]: layer の color encoding を point 数 n の Vector に展開。
--     * 'ColorStatic'  → 全 point 同色
--     * 'ColorByCol'   → 列 (txt or num) を distinct 値ごとに palette index 割当
--     * encoding 無し  → theme の default 色
--   [English]: Expands a layer's color encoding into a Vector of n colors,
--   one per point.
--     * 'ColorStatic'  gives every point the same color.
--     * 'ColorByCol'   assigns a palette index per distinct value of the
--       column (text or numeric).
--     * No encoding    falls back to the theme's default color.
colorVector :: Resolver -> Layout -> ThemePalette -> Layer -> Int -> V.Vector Text
colorVector r layout pal ly n =
  case getLast (lyColor ly) of
    Just (ColorStatic c) -> V.replicate n c
    Just (ColorByCol cr) ->
      let vals = case resolveCol r cr of
            Just (TxtData v) -> V.toList v
            -- ★Phase 19: show でなく numToText (凡例 allColorCategories / PS と
            -- 同じキー。 show だと "1.0" vs 凡例 "1" で union 注入時に引けない)
            Just (NumData v) -> map numToText (V.toList v)
            Nothing          -> []
          -- TODO-3d (2026-05-29): trellis 色一貫性 (= PS Render.colorVector port)。
          -- lyColorCats が非空ならその順序で index、 空なら filtered resolver の
          -- nub vals で旧挙動。 facet panel ごとに同 cat → 同色 を保つため。
          distinct = let cats = lyColorCats ly
                     in if null cats then orderedCats vals else cats
          palArr = lpCategoricalPalette layout
          -- ★ A4-e: scale_color_manual の辞書を最優先 (= 該当カテゴリ名→指定色)。
          --   未登録名は従来の positional palette にフォールバック。
          manual = lpColorManual layout
          colorOf t = case lookup t manual of
            Just c  -> c
            Nothing -> case elemIndex t distinct of
              Just i  -> palArr !! (i `mod` length palArr)
              Nothing -> tpDefault pal
          mapped   = map colorOf vals
          filled   = mapped <> replicate (max 0 (n - length mapped)) (tpDefault pal)
      in V.fromList (take n filled)
    Just (ColorByContinuous cr) ->
      case resolveNum r cr of
        Nothing -> V.replicate n (tpDefault pal)
        Just v  ->
          let lo = V.minimum v
              hi = V.maximum v
              norm x = if hi <= lo then 0.5 else (x - lo) / (hi - lo)
              -- ★ A4-e: scale_color_gradient2 = midpoint を 0.5 に固定する発散 (diverging) 写像。
              --   lo..mid を [0,0.5]・mid..hi を [0.5,1] に個別正規化し 3-stop palette を補間。
              colorAt x = case lpColorGradient2 layout of
                Just (cLo, cMid, cHi, mid) ->
                  let t | x <= mid  = if mid <= lo then 0 else 0.5 * (x - lo) / (mid - lo)
                        | otherwise = if hi <= mid then 1 else 0.5 + 0.5 * (x - mid) / (hi - mid)
                  in continuousColor [cLo, cMid, cHi] t
                Nothing -> continuousColor (lpContinuousPalette layout) (norm x)
              filled = map (\i -> case v V.!? i of
                              Just x  -> colorAt x
                              Nothing -> tpDefault pal)
                           [0 .. n - 1]
          in V.fromList filled
    Nothing -> V.replicate n (tpDefault pal)

-- | [日本語]: Viridis 風 5-stop gradient (= 簡易版、 perceptually uniform に近い)。
--   t in [0, 1]。
--   [English]: A Viridis-like 5-stop gradient (a simplified version, close to
--   perceptually uniform). t is in [0, 1].
viridis :: Double -> Text
viridis = continuousColor
  ["#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725"]

-- | [日本語]: P17: 任意 hex 配列の N-stop palette を t ∈ [0,1] で線形補間。
--   layout.lpContinuousPalette を渡せば spec 指定の sequential が反映される。
--   [English]: P17: linearly interpolates an arbitrary N-stop hex-color
--   palette at t ∈ [0,1]. Passing layout.lpContinuousPalette applies the
--   spec-specified sequential palette.
continuousColor :: [Text] -> Double -> Text
continuousColor palArr t =
  let n = length palArr
      clamp01 x = max 0 (min 1 x)
      tc = clamp01 t
      parseHex hex = case T.length hex of
        7 -> let r = parseHexByte (T.take 2 (T.drop 1 hex))
                 g = parseHexByte (T.take 2 (T.drop 3 hex))
                 b = parseHexByte (T.take 2 (T.drop 5 hex))
             in (r, g, b)
        _ -> (128, 128, 128)
  in case n of
    0 -> "#777777"
    1 -> head palArr
    _ ->
      let segments = fromIntegral (n - 1) :: Double
          pos = tc * segments
          i = max 0 (min (n - 2) (floor pos))
          ratio = pos - fromIntegral i
          c1 = palArr !! i
          c2 = palArr !! (i + 1)
          (r1, g1, b1) = parseHex c1
          (r2, g2, b2) = parseHex c2
          lerp a b = round (fromIntegral a + (fromIntegral b - fromIntegral a) * ratio :: Double) :: Int
      in rgbToHex (lerp r1 r2) (lerp g1 g2) (lerp b1 b2)

parseHexByte :: Text -> Int
parseHexByte s =
  let go acc c = case c of
        c' | c' >= '0' && c' <= '9' -> acc * 16 + (fromEnum c' - fromEnum '0')
           | c' >= 'a' && c' <= 'f' -> acc * 16 + (fromEnum c' - fromEnum 'a' + 10)
           | c' >= 'A' && c' <= 'F' -> acc * 16 + (fromEnum c' - fromEnum 'A' + 10)
           | otherwise -> acc
  in T.foldl go 0 s

rgbToHex :: Int -> Int -> Int -> Text
rgbToHex r g b = T.pack ("#" <> hex r <> hex g <> hex b)
  where
    hex n = let s = showHex (max 0 (min 255 n)) ""
            in if length s == 1 then '0':s else s
    showHex = showHexBase

showHexBase :: Int -> String -> String
showHexBase 0 acc = if null acc then "0" else acc
showHexBase n acc =
  let (q, rem_) = n `divMod` 16
      ch = "0123456789abcdef" !! rem_
  in showHexBase q (ch : acc)

doubleOr :: Last Double -> Double -> Double
doubleOr l d = case getLast l of Just v -> v; Nothing -> d

staticColorOr :: Layer -> Text -> Text
staticColorOr ly defaultC = case getLast (lyColor ly) of
  Just (ColorStatic c) -> c
  _                    -> defaultC

-- ---------------------------------------------------------------------------
-- TODO-3c (2026-05-29): jitter / shape / sizeBy helpers (= PS Render port)
-- ---------------------------------------------------------------------------

-- | [日本語]: P14 PS port: deterministic pseudo-random ∈ [0,1) from Int seed。
--   sin-hash トリック (= classic JS shadertoy)。 同 seed で常に同値。
--   [English]: P14 PS port: a deterministic pseudo-random value in [0,1) from
--   an Int seed, using the classic sin-hash trick (as seen in JS shadertoy
--   code). The same seed always yields the same value.
hashRand :: Int -> Double
hashRand i =
  let s = sin (fromIntegral i * 12.9898) * 43758.5453
      f = fromIntegral (floor s :: Int)
  in s - f

-- | [日本語]: C-6 PS port: shape を Primitive (PCircle or PPath) に変換。
--   MShCircle は PCircle (= hover label 付き)、 他は PPath。
--   [English]: C-6 PS port: converts a shape into a Primitive (PCircle or
--   PPath). MShCircle becomes a PCircle (with a hover label); every other
--   shape becomes a PPath.
shapeToPrim :: MarkShape -> Point -> Double -> FillStyle -> Maybe StrokeStyle
            -> Maybe Text -> Primitive
shapeToPrim sh pt sz fs ms label = case sh of
  MShCircle -> PCircle pt sz fs ms label
  _         -> PPath (shapePath sh pt sz) fs ms

-- | [日本語]: C-6 PS port: shape 別 path 構築 (= PathSegment 列、 bezier 近似含む)。
--   [English]: C-6 PS port: builds a per-shape path (a list of PathSegment,
--   including Bezier approximations).
shapePath :: MarkShape -> Point -> Double -> [PathSegment]
shapePath sh (Point cx cy) r = case sh of
  MShCircle -> []
  MShSquare ->
    [ MoveTo (Point (cx - r) (cy - r))
    , LineTo (Point (cx + r) (cy - r))
    , LineTo (Point (cx + r) (cy + r))
    , LineTo (Point (cx - r) (cy + r))
    , ClosePath ]
  MShTriangle ->
    [ MoveTo (Point cx (cy - r))
    , LineTo (Point (cx + r) (cy + r))
    , LineTo (Point (cx - r) (cy + r))
    , ClosePath ]
  MShCross ->
    let t = r * 0.4
    in [ MoveTo (Point (cx - t) (cy - r))
       , LineTo (Point (cx + t) (cy - r))
       , LineTo (Point (cx + t) (cy - t))
       , LineTo (Point (cx + r) (cy - t))
       , LineTo (Point (cx + r) (cy + t))
       , LineTo (Point (cx + t) (cy + t))
       , LineTo (Point (cx + t) (cy + r))
       , LineTo (Point (cx - t) (cy + r))
       , LineTo (Point (cx - t) (cy + t))
       , LineTo (Point (cx - r) (cy + t))
       , LineTo (Point (cx - r) (cy - t))
       , LineTo (Point (cx - t) (cy - t))
       , ClosePath ]
  -- トランプのスーツ (独自拡張・ggplot に無い拡張)。 形は
  --   htdebeer/SVG-cards (public domain) の vetted パスを移植し、 全 subpath を
  --   同一巻き方向 (CCW) に正規化 + bbox 正規化 (最大辺 ±1) して r 内に収めた。
  --   nonzero fill で複合パスが union=ベタになる (中央の穴/塗り規則依存を回避)。
  --   座標は /tmp/suit_emit.py で生成 (HS=PS 同一リテラル → byte parity)。
  --   ダイヤは通常の菱形を廃しトランプ型のみ・ハートは上下反転 (ユーザ要望)。
  MShHeart ->   -- ★ ユーザ要望 (2026-06-21): 上下反転 (尖りが上)
    let p dx dy = Point (cx + dx * r) (cy + dy * r)
    in
       [ MoveTo (p (-0.9660) (-0.4940))
       , CurveTo (p (-0.9648) (-0.7734)) (p (-0.7483) (-1.0000)) (p (-0.4814) (-0.9987))
       , CurveTo (p (-0.2159) (-0.9987)) (p (-0.0006) (-0.7722)) (p (-0.0006) (-0.4915))
       , CurveTo (p (-0.0006) (-0.7709)) (p 0.2171 (-0.9962)) (p 0.4840 (-0.9962))
       , CurveTo (p 0.7495 (-0.9962)) (p 0.9660 (-0.7684)) (p 0.9648 (-0.4890))
       , CurveTo (p 0.9597 0.1038) (p 0.2159 0.5318) (p (-0.0044) 1.0000)
       , CurveTo (p (-0.2222) 0.5305) (p (-0.9648) 0.0988) (p (-0.9660) (-0.4940))
       , ClosePath ]
  MShDiamond ->   -- ★ トランプのダイヤ (通常の菱形は廃止、 これが唯一のダイヤ)
    let p dx dy = Point (cx + dx * r) (cy + dy * r)
    in
       [ MoveTo (p 0.0000 1.0000)
       , CurveTo (p (-0.0018) 0.8736) (p (-0.0739) 0.7597) (p (-0.1426) 0.6584)
       , CurveTo (p (-0.2897) 0.4572) (p (-0.4759) 0.2851) (p (-0.6748) 0.1351)
       , CurveTo (p (-0.7634) 0.0762) (p (-0.8571) 0.0049) (p (-0.9673) 0.0000)
       , CurveTo (p (-0.6448) 0.0000) (p (-0.3224) 0.0000) (p 0.0000 0.0000)
       , MoveTo (p (-0.9673) 0.0000)
       , CurveTo (p (-0.8275) (-0.0044)) (p (-0.7081) (-0.0934)) (p (-0.6015) (-0.1754))
       , CurveTo (p (-0.4132) (-0.3335)) (p (-0.2503) (-0.5215)) (p (-0.1127) (-0.7251))
       , CurveTo (p (-0.0622) (-0.8093)) (p (-0.0028) (-0.8986)) (p 0.0000 (-1.0000))
       , CurveTo (p 0.0000 (-0.6666)) (p 0.0000 (-0.3334)) (p 0.0000 0.0000)
       , MoveTo (p 0.0000 (-1.0000))
       , CurveTo (p 0.0048 (-0.8511)) (p 0.0955 (-0.7222)) (p 0.1811 (-0.6071))
       , CurveTo (p 0.3338 (-0.4169)) (p 0.5146 (-0.2486)) (p 0.7136 (-0.1079))
       , CurveTo (p 0.7909 (-0.0597)) (p 0.8730 (-0.0023)) (p 0.9673 0.0000)
       , CurveTo (p 0.6448 0.0000) (p 0.3224 0.0000) (p 0.0000 0.0000)
       , MoveTo (p 0.9673 0.0000)
       , CurveTo (p 0.8275 0.0044) (p 0.7081 0.0933) (p 0.6015 0.1753)
       , CurveTo (p 0.4132 0.3335) (p 0.2503 0.5215) (p 0.1127 0.7251)
       , CurveTo (p 0.0622 0.8093) (p 0.0028 0.8986) (p 0.0000 1.0000)
       , CurveTo (p 0.0000 0.6666) (p 0.0000 0.3334) (p 0.0000 0.0000) ]
  MShSpade ->
    let p dx dy = Point (cx + dx * r) (cy + dy * r)
    in
       [ MoveTo (p 0.8512 0.1009)
       , CurveTo (p 0.8512 0.3077) (p 0.6620 0.4754) (p 0.4262 0.4754)
       , CurveTo (p 0.1904 0.4754) (p 0.0000 0.3077) (p 0.0000 0.1009)
       , CurveTo (p 0.0000 0.3077) (p (-0.1904) 0.4754) (p (-0.4250) 0.4754)
       , CurveTo (p (-0.6608) 0.4754) (p (-0.8512) 0.3077) (p (-0.8512) 0.1009)
       , CurveTo (p (-0.8499) (-0.3354)) (p (-0.1929) (-0.6532)) (p 0.0000 (-1.0000))
       , CurveTo (p 0.1929 (-0.6545)) (p 0.8499 (-0.3367)) (p 0.8512 0.1009)
       , ClosePath
       , MoveTo (p 0.4943 1.0000)
       , LineTo (p (-0.4968) 1.0000)
       , CurveTo (p (-0.0757) 1.0000) (p (-0.0555) 0.1009) (p (-0.0555) 0.1009)
       , LineTo (p 0.0517 0.1021)
       , CurveTo (p 0.0517 0.1021) (p 0.0567 0.3266) (p 0.1148 0.5511)
       , CurveTo (p 0.1728 0.7755) (p 0.2837 1.0000) (p 0.4943 1.0000)
       , ClosePath ]
  MShClub ->
    let p dx dy = Point (cx + dx * r) (cy + dy * r)
    in
       [ MoveTo (p 0.4219 (-0.5869))
       , CurveTo (p 0.4219 (-0.3589)) (p 0.2317 (-0.1738)) (p (-0.0038) (-0.1738))
       , CurveTo (p (-0.2393) (-0.1738)) (p (-0.4295) (-0.3589)) (p (-0.4295) (-0.5869))
       , CurveTo (p (-0.4295) (-0.8149)) (p (-0.2393) (-1.0000)) (p (-0.0038) (-1.0000))
       , CurveTo (p 0.2317 (-1.0000)) (p 0.4219 (-0.8149)) (p 0.4219 (-0.5869))
       , ClosePath
       , MoveTo (p 0.9710 0.1952)
       , CurveTo (p 0.9710 0.4232) (p 0.7809 0.6083) (p 0.5453 0.6083)
       , CurveTo (p 0.3098 0.6083) (p 0.1196 0.4232) (p 0.1196 0.1952)
       , CurveTo (p 0.1196 (-0.0327)) (p 0.3098 (-0.2179)) (p 0.5453 (-0.2179))
       , CurveTo (p 0.7809 (-0.2179)) (p 0.9710 (-0.0327)) (p 0.9710 0.1952)
       , ClosePath
       , MoveTo (p (-0.1196) 0.1977)
       , CurveTo (p (-0.1196) 0.2809) (p (-0.1448) 0.3589) (p (-0.1889) 0.4244)
       , CurveTo (p (-0.2645) 0.5365) (p (-0.3967) 0.6108) (p (-0.5453) 0.6108)
       , CurveTo (p (-0.7809) 0.6108) (p (-0.9710) 0.4257) (p (-0.9710) 0.1977)
       , CurveTo (p (-0.9710) (-0.0302)) (p (-0.7809) (-0.2154)) (p (-0.5453) (-0.2154))
       , CurveTo (p (-0.3098) (-0.2154)) (p (-0.1196) (-0.0302)) (p (-0.1196) 0.1977)
       , ClosePath
       , MoveTo (p 0.4962 1.0000)
       , LineTo (p (-0.4950) 1.0000)
       , CurveTo (p (-0.0743) 0.9987) (p (-0.0542) 0.3149) (p (-0.0542) 0.3149)
       , LineTo (p 0.0542 0.3149)
       , CurveTo (p 0.0542 0.3149) (p 0.0592 0.4861) (p 0.1171 0.6574)
       , CurveTo (p 0.1751 0.8287) (p 0.2859 1.0000) (p 0.4962 1.0000)
       , ClosePath
       , MoveTo (p 0.2872 (-0.2834))
       , CurveTo (p 0.0542 (-0.0504)) (p 0.0542 0.3161) (p 0.0542 0.3161)
       , LineTo (p (-0.0529) 0.3149)
       , CurveTo (p (-0.0529) 0.3149) (p (-0.0529) (-0.0441)) (p (-0.2922) (-0.2834))
       , ClosePath
       , MoveTo (p (-0.2456) (-0.0945))
       , CurveTo (p (-0.0126) 0.1385) (p 0.3539 0.1385) (p 0.3539 0.1385)
       , LineTo (p 0.3539 0.2456)
       , CurveTo (p 0.3539 0.2456) (p (-0.0050) 0.2456) (p (-0.2443) 0.4849)
       , ClosePath
       , MoveTo (p 0.2355 0.4861)
       , CurveTo (p (-0.0038) 0.2469) (p (-0.3627) 0.2469) (p (-0.3627) 0.2469)
       , LineTo (p (-0.3627) 0.1398)
       , CurveTo (p (-0.3627) 0.1398) (p 0.0038 0.1398) (p 0.2368 (-0.0932))
       , ClosePath ]

-- | [日本語]: ggplot 同型のマーカー塗り (色 + alpha)。 hollow (中抜き) は透明・輪郭のみ。
--   plot 点 (Render.Basic) と凡例キー (Render.Layer) で__同一の装飾規則__を使うための
--   単一ソース (= 「凡例マークは plot と揃える」 規律)。
--   [English]: A ggplot-equivalent marker fill (color + alpha). Hollow
--   markers are transparent, outline only. This is the single source that
--   lets plotted points (Render.Basic) and legend keys (Render.Layer) share
--   __the same styling rule__ (the discipline that "legend marks match the
--   plot").
markerFillFor :: Layer -> Text -> Double -> FillStyle
markerFillFor ly c ai
  | getLast (lyHollow ly) == Just True = FillStyle c 0.0
  | otherwise                          = FillStyle c ai

-- | [日本語]: ggplot 同型のマーカー縁 (stroke)。 既定は__縁なし__ (= 塗り点 shape 19)。
--   hollow → 点色で輪郭のみ (幅 'lyStroke'|1)。 'lyEdge' 指定時だけ縁を出す
--   (色 'lyEdgeColor'|点色、 幅 'lyEdgeWidth'|1)。 plot/凡例で共通。
--   [English]: A ggplot-equivalent marker edge (stroke). The default has
--   __no edge__ (a filled point, shape 19). hollow draws only an outline in the
--   point's color (width 'lyStroke' or 1). An edge is drawn only when
--   'lyEdge' is set (color 'lyEdgeColor' or the point color, width
--   'lyEdgeWidth' or 1). Shared by both the plot and the legend.
markerStrokeFor :: Layer -> Text -> Maybe StrokeStyle
markerStrokeFor ly c
  | getLast (lyHollow ly) == Just True = Just (StrokeStyle c (doubleOr (lyStroke ly) 1.0))
  | getLast (lyEdge ly)   == Just True =
      Just (StrokeStyle (maybe c id (getLast (lyEdgeColor ly))) (doubleOr (lyEdgeWidth ly) 1.0))
  | otherwise                          = Nothing

-- | [日本語]: C-6 PS port: scatter / strip 等の i 番目 data 点に対応する shape を取得。
--   lyShapeBy 列値を data から resolve し、 cat → shape を引く。 明示の lyShapeMap が
--   あればそれを最優先、 無ければカテゴリ初出順の index で 'shapePalette' を巡回割当
--   (= ggplot @aes(shape=factor(g))@ の自動 shape scale。 colorVector の色割当と同思想)。
--   [English]: C-6 PS port: gets the shape for the i-th data point in
--   scatter / strip and similar marks. Resolves the lyShapeBy column value
--   from the data and looks up shape by category. An explicit lyShapeMap
--   takes priority when present; otherwise 'shapePalette' is cycled by the
--   category's first-occurrence index (the automatic shape scale for ggplot
--   @aes(shape=factor(g))@, following the same idea as colorVector's color
--   assignment).
pointShapeAt :: Layer -> Resolver -> Int -> MarkShape
pointShapeAt ly r i = case getLast (lyShape ly) of
  Just s  -> s                                    -- ★ Phase 30 A3: 固定 shape 最優先
  Nothing -> case getLast (lyShapeBy ly) of
   Nothing -> MShCircle
   Just cr -> case resolveCol r cr of
    Just (TxtData v) -> resolveShape (V.toList v) (v V.!? i)
    Just (NumData v) -> resolveShape (map numToText (V.toList v)) (fmap numToText (v V.!? i))
    Nothing          -> MShCircle
  where
    resolveShape _    Nothing    = MShCircle
    resolveShape vals (Just cat) =
      case [ s | ShapeMapEntry v s <- lyShapeMap ly, v == cat ] of
        (s:_) -> s                                       -- 明示マップ優先
        []    -> case elemIndex cat (orderedCats vals) of  -- 自動割当 (アルファベット順で巡回)
                   Just k  -> shapePalette !! (k `mod` length shapePalette)
                   Nothing -> MShCircle

-- | [日本語]: 自動 shape scale の巡回パレット (ggplot 風: 丸→三角→四角→…)。
--   [English]: The cyclic palette for the automatic shape scale (ggplot
--   style: circle to triangle to square and onward).
shapePalette :: [MarkShape]
shapePalette =
  [ MShCircle, MShSquare, MShTriangle, MShCross
  , MShSpade, MShHeart, MShClub, MShDiamond ]

-- | [日本語]: TODO-3e (2026-05-29): lySizeBy → 各点の半径 (px) Vector。
--   lySizeBy 指定の列値 (= 要 numeric) を min..max → [szLo, szHi] px に線形 map。
--   指定無しなら lySize (or default 3.0) を全点に適用。
--   [English]: TODO-3e (2026-05-29): lySizeBy maps to a per-point radius (px)
--   Vector. The column value given by lySizeBy (must be numeric) is linearly
--   mapped from its min..max range to [szLo, szHi] px. Without lySizeBy,
--   lySize (or the default 3.0) is applied to every point.
-- | [日本語]: per-point マーカー__半径__ (pt) Vector を返す。
--
--   ★ @size@ 意味論を「マーカー外接円の__直径__ (pt)」に統一。
--   'lySize' は直径として解釈し、shapeToPrim が要求する半径 (= 直径/2) を返す。
--   既定直径は 'defaultMarkerDiameter' (= ggplot 実測 1.65mm)。
--   sizeBy (連続 size mapping) の範囲 'lpSizeRange' も__直径__範囲 (= scale_size、
--   既定 (6,20)pt → 半径 3..10pt)。
--   [English]: Returns a Vector of per-point marker __radii__ (pt).
--
--   The "size" semantics is unified as "the __diameter__ (pt) of the
--   marker's bounding circle". 'lySize' is interpreted as a diameter, and
--   this function returns the radius (diameter / 2) that shapeToPrim
--   requires. The default diameter is 'defaultMarkerDiameter' (ggplot's
--   measured 1.65mm). The range for sizeBy (continuous size mapping),
--   'lpSizeRange', is likewise a __diameter__ range (scale_size, default
--   (6,20)pt, giving radius 3..10pt).
sizeVector :: Resolver -> Layout -> Layer -> Int -> V.Vector Double
sizeVector r layout ly n =
  let baseDiam = doubleOr (lySize ly) defaultMarkerDiameter   -- 直径 (pt)
      baseRad  = baseDiam / 2                                  -- shapeToPrim は半径を取る
      (szLo, szHi) = lpSizeRange layout                        -- 直径範囲 (pt)
  in case getLast (lySizeBy ly) of
       Nothing -> V.replicate n baseRad
       Just cr -> case resolveNum r cr of
         Nothing -> V.replicate n baseRad
         Just v  ->
           let lo = V.minimum v
               hi = V.maximum v
               diamOf x = if hi <= lo then baseDiam
                          else szLo + (x - lo) / (hi - lo) * (szHi - szLo)
           in V.fromList [ case v V.!? i of
                             Just x  -> diamOf x / 2
                             Nothing -> baseRad
                         | i <- [0 .. n - 1] ]

-- | [日本語]: lyAlphaBy → 各点の alpha (不透明度) Vector。
--   lyAlphaBy 指定の列値 (= 要 numeric) を min..max → alpha [0.1, 1.0] に線形 map
--   (= ggplot scale_alpha 既定 range)。 指定無しなら baseAlpha (固定 lyAlpha or 既定値)
--   を全点に適用。
--   [English]: lyAlphaBy maps to a per-point alpha (opacity) Vector. The
--   column value given by lyAlphaBy (must be numeric) is linearly mapped
--   from its min..max range to alpha [0.1, 1.0] (ggplot's default
--   scale_alpha range). Without lyAlphaBy, baseAlpha (the fixed lyAlpha, or
--   the default) is applied to every point.
alphaVector :: Resolver -> Layer -> Double -> Int -> V.Vector Double
alphaVector r ly baseAlpha n =
  case getLast (lyAlphaBy ly) of
    Nothing -> V.replicate n baseAlpha
    Just cr -> case resolveNum r cr of
      Nothing -> V.replicate n baseAlpha
      Just v  ->
        let lo = V.minimum v
            hi = V.maximum v
            (aLo, aHi) = (0.1, 1.0)   -- ggplot scale_alpha 既定 range
            alphaOf x = if hi <= lo then baseAlpha
                        else aLo + (x - lo) / (hi - lo) * (aHi - aLo)
        in V.fromList [ case v V.!? i of
                          Just x  -> alphaOf x
                          Nothing -> baseAlpha
                      | i <- [0 .. n - 1] ]

-- ===========================================================================
-- Phase 6+ case C-2 ~ C-5: 基本 / 分布 chart の Render
-- ===========================================================================

-- | [日本語]: カテゴリ名 (= ColTxt) を Layer から取得。 categorical bar / pie 等で labels に。
--   [English]: Gets category names (a ColTxt column) from a Layer, used as
--   labels for categorical bar / pie and similar marks.
catLabelsOf :: Resolver -> Layer -> [Text]
catLabelsOf r ly = case getLast (lyEncX ly) of
  Just cr -> case resolveCol r cr of
    Just (TxtData v) -> V.toList v
    _                -> []
  Nothing -> []

-- ===========================================================================
-- 分布 chart (group × value)
-- ===========================================================================

-- | [日本語]: group 列 (lyEncX ?? colorBy 列) と value 列 (lyEncY) を resolve。 group は
--   categorical (= ColTxt) が普通、 ColNum でも対応 (= ColNum を distinct 値で group)。
--   戻り値: [(group_label, [value])]
--   [English]: Resolves the group column (lyEncX or the colorBy column) and
--   the value column (lyEncY). The group column is usually categorical
--   (ColTxt), but ColNum is also supported (ColNum is grouped by its
--   distinct values). Returns [(group_label, [value])].
groupedValues :: Resolver -> Layer -> [(Text, [Double])]
groupedValues r ly = case distGroupRef ly of
  Just crX -> case resolveCol r crX of
    Just (TxtData labels) ->
      -- ★ NaN (= Maybe 列の Nothing) を整列を保ったまま落とす: vecOrFull (長さ保持) で
      --   ラベルと zip してから NaN 値の行を除く (vecOr で先に縮めると行がズレる)。
      let vals = V.toList (vecOrFull (lyEncY ly) r)
          pairs = [ (l, v) | (l, v) <- zip (V.toList labels) vals, not (isNaN v) ]
          -- foldr で畳むと既に出現順 (A,B,C)。 PS (uniqueOrdered) と一致させる
          -- ため reverse しない (Phase 7 A6: R-2 群↔plot 対応の HS/PS 食違い解消)。
          orderedUniq = foldr (\(l, _) acc -> if l `elem` acc then acc else l : acc) [] pairs
      in [ (l, [v | (lv, v) <- pairs, lv == l]) | l <- orderedUniq ]
    Just (NumData labels) ->
      let vals = V.toList (vecOrFull (lyEncY ly) r)
          pairs = [ (l, v) | (l, v) <- zip (map (T.pack . show . (round :: Double -> Int)) (V.toList labels)) vals, not (isNaN v) ]
          orderedUniq = foldr (\(l, _) acc -> if l `elem` acc then acc else l : acc) [] pairs
      in [ (l, [v | (lv, v) <- pairs, lv == l]) | l <- orderedUniq ]
    Nothing -> []
  Nothing -> []

-- | [日本語]: 'groupedValues' を x カテゴリ軸順 ('lpXCategoryLabels') に整列する。
--   box / violin / strip / swarm / ridge は群を @zip [0..]@ で x 位置に並べるが、
--   x 軸ラベルは 'lpXCategoryLabels' (既定アルファベット順 / discrete-limits override)
--   から来る。 両者の順を一致させないと「箱は Gentoo だがラベルは Chinstrap」 のような
--   ズレが出る (= categorical 既定をアルファベット順にした際の回帰)。 軸ラベルが無い
--   (数値 x 等) ときは 'groupedValues' の順をそのまま返す。
--   [English]: Sorts 'groupedValues' into x-category axis order
--   ('lpXCategoryLabels'). box / violin / strip / swarm / ridge lay out
--   groups at x positions via @zip [0..]@, while the x-axis labels come from
--   'lpXCategoryLabels' (default alphabetical order, or a discrete-limits
--   override). If the two orders don't match, a mismatch results — for
--   example, a box drawn for Gentoo but labeled Chinstrap (a regression from
--   defaulting categorical order to alphabetical). When there is no axis
--   label (for example, a numeric x), 'groupedValues' order is returned
--   as-is.
groupedValuesOrdered :: Layout -> Resolver -> Layer -> [(Text, [Double])]
groupedValuesOrdered layout r ly =
  let gv  = groupedValues r ly
      xls = lpXCategoryLabels layout
  in if null xls then gv
     else [ (g, vs) | g <- xls, Just vs <- [lookup g gv] ]

-- | [日本語]: distribution mark (violin/strip/swarm/raincloud) の群リスト。
--   群列 ('distGroupRef' = encX ?? colorBy) があれば 'groupedValuesOrdered'、 無ければ
--   encY 全体を単一群 ("") にする (= boxplot の単一群挙動と統一)。 これにより 1 引数
--   @violin "v"@ (群なし) でも空にならず 1 つ描ける。
--   [English]: The group list for distribution marks (violin/strip/swarm/
--   raincloud). When a group column ('distGroupRef' = encX or colorBy) is
--   present, uses 'groupedValuesOrdered'; otherwise treats the whole encY as
--   a single group ("") — unifying it with boxplot's single-group behavior.
--   This means even a single-argument @violin "v"@ (no group) draws one
--   group instead of nothing.
distGroupsOrdered :: Layout -> Resolver -> Layer -> [(Text, [Double])]
distGroupsOrdered layout r ly = case distGroupRef ly of
  Just _  -> groupedValuesOrdered layout r ly
  Nothing -> case V.toList (vecOr (lyEncY ly) r) of
    [] -> []
    vs -> [("", vs)]

-- ---------------------------------------------------------------------------
-- Phase 36 B2: dodge (位置列 × 色列の 2 階層) 共通ヘルパ
--   @groupBy "class" <> colorBy "drv"@ のように位置列と色列が別のとき、 各位置
--   カテゴリ内に色サブグループを横並びにする (= ggplot @position_dodge@)。
-- ---------------------------------------------------------------------------

-- | [日本語]: dodge cell 化: (位置列, 色列) について各 (位置 index, 色 index) の値リストを作る。
--   戻り値:
--     positions = 位置カテゴリ ('lpXCategoryLabels' = 既定アルファベット順)
--     colorCats = 色カテゴリ ('lyColorCats' 優先、 無ければ色列の出現順 uniq)
--     cells     = @[(posIx, colIx, [value])]@ (空セルは除外)
--   値は encY、 NaN (= Maybe 列の Nothing) は行整列を保ったまま除外。
--   [English]: Builds dodge cells: given a (position column, color column)
--   pair, builds a value list for each (position index, color index) pair.
--   Returns:
--     positions = the position categories ('lpXCategoryLabels', default
--     alphabetical order)
--     colorCats = the color categories ('lyColorCats' takes priority,
--     otherwise the color column's unique values in appearance order)
--     cells     = @[(posIx, colIx, [value])]@ (empty cells excluded)
--   Values come from encY; NaN (a Nothing in a nullable column) is excluded
--   while preserving row alignment.
dodgeCells :: Layout -> Resolver -> Layer -> ([Text], [Text], [(Int, Int, [Double])])
dodgeCells layout r ly = case distDodgeRef ly of
  Nothing -> ([], [], [])
  Just (posC, colC) ->
    let colLabelsOf cr = case resolveCol r cr of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> map (T.pack . show . (round :: Double -> Int)) (V.toList v)
          _                -> []
        orderedUniqT = foldr (\l acc -> if l `elem` acc then acc else l : acc) []
        posLs  = colLabelsOf posC
        colLs  = colLabelsOf colC
        vals   = V.toList (vecOrFull (lyEncY ly) r)
        triples = [ (p, c, v) | (p, c, v) <- zip3 posLs colLs vals, not (isNaN v) ]
        positions = let xls = lpXCategoryLabels layout
                    in if null xls then orderedUniqT [ p | (p, _, _) <- triples ] else xls
        colorCats = if not (null (lyColorCats ly)) then lyColorCats ly
                    else orderedUniqT [ c | (_, c, _) <- triples ]
        cellAt pix cix = [ v | (p, c, v) <- triples
                             , Just pix == elemIndex p positions
                             , Just cix == elemIndex c colorCats ]
        cells = [ (pix, cix, vs)
                | pix <- [0 .. length positions - 1]
                , cix <- [0 .. length colorCats - 1]
                , let vs = cellAt pix cix, not (null vs) ]
    in (positions, colorCats, cells)

-- | [日本語]: dodge sub-cell の data 空間中心 (= bar 'PosDodge' と同式)。 位置カテゴリ @pix@ の
--   slot (幅 0.9) を色数 @nColor@ で等分し、 @cix@ 番目の中心を data 座標で返す。
--   [English]: The data-space center of a dodge sub-cell (using the same
--   formula as bar's 'PosDodge'). The position category @pix@'s slot (width
--   0.9) is divided evenly by the number of colors @nColor@, and the center
--   of the @cix@-th sub-cell is returned in data coordinates.
dodgeCenterD :: Int -> Int -> Int -> Double
dodgeCenterD pix cix nColor =
  fromIntegral pix - 0.45
    + (fromIntegral cix + 0.5) * 0.9 / fromIntegral (max 1 nColor)

-- ---------------------------------------------------------------------------
-- 分布計算の共通 helper (Phase 8 B2: KDE / 四分位の重複を一元化)
--   raincloud / violin / box / density / ridge が共有する。 ggplot で言う
--   stat_density / stat_boxplot 相当を 1 箇所に集約 (= 各 geom が再利用)。
-- ---------------------------------------------------------------------------

-- | [日本語]: Gaussian KDE (Silverman bandwidth) を nGrid 点で評価し [(y, density)] を返す。
--   戻り値は y 昇順。 violin/raincloud/density/ridge が共有。 grid は vals の min..max。
--   [English]: Evaluates a Gaussian KDE (Silverman bandwidth) at nGrid points
--   and returns [(y, density)]. The result is in ascending y order. Shared by
--   violin/raincloud/density/ridge. The grid spans vals' min..max.
kdeGrid :: Int -> [Double] -> [(Double, Double)]
kdeGrid nGrid vals
  | length vals < 2 = []
  | otherwise       = kdeGridOver (minimum vals) (maximum vals) nGrid vals

-- | [日本語]: grid 範囲を明示する版。 ridge は全群共通の値域 [gLo, gHi] で各群を
--   評価し、 群データ端の外でも KDE 裾を滑らかに減衰させる (= 各群自前 min/max だと裾が
--   打ち切られて横線にならない、 PS renderRidgeLayer と同方式)。 bw は群自身の vals から。
--   [English]: A variant that takes an explicit grid range. ridge evaluates
--   every group over the shared value range [gLo, gHi], letting the KDE tail
--   decay smoothly even beyond each group's own data extent (using each
--   group's own min/max would truncate the tail instead of tapering it off,
--   the same approach as PS renderRidgeLayer). The bandwidth (bw) is still
--   computed from each group's own vals.
kdeGridOver :: Double -> Double -> Int -> [Double] -> [(Double, Double)]
kdeGridOver gLo gHi nGrid vals
  | length vals < 2 = []
  | otherwise =
      let n     = length vals
          mu    = sum vals / fromIntegral n
          var   = sum [(v - mu)^(2::Int) | v <- vals] / fromIntegral (max 1 (n - 1))
          sigma = sqrt var
          bw    = if sigma <= 0 then (gHi - gLo) / 20
                  else 1.06 * sigma * fromIntegral n ** (-0.2 :: Double)
          kdeAt v = sum [ exp (negate (((v - xi) / bw)^(2::Int)) / 2) | xi <- vals ]
                    / (fromIntegral n * bw * sqrt (2 * pi))
          stepG = (gHi - gLo) / fromIntegral nGrid
      in [ (v, kdeAt v) | k <- [0..nGrid], let v = gLo + fromIntegral k * stepG ]

-- | [日本語]: 5 数要約 (Tukey)。 q1/median/q3 + whisker 端 (1.5×IQR 内の最遠データ点)。
--   box / raincloud が共有。 vals はソート不要 (内部で sort)。
--   [English]: The Tukey five-number summary: q1/median/q3 plus the whisker
--   ends (the farthest data point within 1.5×IQR). Shared by box and
--   raincloud. vals need not be pre-sorted (sorted internally).
data FiveNum = FiveNum
  { fnQ1 :: !Double, fnMed :: !Double, fnQ3 :: !Double
  , fnLoW :: !Double, fnHiW :: !Double }

fiveNum :: [Double] -> Maybe FiveNum
fiveNum [] = Nothing
fiveNum vals =
  let sorted = sort vals
      nq = length sorted
      idx j = if j < 0 || j >= nq then Nothing else Just (sorted !! j)
      q p = let pos  = p * fromIntegral (nq - 1)
                loI  = floor pos :: Int
                hiI  = min (nq - 1) (loI + 1)
                frac = pos - fromIntegral loI
            in case (idx loI, idx hiI) of
                 (Just a', Just b') -> a' + (b' - a') * frac
                 _                  -> 0
      q1 = q 0.25
      q2 = q 0.5
      q3 = q 0.75
      iqr = q3 - q1
      loV = case dropWhile (< q1 - 1.5 * iqr) sorted of (x:_) -> x; [] -> q1
      hiV = case reverse (takeWhile (<= q3 + 1.5 * iqr) sorted) of (x:_) -> x; [] -> q3
  in Just (FiveNum { fnQ1 = q1, fnMed = q2, fnQ3 = q3, fnLoW = loV, fnHiW = hiV })

-- | [日本語]: 細い箱ひげ (= raincloud 中央 / 単群 box 用)。 中心 x = cx、 半幅 hw px。
--   共通 'fiveNum' を使い whisker 足 + IQR 箱 + median 白線を返す。
--   [English]: A thin box-and-whisker (used for the raincloud center / a
--   single-group box). Centered at x = cx, with half-width hw px. Uses the
--   shared 'fiveNum' to return the whisker legs, the IQR box, and the
--   white median line.
-- | [日本語]: 'boxAt' の座標系対応版。 cross 位置を 'CrossLoc' + px offset で受け、
--   座標変換を投影層 ('projectCrossPoint' / 'projectCrossSpan' / 'projectCrossBar')
--   に委ねる。 直線座標系は 'boxAt' と同じ primitive 列・同じ px を返す (byte 一致)。
--   極座標では箱が扇形、 髭が radial 線、 cap / median が弧になる。 halfD は極座標用の
--   data 単位半幅。
--   [English]: The coordinate-aware counterpart of 'boxAt'. Takes the cross
--   position as a 'CrossLoc' plus a pixel offset and delegates coordinate
--   conversion to the projection layer ('projectCrossPoint' /
--   'projectCrossSpan' / 'projectCrossBar'). Linear coordinate systems return
--   the same primitive sequence at the same pixels as 'boxAt' (byte
--   identical); in polar the box becomes a sector, the whiskers radial lines,
--   and the caps and median arcs. halfD is the half width in data units, used
--   by the polar path.
boxAtCross :: Coord -> Layout -> CrossLoc -> Double -> Double -> Double
           -> Text -> [Double] -> [Primitive]
boxAtCross coord layout loc offPx hwPx halfD color vals = case fiveNum vals of
  Nothing -> []
  Just fn ->
    let q1 = fnQ1 fn; q2 = fnMed fn; q3 = fnQ3 fn; loV = fnLoW fn; hiV = fnHiW fn
        pt v = projectCrossPoint coord layout loc offPx v
        spanLine w v =
          let pts = projectCrossSpan coord layout loc offPx hwPx halfD v
          in [ PLine p q (solid color w) | (p, q) <- zip pts (drop 1 pts) ]
        body = case projectCrossBar coord layout loc offPx hwPx halfD q3 q1 of
          BarRect rect  -> PRect rect (FillStyle color 0.7) (Just (StrokeStyle color 1.0))
          BarWedge segs -> PPath segs (FillStyle color 0.7) (Just (StrokeStyle color 1.0))
        medianLine =
          let pts = projectCrossSpan coord layout loc offPx hwPx halfD q2
          in [ PLine p q (solid "#ffffff" 1.5) | (p, q) <- zip pts (drop 1 pts) ]
    in [ PLine (pt q3) (pt hiV) (solid color 1.0)
       , PLine (pt q1) (pt loV) (solid color 1.0) ]
       <> spanLine 1.0 hiV
       <> spanLine 1.0 loV
       <> [ body ]
       <> medianLine

boxAt :: (Double -> Double) -> Double -> Double -> Text -> [Double] -> [Primitive]
boxAt sy cx hw color vals = case fiveNum vals of
  Nothing -> []
  Just fn ->
    let q1 = fnQ1 fn; q2 = fnMed fn; q3 = fnQ3 fn; loV = fnLoW fn; hiV = fnHiW fn
    in [ PLine (Point cx (sy q3)) (Point cx (sy hiV)) (solid color 1.0)
       , PLine (Point cx (sy q1)) (Point cx (sy loV)) (solid color 1.0)
       , PLine (Point (cx - hw) (sy hiV)) (Point (cx + hw) (sy hiV)) (solid color 1.0)
       , PLine (Point (cx - hw) (sy loV)) (Point (cx + hw) (sy loV)) (solid color 1.0)
       , PRect (Rect (cx - hw) (sy q3) (2 * hw) (sy q1 - sy q3))
               (FillStyle color 0.7) (Just (StrokeStyle color 1.0))
       , PLine (Point (cx - hw) (sy q2)) (Point (cx + hw) (sy q2))
               (solid "#ffffff" 1.5) ]

-- | [日本語]: Ridge 用 group 化: encX = 値 (numeric)、 encY = 群 (categorical)。
--   groupedValues は encX を群とするため、 ridge では x/y を入れ替えた版が要る。
--   [English]: Grouping for Ridge plots: encX is the value (numeric), encY
--   is the group (categorical). Since groupedValues treats encX as the
--   group, ridge needs a variant with x and y swapped.
ridgeGroups :: Resolver -> Layer -> [(Text, [Double])]
ridgeGroups r ly = case getLast (lyEncY ly) of
  Just crG -> case resolveCol r crG of
    Just (TxtData labels) ->
      let vals = V.toList (vecOr (lyEncX ly) r)
          pairs = zip (V.toList labels) vals
          orderedUniq = foldr (\(l, _) acc -> if l `elem` acc then acc else l : acc) [] pairs
      in [ (l, [v | (lv, v) <- pairs, lv == l]) | l <- orderedUniq ]
    Just (NumData labels) ->
      let vals = V.toList (vecOr (lyEncX ly) r)
          pairs = zip (map (T.pack . show . (round :: Double -> Int)) (V.toList labels)) vals
          orderedUniq = foldr (\(l, _) acc -> if l `elem` acc then acc else l : acc) [] pairs
      in [ (l, [v | (lv, v) <- pairs, lv == l]) | l <- orderedUniq ]
    Nothing -> []
  Nothing -> []

-- | [日本語]: Backend が実装する interface。 IO は canvas / file write のため。
--   [English]: The interface implemented by each backend. IO is needed for
--   canvas drawing / file writing.
class Renderer rndr where
  drawPrimitives :: rndr -> [Primitive] -> IO ()


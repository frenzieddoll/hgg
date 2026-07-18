-- |
-- Module      : Hgg.Plot.Render.Common
-- Description : 共通基盤 (型・theme・projection・axis/grid/tick・color・shape・stat helper)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 7 A4: Render モノリス分割 (出力中立・純粋移動)。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Hgg.Plot.Render.Common where

import           Hgg.Plot.Layout (numToText,
                                      Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      formatTicksGG,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical,
                                      UCtx (..), resolvePosX, resolvePosY)
import           Hgg.Plot.Unit   (Pos (..), mmToPt)
import           Hgg.Plot.Primitive  -- Phase 51: Point/Rect/style/Primitive/scalePrimitives (leaf)
import           Hgg.Plot.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Hgg.Plot.Spec   (Annotation (..), AxisFormat (..),
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
                                      ThemeOverride (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axShowGrid,
                                      FontSpec (..), orderedCats,
                                      colRefName, distGroupRef, distDodgeRef,
                                      resolveCol, resolveNum)
import           Data.Maybe          (mapMaybe, isJust)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Hgg.Plot.Spec
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)


-- Phase 51: Point/Rect/style/PathSegment/Transform/Primitive/solid/scalePrimitives は
-- 'Hgg.Plot.Primitive' (leaf) へ移設 (循環回避)。 本 module は import 済 (下記)。

-- | TODO-10 (2026-05-29) PS port: どの font slot を引くか
-- (= spec の titleFont / axisLabelFont / tickFont / legendFont)。
data FontKind = TitleF | AxisLabelF | TickF | LegendTitleF | LegendItemF
  deriving (Show, Eq)

-- | TODO-10 (2026-05-29) PS port: spec の font 設定 + theme default を merge して TextStyle を生成。
-- spec を取れない場所 (= layer helper 内など) では Nothing を渡すと slot default に fallback。
mkFontTS :: Maybe VisualSpec -> ThemePalette -> FontKind -> TextAnchor -> Double -> TextStyle
mkFontTS mSpec pal fk anchor rot =
  let -- Phase 34: ggplot theme_grey の base_size + 相対比に較正 (R theme_grey() 実測)。
      -- 旧値 (Title16/Axis12/Tick11/LegTitle11/LegItem10) は ggplot より系統的に大きく、
      -- 特に目盛が base 11pt のままだった (ggplot は axis.text = base×0.8 = 8.8pt)。
      baseSize = 11        -- theme_grey base_size
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
  in case mFont of
       Nothing -> TextStyle defColor defSize "sans-serif" anchor rot "normal" False
       Just fs -> TextStyle
         { tsColor  = orElse (Hgg.Plot.Spec.fsColor  fs) defColor
         , tsSize   = orElse (Hgg.Plot.Spec.fsSize   fs) defSize
         , tsFamily = orElse (Hgg.Plot.Spec.fsFamily fs) "sans-serif"
         , tsAnchor = anchor
         , tsRotate = rot
         , tsWeight = orElse (Hgg.Plot.Spec.fsWeight fs) "normal"
         , tsItalic = orElse (Hgg.Plot.Spec.fsItalic fs) False
         }

-- Phase 51: Transform / PathSegment / Primitive は 'Hgg.Plot.Primitive' へ移設。

-- | mm → pt 変換 (Phase 33 B7)。 mark 既定 (point/line/半径/cap/矢じり) を物理 mm で
-- 書くためのヘルパ。 layout/Primitive は純 pt なので、 既定もここで pt に解決する。
-- backend が dpi 係数 (k) を最後に一律適用する ('scalePrimitives')。
mmPt :: Double -> Double
mmPt mm = mm * mmToPt

-- | scatter / point マーカーの既定**直径** (pt)。Phase 34 A1 で ggplot
-- @geom_point@ 既定を実測した 1.65mm (= 半径 2.34pt) に較正
-- (`phase-34-measurements/A1-results.md`)。size 意味論は「外接円の直径」
-- (Phase 34 §2.1)。
defaultMarkerDiameter :: Double
defaultMarkerDiameter = mmPt 1.65

-- | 線 (geom_line/path/step/segment) の既定**線幅** (pt)。Phase 34 A1 で ggplot
-- @linewidth 0.5@ の実描画幅 0.376mm に較正 (解析式 @nominal × .pt/96 × 25.4@ を
-- 太線 bbox 実測で検証)。
defaultLineWidth :: Double
defaultLineWidth = mmPt 0.376

-- | geom_smooth 線の既定線幅 (pt)。ggplot は @linewidth = 2 × 既定@ なので line の
-- 2倍 (0.753mm)。Phase 34 A1。
defaultSmoothWidth :: Double
defaultSmoothWidth = mmPt 0.753

-- | Theme 色 palette (= JSON serialize しないので Render module 内に閉じる)。
-- Phase 9 A-1: 色に加え「panel 背景塗り / grid / border 有無フラグ」 を持つ。
--   * tpPanelBg    = panel (plotArea) 背景色。 tpShowPanel が True のとき塗る。
--   * tpShowPanel  = panel 矩形を塗るか (theme_grey / ブランドは True、 従来 preset は False)。
--   * tpShowGrid   = theme レベルの grid master (False で全 grid 抑制。 軸ごと axShowGrid と AND)。
--   * tpShowBorder = axisFrame の 4 辺枠を描くか (従来 preset True、 panel 塗り系は False)。
data ThemePalette = ThemePalette
  { tpBackground :: !Text
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
  , tpShowBorder :: !Bool   -- axisFrame の 4 辺枠を描くか
  , tpShowAxisLine :: !Bool -- 下辺(x軸)+左辺(y軸)の 2 本軸線を描くか (theme_classic)
  -- ★ Phase 32 (re-apply): ggplot theme_grey fidelity 用の追加 field。
  --   既存テーマは既定値で挙動不変、 ThemeGrey のみ ggplot 厳密値を入れる。
  , tpTitleColor    :: !Text   -- plot.title / axis.title の文字色 (ggplot=black、 既定= tpText)
  , tpTitleHjust    :: !Double -- plot.title の水平揃え (0=左、 0.5=中央。 ggplot theme_grey=0)
  , tpTickLineColor :: !Text   -- 軸目盛線 (tick mark) の色 (ggplot=grey20、 既定= tpAxis)
  , tpLegendKeyBg   :: !Text   -- legend.key 背景塗り色 ("" なら塗らない。 ggplot=grey95)
  } deriving (Show, Eq)

themePalette :: Hgg.Plot.Spec.ThemeName -> ThemePalette
themePalette t = case t of
  -- 従来 4 preset: 白(灰)背景・panel 塗り無し・4 辺枠あり (= G4 の白背景+薄 grid を温存)。
  Hgg.Plot.Spec.ThemeDefault -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#444444", tpText = "#333333", tpGrid = "#dddddd"
    , tpDefault = "#1f77b4", tpDefaultFill = "#1f77b4", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpTitleColor = "#333333", tpTitleHjust = 0.0, tpTickLineColor = "#444444", tpLegendKeyBg = "" }
  Hgg.Plot.Spec.ThemeMinimal -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#333333", tpText = "#333333", tpGrid = "#eeeeee"
    , tpDefault = "#1f77b4", tpDefaultFill = "#1f77b4", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpTitleColor = "#333333", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "" }
  Hgg.Plot.Spec.ThemeLight -> ThemePalette
    { tpBackground = "#fafafa", tpAxis = "#666666", tpText = "#444444", tpGrid = "#e0e0e0"
    , tpDefault = "#3498db", tpDefaultFill = "#3498db", tpPanelBg = "#fafafa"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpTitleColor = "#444444", tpTitleHjust = 0.0, tpTickLineColor = "#666666", tpLegendKeyBg = "" }
  Hgg.Plot.Spec.ThemeDark -> ThemePalette
    { tpBackground = "#222222", tpAxis = "#cccccc", tpText = "#eeeeee", tpGrid = "#444444"
    , tpDefault = "#5dade2", tpDefaultFill = "#5dade2", tpPanelBg = "#222222"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpTitleColor = "#eeeeee", tpTitleHjust = 0.0, tpTickLineColor = "#cccccc", tpLegendKeyBg = "" }
  -- ggplot 既定 theme_grey: 白 plot bg・灰 panel #EBEBEB・白 grid・枠なし・軸線なし。
  -- ★ Phase 34: geom 既定色を ggplot 厳密値に (point/line = black、 bar/hist = grey35)。
  Hgg.Plot.Spec.ThemeGrey -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#4d4d4d", tpText = "#4d4d4d", tpGrid = "#ffffff"
    , tpDefault = "#000000", tpDefaultFill = "#595959", tpPanelBg = "#ebebeb"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    -- ★ Phase 32: ggplot theme_grey 厳密値。 title=black/左寄せ・tick=grey20・legend.key=grey95。
    , tpTitleColor = "#000000", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "#f2f2f2" }
  -- ブランド (panel 塗りあり・grid あり・枠なし、 series は themeSeriesPalette)。
  Hgg.Plot.Spec.ThemeNoir -> ThemePalette
    { tpBackground = "#16161e", tpAxis = "#5a6080", tpText = "#c8ccda", tpGrid = "#2a2e45"
    , tpDefault = "#7aa2f7", tpDefaultFill = "#7aa2f7", tpPanelBg = "#1e2030"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpTitleColor = "#c8ccda", tpTitleHjust = 0.0, tpTickLineColor = "#5a6080", tpLegendKeyBg = "" }
  Hgg.Plot.Spec.ThemeLumen -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#8a857e", tpText = "#2b2b33", tpGrid = "#e7e3db"
    , tpDefault = "#4c5bd4", tpDefaultFill = "#4c5bd4", tpPanelBg = "#f7f5f1"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpTitleColor = "#2b2b33", tpTitleHjust = 0.0, tpTickLineColor = "#8a857e", tpLegendKeyBg = "" }
  -- Parchment 正式テーマ (明)。 panel=羊皮紙 cream-light #F8F5EE は据え置き、
  --   外周 plot bg は白 #FFFFFF にして軸内 panel を額装的に強調 (2026-06-02 ユーザ確定)。
  Hgg.Plot.Spec.ThemeParchment -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#8b6f3a", tpText = "#1a1620", tpGrid = "#e0d6c0"
    , tpDefault = "#f0a5a0", tpDefaultFill = "#f0a5a0", tpPanelBg = "#f8f5ee"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpTitleColor = "#1a1620", tpTitleHjust = 0.0, tpTickLineColor = "#8b6f3a", tpLegendKeyBg = "" }
  -- 暗版 = Charcoal (中性炭、 Red Queen §4.8 Charcoal #2B2B2E 由来。 焦茶から変更 2026-06-02)。
  Hgg.Plot.Spec.ThemeParchmentDark -> ThemePalette
    { tpBackground = "#1e1e22", tpAxis = "#9aa0a8", tpText = "#d6d8dd", tpGrid = "#42424a"
    , tpDefault = "#f0a5a0", tpDefaultFill = "#f0a5a0", tpPanelBg = "#2a2a30"
    , tpShowPanel = True, tpShowGrid = True, tpShowBorder = False, tpShowAxisLine = False
    , tpTitleColor = "#d6d8dd", tpTitleHjust = 0.0, tpTickLineColor = "#9aa0a8", tpLegendKeyBg = "" }
  -- ggplot theme_bw: 白背景・薄グレー grid・黒灰の 4 辺枠 (軸線なし)。
  Hgg.Plot.Spec.ThemeBW -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#333333", tpText = "#4d4d4d", tpGrid = "#ebebeb"
    , tpDefault = "#353535", tpDefaultFill = "#353535", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpTitleColor = "#4d4d4d", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "" }
  -- ggplot theme_classic: 白背景・grid なし・枠なし・下/左の 2 軸線あり。
  Hgg.Plot.Spec.ThemeClassic -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#333333", tpText = "#4d4d4d", tpGrid = "#ffffff"
    , tpDefault = "#353535", tpDefaultFill = "#353535", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = False, tpShowBorder = False, tpShowAxisLine = True
    , tpTitleColor = "#4d4d4d", tpTitleHjust = 0.0, tpTickLineColor = "#333333", tpLegendKeyBg = "" }
  -- ggplot theme_void: 背景・grid・枠・軸線すべてなし (データのみ)。
  Hgg.Plot.Spec.ThemeVoid -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#4d4d4d", tpText = "#4d4d4d", tpGrid = "#ffffff"
    , tpDefault = "#353535", tpDefaultFill = "#353535", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = False, tpShowBorder = False, tpShowAxisLine = False
    , tpTitleColor = "#4d4d4d", tpTitleHjust = 0.0, tpTickLineColor = "#4d4d4d", tpLegendKeyBg = "" }
  -- ggplot theme_linedraw: 白背景・黒寄り細 grid・黒の 4 辺枠。
  Hgg.Plot.Spec.ThemeLinedraw -> ThemePalette
    { tpBackground = "#ffffff", tpAxis = "#000000", tpText = "#1a1a1a", tpGrid = "#b3b3b3"
    , tpDefault = "#000000", tpDefaultFill = "#000000", tpPanelBg = "#ffffff"
    , tpShowPanel = False, tpShowGrid = True, tpShowBorder = True, tpShowAxisLine = False
    , tpTitleColor = "#1a1a1a", tpTitleHjust = 0.0, tpTickLineColor = "#000000", tpLegendKeyBg = "" }

-- | Phase 9 A-2: preset palette に ThemeOverride を合成 (element 単位上書き)。
-- 各 override field が Just なら preset 値を差し替える。 描画は合成後の値のみ参照。
resolveTheme :: Hgg.Plot.Spec.ThemeName -> ThemeOverride -> ThemePalette
resolveTheme name ov =
  let base = themePalette name
  in base
       { tpBackground   = ovT toPlotBg       (tpBackground base)
       , tpPanelBg      = ovT toPanelBg      (tpPanelBg base)
       , tpShowPanel    = ovB toShowPanel    (tpShowPanel base)
       , tpGrid         = ovT toGridColor    (tpGrid base)
       , tpShowGrid     = ovB toShowGrid     (tpShowGrid base)
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

-- | spec の theme + override を解決して ThemePalette を得る (全 render 経路の入口)。
specThemePalette :: VisualSpec -> ThemePalette
specThemePalette spec =
  resolveTheme (fromMaybe Hgg.Plot.Spec.ThemeDefault (getLast (vsTheme spec)))
               (vsThemeOverride spec)

-- | Phase 9 A-4: facet strip.background の (塗り色, 表示) を解決。 ggplot は殆どの preset で
-- 灰矩形 (grey85 #d9d9d9)、 theme_minimal / theme_void は strip 矩形なし。 panel 塗り系
-- (dark/noir/canvas-dark) は panel より少し明るい/暗い帯。 override (toStripBg/toShowStrip) 優先。
themeStripStyle :: VisualSpec -> (Text, Bool)
themeStripStyle spec =
  let name = fromMaybe Hgg.Plot.Spec.ThemeDefault (getLast (vsTheme spec))
      ov   = vsThemeOverride spec
      (dbg, dshow) = case name of
        Hgg.Plot.Spec.ThemeMinimal          -> ("#d9d9d9", False)
        Hgg.Plot.Spec.ThemeVoid             -> ("#ffffff", False)
        Hgg.Plot.Spec.ThemeLight            -> ("#e0e0e0", True)
        Hgg.Plot.Spec.ThemeDark             -> ("#3a3a3a", True)
        Hgg.Plot.Spec.ThemeNoir      -> ("#2a2e45", True)
        Hgg.Plot.Spec.ThemeLumen     -> ("#ece8e0", True)
        Hgg.Plot.Spec.ThemeParchment    -> ("#ece0c8", True)
        Hgg.Plot.Spec.ThemeParchmentDark -> ("#3a3a42", True)
        _                                        -> ("#d9d9d9", True)  -- default/grey/bw/classic/linedraw
      bg  = fromMaybe dbg   (getLast (toStripBg ov))
      shw = fromMaybe dshow (getLast (toShowStrip ov))
  in (bg, shw)

-- | Scale の range (= pixel 出力域) を別 Rect に合わせて作り直す。 domain は不変。
-- plot area を縮める時 (subplot / marginal) は plotArea だけでなく scale の range も
-- 必ず合わせないと、 mark の位置が古い枠基準のまま描かれて軸枠からはみ出す
-- (= ggplot で panel が動けば座標変換も追従するのと同じ原則)。
scaleRetargetX :: Hgg.Plot.Layout.Scale -> Rect -> Hgg.Plot.Layout.Scale
scaleRetargetX scale rect = case scale of
  LinearScale lo hi _ _ -> LinearScale lo hi (rX rect) (rX rect + rW rect)
  LogScale lo hi _ _    -> LogScale    lo hi (rX rect) (rX rect + rW rect)
  SqrtScale lo hi _ _   -> SqrtScale   lo hi (rX rect) (rX rect + rW rect)
  TimeScale lo hi _ _   -> TimeScale   lo hi (rX rect) (rX rect + rW rect)

scaleRetargetY :: Hgg.Plot.Layout.Scale -> Rect -> Hgg.Plot.Layout.Scale
scaleRetargetY scale rect = case scale of
  LinearScale lo hi _ _ -> LinearScale lo hi (rY rect + rH rect) (rY rect)
  LogScale lo hi _ _    -> LogScale    lo hi (rY rect + rH rect) (rY rect)
  SqrtScale lo hi _ _   -> SqrtScale   lo hi (rY rect + rH rect) (rY rect)
  TimeScale lo hi _ _   -> TimeScale   lo hi (rY rect + rH rect) (rY rect)

-- | TODO-3b (2026-05-29): C-5 grid line 描画。 PS Render.purs:gridLines を
-- HS に port。 vsXAxis / vsYAxis の axShowGrid が True なら x/y tick 位置に
-- 薄い grid line を描く。 default false (= 旧 HS 挙動と互換)。
gridLines :: Layout -> VisualSpec -> ThemePalette -> [Primitive]
gridLines layout spec pal =
  let area = lpPlotArea layout
      coord = coordOf spec
      sx = scaleApply (lpXScale layout)
      sy = scaleApply (lpYScale layout)
      -- Phase 9 C: flip 時はデータ x が縦 px・データ y が横 px に写る。
      sxF = scaleApply (lpXScaleFlipped layout)
      syF = scaleApply (lpYScaleFlipped layout)
      majorStyle = solid (tpGrid pal) 1.0
      minorStyle = solid (tpGrid pal) 0.5   -- G4: minor は major の半分の太さ (ggplot 準拠)
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
      -- Phase 9 A-1: theme レベルの grid master (tpShowGrid) が False なら全 grid 抑制。
      gx = if tpShowGrid pal && showXGrid then minorX ++ majorX else []
      gy = if tpShowGrid pal && showYGrid then minorY ++ majorY else []
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

-- | Phase 11 A7-c: 極座標の grid + 軸 (= 直交 gridLines/axisFrame/tickMarks の代わり)。
--   半径方向 = 同心円 (rad tick ごと) + 中心からの r 軸ラベル (上スポーク沿い)。
--   角度方向 = 放射スポーク (theta tick ごと) + 外周の角度ラベル。
--   theta 軸は PolarX なら x、 PolarY なら y。
polarGrid :: VisualSpec -> Layout -> ThemePalette -> [Primitive]
polarGrid spec layout pal =
  let coord = coordOf spec
      (cx, cy, maxR) = polarCenter layout
      gridCol = tpGrid pal
      circleStyle = Just (StrokeStyle gridCol 0.5)
      noFill = FillStyle gridCol 0.0
      spokeStyle = solid gridCol 0.5
      -- theta / radius を担う scale と tick / category ラベルを coord で選ぶ。
      (thetaScale, thetaTicks, thetaCats, radScale, radTicks) = case coord of
        CoordPolarY -> ( lpYScale layout, lpYTicks layout, lpYCategoryLabels layout
                       , lpXScale layout, lpXTicks layout )
        _           -> ( lpXScale layout, lpXTicks layout, lpXCategoryLabels layout
                       , lpYScale layout, lpYTicks layout )
      inUnit f = f >= -1e-9 && f <= 1 + 1e-9
      -- 同心円 (半径 grid)。 domFrac が [0,1] のものだけ。
      circles = [ PCircle (Point cx cy) (domFrac radScale v * maxR) noFill circleStyle Nothing
                | v <- radTicks, inUnit (domFrac radScale v) ]
      -- 外周境界円。
      boundary = [ PCircle (Point cx cy) maxR noFill (Just (StrokeStyle (tpAxis pal) 1.0)) Nothing ]
      -- 放射スポーク (角度 grid)。 中心→外周。
      spokes = [ PLine (Point cx cy) (uncurry Point (polarPointXY (domFrac thetaScale v) 1.0)) spokeStyle
               | v <- thetaTicks ]
      -- r 軸ラベル (上スポーク θ=0 沿い、 各 rad tick)。
      tsR = mkFontTS (Just spec) pal TickF AnchorEnd 0
      radLabels = [ PText (Point (cx - 4) (cy - domFrac radScale v * maxR + 4)) (numToText v) tsR
                  | v <- radTicks, inUnit (domFrac radScale v), domFrac radScale v > 1e-6 ]
      -- θ 軸ラベル (外周のやや外、 各 theta tick)。 categorical なら群名、 でなければ値。
      tsT = mkFontTS (Just spec) pal TickF AnchorMiddle 0
      thetaLabelFor i v = if not (null thetaCats) && i < length thetaCats
                            then thetaCats !! i else numToText v
      thetaLabels = [ let (lx, ly) = polarPointXY (domFrac thetaScale v) 1.12
                      in PText (Point lx (ly + 4)) (thetaLabelFor i v) tsT
                    | (i, v) <- zip [0 ..] thetaTicks ]
      polarPointXY tf rf = polarPoint layout tf rf
  in if tpShowGrid pal then circles <> spokes <> boundary <> radLabels <> thetaLabels
     else boundary <> radLabels <> thetaLabels

-- | Phase 11 A7-c: 極座標の bar = 扇形 (annular sector)。 (角度 frac tf0..tf1、 半径
--   frac rf0..rf1) を弧近似 (約 0.1 rad/seg) した閉路 PathSegment を返す。 pie (rf0=0)
--   は中心からの扇形、 rose (rf0=0, 角度帯) は円形棒。 HS/PS 同一。
wedgeSegments :: Layout -> Double -> Double -> Double -> Double -> [PathSegment]
wedgeSegments l tf0 tf1 rf0 rf1 =
  let dθ    = abs (tf1 - tf0) * 2 * pi
      nSeg  = max 2 (ceiling (dθ / 0.1)) :: Int
      steps = [ tf0 + (tf1 - tf0) * fromIntegral i / fromIntegral nSeg | i <- [0 .. nSeg] ]
      mk t rf = uncurry Point (polarPoint l t rf)
      outer = [ mk t rf1 | t <- steps ]
      inner = [ mk t rf0 | t <- reverse steps ]
  in case outer ++ inner of
       (p0 : rest) -> MoveTo p0 : map LineTo rest ++ [ClosePath]
       []          -> []

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just v) = v

-- ---------------------------------------------------------------------------
-- 軸 / tick
-- ---------------------------------------------------------------------------

background :: Layout -> ThemePalette -> [Primitive]
background layout pal =
  let ViewportSize w h = lpViewport layout
  in [ PRect (Rect 0 0 (fromIntegral w) (fromIntegral h))
             (FillStyle (tpBackground pal) 1.0)
             Nothing ]

-- | Phase 9 A-1: panel (plotArea) 背景の塗り経路。 theme_grey / ブランドは灰/暗の
-- panel 矩形を塗り、 その上に白/淡色 grid を重ねる (ggplot theme_grey 構造)。
-- tpShowPanel が False の preset では何も描かない (= 従来の白背景挙動を温存)。
panelBackground :: Layout -> ThemePalette -> [Primitive]
panelBackground layout pal
  | tpShowPanel pal = [ PRect (lpPlotArea layout) (FillStyle (tpPanelBg pal) 1.0) Nothing ]
  | otherwise       = []

-- | axisFrame: panel の 4 辺枠。 tpShowBorder が False の theme (grey / ブランド) では
-- 枠を描かない (= ggplot theme_grey は border なし)。
-- axisLine (下辺=x軸 + 左辺=y軸 の 2 本) は theme_classic 用に tpShowAxisLine で出す。
-- border と axisLine は排他ではないが、 classic は border なし + axisLine ありの組合せ。
axisFrame :: Layout -> ThemePalette -> [Primitive]
axisFrame layout pal = border ++ axisLine
  where
    a = lpPlotArea layout
    border | tpShowBorder pal =
               [ PRect a (FillStyle (tpBackground pal) 0) (Just (StrokeStyle (tpAxis pal) 1.0)) ]
           | otherwise = []
    axisLine | tpShowAxisLine pal =
                 [ PLine (Point (rX a) (rY a + rH a)) (Point (rX a + rW a) (rY a + rH a)) (solid (tpAxis pal) 1.0)
                 , PLine (Point (rX a) (rY a)) (Point (rX a) (rY a + rH a)) (solid (tpAxis pal) 1.0) ]
             | otherwise = []

-- | TODO-3 (2026-05-29): axRotate / axShowTicks 対応 (= PS Render.tickMarksWithShow port)。
-- TODO-10 (2026-05-29): mSpec を thread して tick font (= spec.tickFont) を反映。
-- rotX/rotY は度数 (0 = 水平、 90 = 縦)。 showX/showY が False の軸は tick line + label を省略。
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
      tkLen    = ggTickLen * sc
      tkGap    = (ggTickLen + ggAxTextMar) * sc
      -- Phase 32 (re-apply): 目盛線 (tick mark) は tpTickLineColor (ggplot=grey20)。
      --   軸線/枠 (axisFrame) は tpAxis のままで別物。
      tickStyle = solid (tpTickLineColor pal) 1.0
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
        in [ PLine (Point px yb) (Point px (yb + tkLen)) tickStyle
           -- Phase 8 C (small-viewport text fix): フォント由来オフセット (tickSize*k) は
           -- 等倍 (tkGap = sc*間隔 のみ scale)。 旧 *sc で小パネル時に数値が軸に被っていた。
           , if rotX == 0
               then PText (Point px (yb + tkGap + tickSize * 0.8)) (xLabel v) ts
               else PText (Point px (yb + tkGap + tickSize * 0.4)) (xLabel v) tsXrot
           ]
      yMark v =
        let py = scaleApply sy v
            xl = rX a
        in [ PLine (Point xl py) (Point (xl - tkLen) py) tickStyle
           , PText (Point (xl - tkGap) (py + tickSize * 0.35)) (yLabel v) tsYrot ]
      -- Phase 9 C flip: データ x 軸を左辺に (= yMark 風)、 データ y 軸を下辺に (= xMark 風)。
      --   ラベルは水平のまま (anchor のみ placement に対応)。 sxF=データ x→縦 px、 syF=データ y→横 px。
      coord = maybe CoordCartesian coordOf mSpec
      sxF = lpXScaleFlipped layout
      syF = lpYScaleFlipped layout
      xMarkFlip v =
        let py = scaleApply sxF v
            xl = rX a
        in [ PLine (Point xl py) (Point (xl - tkLen) py) tickStyle
           , PText (Point (xl - tkGap) (py + tickSize * 0.35)) (xLabel v) tsY ]
      yMarkFlip v =
        let px = scaleApply syF v
            yb = rY a + rH a
        in [ PLine (Point px yb) (Point px (yb + tkLen)) tickStyle
           , PText (Point px (yb + tkGap + tickSize * 0.8)) (yLabel v) ts ]
      (xMarkF, yMarkF) = case coord of
        CoordFlip -> (xMarkFlip, yMarkFlip)
        _         -> (xMark, yMark)
      -- Phase 11 A7-c: 極座標の tick は polarGrid が描く (= 直交辺の tick は出さない)。
      xPrims = if showX && not (isPolar coord) then concatMap xMarkF (lpXTicks layout) else []
      yPrims = if showY && not (isPolar coord) then concatMap yMarkF (lpYTicks layout) else []
  in xPrims <> yPrims

-- | AxisFormat に応じて Double を表示文字列に。 Nothing = auto。
--
-- Phase 6 A7: 'AxisTimeFmt' は Double を unix epoch (= seconds since 1970 UTC) と
-- 解釈し、 Data.Time.formatTime で format 文字列を適用。
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
      hasTitle = case getLast (vsTitle spec) of Just _ -> True; _ -> False
      titleBaseY = boxTop + sc * (ggHalfLine + titleSize * 0.8)
      -- Phase 32 (re-apply): plot.title の水平揃え。 hjust=0 (ggplot theme_grey) は
      --   panel 左端にアンカー開始、 それ以外 (既定 0.5) は従来通り中央。
      (titleX, tsTitle') = if tpTitleHjust pal <= 0.0
                             then (rX a, tsTitle { tsAnchor = AnchorStart })
                             else (cx,   tsTitle)
      titleP = case getLast (vsTitle spec) of
        Just t  -> [ PText (Point titleX titleBaseY) t tsTitle' ]
        Nothing -> []
      -- x 軸タイトル = 最下要素。 baseline = 下 plot.margin の上 (= boxBottom - margin - descent)。
      xLP = case getLast (vsXLabel spec) of
        Just t  -> [ PText (Point cx (boxBottom - sc * (ggHalfLine + labelSize * 0.2))) t tsLabel ]
        Nothing -> []
      -- y 軸タイトル = 最左要素 (rot -90)。 x = 左 plot.margin + ascent。
      yLP = case getLast (vsYLabel spec) of
        Just t  -> [ PText (Point (boxLeft + sc * (ggHalfLine + labelSize * 0.7)) cy) t tsLabelV ]
        Nothing -> []
      -- ★ Phase 11 A5-a: subtitle (title 直下、 小フォント) / caption (図右下・
      --   小フォント・右寄せ) / tag (左上隅・やや大・左寄せ太字)。 Layout の margin 予約
      --   ('hasSubtitle'/'hasCaption'/'hasTag') と座標を揃える。
      --   ★ subtitle の水平揃えは plot.title と同じ ('tpTitleHjust'): theme_grey は左寄せ。
      subSize  = 11 :: Double
      capSize  =  9 :: Double
      tagSize  = 13 :: Double
      tsSub = (mkFontTS (Just spec) pal AxisLabelF AnchorMiddle 0) { tsSize = subSize }
      tsCap = (mkFontTS (Just spec) pal AxisLabelF AnchorEnd    0) { tsSize = capSize }
      tsTag = (mkFontTS (Just spec) pal TitleF     AnchorStart  0) { tsSize = tagSize, tsWeight = "bold" }
      boxRight = rX a + rW a
      -- subtitle baseline: title があればその下、 無ければ title 位置に置く。
      subBaseY = (if hasTitle then titleBaseY + sc * ggHalfLine + subSize * 0.8
                              else boxTop + sc * (ggHalfLine + subSize * 0.8))
      -- plot.title と同じ hjust 規則: hjust=0 (theme_grey) は panel 左端アンカー開始。
      (subX, tsSub') = if tpTitleHjust pal <= 0.0
                         then (rX a, tsSub { tsAnchor = AnchorStart })
                         else (cx,   tsSub)
      subP = case getLast (vsSubtitle spec) of
        Just t  -> [ PText (Point subX subBaseY) t tsSub' ]
        Nothing -> []
      capP = case getLast (vsCaption spec) of
        Just t  -> [ PText (Point boxRight (boxBottom - sc * ggHalfLine)) t tsCap ]
        Nothing -> []
      tagP = case getLast (vsTag spec) of
        Just t  -> [ PText (Point boxLeft (boxTop + sc * ggHalfLine + tagSize * 0.8)) t tsTag ]
        Nothing -> []
  in titleP <> subP <> xLP <> yLP <> capP <> tagP

-- ★ Phase 38: numToText は Layout へ集約 (Layout import 経由で使用)。

-- | Phase 8 B22: lpYScaleRight が Just のとき plotArea 右端に Y 軸線 + tick を描画
-- (= PS renderRightYAxis と同方式)。 Nothing なら何も描かない。
renderRightYAxis :: Layout -> ThemePalette -> Maybe AxisFormat -> [Primitive]
renderRightYAxis layout pal fmtYR = case lpYScaleRight layout of
  Nothing -> []
  Just sR ->
    let a   = lpPlotArea layout
        xR  = rX a + rW a
        ts  = mkFontTS Nothing pal TickF AnchorStart 0
        axisStyle = solid (tpAxis pal) 1.0
        axisLine  = [ PLine (Point xR (rY a)) (Point xR (rY a + rH a)) axisStyle ]
        tickPrim v =
          let py = scaleApply sR v
          in [ PLine (Point xR py) (Point (xR + 5) py) axisStyle
             , PText (Point (xR + 8) (py + 4)) (formatTick fmtYR v) ts ]
    in axisLine <> concatMap tickPrim (lpYTicksRight layout)

-- | Phase 10 A2: データ空間 (dx, dy) を coord に従い px の 'Point' に写す薄いラッパ。
-- projectXY は生 tuple を返す (Layout は Render の Point に依存できない) ので、
-- mark renderer 側はこのラッパで Point に包む。 coord = lpCoord layout を渡す前提で、
-- Cartesian では `Point (scaleApply (lpXScale l) dx) (scaleApply (lpYScale l) dy)` と
-- bit 一致する (= 従来の `Point (sx x) (sy y)` と同値 → ゼロ diff)。
projectPoint :: Coord -> Layout -> Double -> Double -> Point
projectPoint c l dx dy = let (px, py) = projectXY c l dx dy in Point px py

-- | Phase 11 A7-c: 極座標を解さない standalone renderer (ess/autocorr/forest/funnel/
--   box/violin/strip/swarm/waterfall = 直交/flip 専用 2-way 分岐) 用に coord を
--   {Cartesian, Flip} に正規化する (= polar はそれらの mark では Cartesian 扱い)。
--   polar は座標系として点/線/扇形 bar に意味があり、 これらの統計 mark には適用しない。
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

-- | Phase 11 A4-b: categorical 列を群キー列 [Text] に解決 (linetypeBy 用)。
groupKeysOf :: Resolver -> ColRef -> Maybe [Text]
groupKeysOf r cr = case resolveCol r cr of
  Just (TxtData v) -> Just (V.toList v)
  Just (NumData v) -> Just (map (T.pack . show) (V.toList v))
  _                -> Nothing

-- | キー列と値列を zip し、 キー初出順を保ったまま群ごとにまとめる (= group split)。
orderedGroups :: Eq a => [a] -> [b] -> [(a, [b])]
orderedGroups keys vals =
  let paired = zip keys vals
  in [ (k, [ v | (k', v) <- paired, k' == k ]) | k <- nub keys ]

-- | Phase 26 §C-2 #5: scatter 上に「点を結ぶ線」 を生成。 group 列があれば
-- group 内のみで連結、 order 列があればソート後に連結。
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

-- | Phase 26 §C-2 #3: plot area 内に参照線 1 本を描画。
-- domain (= scale の dLo/dHi) を直接見て 2 端点を計算。
-- | ★ Phase 33 B6: 参照線も 'resolvePosX'/'resolvePosY' (UCtx) 経由に統一。
-- 値は PNative、panel 端は PNpc 0/1 で表す (出力は旧実装と bit 一致)。dpi は
-- PAbs Px 用 (参照線は使わないが UCtx 一貫のため受ける)。
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

-- | 列を数値 Vector に解決。 **NA (NaN) を落とす** (nullable 列対応・ggplot na.rm
--   相当)。 単一列 geom (histogram/freqpoly/density/box/ecdf 等) はこれで欠損を内部処理。
--   非 NULL 列 (NaN を含まない) には no-op なので従来挙動と同一。
vecOr :: Last ColRef -> Resolver -> V.Vector Double
vecOr lc = V.filter (not . isNaN) . vecOrFull lc

-- | 'vecOr' の NaN 保持版 (= 長さを保つ)。 **多列 geom (scatter/line) が x/y を
--   行整列したまま欠損対を落とす**ために使う (per-column drop だと x/y がズレるため)。
vecOrFull :: Last ColRef -> Resolver -> V.Vector Double
vecOrFull lc r = case getLast lc of
  Nothing -> V.empty
  Just cr -> maybe V.empty id (resolveNum r cr)

-- | Okabe-Ito 8 色 categorical palette (= 色覚多様性配慮)。
okabeIto :: [Text]
okabeIto =
  [ "#E69F00", "#56B4E9", "#009E73", "#F0E442"
  , "#0072B2", "#D55E00", "#CC79A7", "#000000" ]

-- | layer の color encoding を point 数 n の Vector に展開。
--   * 'ColorStatic'  → 全 point 同色
--   * 'ColorByCol'   → 列 (txt or num) を distinct 値ごとに palette index 割当
--   * encoding 無し  → theme の default 色
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

-- | Viridis 風 5-stop gradient (= 簡易版、 perceptually uniform に近い)。
-- t in [0, 1]。
viridis :: Double -> Text
viridis = continuousColor
  ["#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725"]

-- | P17: 任意 hex 配列の N-stop palette を t ∈ [0,1] で線形補間。
--   layout.lpContinuousPalette を渡せば spec 指定の sequential が反映される。
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

-- | P14 PS port: deterministic pseudo-random ∈ [0,1) from Int seed。
-- sin-hash トリック (= classic JS shadertoy)。 同 seed で常に同値。
hashRand :: Int -> Double
hashRand i =
  let s = sin (fromIntegral i * 12.9898) * 43758.5453
      f = fromIntegral (floor s :: Int)
  in s - f

-- | C-6 PS port: shape を Primitive (PCircle or PPath) に変換。
-- MShCircle は PCircle (= hover label 付き)、 他は PPath。
shapeToPrim :: MarkShape -> Point -> Double -> FillStyle -> Maybe StrokeStyle
            -> Maybe Text -> Primitive
shapeToPrim sh pt sz fs ms label = case sh of
  MShCircle -> PCircle pt sz fs ms label
  _         -> PPath (shapePath sh pt sz) fs ms

-- | C-6 PS port: shape 別 path 構築 (= PathSegment 列、 bezier 近似含む)。
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

-- | ggplot 同型のマーカー塗り (色 + alpha)。 hollow (中抜き) は透明・輪郭のみ。
--   plot 点 (Render.Basic) と凡例キー (Render.Layer) で**同一の装飾規則**を使うための
--   単一ソース (= 「凡例マークは plot と揃える」 規律)。
markerFillFor :: Layer -> Text -> Double -> FillStyle
markerFillFor ly c ai
  | getLast (lyHollow ly) == Just True = FillStyle c 0.0
  | otherwise                          = FillStyle c ai

-- | ggplot 同型のマーカー縁 (stroke)。 既定は**縁なし** (= 塗り点 shape 19)。
--   hollow → 点色で輪郭のみ (幅 'lyStroke'|1)。 'lyEdge' 指定時だけ縁を出す
--   (色 'lyEdgeColor'|点色、 幅 'lyEdgeWidth'|1)。 plot/凡例で共通。
markerStrokeFor :: Layer -> Text -> Maybe StrokeStyle
markerStrokeFor ly c
  | getLast (lyHollow ly) == Just True = Just (StrokeStyle c (doubleOr (lyStroke ly) 1.0))
  | getLast (lyEdge ly)   == Just True =
      Just (StrokeStyle (maybe c id (getLast (lyEdgeColor ly))) (doubleOr (lyEdgeWidth ly) 1.0))
  | otherwise                          = Nothing

-- | C-6 PS port: scatter / strip 等の i 番目 data 点に対応する shape を取得。
-- lyShapeBy 列値を data から resolve し、 cat → shape を引く。 明示の lyShapeMap が
-- あればそれを最優先、 無ければカテゴリ初出順の index で 'shapePalette' を巡回割当
-- (= ggplot @aes(shape=factor(g))@ の自動 shape scale。 colorVector の色割当と同思想)。
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

-- | 自動 shape scale の巡回パレット (ggplot 風: 丸→三角→四角→…)。
shapePalette :: [MarkShape]
shapePalette =
  [ MShCircle, MShSquare, MShTriangle, MShCross
  , MShSpade, MShHeart, MShClub, MShDiamond ]

-- | TODO-3e (2026-05-29): lySizeBy → 各点の半径 (px) Vector。
-- lySizeBy 指定の列値 (= 要 numeric) を min..max → [szLo, szHi] px に線形 map。
-- 指定無しなら lySize (or default 3.0) を全点に適用。
-- | per-point マーカー**半径** (pt) Vector を返す。
--
-- ★ Phase 34 A3: 'size' 意味論を「マーカー外接円の**直径** (pt)」に統一
-- (§2.1)。'lySize' は直径として解釈し、shapeToPrim が要求する半径 (= 直径/2) を返す。
-- 既定直径は 'defaultMarkerDiameter' (= ggplot 実測 1.65mm)。
-- sizeBy (連続 size mapping) の範囲 'lpSizeRange' も**直径**範囲 (= scale_size、
-- 既定 (6,20)pt → 半径 3..10pt)。
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

-- | Phase 30 A8: lyAlphaBy → 各点の alpha (不透明度) Vector。
-- lyAlphaBy 指定の列値 (= 要 numeric) を min..max → alpha [0.1, 1.0] に線形 map
-- (= ggplot scale_alpha 既定 range)。 指定無しなら baseAlpha (固定 lyAlpha or 既定値)
-- を全点に適用。
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

-- | カテゴリ名 (= ColTxt) を Layer から取得。 categorical bar / pie 等で labels に。
catLabelsOf :: Resolver -> Layer -> [Text]
catLabelsOf r ly = case getLast (lyEncX ly) of
  Just cr -> case resolveCol r cr of
    Just (TxtData v) -> V.toList v
    _                -> []
  Nothing -> []

-- ===========================================================================
-- 分布 chart (group × value)
-- ===========================================================================

-- | group 列 (lyEncX ?? colorBy 列) と value 列 (lyEncY) を resolve。 group は
-- categorical (= ColTxt) が普通、 ColNum でも対応 (= ColNum を distinct 値で group)。
-- 戻り値: [(group_label, [value])]
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

-- | Phase 28: 'groupedValues' を x カテゴリ軸順 ('lpXCategoryLabels') に整列する。
--   box / violin / strip / swarm / ridge は群を @zip [0..]@ で x 位置に並べるが、
--   x 軸ラベルは 'lpXCategoryLabels' (既定アルファベット順 / discrete-limits override)
--   から来る。 両者の順を一致させないと「箱は Gentoo だがラベルは Chinstrap」 のような
--   ズレが出る (= categorical 既定をアルファベット順にした際の回帰)。 軸ラベルが無い
--   (数値 x 等) ときは 'groupedValues' の順をそのまま返す。
groupedValuesOrdered :: Layout -> Resolver -> Layer -> [(Text, [Double])]
groupedValuesOrdered layout r ly =
  let gv  = groupedValues r ly
      xls = lpXCategoryLabels layout
  in if null xls then gv
     else [ (g, vs) | g <- xls, Just vs <- [lookup g gv] ]

-- | Phase 36 B1c: distribution mark (violin/strip/swarm/raincloud) の群リスト。
--   群列 ('distGroupRef' = encX ?? colorBy) があれば 'groupedValuesOrdered'、 無ければ
--   encY 全体を単一群 ("") にする (= boxplot の単一群挙動と統一)。 これにより 1 引数
--   @violin "v"@ (群なし) でも空にならず 1 つ描ける。
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

-- | dodge cell 化: (位置列, 色列) について各 (位置 index, 色 index) の値リストを作る。
--   戻り値:
--     positions = 位置カテゴリ ('lpXCategoryLabels' = 既定アルファベット順)
--     colorCats = 色カテゴリ ('lyColorCats' 優先、 無ければ色列の出現順 uniq)
--     cells     = @[(posIx, colIx, [value])]@ (空セルは除外)
--   値は encY、 NaN (= Maybe 列の Nothing) は行整列を保ったまま除外。
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

-- | dodge sub-cell の data 空間中心 (= bar 'PosDodge' と同式)。 位置カテゴリ @pix@ の
--   slot (幅 0.9) を色数 @nColor@ で等分し、 @cix@ 番目の中心を data 座標で返す。
dodgeCenterD :: Int -> Int -> Int -> Double
dodgeCenterD pix cix nColor =
  fromIntegral pix - 0.45
    + (fromIntegral cix + 0.5) * 0.9 / fromIntegral (max 1 nColor)

-- ---------------------------------------------------------------------------
-- 分布計算の共通 helper (Phase 8 B2: KDE / 四分位の重複を一元化)
--   raincloud / violin / box / density / ridge が共有する。 ggplot で言う
--   stat_density / stat_boxplot 相当を 1 箇所に集約 (= 各 geom が再利用)。
-- ---------------------------------------------------------------------------

-- | Gaussian KDE (Silverman bandwidth) を nGrid 点で評価し [(y, density)] を返す。
-- 戻り値は y 昇順。 violin/raincloud/density/ridge が共有。 grid は vals の min..max。
kdeGrid :: Int -> [Double] -> [(Double, Double)]
kdeGrid nGrid vals
  | length vals < 2 = []
  | otherwise       = kdeGridOver (minimum vals) (maximum vals) nGrid vals

-- | Phase 8 B23-fix: grid 範囲を明示する版。 ridge は全群共通の値域 [gLo, gHi] で各群を
-- 評価し、 群データ端の外でも KDE 裾を滑らかに減衰させる (= 各群自前 min/max だと裾が
-- 打ち切られて横線にならない、 PS renderRidgeLayer と同方式)。 bw は群自身の vals から。
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

-- | 5 数要約 (Tukey)。 q1/median/q3 + whisker 端 (1.5×IQR 内の最遠データ点)。
-- box / raincloud が共有。 vals はソート不要 (内部で sort)。
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

-- | 細い箱ひげ (= raincloud 中央 / 単群 box 用)。 中心 x = cx、 半幅 hw px。
-- 共通 'fiveNum' を使い whisker 足 + IQR 箱 + median 白線を返す。
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

-- | Ridge 用 group 化: encX = 値 (numeric)、 encY = 群 (categorical)。
-- groupedValues は encX を群とするため、 ridge では x/y を入れ替えた版が要る。
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

-- | Backend が実装する interface。 IO は canvas / file write のため。
class Renderer rndr where
  drawPrimitives :: rndr -> [Primitive] -> IO ()


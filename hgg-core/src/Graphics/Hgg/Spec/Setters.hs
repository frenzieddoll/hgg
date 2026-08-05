-- |
-- Module      : Graphics.Hgg.Spec.Setters
-- Description : Top-level setters that build VisualSpec (title / theme / axis / legend / annot etc.)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'Graphics.Hgg.Spec' の module 分割で切り出し。 'VisualSpec' を
-- `<>` で組み立てる top-level setter 群 ('layer' / 'title' / 'theme' /
-- 'facet' 系 / 'legend' 系 / @annot@ 系 / inset / 図サイズ / font setter 等) と
-- 'Labs'、 VisualSpec 依存の mark 構築子 3 種 ('histogramWide' / 'distCols' /
-- 'ridgeAutoFlip') を持つ。 図の合成演算子は 'Graphics.Hgg.Spec.Concat' 側。
-- 公開 API は従来どおり 'Graphics.Hgg.Spec' (facade) が re-export する。
-- 挙動・出力は完全に不変。
-- [English]: Split out of 'Graphics.Hgg.Spec' as part of its module split.
-- Holds the top-level setters that build 'VisualSpec' via `<>` ('layer' /
-- 'title' / 'theme' / the 'facet' family / the 'legend' family / the
-- @annot@ family / inset / figure size / font setters, etc.), 'Labs', and
-- the three VisualSpec-dependent mark constructors ('histogramWide' /
-- 'distCols' / 'ridgeAutoFlip'). Figure-composition operators live in
-- 'Graphics.Hgg.Spec.Concat'. The public API is unchanged:
-- 'Graphics.Hgg.Spec' (the facade) still re-exports everything. Behavior
-- and output are fully unchanged.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Setters
  ( -- * layer 装着 + 基本 setter
    layer, layers, purePlot, title, subtitle, caption, tag, xLabel, yLabel
  , Labs(..), labs, emptyLabs
  , theme, facet, facetWrap, facetGrid, facetCols, facetScales, facetSpace
  , subplots, subplotCols, subplotWidths, subplotHeights, subplotTags
  , repeatFields, selectPanels, selectedSubplots
  , scaleXDiscreteLimits, scaleYDiscreteLimits, applyDiscreteLimits, reindexLayer
    -- * theme override setter
  , plotBg, themePlotBg, panelFill, panelBorder, gridColor, themeGrid, themeAxisLine
  , themeGridMajor, themeGridMinor, themeLegendPos
  , themeTickLength, themeTickDir, themePlotMargin, themeBaseFontSize
  , themeAxisText, themeAxisTitle, themeLegendKeySize
  , axisColor, textColor, tickColor, titleColor, titleHjust
  , stripFill, themeStrip, legendKeyBg
  , themeTitleFont, themeAxisLabelFont, themeTickFont, themeLegendFont
  , themeFontFamily
  , themeAxisTextAngle, themeAxisTextAngleX, themeAxisTextAngleY
  , axisTextAngleXOf, axisTextAngleYOf
    -- * 合成 preset (cowplot 風)
  , themeCowplot, themeMinimalGrid, themeMap
  , themeCowplotSized, themeMinimalGridSized, themeMapSized
    -- * VisualSpec 依存の mark 構築子 (Constructors に置けない 3 種)
  , histogramWide, distCols, ridgeAutoFlip
    -- * 軸 / 凡例 / 装飾 / 座標系 / サイズ
  , xAxis, yAxis, yAxisRight, toLeftY, toRightY
  , legend, legendPos, legendOff, legendTitle, legendReverse, legendNcol, legendNrow
  , guideColorNone
  , refLine, refVertical, refHorizontal, refIdentity
  , annotate, annotText, annotTextP, annotLine, annotLineP
  , annotRect, annotRectP, annotArrow, annotArrowP
  , inset, insetAt, insetElement
  , marginal, marginalX, marginalY
  , palette, paletteGGplot, continuousPalette
  , scaleColorManual, scaleColorGradient2, scaleSize
  , coordFlip, coordPolar, coordPolarY, coordCartesian, coordCartesianX, coordCartesianY
  , reverseX, reverseY, aspectRatio
  , width, height, widthUnit, heightUnit, widthMm, heightMm, dpi
    -- * font setter
  , titleFont, axisLabelFont, tickFont, legendFont
  ) where

import           Data.Maybe      (catMaybes)
import           Data.Monoid     (First (..), Last (..))
import           Data.Text       (Text)
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

import           Graphics.Hgg.Unit (Length, Pos (..), mm, (*~))
import           Graphics.Hgg.Spec.Axis (AxisSpec)
import           Graphics.Hgg.Spec.Column
import           Graphics.Hgg.Spec.Bake (bakeSpec)
import           Graphics.Hgg.Spec.Constructors (binCount, histogram, (<+>))
import           Graphics.Hgg.Spec.Decoration
import           Graphics.Hgg.Spec.Layer
import           Graphics.Hgg.Spec.Mark
import           Graphics.Hgg.Spec.Theme (Margin (..), ThemeName (..),
                                          ThemeOverride (..), TickDir)
import           Graphics.Hgg.Spec.Visual


-- ===========================================================================
-- Top-level setters
-- ===========================================================================

-- | [日本語]: spec の純粋値起点 (= 'mempty' alias)。 mempty 直接でも良いが、
--   「これは plot spec の最初の値ですよ」 という意図を名前で示す。 副作用関数
--   (@plot@ / @saveSVG@ 等) との対比で `pure-` prefix。
--   [English]: The pure starting value for a spec (an alias for
--   'mempty'). Using mempty directly would also work, but the name
--   signals the intent "this is the initial value of a plot spec" — the
--   `pure-` prefix contrasts with effectful functions (@plot@ /
--   @saveSVG@, etc.).
purePlot :: VisualSpec
purePlot = mempty

-- | [日本語]: 'Layer' を 'VisualSpec' に lift (= layer リストの単一要素
--   spec)。
--   [English]: Lifts a 'Layer' into a 'VisualSpec' (a spec with a
--   single-element layer list).
layer :: Layer -> VisualSpec
layer l = mempty { vsLayers = [l] }

-- | [日本語]: 'layer' のリスト版 (= @layer . mconcat@)。 hvega 風のリスト書きが
--   好みの場合に: @layers [scatter "x" "y", colorBy "group"] =
--   layer (scatter "x" "y" <> colorBy "group")@。 等価な別名であり '<>' 版が正典
--   (doc の例は '<>' で統一)。 外部フィードバック (公開版 hgg へのコメント) を
--   受けて追加した。
--   [English]: A list version of 'layer' (@layer . mconcat@). For those
--   who prefer hvega-style list syntax:
--   @layers [scatter "x" "y", colorBy "group"] =
--   layer (scatter "x" "y" <> colorBy "group")@. An equivalent alias — the
--   '<>' form is canonical (doc examples stick to '<>'). Added in response
--   to external feedback (comments on the public hgg release).
layers :: [Layer] -> VisualSpec
layers = layer . mconcat

title, xLabel, yLabel :: Text -> VisualSpec
title  t = mempty { vsTitle  = Last (Just t) }
xLabel t = mempty { vsXLabel = Last (Just t) }
yLabel t = mempty { vsYLabel = Last (Just t) }

-- | [日本語]: 凡例タイトル (= ggplot scale_color_*(name=) / labs(color=))。
--   color/fill/shape/linetype の凡例ヘッダに表示。 軸タイトルは
--   'xLabel'/'yLabel' を使う (= positional scale の name = 軸ラベル)。
--   [English]: The legend title (like ggplot's scale_color_*(name=) /
--   labs(color=)). Shown as the header of the color/fill/shape/linetype
--   legend. Use 'xLabel'/'yLabel' for axis titles (a positional scale's
--   name is the axis label).
legendTitle :: Text -> VisualSpec
legendTitle t = mempty { vsLegendTitle = Last (Just t) }

-- | [日本語]: labs サブシステムの個別 setter (= ggplot
--   labs(subtitle=,caption=,tag=))。 'subtitle' = title 直下の小見出し、
--   'caption' = 図右下の注記、 'tag' = 左上隅のタグ。
--   [English]: Individual setters in the labs subsystem (like ggplot's
--   labs(subtitle=,caption=,tag=)). 'subtitle' is the sub-heading right
--   under the title, 'caption' is the note in the bottom-right corner,
--   and 'tag' is the tag in the top-left corner.
subtitle, caption, tag :: Text -> VisualSpec
subtitle t = mempty { vsSubtitle = Last (Just t) }
caption  t = mempty { vsCaption  = Last (Just t) }
tag      t = mempty { vsTag      = Last (Just t) }

-- | [日本語]: ggplot @labs()@ 相当のまとめ setter。 各フィールドは 'Maybe' で
--   「指定しない」 を表す。 @labs emptyLabs { labsTitle = Just "T", labsX = Just "x" }@
--   のように 'emptyLabs' を起点に必要な label だけ埋める。 @labsColor@ は凡例
--   タイトル ('legendTitle')。 指定した label を 'mconcat' で合成するので既存
--   setter と等価。
--   [English]: A bundled setter equivalent to ggplot's @labs()@. Each
--   field is a 'Maybe' representing "not specified". Start from
--   'emptyLabs' and fill only the labels you need, e.g.
--   @labs emptyLabs { labsTitle = Just "T", labsX = Just "x" }@.
--   @labsColor@ is the legend title ('legendTitle'). The specified labels
--   are combined with 'mconcat', so this is equivalent to the individual
--   setters.
data Labs = Labs
  { labsTitle    :: Maybe Text
  , labsSubtitle :: Maybe Text
  , labsCaption  :: Maybe Text
  , labsTag      :: Maybe Text
  , labsX        :: Maybe Text
  , labsY        :: Maybe Text
  , labsColor    :: Maybe Text   -- = 凡例タイトル ('legendTitle')
  } deriving (Show, Eq)

-- | [日本語]: 全フィールド未指定の 'Labs' 起点 (= record update のベース)。
--   [English]: The starting 'Labs' value with every field unspecified
--   (the base for record updates).
emptyLabs :: Labs
emptyLabs = Labs Nothing Nothing Nothing Nothing Nothing Nothing Nothing

labs :: Labs -> VisualSpec
labs lb = mconcat $ catMaybes
  [ title       <$> labsTitle    lb
  , subtitle    <$> labsSubtitle lb
  , caption     <$> labsCaption  lb
  , tag         <$> labsTag      lb
  , xLabel      <$> labsX        lb
  , yLabel      <$> labsY        lb
  , legendTitle <$> labsColor    lb
  ]

theme :: ThemeName -> VisualSpec
theme t = mempty { vsTheme = Last (Just t) }

-- | [日本語]: element 単位 theme override の setter 群 (ggplot theme(element_*)
--   相当)。 `theme ThemeGrey <> themeGrid False <> panelFill "#fafafa"` のように
--   `<>` で重ねる。
--   [English]: A family of per-element theme-override setters (like
--   ggplot's theme(element_*)). Stack them with `<>`, e.g.
--   `theme ThemeGrey <> themeGrid False <> panelFill "#fafafa"`.
themeGrid :: Bool -> VisualSpec       -- panel.grid on/off (= major/minor 両方の糖衣)
themeGrid b = mempty { vsThemeOverride = mempty { toShowGrid = Last (Just b) } }

-- | [日本語]: grid major/minor の個別 on/off (cowplot @theme_minimal_grid()@
--   等)。 優先順は 個別 > 一括 'themeGrid' > preset。
--   `theme ThemeMinimal <> themeGridMinor False` のように重ねる。
--   [English]: Individual on/off for grid major/minor (like cowplot's
--   @theme_minimal_grid()@). Priority is individual > the bulk
--   'themeGrid' > preset. Stack it like
--   `theme ThemeMinimal <> themeGridMinor False`.
themeGridMajor :: Bool -> VisualSpec  -- panel.grid.major on/off
themeGridMajor b = mempty { vsThemeOverride = mempty { toShowGridMajor = Last (Just b) } }

themeGridMinor :: Bool -> VisualSpec  -- panel.grid.minor on/off
themeGridMinor b = mempty { vsThemeOverride = mempty { toShowGridMinor = Last (Just b) } }

-- | [日本語]: legend.position を theme に焼き込む (自作 theme を `<>` で配る用)。
--   図レベルの 'legendPos' が指定されていればそちらが優先 (ggplot の theme() と
--   個別指定の関係に同じ)。
--   [English]: Bakes legend.position into the theme (for distributing a
--   custom theme via `<>`). If the figure-level 'legendPos' is set, it
--   takes priority (the same relationship as ggplot's theme() vs. an
--   individual setting).
themeLegendPos :: LegendPosition -> VisualSpec
themeLegendPos p = mempty { vsThemeOverride = mempty { toLegendPos = Last (Just p) } }

-- | [日本語]: 軸目盛線の長さ (pt) を theme に焼き込む (ggplot
--   @axis.ticks.length@ 相当、 既定 2.75pt)。 tick 長は軸ラベル位置・マージン
--   予約にも波及する (computeLayout が実効値を参照)。
--   [English]: Bakes the axis tick length (pt) into the theme (like
--   ggplot's @axis.ticks.length@, default 2.75pt). The tick length also
--   affects axis-label position and margin reservation (computeLayout
--   reads the effective value).
themeTickLength :: Double -> VisualSpec
themeTickLength d = mempty { vsThemeOverride = mempty { toTickLength = Last (Just d) } }

-- | [日本語]: 軸目盛線の向き ('Graphics.Hgg.Spec.Theme.TickOut' 外 /
--   'Graphics.Hgg.Spec.Theme.TickIn' 内 / 'Graphics.Hgg.Spec.Theme.TickBoth'
--   両)。 TickIn は panel 外に出ないため、 軸ラベルは tick 長 0 と同じ位置に寄る
--   (ggplot の負 axis.ticks.length と同挙動)。
--   [English]: The axis tick direction ('Graphics.Hgg.Spec.Theme.TickOut'
--   outward / 'Graphics.Hgg.Spec.Theme.TickIn' inward /
--   'Graphics.Hgg.Spec.Theme.TickBoth' both). Since TickIn does not extend
--   outside the panel, axis labels move to the same position as a tick
--   length of 0 (the same behavior as ggplot's negative
--   axis.ticks.length).
themeTickDir :: TickDir -> VisualSpec
themeTickDir d = mempty { vsThemeOverride = mempty { toTickDir = Last (Just d) } }

-- | [日本語]: 図の外周余白 (pt) を theme に焼き込む (ggplot @plot.margin@
--   相当)。 引数順は ggplot @margin(t, r, b, l)@ と同じ。 指定時は自動算出の
--   外周分 (各辺 half_line = 5.5pt) を __置き換える__ (加算ではない)。 軸ラベル・
--   title 帯・凡例などの内側予約は従来どおり自動算出のまま。
--   [English]: Bakes the figure's outer margin (pt) into the theme (like
--   ggplot's @plot.margin@). Argument order matches ggplot's
--   @margin(t, r, b, l)@. When specified, it __replaces__ the
--   automatically computed outer margin (each side's half_line = 5.5pt) —
--   it does not add to it. Inner reservations such as axis labels, the
--   title band and the legend are still computed automatically as
--   before.
themePlotMargin :: Double -> Double -> Double -> Double -> VisualSpec
themePlotMargin t r b l =
  mempty { vsThemeOverride = mempty { toPlotMargin = Last (Just (Margin t r b l)) } }

-- | [日本語]: base font size (pt) を theme に焼き込む (ggplot @base_size@
--   相当、 既定 11)。 各 slot の既定 font size はこれからの相対倍率で派生する
--   (title ×1.2 / axis.title ×1 / axis.text ×0.8 / legend.title ×1 /
--   legend.text ×0.8)。 'themeTitleFont' 等の個別指定 (fsSize) があればそちらが
--   優先。
--   [English]: Bakes the base font size (pt) into the theme (like
--   ggplot's @base_size@, default 11). Each slot's default font size is
--   derived from this via a relative multiplier (title ×1.2 /
--   axis.title ×1 / axis.text ×0.8 / legend.title ×1 / legend.text ×0.8).
--   An individual override (fsSize) such as 'themeTitleFont' takes
--   priority when present.
themeBaseFontSize :: Double -> VisualSpec
themeBaseFontSize s =
  mempty { vsThemeOverride = mempty { toBaseFontSize = Last (Just s) } }

-- | [日本語]: 軸目盛ラベル文字 (ggplot @axis.text@) の表示。 False =
--   element_blank 相当で文字のみ消える (tick 線の有無は 'themeTickLength' と
--   独立)。 ラベル文字ぶんの margin 予約も連動して落ちる。 既定は 'ThemeVoid'
--   のみ False。
--   [English]: Whether to show axis tick-label text (ggplot's
--   @axis.text@). False is equivalent to element_blank — only the text
--   disappears (independent of whether tick marks are shown, controlled
--   by 'themeTickLength'). The margin reserved for label text is also
--   dropped accordingly. Default is False only for 'ThemeVoid'.
themeAxisText :: Bool -> VisualSpec   -- axis.text on/off
themeAxisText b = mempty { vsThemeOverride = mempty { toShowAxisText = Last (Just b) } }

-- | [日本語]: 軸タイトル (ggplot @axis.title@) の表示。 False = element_blank
--   相当 ('xLabel' / 'yLabel' 指定があっても描かず margin も予約しない)。
--   既定は 'ThemeVoid' のみ False。
--   [English]: Whether to show the axis title (ggplot's @axis.title@).
--   False is equivalent to element_blank (even if 'xLabel' / 'yLabel' is
--   set, it is neither drawn nor reserves margin). Default is False only
--   for 'ThemeVoid'.
themeAxisTitle :: Bool -> VisualSpec  -- axis.title on/off
themeAxisTitle b = mempty { vsThemeOverride = mempty { toShowAxisTitle = Last (Just b) } }

-- | [日本語]: 凡例キー 1 辺 (pt、 ggplot @legend.key.size@ 相当)。 キーの行
--   pitch = キー辺なので凡例の行間もこれで決まる (既定 = 1.2 lines = 1.2 × base ×
--   1.3133、 base 11 で 17.34pt)。 凡例の margin 予約にも波及する。
--   [English]: The legend key's side length (pt, like ggplot's
--   @legend.key.size@). Since the key's row pitch equals the key side,
--   this also determines the legend's line spacing (default = 1.2 lines
--   = 1.2 × base × 1.3133, i.e. 17.34pt at base 11). It also affects the
--   legend's margin reservation.
themeLegendKeySize :: Double -> VisualSpec  -- legend.key.size (pt)
themeLegendKeySize d = mempty { vsThemeOverride = mempty { toLegendKeySize = Last (Just d) } }

-- ===========================================================================
-- 合成 preset (cowplot 風)
-- ===========================================================================
-- 'ThemeName' の enum には足さず (JSON parity 維持)、 既存 setter を `<>` で
-- 束ねた 'VisualSpec' 値として提供する = 「自作 theme は setter 合成で表現する」
-- 方針の自己適用。 後ろに setter を重ねれば個別上書きできる
-- (例 @themeCowplot <> themeTickLength 5@)。 数値は R cowplot 1.2.0 の既定
-- (基準 font_size N: half_line=N/2 → margin N/2 pt / tick N/4 pt、 文字は
-- title ×16/14 bold / axis.title ×1 / axis.text ×12/14 の相対倍率、 黒基調)。
-- ★ 'themeBaseFontSize' を焼き込む sized 版が本体。 tick 長・外周
-- margin は base 派生の既定値に任せ、 明示 setter は置かない (= preset 後の
-- 'themeBaseFontSize' 上書きにも spacing が連動する)。

-- | [日本語]: cowplot @theme_cowplot(font_size = N)@ 相当 = grid なし・下/左の
--   黒軸線・外向き tick N/4 pt・外周余白 N/2 pt・黒基調の文字・背景透過。
--   [English]: The equivalent of cowplot's
--   @theme_cowplot(font_size = N)@: no grid, black axis lines on the
--   bottom/left, outward ticks of N/4 pt, an outer margin of N/2 pt,
--   black-toned text, and a transparent background.
themeCowplotSized :: Double -> VisualSpec
themeCowplotSized n =
     theme ThemeClassic
  <> themeBaseFontSize n
  <> cowplotFontsSized n
  <> axisColor "#000000" <> tickColor "#000000"
  <> textColor "#000000" <> titleColor "#000000"
  <> themePlotBg False   -- ★ cowplot は rect fill NA = 背景透過
  <> themeLegendKeySize (1.1 * n)  -- ★ cowplot は legend.key.size = 1.1×font_size

-- | [日本語]: cowplot @theme_cowplot()@ 相当 (= 既定 font_size 14)。
--   [English]: The equivalent of cowplot's @theme_cowplot()@ (default
--   font_size 14).
themeCowplot :: VisualSpec
themeCowplot = themeCowplotSized 14

-- | [日本語]: cowplot @theme_minimal_grid(font_size = N)@ 相当 = major grid
--   (grey85) のみ・軸線/枠/tick なし・黒基調の文字。
--   [English]: The equivalent of cowplot's
--   @theme_minimal_grid(font_size = N)@: only the major grid (grey85), no
--   axis lines/border/ticks, black-toned text.
themeMinimalGridSized :: Double -> VisualSpec
themeMinimalGridSized n =
     theme ThemeMinimal
  <> themeBaseFontSize n
  <> cowplotFontsSized n
  <> textColor "#000000" <> titleColor "#000000"
  <> themeGridMinor False
  <> gridColor "#d9d9d9"
  <> panelBorder False
  <> themeTickLength 0
  <> themePlotBg False   -- ★ cowplot は rect fill NA = 背景透過
  <> themeLegendKeySize (1.1 * n)  -- ★ cowplot は legend.key.size = 1.1×font_size

-- | [日本語]: cowplot @theme_minimal_grid()@ 相当 (= 既定 font_size 14)。
--   [English]: The equivalent of cowplot's @theme_minimal_grid()@
--   (default font_size 14).
themeMinimalGrid :: VisualSpec
themeMinimalGrid = themeMinimalGridSized 14

-- | [日本語]: cowplot @theme_map(font_size = N)@ 相当 = 軸線・grid・枠・tick
--   線・軸ラベル文字・軸タイトルを全て消す (★ 'ThemeVoid' 既定で axis.text /
--   axis.title も blank)。 タイトル系と凡例は残る。 facet strip は theme_map が
--   grey80 で残すため 'stripFill' を明示 (ThemeVoid 既定は strip なし)。
--   [English]: The equivalent of cowplot's @theme_map(font_size = N)@:
--   removes axis lines, grid, border, tick marks, axis-label text and
--   axis titles entirely (★ 'ThemeVoid' also blanks axis.text /
--   axis.title by default). Title elements and the legend remain. Since
--   theme_map keeps the facet strip at grey80, 'stripFill' is set
--   explicitly (ThemeVoid's default has no strip).
themeMapSized :: Double -> VisualSpec
themeMapSized n =
     theme ThemeVoid
  <> themeBaseFontSize n
  <> cowplotFontsSized n
  <> textColor "#000000" <> titleColor "#000000"
  <> themeTickLength 0
  <> themePlotBg False   -- ★ cowplot は rect fill NA = 背景透過
  <> stripFill "#cccccc" -- ★ theme_map は strip.background grey80 を残す
  <> themeLegendKeySize (1.1 * n)  -- ★ cowplot は legend.key.size = 1.1×font_size

-- | [日本語]: cowplot @theme_map()@ 相当 (= 既定 font_size 14)。
--   [English]: The equivalent of cowplot's @theme_map()@ (default
--   font_size 14).
themeMap :: VisualSpec
themeMap = themeMapSized 14

-- | [日本語]: preset 3 種で共有する cowplot(N) の文字設定 (title は bold)。
--   倍率は cowplot 既定 rel_large = 16/14 (title) / rel_small = 12/14
--   (axis.text・legend)。 ggplot 既定倍率 (1.2/0.8) と異なるため base 派生に
--   任せず明示する。
--   [English]: The cowplot(N) font settings shared by the three presets
--   (title is bold). The multipliers follow cowplot's defaults
--   rel_large = 16/14 (title) / rel_small = 12/14 (axis.text, legend).
--   Since these differ from ggplot's default multipliers (1.2/0.8), they
--   are set explicitly rather than left to derive from base.
cowplotFontsSized :: Double -> VisualSpec
cowplotFontsSized n =
     themeTitleFont     (fontSize (n * 16 / 14) <> fontWeight "bold")
  <> themeAxisLabelFont (fontSize n)
  <> themeTickFont      (fontSize (n * 12 / 14))
  <> themeLegendFont    (fontSize (n * 12 / 14))

panelFill :: Text -> VisualSpec       -- panel.background fill (= 塗り on + 色指定)
panelFill c = mempty { vsThemeOverride = mempty { toPanelBg = Last (Just c), toShowPanel = Last (Just True) } }

panelBorder :: Bool -> VisualSpec     -- panel.border on/off
panelBorder b = mempty { vsThemeOverride = mempty { toShowBorder = Last (Just b) } }

themeAxisLine :: Bool -> VisualSpec   -- axis.line (下/左 2 辺) on/off
themeAxisLine b = mempty { vsThemeOverride = mempty { toShowAxisLine = Last (Just b) } }

gridColor :: Text -> VisualSpec       -- panel.grid colour
gridColor c = mempty { vsThemeOverride = mempty { toGridColor = Last (Just c) } }

plotBg :: Text -> VisualSpec          -- plot.background fill
plotBg c = mempty { vsThemeOverride = mempty { toPlotBg = Last (Just c) } }

-- | [日本語]: plot.background を塗るか (★)。 @themePlotBg False@ = 塗らない
--   (= 透過、 ggplot @plot.background = element_blank()@ / cowplot fill NA
--   相当)。
--   [English]: Whether to paint plot.background. ★ @themePlotBg False@
--   means no paint (transparent, equivalent to ggplot's
--   @plot.background = element_blank()@ / cowplot's fill NA).
themePlotBg :: Bool -> VisualSpec     -- plot.background 塗り on/off
themePlotBg b = mempty { vsThemeOverride = mempty { toShowBackground = Last (Just b) } }

axisColor :: Text -> VisualSpec       -- axis 線/目盛り色
axisColor c = mempty { vsThemeOverride = mempty { toAxisColor = Last (Just c) } }

textColor :: Text -> VisualSpec       -- 文字色
textColor c = mempty { vsThemeOverride = mempty { toTextColor = Last (Just c) } }

-- | [日本語]: theme 経由の font setter 群 (ggplot
--   theme(plot.title=element_text(...)) 等)。 vsTitleFont 等の専用 setter より
--   優先される (= 後付け theme 上書き)。 `<>` で重ねる。
--   [English]: A family of font setters via theme (like ggplot's
--   theme(plot.title=element_text(...))). Takes priority over dedicated
--   setters such as vsTitleFont (a later theme override applied on top).
--   Stack them with `<>`.
themeTitleFont :: FontSpec -> VisualSpec      -- plot.title
themeTitleFont f = mempty { vsThemeOverride = mempty { toTitleFont = Last (Just f) } }

themeAxisLabelFont :: FontSpec -> VisualSpec  -- axis.title
themeAxisLabelFont f = mempty { vsThemeOverride = mempty { toAxisLabelFont = Last (Just f) } }

themeTickFont :: FontSpec -> VisualSpec       -- axis.text
themeTickFont f = mempty { vsThemeOverride = mempty { toTickFont = Last (Just f) } }

themeLegendFont :: FontSpec -> VisualSpec     -- legend.title / legend.text
themeLegendFont f = mempty { vsThemeOverride = mempty { toLegendFont = Last (Just f) } }

-- | [日本語]: ★ 全 text slot 共通の font family (ggplot
--   theme(text = element_text(family=...)) 相当)。 slot 別 FontSpec の
--   'Graphics.Hgg.Spec.fontFamily' 指定があればそちらが優先 ('Graphics.Hgg.Render.Common.mkFontTS'
--   解決)。 slot 丸ごとの 'themeTitleFont' 等と違い preset の fontSize
--   焼き込みを潰さない。 PNG backend は family 名を正規化してフォントファイルを
--   解決する (不在なら既定フォント + stderr 警告)。
--   [English]: ★ The font family shared by all text slots (like ggplot's
--   theme(text = element_text(family=...))). A per-slot FontSpec setting
--   via 'Graphics.Hgg.Spec.fontFamily' takes priority when present
--   (resolved by 'Graphics.Hgg.Render.Common.mkFontTS'). Unlike whole-slot setters such as
--   'themeTitleFont', it does not clobber a preset's baked-in fontSize.
--   The PNG backend normalizes the family name to resolve a font file (if
--   absent, it falls back to the default font plus a stderr warning).
themeFontFamily :: Text -> VisualSpec
themeFontFamily fam = mempty { vsThemeOverride = mempty { toFontFamily = Last (Just fam) } }

-- | [日本語]: axis.text の回転角 (度) を theme から指定。 per-axis 'Graphics.Hgg.Spec.Axis.axisRotate'
--   未指定時の fallback。
--   [English]: Sets the axis.text rotation angle (degrees) from the
--   theme. A fallback used when the per-axis 'Graphics.Hgg.Spec.Axis.axisRotate' is not
--   specified.
themeAxisTextAngle :: Double -> VisualSpec
themeAxisTextAngle a = mempty { vsThemeOverride = mempty { toAxisTextAngle = Last (Just a) } }

-- | [日本語]: axis.text の __x 軸のみ__ の回転角 (度・CCW) を theme から指定。
--   共通 'themeAxisTextAngle' より優先。 per-axis 'xAxis (axisRotate …)' が更に
--   優先。
--   [English]: Sets the rotation angle (degrees, CCW) for axis.text on
--   __the x axis only__, from the theme. Takes priority over the common
--   'themeAxisTextAngle'; the per-axis 'xAxis (axisRotate …)' takes
--   priority over this.
themeAxisTextAngleX :: Double -> VisualSpec
themeAxisTextAngleX a = mempty { vsThemeOverride = mempty { toAxisTextAngleX = Last (Just a) } }

-- | [日本語]: axis.text の __y 軸のみ__ の回転角 (度・CCW) を theme から指定。
--   [English]: Sets the rotation angle (degrees, CCW) for axis.text on
--   __the y axis only__, from the theme.
themeAxisTextAngleY :: Double -> VisualSpec
themeAxisTextAngleY a = mempty { vsThemeOverride = mempty { toAxisTextAngleY = Last (Just a) } }

-- | [日本語]: theme の x 軸 axis.text 回転角を解決 (軸別 'toAxisTextAngleX' >
--   共通 'toAxisTextAngle')。 'Graphics.Hgg.Spec.Axis.resolveAxisAngle' の theme fallback 引数に渡す。
--   [English]: Resolves the theme's x-axis axis.text rotation angle
--   (the per-axis 'toAxisTextAngleX' takes priority over the common
--   'toAxisTextAngle'). Passed as the theme fallback argument to
--   'Graphics.Hgg.Spec.Axis.resolveAxisAngle'.
axisTextAngleXOf :: ThemeOverride -> Last Double
axisTextAngleXOf o = toAxisTextAngle o <> toAxisTextAngleX o

-- | [日本語]: theme の y 軸 axis.text 回転角を解決 (軸別 'toAxisTextAngleY' >
--   共通 'toAxisTextAngle')。
--   [English]: Resolves the theme's y-axis axis.text rotation angle
--   (the per-axis 'toAxisTextAngleY' takes priority over the common
--   'toAxisTextAngle').
axisTextAngleYOf :: ThemeOverride -> Last Double
axisTextAngleYOf o = toAxisTextAngle o <> toAxisTextAngleY o

-- | [日本語]: facet strip.background の塗り色を指定 (= 塗り on + 色)。
--   [English]: Sets the fill color for the facet strip.background
--   (turns fill on and sets the color).
stripFill :: Text -> VisualSpec
stripFill c = mempty { vsThemeOverride = mempty { toStripBg = Last (Just c), toShowStrip = Last (Just True) } }

-- | [日本語]: facet strip 矩形の on/off。
--   [English]: Toggles the facet strip rectangle on/off.
themeStrip :: Bool -> VisualSpec
themeStrip b = mempty { vsThemeOverride = mempty { toShowStrip = Last (Just b) } }

-- | [日本語]: プリセット専用だった 4 項目の theme 上書き setter (= 全プロパティ
--   `<>` 上書き)。 `theme ThemeGrey <> titleHjust 0.5 <> legendKeyBg "#fff"` の
--   ように重ねる。
--   [English]: Theme-override setters for four items that used to be
--   preset-only (all properties can be overridden with `<>`). Stack them
--   like `theme ThemeGrey <> titleHjust 0.5 <> legendKeyBg "#fff"`.
titleHjust :: Double -> VisualSpec    -- plot.title の水平揃え (0=左、 0.5=中央)
titleHjust h = mempty { vsThemeOverride = mempty { toTitleHjust = Last (Just h) } }

titleColor :: Text -> VisualSpec      -- plot.title / axis.title の文字色
titleColor c = mempty { vsThemeOverride = mempty { toTitleColor = Last (Just c) } }

tickColor :: Text -> VisualSpec       -- 軸目盛線 (tick mark) の色
tickColor c = mempty { vsThemeOverride = mempty { toTickLineColor = Last (Just c) } }

legendKeyBg :: Text -> VisualSpec     -- legend.key 背景塗り色 ("" なら塗らない)
legendKeyBg c = mempty { vsThemeOverride = mempty { toLegendKeyBg = Last (Just c) } }

facet :: ColRef -> VisualSpec
facet c = mempty { vsFacet = Last (Just c) }

-- | [日本語]: facet_wrap(~c, ncol=n)。 c で分割し n 列で複数行に折り返す。
--   ncol 未使用 (= 'facet' のみ) なら従来の 1 行 N 列。
--   [English]: facet_wrap(~c, ncol=n). Splits by c and wraps into
--   multiple rows of n columns. When ncol is unused ('facet' alone), it
--   falls back to the original single row of N columns.
facetWrap :: ColRef -> Int -> VisualSpec
facetWrap c n = mempty { vsFacet = Last (Just c), vsFacetNcol = Last (Just n) }

-- | [日本語]: facet の列数のみ指定 (= 既存 'facet' と併用)。
--   [English]: Specifies only the facet column count (used together with
--   the existing 'facet').
facetCols :: Int -> VisualSpec
facetCols n = mempty { vsFacetNcol = Last (Just n) }

-- | [日本語]: facet_wrap の scale 共有方式 (= ggplot facet_wrap(scales=))。
--   'FacetFixed' (既定) = 共通 domain、 'FacetFree'/'FacetFreeX'/'FacetFreeY' =
--   該当軸を panel ごとに独立 domain に。 free な軸は全 panel に軸を表示する。
--   'facet' と併用。
--   [English]: The scale-sharing mode for facet_wrap (like ggplot's
--   facet_wrap(scales=)). 'FacetFixed' (default) uses a shared domain;
--   'FacetFree'/'FacetFreeX'/'FacetFreeY' give the corresponding axis an
--   independent domain per panel. A free axis is drawn on every panel.
--   Used together with 'facet'.
facetScales :: FacetScales -> VisualSpec
facetScales fs = mempty { vsFacetScales = Last (Just fs) }

-- | [日本語]: facet_grid の panel サイズ配分 (= ggplot facet_grid(space=))。
--   'SpaceFree' 等で free 軸の track 幅/高を data 範囲に比例配分する。 通常
--   'facetScales' と併用。 facet_grid のみ有効。
--   [English]: The panel-size allocation for facet_grid (like ggplot's
--   facet_grid(space=)). 'SpaceFree' etc. allocate a free axis's track
--   width/height proportionally to its data range. Usually used together
--   with 'facetScales'; only effective for facet_grid.
facetSpace :: FacetSpace -> VisualSpec
facetSpace fs = mempty { vsFacetSpace = Last (Just fs) }

-- | [日本語]: facet_grid(row ~ col)。 row 変数の levels で行、 col 変数の
--   levels で列を作り 2 次元の cross 配置にする。 strip は上 (col 名)・右
--   (row 名)、 軸は最下行 x・左端列 y のみ (ggplot facet_grid 既定)。
--   [English]: facet_grid(row ~ col). Builds rows from the row variable's
--   levels and columns from the col variable's levels, giving a 2D
--   cross-tabulated layout. Strips appear at the top (col name) and right
--   (row name); axes appear only on the bottom row (x) and left column
--   (y) — ggplot's facet_grid default.
facetGrid :: ColRef -> ColRef -> VisualSpec
facetGrid rowC colC = mempty { vsFacetRow = Last (Just rowC)
                             , vsFacetCol = Last (Just colC) }

-- | [日本語]: panel grid (= facet とは独立、 各 spec を独立 panel として並べる)。
--   facet は 1 列でデータを分割するのに対し、 subplots は完全に別 spec を
--   並べる。 DoE の MainEffects (= 複数 factor を横並び) で使う。
--   [English]: A panel grid (independent of facet — lays out separate
--   specs as independent panels). Whereas facet splits data by a single
--   column, subplots lays out entirely distinct specs. Used for DoE's
--   MainEffects (multiple factors side by side).
subplots :: [VisualSpec] -> VisualSpec
subplots ss = mempty { vsSubplots = ss }

-- | [日本語]: P18: subplots の 2D grid 折り返し列数。
--   [English]: P18: the wrap column count for the subplots 2D grid.
subplotCols :: Int -> VisualSpec
subplotCols n = mempty { vsSubplotCols = Last (Just n) }

-- | [日本語]: subplot 列の相対幅 (cowplot @plot_grid(rel_widths=)@ 相当)。
--   統一グリッドの列 index 順の重みで、 列数に対して不足分は 1 で埋める
--   (エラーにしない)。 @(a <-> b) <> subplotWidths [1.3, 1]@ のように使う。
--   [English]: The relative widths of subplot columns (like cowplot's
--   @plot_grid(rel_widths=)@). Weights in the unified grid's column-index
--   order; any shortfall relative to the column count is padded with 1
--   (not an error). Use it like
--   @(a <-> b) <> subplotWidths [1.3, 1]@.
subplotWidths :: [Double] -> VisualSpec
subplotWidths ws = mempty { vsSubplotWidths = Last (Just ws) }

-- | [日本語]: subplot 行の相対高 (cowplot @plot_grid(rel_heights=)@ 相当)。
--   [English]: The relative heights of subplot rows (like cowplot's
--   @plot_grid(rel_heights=)@).
subplotHeights :: [Double] -> VisualSpec
subplotHeights hs = mempty { vsSubplotHeights = Last (Just hs) }

-- | [日本語]: subplot panel の自動タグ (cowplot @plot_grid(labels="AUTO")@
--   相当)。 統一グリッドの panel 列挙順に \"A\",\"B\",… ('TagUpper') \/
--   \"a\",\"b\",… ('TagLower') \/ \"1\",\"2\",… ('TagNumeric') を各 panel の
--   'tag' として注入する。 panel 自身の 'tag' 明示指定が優先 (個別 > 一括)。
--   @(a <-> b) <> subplotTags TagUpper@ のように使う。
--   [English]: Automatic subplot panel tags (like cowplot's
--   @plot_grid(labels="AUTO")@). Injects \"A\",\"B\",… ('TagUpper') /
--   \"a\",\"b\",… ('TagLower') / \"1\",\"2\",… ('TagNumeric') as each
--   panel's 'tag', in the unified grid's panel-enumeration order. A
--   panel's own explicit 'tag' takes priority (individual over bulk). Use
--   it like @(a <-> b) <> subplotTags TagUpper@.
subplotTags :: TagStyle -> VisualSpec
subplotTags s = mempty { vsSubplotTags = Last (Just s) }

-- | [日本語]: subplot panel を __名前 (= 子 spec の 'vsTitle') で選択 + 並べ替え__。
--   'repeatFields' (名前リスト → panel 群) の逆方向。 列挙順がそのまま表示順に
--   なる (ggplot @scale_*_discrete(limits=)@ と同じ「選択 + 順序」 の意味論)。
--   一致しない名前は無視、 title 無し panel は選択時には常に落ちる。
--   [English]: Selects and reorders subplot panels __by name (the child spec's 'vsTitle')__.
--   The reverse direction of 'repeatFields' (name
--   list → panel group). The enumeration order becomes the display order
--   directly (the same "select + order" semantics as ggplot's
--   @scale_*_discrete(limits=)@). Non-matching names are ignored, and
--   panels with no title are always dropped when selecting.
--
-- > subplots panels <> selectPanels ["b", "a"] <> subplotCols 2
selectPanels :: [Text] -> VisualSpec
selectPanels ws = mempty { vsPanelSel = Last (Just ws) }

-- | [日本語]: 'vsPanelSel' を適用した後の実効 subplot 列。 描画
--   ('Graphics.Hgg.Render.Layer.renderSubplots') の正本で、 HS 外 (canvas / PS codec) へ spec を送る側も
--   serialise 前にこれで解決すれば PS 非改修で選択が効く。 選択未指定
--   ('Nothing') は全 panel をそのまま返す。
--   [English]: The effective subplot list after applying 'vsPanelSel'.
--   The source of truth for rendering ('Graphics.Hgg.Render.Layer.renderSubplots'); resolving
--   through this before serializing on the side that sends the spec
--   outside HS (canvas / PS codec) makes selection work with no PS
--   changes. With no selection ('Nothing'), all panels are returned
--   as-is.
selectedSubplots :: VisualSpec -> [VisualSpec]
selectedSubplots s = case getLast (vsPanelSel s) of
  Nothing -> vsSubplots s
  Just ws -> [ p | nm <- ws, p <- vsSubplots s, getLast (vsTitle p) == Just nm ]

-- | [日本語]: 離散 x 軸の limits (= ggplot @scale_x_discrete(limits=)@)。
--   x encoding が ColTxt の layer のカテゴリ行を __選択 + 列挙順に並べ替え__る。
--   aes 基準なので coord_flip と直交 (flip 後も x データ軸を指す)。
--   [English]: The limits for a discrete x axis (like ggplot's
--   @scale_x_discrete(limits=)@). For layers whose x encoding is
--   ColTxt, __selects and reorders__ the category rows by enumeration
--   order. Since it is aes-based, it is orthogonal to coord_flip (still
--   refers to the x data axis even after a flip).
scaleXDiscreteLimits :: [Text] -> VisualSpec
scaleXDiscreteLimits ws = mempty { vsXDiscreteLimits = Last (Just ws) }

-- | [日本語]: 離散 y 軸の limits (= ggplot @scale_y_discrete(limits=)@)。
--   'Graphics.Hgg.Spec.Constructors.forest' は cat ラベルが y encoding なのでこちらを使う。
--   [English]: The limits for a discrete y axis (like ggplot's
--   @scale_y_discrete(limits=)@). Since 'Graphics.Hgg.Spec.Constructors.forest' encodes its cat label
--   as y, use this one for it.
scaleYDiscreteLimits :: [Text] -> VisualSpec
scaleYDiscreteLimits ws = mempty { vsYDiscreteLimits = Last (Just ws) }

-- | [日本語]: 離散軸 limits の解決 (正本): 'vsXDiscreteLimits' /
--   'vsYDiscreteLimits' を layer の行 filter + 並べ替えとして適用する。
--   layout / render の入口で呼ぶ (冪等)。
--
--   * 当該軸の encoding が 'ColTxt' の layer のみ対象 (数値軸 layer は不変)。
--   * 行 filter は __全 row-aligned encoding__ (encX/encY/encY2/errorX/errorY/
--     shapeBy/sizeBy/chain/linetypeBy/label/hover/color 列) を同 index で間引く
--     (整合維持)。
--   * 'ColByName' (resolver 参照) を含む spec は先に 'bakeSpec' で inline 化して
--     から filter する (limits 未指定なら bake もしない = 従来経路完全不変)。
--   * limits は当該 spec 自身の layer にのみ効く (subplot 子へは伝播しない —
--     子は自分の limits を持てる)。
--   [English]: The resolution (source of truth) for discrete-axis
--   limits: applies 'vsXDiscreteLimits' / 'vsYDiscreteLimits' as a
--   row-filter-plus-reorder on layers. Called at the layout / render
--   entry point (idempotent).
--
--   * Only layers whose axis encoding is 'ColTxt' are affected (numeric
--     axis layers are unchanged).
--   * The row filter thins __all row-aligned encodings__
--     (encX/encY/encY2/errorX/errorY/shapeBy/sizeBy/chain/linetypeBy/
--     label/hover/color columns) using the same index (keeping them
--     consistent).
--   * A spec containing 'ColByName' (a resolver reference) is first
--     inlined via 'bakeSpec' before filtering (if limits are unspecified,
--     no baking happens either — the original path is fully unchanged).
--   * limits only affect the spec's own layers (they do not propagate to
--     subplot children — a child may have its own limits).
applyDiscreteLimits :: Resolver -> VisualSpec -> VisualSpec
applyDiscreteLimits r spec =
  case (getLast (vsXDiscreteLimits spec), getLast (vsYDiscreteLimits spec)) of
    (Nothing, Nothing) -> spec
    (mxs, mys) ->
      let b = bakeSpec r spec
          limited = map (limitAxis lyEncY mys . limitAxis lyEncX mxs) (vsLayers b)
      in b { vsLayers = limited }
  where
    limitAxis enc (Just ws) ly
      | Just (ColTxt cats) <- getLast (enc ly) =
          let n   = V.length cats
              idx = V.fromList
                      [ i | w <- ws
                          , (i, c) <- zip [0 ..] (V.toList cats), c == w ]
          in reindexLayer n idx ly
    limitAxis _ _ ly = ly

-- | [日本語]: layer の全 row-aligned encoding を同じ index 列で間引く
--   ('applyDiscreteLimits' 用)。 長さ @n@ (= cat 列長) と一致する inline 列のみ
--   対象 (不一致・'ColByName' は据え置き)。
--   [English]: Thins every row-aligned encoding of a layer using the same
--   index column (used by 'applyDiscreteLimits'). Only inline columns
--   whose length matches @n@ (the cat column's length) are affected
--   (mismatched columns and 'ColByName' are left untouched).
reindexLayer :: Int -> Vector Int -> Layer -> Layer
reindexLayer n idx ly = ly
  { lyEncX       = reC <$> lyEncX ly
  , lyEncY       = reC <$> lyEncY ly
  , lyEncY2      = reC <$> lyEncY2 ly
  , lyErrorX     = reC <$> lyErrorX ly
  , lyErrorY     = reC <$> lyErrorY ly
  , lyShapeBy    = reC <$> lyShapeBy ly
  , lySizeBy     = reC <$> lySizeBy ly
  , lyAlphaBy    = reC <$> lyAlphaBy ly
  , lyChain      = reC <$> lyChain ly
  , lyLinetypeBy = reC <$> lyLinetypeBy ly
  , lyLabel      = reC <$> lyLabel ly
  , lyHover      = map reC (lyHover ly)
  , lyColor      = reColor <$> lyColor ly
  -- ★ quiver 成分 (row-aligned) + sub-mark 再帰。
  , lyEncU       = reC <$> lyEncU ly
  , lyEncV       = reC <$> lyEncV ly
  , lyOverlay    = map (reindexLayer n idx) (lyOverlay ly)
  }
  where
    reC c = case c of
      ColNum v | V.length v == n -> ColNum (V.backpermute v idx)
      ColTxt v | V.length v == n -> ColTxt (V.backpermute v idx)
      _                          -> c
    reColor ce = case ce of
      ColorByCol c        -> ColorByCol (reC c)
      ColorByContinuous c -> ColorByContinuous (reC c)
      ColorStatic t       -> ColorStatic t

-- | [日本語]: Vega-Lite @repeat@ 相当: フィールド名のリストを反復し、 各
--   フィールドから 1 つの view (VisualSpec) を生成して 'subplots' に並べる
--   (= フィールド自動反復)。 @repeatFields ["a","b","c"] (\\f -> layer (hist f))@
--   は 3 パネルを作る。 列数は @<> subplotCols n@ で指定する。 Vega の @repeat@
--   が encoding 内の @{repeat: ...}@ でフィールドを差し込むのに対し、 こちらは
--   生成関数にフィールド名を渡す明示形 (spec を値として組む方針ゆえ)。
--   [English]: The equivalent of Vega-Lite's @repeat@: iterates a list of
--   field names, generating one view (VisualSpec) per field and laying
--   them out with 'subplots' (automatic field repetition).
--   @repeatFields ["a","b","c"] (\\f -> layer (hist f))@ produces 3
--   panels. The column count is set via @<> subplotCols n@. Whereas
--   Vega's @repeat@ splices in fields via @{repeat: ...}@ inside the
--   encoding, this is an explicit form that passes the field name to a
--   generator function (following the approach of building specs as
--   values).
repeatFields :: [Text] -> (Text -> VisualSpec) -> VisualSpec
repeatFields fields mk = subplots (map mk fields)

-- 以下 3 関数は mark 構築子だが 'VisualSpec' と 'layer' に依存するため
-- 'Spec.Constructors' には置けず、 top-level setter 群と同居する。

-- | [日本語]: Wide-form histogram (P1): 複数列を __同一 plot に半透明で重ねる__。
--
--   `histogramWide [c1, c2, c3]` は 'VisualSpec' を返し、 内部で各列を独立
--   layer 化:
--
--     * layer i = `histogram cᵢ <> color (fromHex (palette i)) <> alpha 0.4 <> binCount 20`
--
--   palette は ColorBrewer Set1 (= categorical 9-class、 wong / 独自 切替は
--   今後)。 bin 数は全列で __共通__ (= seaborn の `multiple="layer"` 同等)、
--   デフォ 20。
--
--   matplotlib との対応: `plt.hist([c1, c2, c3], alpha=0.5, label=names)`
--   相当。
--   [English]: Wide-form histogram (P1): overlays multiple columns
--   __semi-transparently on the same plot__.
--
--   `histogramWide [c1, c2, c3]` returns a 'VisualSpec', internally
--   turning each column into an independent layer:
--
--     * layer i = `histogram cᵢ <> color (fromHex (palette i)) <> alpha 0.4 <> binCount 20`
--
--   The palette is ColorBrewer Set1 (categorical 9-class; switching to
--   wong / a dedicated scheme is a future addition). The bin count is
--   __shared__ across all columns (equivalent to seaborn's
--   `multiple="layer"`), defaulting to 20.
--
--   Corresponds to matplotlib's
--   `plt.hist([c1, c2, c3], alpha=0.5, label=names)`.
histogramWide :: [ColRef] -> VisualSpec
histogramWide cols =
  let pal = ["#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"
            , "#FFFF33", "#A65628", "#F781BF", "#999999"]
      mkLayer i c = layer
        ( histogram c
        -- 内部 palette は Text 経路ゆえ ColorStatic 直構築で温存 (Color 型を通さない)
        <> mempty { lyColor = Last (Just (ColorStatic (cycleColor pal i))) }
        <> alpha 0.4
        <> binCount 20
        )
  in mconcat [ mkLayer i c | (i, c) <- zip [0 ..] cols ]
  where
    cycleColor cs i = cs !! (i `mod` length cs)

-- | [日本語]: 別列・別 mark を 1 パネルに併置 (= mixed-mark)。 @<+>@ の list 版
--   (@distCols xs = layer (foldl1 (<+>) xs)@)。 各マークの値列 (encY) が別なので
--   別 slot (列名) に横並び・y は全列の値域和・単一パネル (subplot とは別)。
--   lane は 1D 分布 mark 専用 (box/violin/strip/swarm/raincloud)。 raincloud は
--   全マーク同一列ゆえ 1 slot に重畳する ('Graphics.Hgg.Spec.Constructors.compositeLanes' が列数を決める)。
--   [English]: Places different columns / different marks side by side in
--   a single panel (mixed-mark). A list version of @<+>@
--   (@distCols xs = layer (foldl1 (<+>) xs)@). Since each mark's value
--   column (encY) differs, they lay out in separate slots (columns); y
--   spans the union of all columns' ranges in a single panel (distinct
--   from subplot). Lanes are specific to 1D distribution marks
--   (box/violin/strip/swarm/raincloud). Since raincloud uses the same
--   column for every mark, they overlay into a single slot
--   ('Graphics.Hgg.Spec.Constructors.compositeLanes' determines the number of columns).
--
-- > distCols [ boxplot "a", violin "c", boxplot "d" ]
distCols :: [Layer] -> VisualSpec
distCols []       = mempty
distCols (l : ls) = layer (foldl (<+>) l ls)

-- | [日本語]: ★ ridge レイヤを含み coord 未指定の spec に coord_flip を自動
--   付与する。 ridge は「値→x(連続)・群→y(カテゴリ)」だが combinator は
--   box/violin と統一 (値=encY・群=encX via groupBy)。 coord_flip で
--   encY(値)→x・encX(群)→y に回す (box-flip と同機構)。 computeLayout /
--   renderToPrimitives の入口で適用する。
--   [English]: ★ Automatically applies coord_flip to a spec that contains
--   a ridge layer and has no coord specified. Although ridge is
--   conceptually "value→x (continuous), group→y (categorical)", its
--   combinator is unified with box/violin (value=encY, group=encX via
--   groupBy). coord_flip rotates encY (value) to x and encX (group) to y
--   (the same mechanism as box-flip). Applied at the computeLayout /
--   renderToPrimitives entry point.
ridgeAutoFlip :: VisualSpec -> VisualSpec
ridgeAutoFlip spec
  | any (\l -> getFirst (lyKind l) == Just MRidge) (vsLayers spec)
  , Nothing <- getLast (vsCoord spec)
  = spec { vsCoord = Last (Just CoordFlip) }
  | otherwise = spec


-- | [日本語]: P6: annotation 1 個を追加。
--   [English]: P6: adds a single annotation.
annotate :: Annotation -> VisualSpec
annotate a = mempty { vsAnnotations = [a] }

-- | [日本語]: P6: data 座標で text label を打つ shortcut (= 'annotTextP' の
--   PNative ラッパ)。
--   [English]: P6: a shortcut for placing a text label in data
--   coordinates (a PNative wrapper around 'annotTextP').
annotText :: Double -> Double -> Text -> VisualSpec
annotText x y t = annotTextP (PNative x) (PNative y) t

-- | [日本語]: ★ 'Pos' で text を打つ (native/npc/絶対長を軸ごと混在可)。
--   例: @annotTextP (PNpc 0.95) (PNative 3.0) "R²"@ (右端 npc・data y)。
--   [English]: ★ Places text using 'Pos' (native/npc/absolute length can be
--   mixed per axis). Example:
--   @annotTextP (PNpc 0.95) (PNative 3.0) "R²"@ (npc for the right edge,
--   data for y).
annotTextP :: Pos -> Pos -> Text -> VisualSpec
annotTextP x y t = annotate $ AnnText
  { anX = x, anY = y, anText = t, anColor = "", anSize = 12 }

-- | [日本語]: P6: data 座標で arrow を引く shortcut。
--   [English]: P6: a shortcut for drawing an arrow in data coordinates.
annotArrow :: Double -> Double -> Double -> Double -> VisualSpec
annotArrow x1 y1 x2 y2 =
  annotArrowP (PNative x1) (PNative y1) (PNative x2) (PNative y2)

-- | [日本語]: ★ 'Pos' で arrow を引く。
--   [English]: ★ Draws an arrow using 'Pos'.
annotArrowP :: Pos -> Pos -> Pos -> Pos -> VisualSpec
annotArrowP x1 y1 x2 y2 = annotate $ AnnArrow
  { anX1 = x1, anY1 = y1, anX2 = x2, anY2 = y2
  , anColor = "#444", anWidth = 1.5 }

-- | [日本語]: P6: data 座標で rect を描く shortcut (x,y,w,h → 2 隅 Pos へ変換)。
--   [English]: P6: a shortcut for drawing a rect in data coordinates
--   (converts x,y,w,h into two corner 'Pos' values).
annotRect :: Double -> Double -> Double -> Double -> Text -> VisualSpec
annotRect x y w h col =
  annotRectP (PNative x) (PNative y) (PNative (x + w)) (PNative (y + h)) col

-- | [日本語]: ★ 'Pos' 2 隅で rect を描く。
--   例: @annotRectP (PNpc 0.0) (PNative 1.0) (PNpc 1.0) (PNative 2.0) "grey"@
--   (帯: x 全幅 npc・y は data 1..2)。
--   [English]: ★ Draws a rect from two 'Pos' corners. Example:
--   @annotRectP (PNpc 0.0) (PNative 1.0) (PNpc 1.0) (PNative 2.0) "grey"@
--   (a band: x spans the full width in npc, y is data 1..2).
annotRectP :: Pos -> Pos -> Pos -> Pos -> Text -> VisualSpec
annotRectP x1 y1 x2 y2 col = annotate $ AnnRect
  { anX1 = x1, anY1 = y1, anX2 = x2, anY2 = y2
  , anFill = col, anStroke = "", anStrokeWidth = 0, anFillOpacity = 0.2 }

-- | [日本語]: P6: data 座標で line を引く shortcut。
--   [English]: P6: a shortcut for drawing a line in data coordinates.
annotLine :: Double -> Double -> Double -> Double -> VisualSpec
annotLine x1 y1 x2 y2 =
  annotLineP (PNative x1) (PNative y1) (PNative x2) (PNative y2)

-- | [日本語]: ★ 'Pos' で line を引く。
--   [English]: ★ Draws a line using 'Pos'.
annotLineP :: Pos -> Pos -> Pos -> Pos -> VisualSpec
annotLineP x1 y1 x2 y2 = annotate $ AnnLine
  { anX1 = x1, anY1 = y1, anX2 = x2, anY2 = y2
  , anColor = "#444", anWidth = 1 }

-- | [日本語]: P13: inset 1 個追加 (= デフォルト位置 右上 30%×30%)。
--   [English]: P13: adds a single inset (default position: top-right,
--   30%×30%).
inset :: VisualSpec -> VisualSpec
inset s = insetAt 0.65 0.05 0.3 0.3 s

-- | [日本語]: P13: 位置 + サイズ (plotArea 比率 0..1) 指定で inset を追加。
--   inX/inY は __左上原点・y 下向き__ (= 描画系と同じ)。
--   [English]: P13: adds an inset with a given position + size (a
--   plotArea ratio in 0..1). inX/inY use __the top-left origin with y pointing down__
--   (matching the rendering coordinate system).
insetAt :: Double -> Double -> Double -> Double -> VisualSpec -> VisualSpec
insetAt x y w h s = mempty
  { vsInsets = [ Inset { inSpec = s, inX = x, inY = y, inW = w, inH = h } ] }

-- | [日本語]: patchwork @inset_element@ 準拠の inset 追加。 left/bottom/right/top
--   は plotArea 比率 0..1 で __左下原点・y 上向き__ (patchwork 慣例)。 内部で
--   従来 'insetAt' (左上原点・y 下向き) へ変換するだけの薄いラッパ (非破壊)。
--   patchwork 感覚で `inset_element(p, left, bottom, right, top)` と同じ向きに
--   置ける。
--   [English]: Adds an inset following patchwork's @inset_element@
--   convention. left/bottom/right/top are a plotArea ratio in 0..1 using
--   __the bottom-left origin with y pointing up__ (the patchwork
--   convention). A thin, non-destructive wrapper that internally converts
--   to the original 'insetAt' (top-left origin, y down). Lets you place
--   insets in the same orientation as patchwork's
--   `inset_element(p, left, bottom, right, top)`.
insetElement :: Double -> Double -> Double -> Double -> VisualSpec -> VisualSpec
insetElement left bottom right top s =
  insetAt left (1 - top) (right - left) (top - bottom) s

-- | [日本語]: P17: categorical palette を指定。 default = hggMain (F-3)。
--   [English]: P17: specifies the categorical palette. Default =
--   hggMain (F-3).
palette :: [Text] -> VisualSpec
palette colors = mempty { vsPalette = Last (Just colors) }

-- | [日本語]: ggplot2 hue パレット (= @scales::hue_pal()@) を選ぶ。 群数 n は
--   描画時に決まるため sentinel を渡し、 Layout で n 展開する
--   (= 'Graphics.Hgg.Palette.ggplotHue')。
--   [English]: Selects ggplot2's hue palette (like @scales::hue_pal()@).
--   Since the group count n is only known at draw time, a sentinel is
--   passed and expanded to n in Layout (via
--   'Graphics.Hgg.Palette.ggplotHue').
paletteGGplot :: VisualSpec
paletteGGplot = mempty { vsPalette = Last (Just ["__ggplot_hue__"]) }

-- | [日本語]: P17: continuous (sequential) palette を指定。 default =
--   viridis5。
--   [English]: P17: specifies the continuous (sequential) palette.
--   Default = viridis5.
continuousPalette :: [Text] -> VisualSpec
continuousPalette colors = mempty { vsContinuousPal = Last (Just colors) }

-- | [日本語]: A4-e: ggplot @scale_color_manual(values=)@。 カテゴリ名→色(hex)
--   の辞書を指定。 'color' (ColorByCol) のカテゴリ名がここにあればその色を
--   最優先で使う。 未登録名は従来の positional palette ('palette'/theme) に
--   フォールバック。
--   [English]: A4-e: ggplot's @scale_color_manual(values=)@. Specifies a
--   category-name → color(hex) dictionary. If a 'color' (ColorByCol)
--   category name is present here, its color takes top priority.
--   Unregistered names fall back to the usual positional palette
--   ('palette'/theme).
scaleColorManual :: [(Text, Text)] -> VisualSpec
scaleColorManual dict = mempty { vsColorManual = Last (Just dict) }

-- | [日本語]: A4-e: ggplot
--   @scale_color_gradient2(low,mid,high,midpoint=)@。 発散 (diverging)
--   continuous palette。 'colorContinuousBy' (ColorByContinuous) のとき、
--   midpoint を中心 (0.5) に固定し lo..mid を [0,0.5]・mid..hi を [0.5,1] へ
--   個別正規化して 3-stop 補間。
--   [English]: A4-e: ggplot's
--   @scale_color_gradient2(low,mid,high,midpoint=)@. A diverging
--   continuous palette. For 'colorContinuousBy' (ColorByContinuous),
--   fixes midpoint at the center (0.5) and independently normalizes
--   lo..mid to [0,0.5] and mid..hi to [0.5,1] for 3-stop interpolation.
scaleColorGradient2 :: Text -> Text -> Text -> Double -> VisualSpec
scaleColorGradient2 low mid high midpoint =
  mempty { vsColorGradient2 = Last (Just (low, mid, high, midpoint)) }

-- | [日本語]: A4-e: ggplot @scale_size(range=c(min,max))@。 'Graphics.Hgg.Spec.Constructors.sizeBy'
--   (continuous size aesthetic) の半径 px 範囲を指定 (default (3,10))。
--   sizeBy 未使用なら無影響。
--   [English]: A4-e: ggplot's @scale_size(range=c(min,max))@. Specifies
--   the radius range in px for 'Graphics.Hgg.Spec.Constructors.sizeBy' (the continuous size aesthetic),
--   default (3,10). Has no effect if sizeBy is unused.
scaleSize :: Double -> Double -> VisualSpec
scaleSize lo hi = mempty { vsSizeRange = Last (Just (lo, hi)) }

-- | [日本語]: P8: 凡例を有効化 (= 既定: 右側)。
--   [English]: P8: enables the legend (default: right side).
legend :: VisualSpec
legend = mempty { vsLegend = Last (Just defaultLegendSpec) }

-- | [日本語]: P8: 凡例を抑制。
--   [English]: P8: suppresses the legend.
legendOff :: VisualSpec
legendOff = mempty
  { vsLegend = Last (Just (LegendSpec LegendNone mempty)) }

-- | [日本語]: P8: 凡例位置を指定。
--   [English]: P8: specifies the legend position.
legendPos :: LegendPosition -> VisualSpec
legendPos pos = mempty { vsLegend = Last (Just (LegendSpec pos mempty)) }

-- | [日本語]: 色凡例を非表示 (= ggplot @guides(color="none")@)。 この系では
--   凡例は色 (color/fill) のみなので 'legendOff' と同義。 ggplot 慣習名の別名
--   として提供。
--   [English]: Hides the color legend (like ggplot's
--   @guides(color="none")@). Since this system's legend is color/fill
--   only, it is synonymous with 'legendOff'. Provided as an alias using
--   ggplot's conventional name.
guideColorNone :: VisualSpec
guideColorNone = legendOff

-- | [日本語]: 凡例キーの表示順を逆に (= ggplot
--   @guide_legend(reverse=TRUE)@)。 各キーの色は固定のまま順序のみ反転。
--   位置設定 ('legend'/'legendPos') と独立合成可。
--   [English]: Reverses the display order of legend keys (like ggplot's
--   @guide_legend(reverse=TRUE)@). Each key's color stays fixed; only the
--   order is reversed. Composes independently of the position setting
--   ('legend'/'legendPos').
legendReverse :: VisualSpec
legendReverse = mempty { vsLegendReverse = Last (Just True) }

-- | [日本語]: 縦凡例 (Right/Inside) の列数 (= ggplot
--   @guide_legend(ncol=)@)。
--   [English]: The column count for a vertical legend (Right/Inside),
--   like ggplot's @guide_legend(ncol=)@.
legendNcol :: Int -> VisualSpec
legendNcol n = mempty { vsLegendNcol = Last (Just n) }

-- | [日本語]: 横凡例 (Bottom) の行数 (= ggplot @guide_legend(nrow=)@)。
--   [English]: The row count for a horizontal legend (Bottom), like
--   ggplot's @guide_legend(nrow=)@.
legendNrow :: Int -> VisualSpec
legendNrow n = mempty { vsLegendNrow = Last (Just n) }

-- | [日本語]: 図サイズ ('Length')。 bare 数値リテラルは @Num Length@ 経由で
--   __pt__ (@width 600@ = 600pt)。 mm で書きたいときは 'widthMm' / 'heightMm'、
--   その他の単位は @width (7 *~ inch)@ / 'widthUnit' を使う。
--   [English]: The figure size ('Length'). A bare numeric literal is
--   __pt__ via @Num Length@ (@width 600@ = 600pt). To write in mm, use
--   'widthMm' / 'heightMm'; for other units use @width (7 *~ inch)@ /
--   'widthUnit'.
width, height :: Length -> VisualSpec
width  = widthUnit
height = heightUnit

-- | [日本語]: 図サイズ (mm 直接)。 @widthMm 180@ = 180mm。 A4 で 'width' の
--   bare が pt に変わったので、 従来の mm 指定はこちらへ移行する。
--   [English]: The figure size, directly in mm. @widthMm 180@ = 180mm.
--   Since A4 changed the bare value of 'width' to pt, migrate previous mm
--   specifications to this instead.
widthMm, heightMm :: Double -> VisualSpec
widthMm  w = widthUnit  (w *~ mm)
heightMm h = heightUnit (h *~ mm)

-- | [日本語]: 図サイズ (単位明示)。 @widthUnit (7 *~ inch)@ /
--   @widthUnit (800 *~ px)@。
--   [English]: The figure size with an explicit unit. Examples:
--   @widthUnit (7 *~ inch)@ / @widthUnit (800 *~ px)@.
widthUnit, heightUnit :: Length -> VisualSpec
widthUnit  l = mempty { vsWidth  = Last (Just l) }
heightUnit l = mempty { vsHeight = Last (Just l) }

-- | [日本語]: 描画 dpi (px backend は px=pt×dpi/72)。 @plot <> dpi 300@。 既定
--   96。 PDF は無視。
--   [English]: The rendering dpi (px backends use px = pt × dpi/72).
--   Example: @plot <> dpi 300@. Default 96; ignored by PDF.
dpi :: Double -> VisualSpec
dpi d = mempty { vsDpi = Last (Just d) }

-- | [日本語]: coord_fixed(ratio) 相当。 panel の 高/幅 比 (aspect) を固定。
--   指定時は可用域内で aspect を保つ最大 panel を取り中央寄せ (ggplot
--   Coord$aspect)。
--   [English]: The equivalent of coord_fixed(ratio). Fixes the panel's
--   height/width ratio (aspect). When specified, takes the largest panel
--   that preserves the aspect within the available area and centers it
--   (ggplot's Coord$aspect).
aspectRatio :: Double -> VisualSpec
aspectRatio a = mempty { vsAspect = Last (Just a) }

-- | [日本語]: coord_flip。 x/y 軸を入れ替える (= 横棒グラフ等)。 ggplot
--   coord_flip() 相当。
--   [English]: coord_flip. Swaps the x/y axes (used for horizontal bar
--   charts, etc.). Equivalent to ggplot's coord_flip().
--
--   > bar "cat" "y" `layer'` purePlot <> coordFlip
coordFlip :: VisualSpec
coordFlip = mempty { vsCoord = Last (Just CoordFlip) }

-- | [日本語]: 極座標 (= ggplot @coord_polar(theta="x")@)。 データ x を角度
--   (0..2π、 上始点・時計回り)、 データ y を半径に写す。 line/point は
--   radar / spiral に。
--   [English]: Polar coordinates (like ggplot's
--   @coord_polar(theta="x")@). Maps data x to angle (0..2π, starting at
--   the top, clockwise) and data y to radius. Turns line/point marks into
--   a radar / spiral shape.
coordPolar :: VisualSpec
coordPolar = mempty { vsCoord = Last (Just CoordPolarX) }

-- | [日本語]: 極座標 (= ggplot @coord_polar(theta="y")@)。 データ y を角度、
--   データ x を半径に写す。 単一カテゴリの stacked bar と併せると円グラフに
--   なる。
--   [English]: Polar coordinates (like ggplot's
--   @coord_polar(theta="y")@). Maps data y to angle and data x to radius.
--   Combined with a single-category stacked bar, it becomes a pie chart.
coordPolarY :: VisualSpec
coordPolarY = mempty { vsCoord = Last (Just CoordPolarY) }

-- | [日本語]: X 軸反転 (= ggplot @scale_x_reverse()@)。 大値が左、 小値が右へ。
--   coord_flip と独立合成可。
--   [English]: Reverses the X axis (like ggplot's @scale_x_reverse()@):
--   large values move to the left, small values to the right. Composes
--   independently of coord_flip.
--
--   > scatter "x" "y" `layer'` purePlot <> reverseX
reverseX :: VisualSpec
reverseX = mempty { vsReverseX = Last (Just True) }

-- | [日本語]: Y 軸反転 (= ggplot @scale_y_reverse()@)。 大値が下、 小値が上へ。
--   [English]: Reverses the Y axis (like ggplot's @scale_y_reverse()@):
--   large values move to the bottom, small values to the top.
reverseY :: VisualSpec
reverseY = mempty { vsReverseY = Last (Just True) }

-- | [日本語]: X 軸 zoom (= ggplot @coord_cartesian(xlim=c(lo,hi))@)。
--   'Graphics.Hgg.Spec.Axis.axisRange' (= scale limits、 範囲外データを切る) と異なり __データを落とさず__
--   表示範囲だけを [lo,hi] に上書きする。 stat (regression/density
--   等) は全データから計算され、 範囲外の glyph は panel に clip される。
--   numeric 軸のみ有効。
--   [English]: X-axis zoom (like ggplot's
--   @coord_cartesian(xlim=c(lo,hi))@). Unlike 'Graphics.Hgg.Spec.Axis.axisRange' (scale limits,
--   which drops out-of-range data), this __keeps all data__ and only
--   overrides the display range to [lo,hi]. Stats (regression/density,
--   etc.) are still computed from the full data, and out-of-range glyphs
--   are clipped to the panel. Only effective for numeric axes.
coordCartesianX :: Double -> Double -> VisualSpec
coordCartesianX lo hi = mempty { vsCoordXLim = Last (Just (lo, hi)) }

-- | [日本語]: Y 軸 zoom (= ggplot @coord_cartesian(ylim=c(lo,hi))@)。
--   [English]: Y-axis zoom (like ggplot's
--   @coord_cartesian(ylim=c(lo,hi))@).
coordCartesianY :: Double -> Double -> VisualSpec
coordCartesianY lo hi = mempty { vsCoordYLim = Last (Just (lo, hi)) }

-- | [日本語]: X/Y 同時 zoom (= ggplot @coord_cartesian(xlim=,ylim=)@)。
--   'coordCartesianX' と 'coordCartesianY' の合成。
--   [English]: Simultaneous X/Y zoom (like ggplot's
--   @coord_cartesian(xlim=,ylim=)@). A combination of 'coordCartesianX'
--   and 'coordCartesianY'.
coordCartesian :: Double -> Double -> Double -> Double -> VisualSpec
coordCartesian xlo xhi ylo yhi = coordCartesianX xlo xhi <> coordCartesianY ylo yhi

-- | [日本語]: 軸 (X / Y) 設定の合成 helper。
--   [English]: A helper for composing axis (X / Y) settings.
--
-- > example = ... <> xAxis logAxis <> yAxis (linearAxis <> ...)
xAxis, yAxis :: AxisSpec -> VisualSpec
xAxis a = mempty { vsXAxis = Last (Just a) }
yAxis a = mempty { vsYAxis = Last (Just a) }

-- | [日本語]: P5: 右側 Y 軸の AxisSpec (= dual Y を有効化)。
--   [English]: P5: the AxisSpec for the right-side Y axis (enables dual
--   Y).
yAxisRight :: AxisSpec -> VisualSpec
yAxisRight a = mempty { vsYAxisRight = Last (Just a) }

-- | [日本語]: P5: layer を右側 Y 軸に紐付ける。
--   [English]: P5: binds a layer to the right-side Y axis.
toRightY :: Layer
toRightY = mempty { lyYAxisSide = Last (Just YAxisRight) }

-- | [日本語]: P5: layer を左側 Y 軸に紐付ける (= default なので通常不要)。
--   [English]: P5: binds a layer to the left-side Y axis (usually
--   unnecessary since it is the default).
toLeftY :: Layer
toLeftY = mempty { lyYAxisSide = Last (Just YAxisLeft) }

-- | [日本語]: 参照線を 1 本追加 (= 重ねがけで複数本)。
--   [English]: Adds a single reference line (stack multiple by layering).
--
-- > example = ... <> refLine RefIdentity <> refLine (RefHorizontalAt 0)
refLine :: ReferenceLine -> VisualSpec
refLine rl = mempty { vsRefLines = [rl] }

-- | [日本語]: shortcut。
--   [English]: A shortcut.
refIdentity   :: VisualSpec
refIdentity   = refLine RefIdentity
refHorizontal :: Double -> VisualSpec
refHorizontal y = refLine (RefHorizontalAt y)
refVertical   :: Double -> VisualSpec
refVertical x   = refLine (RefVerticalAt x)

-- | [日本語]: scatter の周辺に X/Y 両方の histogram。
--   [English]: Adds both X and Y marginal histograms around a scatter.
marginal :: VisualSpec
marginal = mempty { vsMarginal = Last (Just (defaultMarginalSpec { msShowX = True, msShowY = True })) }

-- | [日本語]: 周辺 histogram X 軸のみ。
--   [English]: Only the X-axis marginal histogram.
marginalX :: VisualSpec
marginalX = mempty { vsMarginal = Last (Just (defaultMarginalSpec { msShowX = True })) }

-- | [日本語]: 周辺 histogram Y 軸のみ。
--   [English]: Only the Y-axis marginal histogram.
marginalY :: VisualSpec
marginalY = mempty { vsMarginal = Last (Just (defaultMarginalSpec { msShowY = True })) }

-- ===========================================================================
-- Font customization setter (= hgg-frontend-settings-spec v0.1 §1.3)
-- ===========================================================================

titleFont :: FontSpec -> VisualSpec
titleFont f = mempty { vsTitleFont = Last (Just f) }

axisLabelFont :: FontSpec -> VisualSpec
axisLabelFont f = mempty { vsAxisLabelFont = Last (Just f) }

tickFont :: FontSpec -> VisualSpec
tickFont f = mempty { vsTickFont = Last (Just f) }

legendFont :: FontSpec -> VisualSpec
legendFont f = mempty { vsLegendFont = Last (Just f) }

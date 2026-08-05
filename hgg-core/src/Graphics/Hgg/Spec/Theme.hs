-- |
-- Module      : Graphics.Hgg.Spec.Theme
-- Description : theme presets (ThemeName), series palettes, and per-element overrides
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'Graphics.Hgg.Spec' の module 分割で切り出し。 描画 theme の名前
--   ('ThemeName')、 preset ごとの series palette、 named palette (Okabe-Ito 等)、
--   element 単位の上書き ('ThemeOverride'、 ggplot theme(element_*) 相当) を持つ。
--   公開 API は従来どおり 'Graphics.Hgg.Spec' (facade) が re-export する。
--   挙動・出力は完全に不変。
--
--   [English]: Split out from 'Graphics.Hgg.Spec' during a module split.
--   Holds the theme name ('ThemeName'), the series palette for each preset,
--   named palettes (Okabe-Ito etc.), and per-element overrides
--   ('ThemeOverride', equivalent to ggplot's theme(element_*)). The public
--   API is still re-exported by 'Graphics.Hgg.Spec' (the facade) as before.
--   Behavior and output are entirely unchanged.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Theme
  ( -- * theme preset + palette
    ThemeName(..)
  , themeSeriesPalette
  , okabeIto, tolBright, brewerSet2, brewerDark2
    -- * element 単位 override
  , ThemeOverride(..)
  , TickDir(..)
  , Margin(..)
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic, Generically (..))

import           Graphics.Hgg.Spec.Decoration (FontSpec, LegendPosition)

-- | [日本語]: 描画 theme (= 名前で参照、 関数を持たない = JSON serializable)。
--   ggplot 標準 preset (ThemeGrey) + ブランドテーマを追加。
--
--     * ThemeGrey          = ggplot 既定 theme_grey (灰背景 #EBEBEB・白 grid・枠なし・軸線なし)
--     * ThemeNoir          = ブランド (暗・上品・寒色アクセント、 コンペ用に残置)
--     * ThemeLumen         = ブランド (白基調・深い差し色・清潔、 コンペ用に残置)
--     * ThemeParchment     = 羊皮紙基調・明の正式テーマ。 配色は cream/gold/ink +
--         Universal Categorical series 由来。
--     * ThemeParchmentDark = 同テーマの暗版 (焦茶インク背景・series は shade 300 で沈み防止)。
--
--   [English]: A rendering theme (referenced by name; carries no functions,
--   so it is JSON-serializable). Adds ggplot's standard preset (ThemeGrey)
--   plus the brand themes.
--
--     * ThemeGrey          = ggplot's default theme_grey (grey #EBEBEB
--         background, white grid, no border, no axis line)
--     * ThemeNoir          = brand theme (dark, elegant, cool accents;
--         kept around from an earlier competition entry)
--     * ThemeLumen         = brand theme (white-based, deep accent
--         colors, clean; kept around from an earlier competition entry)
--     * ThemeParchment     = the official parchment-based light theme.
--         Colors come from cream/gold/ink + the Universal Categorical
--         series.
--     * ThemeParchmentDark = the dark variant of the same theme (dark
--         umber ink background; series colors use shade 300 to avoid
--         getting lost against it)
data ThemeName = ThemeDefault | ThemeMinimal | ThemeDark | ThemeLight
               | ThemeGrey | ThemeBW | ThemeClassic | ThemeVoid | ThemeLinedraw
               | ThemeNoir | ThemeLumen
               | ThemeParchment | ThemeParchmentDark
  deriving (Show, Eq, Generic)

instance ToJSON   ThemeName
instance FromJSON ThemeName

-- | [日本語]: preset ごとの既定 series palette (= palette 未指定時に使う色順)。
--   ggplot 系 preset は従来通り hggMain (既定配色)、 ブランド 3 種は専用 series。
--   Layout.computeLayout の catPal 既定がこれを参照する (= palette 指定で上書き可)。
--   [English]: The default series palette for each preset (the color order
--   used when no palette is specified). ggplot-family presets use
--   hggMain (the default color scheme) as before; the three brand themes
--   use their own dedicated series. Layout.computeLayout's default catPal
--   refers to this (overridable by specifying a palette).
themeSeriesPalette :: ThemeName -> [Text]
themeSeriesPalette t = case t of
  ThemeNoir  -> ["#7AA2F7", "#BB9AF7", "#7DCFFF", "#9ECE6A", "#E0AF68", "#F7768E"]
  ThemeLumen -> ["#4C5BD4", "#D6336C", "#2F9E44", "#E8590C", "#7048E8", "#1098AD"]
  -- [日本語]: Parchment 明: 案3 (#1 = White Rabbit Inner Ear Pink #F0A5A0、
  --   #3 = Dormouse 系 Warm Yellow #E8D58A、 他は既定配色)。 2026-06-02 確定。
  --   [English]: Parchment (light): option 3 (#1 = White Rabbit
  --   Inner Ear Pink #F0A5A0, #3 = Dormouse-family Warm Yellow #E8D58A, the
  --   rest is the default color scheme). Finalized 2026-06-02.
  ThemeParchment     -> canvasPal
  -- [日本語]: 暗版 (Charcoal 背景): 案3 の暗色 (purple/teal/rose/wine) を明度調整し沈み防止。色相・順序は維持。
  --   [English]: Dark variant (charcoal background): brightness-adjusted the
  --   dark colors from option 3 (purple/teal/rose/wine) to avoid getting
  --   lost against the background, while keeping hue and order unchanged.
  ThemeParchmentDark -> [ "#F0A5A0", "#A98BD0", "#E8D58A", "#5FA0A8"
                            , "#E0617E", "#B8C7D9", "#D9685F" ]
  -- [日本語]: default 系 (grey/default/minimal/light/dark) は ggplot2 既定 scales::hue_pal() にならう。
  --   ★Phase 28 (2026-06-14): 固定 7 色版でなく __群数 n 依存の hue sentinel__ を返す。
  --   ggplot は離散色スケールごとに hue_pal()(n) を再計算するため、 群数 3 なら
  --   赤/緑/青、 4 なら別配色…と変わる。 固定 7 色だと群数 3 でも index 0,1,2 =
  --   赤/金/緑 になり R4DS と食い違っていた。 sentinel は Layout.catPal /
  --   Bridge.resolveGrouped が @ggplotHue@ n で展開する。
  --   [English]: The default-family presets (grey/default/minimal/light/dark)
  --   follow ggplot2's default scales::hue_pal(). Rather than a fixed 7-color
  --   set, this returns a __hue sentinel that depends on the group count n__.
  --   ggplot recomputes hue_pal()(n) for every discrete color scale, so a
  --   group count of 3 gives red/green/blue, 4 gives a different set, and so
  --   on. With a fixed 7-color set, a group count of 3 would still take
  --   indices 0,1,2 (red/gold/green), which disagreed with R4DS. Layout.catPal
  --   / Bridge.resolveGrouped expand the sentinel via @ggplotHue@ n.
  _ -> ["__ggplot_hue__"]
  where
    canvasPal = [ "#F0A5A0", "#7A5C92", "#E8D58A", "#3E6A6F"
                , "#C7445D", "#B8C7D9", "#7E1F23" ]

-- | [日本語]: 学術向け named series palette。 theme とは独立に `palette <名>` で使う (colorblind-safe 中心)。
--   Okabe-Ito (Okabe & Ito 2008、 色覚バリアフリー定番、 R palette.colors("Okabe-Ito") と同一)。
--   [English]: An academic-style named series palette, used independently of
--   the theme via `palette <name>` (mostly colorblind-safe). Okabe-Ito
--   (Okabe & Ito 2008, a colorblind-accessibility standard, identical to R's
--   palette.colors("Okabe-Ito")).
okabeIto :: [Text]
okabeIto = [ "#000000", "#E69F00", "#56B4E9", "#009E73"
           , "#F0E442", "#0072B2", "#D55E00", "#CC79A7" ]

-- | [日本語]: Paul Tol bright (7 色、 色覚バリアフリー)。
--   [English]: Paul Tol bright (7 colors, colorblind-accessible).
tolBright :: [Text]
tolBright = [ "#4477AA", "#EE6677", "#228833", "#CCBB44"
            , "#66CCEE", "#AA3377", "#BBBBBB" ]

-- | [日本語]: ColorBrewer Set2 (8 色、 柔らかい定性)。
--   [English]: ColorBrewer Set2 (8 colors, soft qualitative palette).
brewerSet2 :: [Text]
brewerSet2 = [ "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"
             , "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3" ]

-- | [日本語]: ColorBrewer Dark2 (8 色、 濃いめ定性、 白背景向き)。
--   [English]: ColorBrewer Dark2 (8 colors, deeper qualitative palette,
--   suited to a white background).
brewerDark2 :: [Text]
brewerDark2 = [ "#1B9E77", "#D95F02", "#7570B3", "#E7298A"
              , "#66A61E", "#E6AB02", "#A6761D", "#666666" ]

-- | [日本語]: 軸目盛線 (tick mark) の向き。 'TickOut' = panel 外向き
--   (ggplot 既定)、 'TickIn' = panel 内向き (base R / 金融チャート系)、
--   'TickBoth' = 両向き。 JSON は nullary constructor 名 (canvas Codec と同形)。
--   [English]: The direction of the axis tick marks. 'TickOut' points
--   outward from the panel (ggplot's default), 'TickIn' points inward
--   (base R / financial-chart style), and 'TickBoth' points both ways. The
--   JSON encoding uses the nullary constructor name (matching the canvas
--   codec's shape).
data TickDir = TickOut | TickIn | TickBoth
  deriving (Show, Eq, Generic)

instance ToJSON   TickDir
instance FromJSON TickDir

-- | [日本語]: 図の外周余白 (pt)。 フィールド順は ggplot @margin(t, r, b, l)@
--   と同じ。 'ThemeOverride' の @toPlotMargin@ に指定すると自動算出の外周分
--   (各辺 half_line = 5.5pt) を __置き換える__ (加算ではない)。
--   [English]: The outer margin of the figure (pt). Field order matches
--   ggplot's @margin(t, r, b, l)@. When specified via the @toPlotMargin@
--   field of 'ThemeOverride', it __replaces__ the automatically computed
--   outer margin
--   (each side's half_line = 5.5pt) rather than adding to it.
data Margin = Margin
  { marTop    :: !Double
  , marRight  :: !Double
  , marBottom :: !Double
  , marLeft   :: !Double
  } deriving (Show, Eq, Generic)

instance ToJSON   Margin
instance FromJSON Margin

-- ===========================================================================
-- element 単位 theme override (ggplot theme(element_*) 相当)
-- ===========================================================================
-- | [日本語]: preset (ThemeName) に要素単位で上書きを合成する override。 各 field は Last で
--   「指定があれば優先」。 @resolveTheme@ (Render) が preset palette に合成する。
--   全 field Monoid なので setter を `<>` で重ねられる (ggplot の theme() 加算と同様)。
--   [English]: An override that composes element-by-element adjustments onto
--   a preset (ThemeName). Each field is a Last, meaning "prefer it if
--   specified". @resolveTheme@ (Render) composes it onto the preset palette.
--   Since every field is a Monoid, setters can be stacked with `<>` (the same
--   way ggplot's theme() calls add up).
data ThemeOverride = ThemeOverride
  { toPlotBg       :: !(Last Text)   -- plot.background fill
    -- [日本語]: ★ Phase 63 A18: plot.background を塗るか (False = 塗らない = 透過。
    --   cowplot は rect fill NA = 透過なので合成 preset が False を焼き込む)。
    --   [English]: Whether to fill plot.background (False = don't fill, i.e.
    --   transparent; since cowplot's rect fill NA means transparent, the
    --   composed preset bakes in False).
  , toShowBackground :: !(Last Bool)
  , toPanelBg      :: !(Last Text)   -- panel.background fill
  , toShowPanel    :: !(Last Bool)   -- panel 矩形を塗るか
  , toGridColor    :: !(Last Text)   -- panel.grid colour
  , toShowGrid     :: !(Last Bool)   -- panel.grid on/off (= major/minor 両方の糖衣)
    -- [日本語]: ★ Phase 63 A2: grid major/minor の個別 on/off (cowplot theme_minimal_grid 等)。
    --   優先順は 個別 (これ) > 一括 toShowGrid > preset (@resolveTheme@ で解決)。
    --   [English]: Individual on/off for grid major/minor (cowplot's
    --   theme_minimal_grid etc.). Priority is: individual (this) > the blanket
    --   toShowGrid > the preset (resolved by @resolveTheme@).
  , toShowGridMajor :: !(Last Bool)  -- panel.grid.major on/off
  , toShowGridMinor :: !(Last Bool)  -- panel.grid.minor on/off
  , toShowBorder   :: !(Last Bool)   -- panel.border on/off
  , toShowAxisLine :: !(Last Bool)   -- axis.line on/off
  , toAxisColor    :: !(Last Text)   -- axis 線/目盛り色
  , toTextColor    :: !(Last Text)   -- 文字色
    -- [日本語]: ★ Phase 9 A-3: 文字 theme 統合 (ggplot theme(text/plot.title/axis.title/...) 相当)。
    --   各 slot の FontSpec を theme から差し替え可能に。 優先順位は
    --   override (これ) > font setter (vsTitleFont 等) > preset 既定 (@mkFontTS@)。
    --   [English]: Unified text theming (equivalent to ggplot's
    --   theme(text/plot.title/axis.title/...)). Lets the FontSpec of each slot
    --   be swapped out from the theme. Priority is: override (this) > font
    --   setters (vsTitleFont etc.) > the preset default (@mkFontTS@).
  , toTitleFont     :: !(Last FontSpec)  -- plot.title
  , toAxisLabelFont :: !(Last FontSpec)  -- axis.title
  , toTickFont      :: !(Last FontSpec)  -- axis.text
  , toLegendFont    :: !(Last FontSpec)  -- legend.title / legend.text
    -- [日本語]: axis.text の回転角 (度・CCW)。 per-axis @axisRotate@ 未指定時の fallback。
    --   'toAxisTextAngle' = x/y 共通既定、 'toAxisTextAngleX'/'toAxisTextAngleY' = 軸別上書き
    --   (Phase 50 A3・軸別 > 共通 の優先。 @axisTextAngleXOf@/@axisTextAngleYOf@ で解決)。
    --   [English]: The rotation angle of axis.text (degrees, CCW). The
    --   fallback used when the per-axis @axisRotate@ is not specified.
    --   'toAxisTextAngle' is the shared x/y default; 'toAxisTextAngleX' /
    --   'toAxisTextAngleY' are the per-axis overrides (per-axis takes priority
    --   over shared; resolved by @axisTextAngleXOf@/@axisTextAngleYOf@).
  , toAxisTextAngle  :: !(Last Double)
  , toAxisTextAngleX :: !(Last Double)
  , toAxisTextAngleY :: !(Last Double)
    -- [日本語]: ★ Phase 9 A-4: strip.background (facet strip の灰矩形)。
    --   [English]: strip.background (the grey rectangle behind facet strips).
  , toStripBg       :: !(Last Text)   -- strip.background fill
  , toShowStrip     :: !(Last Bool)   -- strip 矩形を塗るか
    -- [日本語]: ★ Phase 43 A4: プリセット専用だった 4 項目に上書き口を追加 (= 全プロパティ `<>` 上書き
    --   可能に)。対応 @ThemePalette@ field = tpTitleHjust / tpTitleColor / tpTickLineColor /
    --   tpLegendKeyBg。generic 導出なので field 追加のみで instance は自動追従。
    --   [English]: Added override hooks for four fields that used to be
    --   preset-only (so every property can now be overridden with `<>`).
    --   The corresponding @ThemePalette@ fields are tpTitleHjust /
    --   tpTitleColor / tpTickLineColor / tpLegendKeyBg. Since the instance is
    --   generically derived, adding a field is all that's needed and the
    --   instance follows automatically.
  , toTitleHjust    :: !(Last Double) -- plot.title の水平揃え (0=左、 0.5=中央)
  , toTitleColor    :: !(Last Text)   -- plot.title / axis.title の文字色
  , toTickLineColor :: !(Last Text)   -- 軸目盛線 (tick mark) の色
  , toLegendKeyBg   :: !(Last Text)   -- legend.key 背景塗り色 ("" なら塗らない)
    -- [日本語]: ★ Phase 63 A3: legend.position を theme に焼き込む口 (cowplot 自作 theme 用)。
    --   優先順は 図レベル vsLegend (legendPos setter) > これ > 既定 LegendRightCenter
    --   (@effectiveLegendPos@ で解決。 ggplot の theme() と個別指定の関係に同じ)。
    --   [English]: A hook for baking legend.position into the theme (for
    --   cowplot-style custom themes). Priority is: the figure-level vsLegend
    --   (the legendPos setter) > this > the default LegendRightCenter
    --   (resolved by @effectiveLegendPos@; the same relationship as ggplot's
    --   theme() versus per-call specification).
  , toLegendPos     :: !(Last LegendPosition) -- legend.position
    -- [日本語]: ★ Phase 63 A4: 軸目盛線の長さ (pt)・向き (ggplot axis.ticks.length 相当)。
    --   tick 長は軸ラベル/マージン位置に波及するため、 palette でなく
    --   Layout の @effectiveTickLength@/@effectiveTickDir@ が解決し
    --   computeLayout (予約) と Render.tickMarks (描画) の単一情報源になる。
    --   未指定時は ggTickLen (2.75pt) / TickOut (= 従来挙動と同一)。
    --   [English]: The length (pt) and direction of the axis tick marks
    --   (equivalent to ggplot's axis.ticks.length). Since tick length
    --   affects axis-label and margin placement, this is resolved not by the
    --   palette but by Layout's @effectiveTickLength@/@effectiveTickDir@,
    --   making them the single source of truth for computeLayout (reserving
    --   space) and Render.tickMarks (drawing). When unspecified, defaults to
    --   ggTickLen (2.75pt) / TickOut (identical to the previous behavior).
  , toTickLength    :: !(Last Double)  -- axis.ticks.length (pt)
  , toTickDir       :: !(Last TickDir) -- 目盛線の向き (外/内/両)
    -- [日本語]: ★ Phase 63 A5: 図の外周余白 (ggplot plot.margin 相当)。 指定時は自動算出の
    --   外周分 (各辺 ggHalfLine) を置き換える。 軸ラベル・title 帯・凡例などの
    --   内側予約は従来どおり自動。 Layout の @effectivePlotMargin@ が解決する。
    --   [English]: The outer margin of the figure (equivalent to ggplot's
    --   plot.margin). When specified, it replaces the automatically
    --   computed outer margin (each side's ggHalfLine). Inner reservations
    --   such as axis labels, the title band, and the legend remain
    --   automatic as before. Resolved by Layout's @effectivePlotMargin@.
  , toPlotMargin    :: !(Last Margin)  -- plot.margin (t/r/b/l、 pt)
    -- [日本語]: ★ Phase 63 A12: base font size (pt、 ggplot base_size 相当)。 各 slot の既定
    --   font size はこれからの相対倍率 (title ×1.2 / axis.title ×1 / axis.text ×0.8 /
    --   legend.title ×1 / legend.text ×0.8) で派生する。 優先順 = 個別 theme*Font
    --   (fsSize) > これによる base 派生 > 既定 11 (theme_grey base_size)。
    --   font size は layout 予約 (titleSize 等) に波及するため Layout の
    --   @effectiveBaseFontSize@ が解決し、 computeLayout (予約) と
    --   Render.mkFontTS (描画) の単一情報源になる。
    --   [English]: The base font size (pt, equivalent to ggplot's base_size).
    --   Each slot's default font size is derived from this via a relative
    --   multiplier (title x1.2 / axis.title x1 / axis.text x0.8 /
    --   legend.title x1 / legend.text x0.8). Priority is: the per-slot
    --   theme*Font (fsSize) > the base-derived value > the default 11
    --   (theme_grey's base_size). Since font size affects layout
    --   reservations (titleSize etc.), Layout's @effectiveBaseFontSize@
    --   resolves it, making it the single source of truth for computeLayout
    --   (reserving space) and Render.mkFontTS (drawing).
  , toBaseFontSize  :: !(Last Double)  -- base font size (pt)
    -- [日本語]: ★ Phase 63 A19: axis.text (目盛ラベル文字) / axis.title (軸タイトル) の表示。
    --   False = ggplot element_blank 相当 (tick 線の有無は toTickLength と独立)。
    --   表示 off は margin 予約に波及するため Layout の @effectiveShowAxisText@ /
    --   @effectiveShowAxisTitle@ が解決し、 computeLayout (予約) と Render
    --   (tickMarks/labels の描画) の単一情報源になる。 既定は ThemeVoid のみ False
    --   (ggplot theme_void = axis.text/axis.title とも element_blank)、 他 preset True。
    --   [English]: Visibility of axis.text (tick labels) / axis.title (axis
    --   titles). False is equivalent to ggplot's element_blank (independent
    --   of whether tick marks themselves are shown via toTickLength). Since
    --   turning display off affects margin reservations, Layout's
    --   @effectiveShowAxisText@ / @effectiveShowAxisTitle@ resolve it, making
    --   them the single source of truth for computeLayout (reserving space)
    --   and Render (drawing tickMarks/labels). Defaults to False only for
    --   ThemeVoid (ggplot's theme_void makes both axis.text/axis.title
    --   element_blank), True for every other preset.
  , toShowAxisText  :: !(Last Bool)  -- axis.text on/off
  , toShowAxisTitle :: !(Last Bool)  -- axis.title on/off
    -- [日本語]: ★ Phase 63 A19.5: 凡例キー 1 辺 (pt、 ggplot legend.key.size 相当)。 キーの
    --   行 pitch = キー辺なので凡例の行間もこれで決まる。 cowplot は全 preset で
    --   1.1 × font_size を明示上書きする (既定 = 1.2 lines = 1.2 × base × 1.3133)。
    --   凡例幅/高さの margin 予約に波及するため Layout の @effectiveLegendKeyW@ が解決。
    --   [English]: The side length (pt) of a single legend key (equivalent
    --   to ggplot's legend.key.size). Since a key's row pitch equals its
    --   side length, this also determines the legend's line spacing.
    --   cowplot explicitly overrides this to 1.1 x font_size for every
    --   preset (the default is 1.2 lines = 1.2 x base x 1.3133). Since this
    --   affects the legend's width/height margin reservation, Layout's
    --   @effectiveLegendKeyW@ resolves it.
  , toLegendKeySize :: !(Last Double)  -- legend.key.size (pt)
    -- [日本語]: ★ Phase 63 A20.5: 全 text slot 共通の font family fallback (ggplot
    --   theme(text = element_text(family=...)) 相当)。 優先順位は slot 別 FontSpec の
    --   fsFamily > これ > "sans-serif" (@mkFontTS@ が解決)。 slot 丸ごと置換
    --   (Last FontSpec) と違い preset の fontSize 焼き込みを潰さない。
    --   [English]: A font-family fallback shared by every text slot
    --   (equivalent to ggplot's theme(text = element_text(family=...))).
    --   Priority is: the per-slot FontSpec's fsFamily > this > "sans-serif"
    --   (resolved by @mkFontTS@). Unlike replacing a whole slot (Last
    --   FontSpec), this doesn't clobber the preset's baked-in fontSize.
  , toFontFamily    :: !(Last Text)    -- text family (全 slot 共通 fallback)
  } deriving stock (Generic, Show, Eq)
    -- [日本語]: ★ Phase 43 A3: 全 field が `Last` の素直な per-field 合成なので generic 導出。
    --   位置依存の手書き instance (旧 `a1..p1` を数で揃える形) を撲滅し、 以後の field
    --   追加を「field を足すだけ」で安全にする。挙動は旧手書きと完全同型。
    --   [English]: Since every field is a straightforward per-field
    --   composition of `Last`, the instance is generically derived. This
    --   eliminates the old position-dependent hand-written instance (which
    --   lined up `a1..p1` by count) and makes future field additions safe —
    --   just add the field. Behavior is exactly identical to the old
    --   hand-written version.
    deriving (Semigroup, Monoid) via Generically ThemeOverride

instance ToJSON   ThemeOverride
instance FromJSON ThemeOverride

-- |
-- Module      : Hgg.Plot.Spec.Theme
-- Description : theme preset (ThemeName) + series palette + element 単位 override
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Hgg.Plot.Spec' の module 分割で切り出し。 描画 theme の名前
-- ('ThemeName')、 preset ごとの series palette、 named palette (Okabe-Ito 等)、
-- element 単位の上書き ('ThemeOverride'、 ggplot theme(element_*) 相当) を持つ。
-- 公開 API は従来どおり 'Hgg.Plot.Spec' (facade) が re-export する。
-- 挙動・出力は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE OverloadedStrings         #-}
module Hgg.Plot.Spec.Theme
  ( -- * theme preset + palette
    ThemeName(..)
  , themeSeriesPalette
  , okabeIto, tolBright, brewerSet2, brewerDark2
    -- * element 単位 override
  , ThemeOverride(..)
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic, Generically (..))

import           Hgg.Plot.Spec.Decoration (FontSpec)

-- | 描画 theme (= 名前で参照、 関数を持たない = JSON serializable)。
-- ggplot 標準 preset (ThemeGrey) + ブランドテーマを追加。
--   * ThemeGrey            = ggplot 既定 theme_grey (灰背景 #EBEBEB・白 grid・枠なし・軸線なし)
--   * ThemeNoir     = ブランド (暗・上品・寒色アクセント、 コンペ用に残置)
--   * ThemeLumen    = ブランド (白基調・深い差し色・清潔、 コンペ用に残置)
--   * ThemeParchment     = 羊皮紙基調・明の正式テーマ。 配色は cream/gold/ink +
--       Universal Categorical series 由来。
--   * ThemeParchmentDark = 同テーマの暗版 (焦茶インク背景・series は shade 300 で沈み防止)。
data ThemeName = ThemeDefault | ThemeMinimal | ThemeDark | ThemeLight
               | ThemeGrey | ThemeBW | ThemeClassic | ThemeVoid | ThemeLinedraw
               | ThemeNoir | ThemeLumen
               | ThemeParchment | ThemeParchmentDark
  deriving (Show, Eq, Generic)

instance ToJSON   ThemeName
instance FromJSON ThemeName

-- | preset ごとの既定 series palette (= palette 未指定時に使う色順)。
-- ggplot 系 preset は従来通り hggMain (既定配色)、 ブランド 3 種は専用 series。
-- Layout.computeLayout の catPal 既定がこれを参照する (= palette 指定で上書き可)。
themeSeriesPalette :: ThemeName -> [Text]
themeSeriesPalette t = case t of
  ThemeNoir  -> ["#7AA2F7", "#BB9AF7", "#7DCFFF", "#9ECE6A", "#E0AF68", "#F7768E"]
  ThemeLumen -> ["#4C5BD4", "#D6336C", "#2F9E44", "#E8590C", "#7048E8", "#1098AD"]
  -- Parchment 明: 案3 (#1 = White Rabbit Inner Ear Pink #F0A5A0、
  --   #3 = Dormouse 系 Warm Yellow #E8D58A、 他は既定配色)。
  ThemeParchment     -> canvasPal
  -- 暗版 (Charcoal 背景): 案3 の暗色 (purple/teal/rose/wine) を明度調整し沈み防止。色相・順序は維持。
  ThemeParchmentDark -> [ "#F0A5A0", "#A98BD0", "#E8D58A", "#5FA0A8"
                            , "#E0617E", "#B8C7D9", "#D9685F" ]
  -- default 系 (grey/default/minimal/light/dark) は ggplot2 既定 scales::hue_pal() にならう。
  --   ★Phase 28 (2026-06-14): 固定 7 色版でなく **群数 n 依存の hue sentinel** を返す。
  --   ggplot は離散色スケールごとに hue_pal()(n) を再計算するため、 群数 3 なら
  --   赤/緑/青、 4 なら別配色…と変わる。 固定 7 色だと群数 3 でも index 0,1,2 =
  --   赤/金/緑 になり R4DS と食い違っていた。 sentinel は Layout.catPal /
  --   Bridge.resolveGrouped が 'ggplotHue' n で展開する。
  _ -> ["__ggplot_hue__"]
  where
    canvasPal = [ "#F0A5A0", "#7A5C92", "#E8D58A", "#3E6A6F"
                , "#C7445D", "#B8C7D9", "#7E1F23" ]

-- | 学術向け named series palette。 theme とは独立に `palette <名>` で使う (colorblind-safe 中心)。
-- Okabe-Ito (Okabe & Ito 2008、 色覚バリアフリー定番、 R palette.colors("Okabe-Ito") と同一)。
okabeIto :: [Text]
okabeIto = [ "#000000", "#E69F00", "#56B4E9", "#009E73"
           , "#F0E442", "#0072B2", "#D55E00", "#CC79A7" ]

-- | Paul Tol bright (7 色、 色覚バリアフリー)。
tolBright :: [Text]
tolBright = [ "#4477AA", "#EE6677", "#228833", "#CCBB44"
            , "#66CCEE", "#AA3377", "#BBBBBB" ]

-- | ColorBrewer Set2 (8 色、 柔らかい定性)。
brewerSet2 :: [Text]
brewerSet2 = [ "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"
             , "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3" ]

-- | ColorBrewer Dark2 (8 色、 濃いめ定性、 白背景向き)。
brewerDark2 :: [Text]
brewerDark2 = [ "#1B9E77", "#D95F02", "#7570B3", "#E7298A"
              , "#66A61E", "#E6AB02", "#A6761D", "#666666" ]

-- ===========================================================================
-- Phase 9 A-2: element 単位 theme override (ggplot theme(element_*) 相当)
-- ===========================================================================
-- | preset (ThemeName) に要素単位で上書きを合成する override。 各 field は Last で
-- 「指定があれば優先」。 'resolveTheme' (Render) が preset palette に合成する。
-- 全 field Monoid なので setter を `<>` で重ねられる (ggplot の theme() 加算と同様)。
data ThemeOverride = ThemeOverride
  { toPlotBg       :: !(Last Text)   -- plot.background fill
  , toPanelBg      :: !(Last Text)   -- panel.background fill
  , toShowPanel    :: !(Last Bool)   -- panel 矩形を塗るか
  , toGridColor    :: !(Last Text)   -- panel.grid colour
  , toShowGrid     :: !(Last Bool)   -- panel.grid on/off
  , toShowBorder   :: !(Last Bool)   -- panel.border on/off
  , toShowAxisLine :: !(Last Bool)   -- axis.line on/off
  , toAxisColor    :: !(Last Text)   -- axis 線/目盛り色
  , toTextColor    :: !(Last Text)   -- 文字色
    -- ★ Phase 9 A-3: 文字 theme 統合 (ggplot theme(text/plot.title/axis.title/...) 相当)。
    --   各 slot の FontSpec を theme から差し替え可能に。 優先順位は
    --   override (これ) > font setter (vsTitleFont 等) > preset 既定 ('mkFontTS')。
  , toTitleFont     :: !(Last FontSpec)  -- plot.title
  , toAxisLabelFont :: !(Last FontSpec)  -- axis.title
  , toTickFont      :: !(Last FontSpec)  -- axis.text
  , toLegendFont    :: !(Last FontSpec)  -- legend.title / legend.text
    -- ★ axis.text の回転角 (度・CCW)。 per-axis 'axisRotate' 未指定時の fallback。
    --   'toAxisTextAngle' = x/y 共通既定、 'toAxisTextAngleX'/'toAxisTextAngleY' = 軸別上書き
    --   (Phase 50 A3・軸別 > 共通 の優先。 'axisTextAngleXOf'/'axisTextAngleYOf' で解決)。
  , toAxisTextAngle  :: !(Last Double)
  , toAxisTextAngleX :: !(Last Double)
  , toAxisTextAngleY :: !(Last Double)
    -- ★ Phase 9 A-4: strip.background (facet strip の灰矩形)。
  , toStripBg       :: !(Last Text)   -- strip.background fill
  , toShowStrip     :: !(Last Bool)   -- strip 矩形を塗るか
    -- ★ Phase 43 A4: プリセット専用だった 4 項目に上書き口を追加 (= 全プロパティ `<>` 上書き
    --   可能に)。対応 'ThemePalette' field = tpTitleHjust / tpTitleColor / tpTickLineColor /
    --   tpLegendKeyBg。generic 導出なので field 追加のみで instance は自動追従。
  , toTitleHjust    :: !(Last Double) -- plot.title の水平揃え (0=左、 0.5=中央)
  , toTitleColor    :: !(Last Text)   -- plot.title / axis.title の文字色
  , toTickLineColor :: !(Last Text)   -- 軸目盛線 (tick mark) の色
  , toLegendKeyBg   :: !(Last Text)   -- legend.key 背景塗り色 ("" なら塗らない)
  } deriving stock (Generic, Show, Eq)
    -- ★ Phase 43 A3: 全 field が `Last` の素直な per-field 合成なので generic 導出。
    --   位置依存の手書き instance (旧 `a1..p1` を数で揃える形) を撲滅し、 以後の field
    --   追加 (A4) を「field を足すだけ」で安全にする。挙動は旧手書きと完全同型。
    deriving (Semigroup, Monoid) via Generically ThemeOverride

instance ToJSON   ThemeOverride
instance FromJSON ThemeOverride


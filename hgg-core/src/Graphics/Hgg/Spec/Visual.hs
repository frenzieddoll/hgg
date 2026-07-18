-- |
-- Module      : Graphics.Hgg.Spec.Visual
-- Description : VisualSpec (= 外側 Monoid、 図全体の宣言型 spec) + Inset
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Graphics.Hgg.Spec' の module 分割で切り出し。 図全体の宣言型 spec
-- 'VisualSpec' と field-wise Monoid 合成 (@design/monoid-semantics.md@)、 および
-- 'Inset' を持つ。 'Inset.inSpec :: VisualSpec' ⇄ 'VisualSpec.vsInsets :: [Inset]'
-- の相互参照ゆえ 2 型は本 module に同居する (Phase 55 A1 実測・唯一の循環ペア)。
-- 公開 API は従来どおり 'Graphics.Hgg.Spec' (facade) が re-export する。
-- 挙動・出力 (JSON 形含む) は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Visual
  ( VisualSpec(..)
  , Inset(..)
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.List
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           Graphics.Hgg.Unit (Length)
import           Graphics.Hgg.Spec.Axis (AxisSpec)
import           Graphics.Hgg.Spec.Column (ColRef)
import           Graphics.Hgg.Spec.Decoration (Annotation, FontSpec, LegendSpec,
                                               MarginalSpec, ReferenceLine)
import           Graphics.Hgg.Spec.Layer (Layer)
import           Graphics.Hgg.Spec.Mark (Coord, FacetScales, FacetSpace)
import           Graphics.Hgg.Spec.Theme (ThemeName, ThemeOverride)

-- ===========================================================================
-- Inset (= P13、 親 plot に小型 sub-plot を埋込み)
-- ===========================================================================

data Inset = Inset
  { inSpec :: !VisualSpec
  , inX    :: !Double, inY :: !Double
  , inW    :: !Double, inH :: !Double
  } deriving (Show, Eq, Generic)

instance ToJSON   Inset
instance FromJSON Inset

-- ===========================================================================
-- VisualSpec (= 外側 Monoid)
-- ===========================================================================

-- | 図全体の宣言型 spec。 全 field を Monoid 化して field-wise `<>` 合成。
data VisualSpec = VisualSpec
  { vsLayers :: ![Layer]
  , vsTitle  :: !(Last Text)
  , vsTheme  :: !(Last ThemeName)
  , vsFacet  :: !(Last ColRef)
  , vsXLabel :: !(Last Text)
  , vsYLabel :: !(Last Text)
  , vsXAxis  :: !(Last AxisSpec)        -- ★ Phase 26 §C-2 #1
  , vsYAxis  :: !(Last AxisSpec)        -- ★ Phase 26 §C-2 #1
  , vsYAxisRight :: !(Last AxisSpec)    -- ★ P5 dual Y 軸 (右側)
  , vsRefLines :: ![ReferenceLine]      -- ★ Phase 26 §C-2 #3
  , vsMarginal :: !(Last MarginalSpec)  -- ★ Phase 26 §C-2 #10
  , vsSubplots :: ![VisualSpec]         -- ★ Phase 26 S5-e-1 panel grid (= facet と独立、 任意の sub-spec 並列)
  , vsSubplotCols :: !(Last Int)         -- ★ P18 2D grid 折り返し列数
  , vsLegend   :: !(Last LegendSpec)    -- ★ P8 2026-05-25 凡例設定 (= Nothing なら auto)
  , vsAnnotations :: ![Annotation]      -- ★ P6 任意 overlay (text/arrow/rect/line)
  , vsInsets      :: ![Inset]            -- ★ P13 inset axes
  , vsPalette     :: !(Last [Text])       -- ★ P17 categorical palette (= Nothing なら hggMain F-3)
  , vsContinuousPal :: !(Last [Text])     -- ★ P17 continuous palette (= Nothing なら viridis5)
  , vsTitleFont     :: !(Last FontSpec)   -- ★ frontend-settings v0.1 §1.3
  , vsAxisLabelFont :: !(Last FontSpec)   -- ★ 〃
  , vsTickFont      :: !(Last FontSpec)   -- ★ 〃
  , vsLegendFont    :: !(Last FontSpec)   -- ★ 〃
  , vsWidth  :: !(Last Length)   -- ★ Phase 33: 図幅 (Length・既定 mm)。px=pt×dpi/72。
  , vsHeight :: !(Last Length)   -- ★ Phase 33: 図高 (Length・既定 mm)。
    -- ★ Phase 33: 描画 dpi。px backend は px=pt×dpi/72、PDF backend は無視 (pt 直結)。
    --   未指定 = 96 (web 標準)。
  , vsDpi    :: !(Last Double)
    -- ★ Phase 8 A2 Step2: coord_fixed(ratio) 相当。 panel の 高/幅 比 (aspect)。
    --   Nothing = 可用域を埋める (ggplot 既定 Coord$aspect = NULL)。 Just a (a>0) =
    --   可用域内で aspect を保つ最大 panel を取り中央寄せ。 root: ggplot R/coord-.R。
  , vsAspect :: !(Last Double)
    -- ★ Phase 8 C G7: facet_wrap の列数 (ncol)。 Nothing = 従来の 1 行 N 列 (非破壊)、
    --   Just n = n 列で複数行に折り返し (nrow = ceil(panel 数 / n))。 root: ggplot facet_wrap。
  , vsFacetNcol :: !(Last Int)
    -- ★ Phase 8 C G7 part-b: facet_grid(row ~ col)。 2 変数 cross 配置。
    --   vsFacetRow = 行を作る変数 (levels が各行、 右側 strip)、 vsFacetCol = 列を作る変数
    --   (levels が各列、 上側 strip)。 両 Nothing = grid 無し (= 従来 facet_wrap 経路)。
    --   片方のみ指定も可 (1 行 or 1 列の grid)。 root: ggplot facet_grid。
  , vsFacetRow :: !(Last ColRef)
  , vsFacetCol :: !(Last ColRef)
    -- ★ Phase 9 A-2: element 単位 theme override (preset に合成、 resolveTheme で解決)。
  , vsThemeOverride :: !ThemeOverride
    -- ★ Phase 9 C: 座標系 (coord_flip 等)。 Nothing = CoordCartesian (= ggplot 既定)。
  , vsCoord :: !(Last Coord)
    -- ★ Phase 11 A4-a: 軸反転 (= ggplot scale_x_reverse / scale_y_reverse)。
    --   Just True で該当軸の scale range (rLo/rHi) を入替え、 大値が小座標側に。
    --   tick/grid/glyph は scaleApply 経由なので自動追従 (renderer 無変更)。
    --   coord_flip とは独立合成 (= データ軸基準で反転、 flip 後も x/y データ軸を指す)。
  , vsReverseX :: !(Last Bool)
  , vsReverseY :: !(Last Bool)
    -- ★ Phase 11 A4-c: 明示凡例タイトル (= ggplot scale_color_*(name=) / labs(color=))。
    --   Nothing なら従来通りタイトル非表示 (= legend 項目が自己説明的)。 明示値なので
    --   bakeSpec (色列 inline 化) 後も保持され HS/PS が同一描画 (Phase 9 A-5 の食い違い回避)。
  , vsLegendTitle :: !(Last Text)
    -- ★ Phase 11 A4-e: 色/サイズ scale 拡充。
    --   vsColorManual = ggplot scale_color_manual(values=)。 カテゴリ名→hex の辞書。
    --     ColorByCol で当該名があれば palette index より優先。 未登録名は従来の palette。
  , vsColorManual :: !(Last [(Text, Text)])
    --   vsColorGradient2 = ggplot scale_color_gradient2(low,mid,high,midpoint=)。 発散 palette。
    --     ColorByContinuous で midpoint を 0.5 に固定 (lo..mid→[0,.5]・mid..hi→[.5,1] 個別正規化)。
  , vsColorGradient2 :: !(Last (Text, Text, Text, Double))
    --   vsSizeRange = ggplot scale_size(range=c(min,max))。 sizeBy の px 範囲 (default (3,10))。
  , vsSizeRange :: !(Last (Double, Double))
    -- ★ Phase 11 A5-a: labs サブシステム (= ggplot labs(subtitle=,caption=,tag=))。
    --   vsSubtitle = title 直下の小見出し。 vsCaption = 図右下の注記。 vsTag = 左上隅のタグ。
    --   いずれも Nothing で従来同一 (= 描画も margin 予約も無し)。
  , vsSubtitle :: !(Last Text)
  , vsCaption  :: !(Last Text)
  , vsTag      :: !(Last Text)
    -- ★ Phase 11 A5-c: guides サブシステム (= ggplot guide_legend(reverse=, ncol=, nrow=))。
    --   位置 ('vsLegend') とは独立 (= vsLegendTitle と同じく VisualSpec レベルに置き
    --   LegendSpec Semigroup の position 上書き footgun を回避)。 いずれも Nothing で従来同一。
    --   vsLegendReverse = 凡例キーの表示順を逆に (色は各キーに固定のまま)。
    --   vsLegendNcol = 縦凡例 (Right/Inside) の列数、 vsLegendNrow = 横凡例 (Bottom) の行数。
  , vsLegendReverse :: !(Last Bool)
  , vsLegendNcol    :: !(Last Int)
  , vsLegendNrow    :: !(Last Int)
    -- ★ Phase 11 A7-a: coord_cartesian(xlim,ylim) = データを落とさない zoom。
    --   axisRange (= scale limits、 範囲外データを切る) と別概念で、 scale domain を
    --   指定範囲に上書きするだけ。 stat (regression/density 等) は全データから計算され、
    --   範囲外の glyph は panel に clip される (= ggplot coord_cartesian, expand=FALSE)。
    --   numeric 軸のみ有効 (categorical / funnel 軸は無視)。 Nothing で従来同一。
  , vsCoordXLim :: !(Last (Double, Double))
  , vsCoordYLim :: !(Last (Double, Double))
    -- ★ Phase 11 A7-b: facet free scales (= ggplot facet_wrap(scales=))。 Nothing =
    --   FacetFixed (全 panel 共通 domain)。 free な軸は各 panel が自分のデータで domain を
    --   再計算し、 全 panel に軸を表示する (= 値比較より panel 内分布を優先)。 facet_wrap
    --   (renderFaceted) のみ対応 (facet_grid は別途)。
  , vsFacetScales :: !(Last FacetScales)
    -- ★ Phase 11 A7-b: facet_grid の panel サイズ配分 (= ggplot facet_grid(space=))。
    --   Nothing = SpaceFixed (全 panel 同サイズ)。 free な軸は track 重みを data 範囲比例に。
  , vsFacetSpace :: !(Last FacetSpace)
    -- ★ Phase 18 A1: subplot panel の名前選択 (= 'repeatFields' の逆方向)。
    --   Just ws = vsSubplots の子を vsTitle ∈ ws で filter し **ws の列挙順に並べ替え**
    --   (ggplot discrete limits と同じ「選択 + 順序」 の意味論)。 名前不一致は無視。
    --   Nothing = 従来通り全 panel。 facet panel (データ分割) は対象外 (subplots 専用)。
  , vsPanelSel :: !(Last [Text])
    -- ★ Phase 18 A2: 離散軸カテゴリの limits (= ggplot @scale_x_discrete(limits=)@ /
    --   @scale_y_discrete(limits=)@、 連続版 'axisRange' の離散対応)。 Just ws = 当該軸の
    --   encoding が ColTxt の layer について **カテゴリ行を選択 + ws の列挙順に並べ替え**
    --   (行 filter は全 row-aligned encoding を同 index で間引く)。 aes 基準 (coord_flip と
    --   直交 = flip 後も x/y データ軸を指す、 'vsReverseX' と同思想)。 Nothing = 従来通り。
    --   ★Last-上書き footgun 回避のため AxisSpec でなく VisualSpec 直 field
    --   ('vsLegendTitle' / Phase 11 A4-c と同じ判断)。
  , vsXDiscreteLimits :: !(Last [Text])
  , vsYDiscreteLimits :: !(Last [Text])
  } deriving (Generic, Show, Eq)

instance ToJSON   VisualSpec
instance FromJSON VisualSpec

-- | 図全体の合成。 list 系 (layers/refLines/subplots/annotations/insets) は
-- concat、 残りは 'Last' で後勝ち、 themeOverride は element 単位 Monoid。
-- 合成規則の全体表は @design/monoid-semantics.md@ を参照。
-- ★ Phase 43 A3: レコードフィールド形式 (位置依存撲滅・挙動不変)。49 field の位置揃え
--   (旧 `l1 t1 th1 …`) を撲滅し、 以後の field 追加を「行を 1 本足すだけ」 + `-Wmissing-fields`
--   保護下にする。唯一の特殊合成 'mergeColorManual' (= Phase 52.A10/19 の dedup 合成) のみ
--   名前付きで温存。list 系は `<>`=concat、 残りは `Last` 後勝ち、 themeOverride は element Monoid。
instance Semigroup VisualSpec where
  a <> b = VisualSpec
    { vsLayers       = vsLayers a       <> vsLayers b
    , vsTitle        = vsTitle a        <> vsTitle b
    , vsTheme        = vsTheme a        <> vsTheme b
    , vsFacet        = vsFacet a        <> vsFacet b
    , vsXLabel       = vsXLabel a       <> vsXLabel b
    , vsYLabel       = vsYLabel a       <> vsYLabel b
    , vsXAxis        = vsXAxis a        <> vsXAxis b
    , vsYAxis        = vsYAxis a        <> vsYAxis b
    , vsYAxisRight   = vsYAxisRight a   <> vsYAxisRight b
    , vsRefLines     = vsRefLines a     <> vsRefLines b
    , vsMarginal     = vsMarginal a     <> vsMarginal b
    , vsSubplots     = vsSubplots a     <> vsSubplots b
    , vsSubplotCols  = vsSubplotCols a  <> vsSubplotCols b
    , vsLegend       = vsLegend a       <> vsLegend b
    , vsAnnotations  = vsAnnotations a  <> vsAnnotations b
    , vsInsets       = vsInsets a       <> vsInsets b
    , vsPalette      = vsPalette a      <> vsPalette b
    , vsContinuousPal = vsContinuousPal a <> vsContinuousPal b
    , vsTitleFont    = vsTitleFont a    <> vsTitleFont b
    , vsAxisLabelFont = vsAxisLabelFont a <> vsAxisLabelFont b
    , vsTickFont     = vsTickFont a     <> vsTickFont b
    , vsLegendFont   = vsLegendFont a   <> vsLegendFont b
    , vsWidth        = vsWidth a        <> vsWidth b
    , vsHeight       = vsHeight a       <> vsHeight b
    , vsDpi          = vsDpi a          <> vsDpi b
    , vsAspect       = vsAspect a       <> vsAspect b
    , vsFacetNcol    = vsFacetNcol a    <> vsFacetNcol b
    , vsFacetRow     = vsFacetRow a     <> vsFacetRow b
    , vsFacetCol     = vsFacetCol a     <> vsFacetCol b
    , vsThemeOverride = vsThemeOverride a <> vsThemeOverride b
    , vsCoord        = vsCoord a        <> vsCoord b
    , vsReverseX     = vsReverseX a     <> vsReverseX b
    , vsReverseY     = vsReverseY a     <> vsReverseY b
    , vsLegendTitle  = vsLegendTitle a  <> vsLegendTitle b
      -- ★特殊: 全群の色辞書を concat+dedup (Last 後勝ちだと先頭群が消える・Phase 52.A10/19)
    , vsColorManual  = mergeColorManual (vsColorManual a) (vsColorManual b)
    , vsColorGradient2 = vsColorGradient2 a <> vsColorGradient2 b
    , vsSizeRange    = vsSizeRange a    <> vsSizeRange b
    , vsSubtitle     = vsSubtitle a     <> vsSubtitle b
    , vsCaption      = vsCaption a      <> vsCaption b
    , vsTag          = vsTag a          <> vsTag b
    , vsLegendReverse = vsLegendReverse a <> vsLegendReverse b
    , vsLegendNcol   = vsLegendNcol a   <> vsLegendNcol b
    , vsLegendNrow   = vsLegendNrow a   <> vsLegendNrow b
    , vsCoordXLim    = vsCoordXLim a    <> vsCoordXLim b
    , vsCoordYLim    = vsCoordYLim a    <> vsCoordYLim b
    , vsFacetScales  = vsFacetScales a  <> vsFacetScales b
    , vsFacetSpace   = vsFacetSpace a   <> vsFacetSpace b
    , vsPanelSel     = vsPanelSel a     <> vsPanelSel b
    , vsXDiscreteLimits = vsXDiscreteLimits a <> vsXDiscreteLimits b
    , vsYDiscreteLimits = vsYDiscreteLimits a <> vsYDiscreteLimits b
    }

instance Monoid VisualSpec where
  -- 全 field が Monoid (list=[]・Last=Last Nothing・ThemeOverride=element mempty) なので
  -- 一律 mempty。レコード形式により field 追加時の位置ズレ事故が起きない。
  mempty = VisualSpec
    { vsLayers = mempty, vsTitle = mempty, vsTheme = mempty, vsFacet = mempty
    , vsXLabel = mempty, vsYLabel = mempty, vsXAxis = mempty, vsYAxis = mempty
    , vsYAxisRight = mempty, vsRefLines = mempty, vsMarginal = mempty
    , vsSubplots = mempty, vsSubplotCols = mempty, vsLegend = mempty
    , vsAnnotations = mempty, vsInsets = mempty, vsPalette = mempty
    , vsContinuousPal = mempty, vsTitleFont = mempty, vsAxisLabelFont = mempty
    , vsTickFont = mempty, vsLegendFont = mempty, vsWidth = mempty, vsHeight = mempty
    , vsDpi = mempty, vsAspect = mempty, vsFacetNcol = mempty, vsFacetRow = mempty
    , vsFacetCol = mempty, vsThemeOverride = mempty, vsCoord = mempty
    , vsReverseX = mempty, vsReverseY = mempty, vsLegendTitle = mempty
    , vsColorManual = mempty, vsColorGradient2 = mempty, vsSizeRange = mempty
    , vsSubtitle = mempty, vsCaption = mempty, vsTag = mempty
    , vsLegendReverse = mempty, vsLegendNcol = mempty, vsLegendNrow = mempty
    , vsCoordXLim = mempty, vsCoordYLim = mempty, vsFacetScales = mempty
    , vsFacetSpace = mempty, vsPanelSel = mempty, vsXDiscreteLimits = mempty
    , vsYDiscreteLimits = mempty
    }

-- | Phase 52.A10: scale_color_manual 辞書の合成。 旧実装は Last の最後勝ちで、 異モデル
-- 重畳 (各レイヤが 1 群の ColorByCol + 単一辞書) のとき先頭群の色辞書が捨てられ全線同色化
-- していた。 ここでは両辞書を concat し同じカテゴリ名は後勝ちで dedup する (= 全群の色が
-- 残り各 ColorByCol レイヤが自色を引ける)。 片方 Nothing は他方をそのまま採用。
mergeColorManual :: Last [(Text, Text)] -> Last [(Text, Text)] -> Last [(Text, Text)]
mergeColorManual (Last Nothing) b = b
mergeColorManual a (Last Nothing) = a
mergeColorManual (Last (Just d1)) (Last (Just d2)) =
  Last (Just (dedupColorManual (d1 <> d2)))
  where
    -- 同 key (カテゴリ名) は後勝ち = 後方の値を優先。 出現順は最初の出現位置で保存。
    dedupColorManual kvs =
      let lastVal k = last [ v | (k', v) <- kvs, k' == k ]
          -- ★Phase 19: 旧 foldr 形は interleaved 重複で最終出現順になっていた
          -- (辞書は lookup のみで順序非依存だが、 コメント通り初出順に統一)
          keysInOrder = nubKeep (map fst kvs)
      in [ (k, lastVal k) | k <- keysInOrder ]
    nubKeep = Data.List.nub

-- |
-- Module      : Hgg.Plot.Spec.Setters
-- Description : VisualSpec への top-level setter (title / theme / axis / legend / annot 等)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Hgg.Plot.Spec' の module 分割で切り出し。 'VisualSpec' を
-- `<>` で組み立てる top-level setter 群 ('layer' / 'title' / 'theme' /
-- 'facet' 系 / 'legend' 系 / 'annot' 系 / inset / 図サイズ / font setter 等) と
-- 'Labs'、 VisualSpec 依存の mark 構築子 3 種 ('histogramWide' / 'distCols' /
-- 'ridgeAutoFlip') を持つ。 図の合成演算子は 'Hgg.Plot.Spec.Concat' 側。
-- 公開 API は従来どおり 'Hgg.Plot.Spec' (facade) が re-export する。
-- 挙動・出力は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Hgg.Plot.Spec.Setters
  ( -- * layer 装着 + 基本 setter
    layer, purePlot, title, subtitle, caption, tag, xLabel, yLabel
  , Labs(..), labs, emptyLabs
  , theme, facet, facetWrap, facetGrid, facetCols, facetScales, facetSpace
  , subplots, subplotCols, repeatFields, selectPanels, selectedSubplots
  , scaleXDiscreteLimits, scaleYDiscreteLimits, applyDiscreteLimits, reindexLayer
    -- * theme override setter
  , plotBg, panelFill, panelBorder, gridColor, themeGrid, themeAxisLine
  , axisColor, textColor, tickColor, titleColor, titleHjust
  , stripFill, themeStrip, legendKeyBg
  , themeTitleFont, themeAxisLabelFont, themeTickFont, themeLegendFont
  , themeAxisTextAngle, themeAxisTextAngleX, themeAxisTextAngleY
  , axisTextAngleXOf, axisTextAngleYOf
    -- * VisualSpec 依存の mark 構築子 (Phase 55: Constructors に置けない 3 種)
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

import           Hgg.Plot.Unit (Length, Pos (..), mm, (*~))
import           Hgg.Plot.Spec.Axis (AxisSpec)
import           Hgg.Plot.Spec.Column
import           Hgg.Plot.Spec.Bake (bakeSpec)
import           Hgg.Plot.Spec.Constructors (binCount, histogram, (<+>))
import           Hgg.Plot.Spec.Decoration
import           Hgg.Plot.Spec.Layer
import           Hgg.Plot.Spec.Mark
import           Hgg.Plot.Spec.Theme (ThemeName, ThemeOverride (..))
import           Hgg.Plot.Spec.Visual


-- ===========================================================================
-- Top-level setters
-- ===========================================================================

-- | spec の純粋値起点 (= 'mempty' alias)。 mempty 直接でも良いが、 「これは
-- plot spec の最初の値ですよ」 という意図を名前で示す。 副作用関数
-- ('plot' / 'saveSVG' 等) との対比で `pure-` prefix。
purePlot :: VisualSpec
purePlot = mempty

-- | 'Layer' を 'VisualSpec' に lift (= layer リストの単一要素 spec)。
layer :: Layer -> VisualSpec
layer l = mempty { vsLayers = [l] }

title, xLabel, yLabel :: Text -> VisualSpec
title  t = mempty { vsTitle  = Last (Just t) }
xLabel t = mempty { vsXLabel = Last (Just t) }
yLabel t = mempty { vsYLabel = Last (Just t) }

-- | Phase 11 A4-c: 凡例タイトル (= ggplot scale_color_*(name=) / labs(color=))。
--   color/fill/shape/linetype の凡例ヘッダに表示。 軸タイトルは 'xLabel'/'yLabel' を使う
--   (= positional scale の name = 軸ラベル)。
legendTitle :: Text -> VisualSpec
legendTitle t = mempty { vsLegendTitle = Last (Just t) }

-- | Phase 11 A5-a: labs サブシステムの個別 setter (= ggplot labs(subtitle=,caption=,tag=))。
--   'subtitle' = title 直下の小見出し、 'caption' = 図右下の注記、 'tag' = 左上隅のタグ。
subtitle, caption, tag :: Text -> VisualSpec
subtitle t = mempty { vsSubtitle = Last (Just t) }
caption  t = mempty { vsCaption  = Last (Just t) }
tag      t = mempty { vsTag      = Last (Just t) }

-- | Phase 11 A5-a: ggplot @labs()@ 相当のまとめ setter。 各フィールドは 'Maybe' で
--   「指定しない」 を表す。 @labs emptyLabs { labsTitle = Just "T", labsX = Just "x" }@ の
--   ように 'emptyLabs' を起点に必要な label だけ埋める。 @labsColor@ は凡例タイトル
--   ('legendTitle')。 指定した label を 'mconcat' で合成するので既存 setter と等価。
data Labs = Labs
  { labsTitle    :: Maybe Text
  , labsSubtitle :: Maybe Text
  , labsCaption  :: Maybe Text
  , labsTag      :: Maybe Text
  , labsX        :: Maybe Text
  , labsY        :: Maybe Text
  , labsColor    :: Maybe Text   -- = 凡例タイトル ('legendTitle')
  } deriving (Show, Eq)

-- | 全フィールド未指定の 'Labs' 起点 (= record update のベース)。
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

-- | Phase 9 A-2: element 単位 theme override の setter 群 (ggplot theme(element_*) 相当)。
-- `theme ThemeGrey <> themeGrid False <> panelFill "#fafafa"` のように `<>` で重ねる。
themeGrid :: Bool -> VisualSpec       -- panel.grid on/off
themeGrid b = mempty { vsThemeOverride = mempty { toShowGrid = Last (Just b) } }

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

axisColor :: Text -> VisualSpec       -- axis 線/目盛り色
axisColor c = mempty { vsThemeOverride = mempty { toAxisColor = Last (Just c) } }

textColor :: Text -> VisualSpec       -- 文字色
textColor c = mempty { vsThemeOverride = mempty { toTextColor = Last (Just c) } }

-- | Phase 9 A-3: theme 経由の font setter 群 (ggplot theme(plot.title=element_text(...)) 等)。
-- vsTitleFont 等の専用 setter より優先される (= 後付け theme 上書き)。 `<>` で重ねる。
themeTitleFont :: FontSpec -> VisualSpec      -- plot.title
themeTitleFont f = mempty { vsThemeOverride = mempty { toTitleFont = Last (Just f) } }

themeAxisLabelFont :: FontSpec -> VisualSpec  -- axis.title
themeAxisLabelFont f = mempty { vsThemeOverride = mempty { toAxisLabelFont = Last (Just f) } }

themeTickFont :: FontSpec -> VisualSpec       -- axis.text
themeTickFont f = mempty { vsThemeOverride = mempty { toTickFont = Last (Just f) } }

themeLegendFont :: FontSpec -> VisualSpec     -- legend.title / legend.text
themeLegendFont f = mempty { vsThemeOverride = mempty { toLegendFont = Last (Just f) } }

-- | axis.text の回転角 (度) を theme から指定。 per-axis 'axisRotate' 未指定時の fallback。
themeAxisTextAngle :: Double -> VisualSpec
themeAxisTextAngle a = mempty { vsThemeOverride = mempty { toAxisTextAngle = Last (Just a) } }

-- | axis.text の **x 軸のみ** の回転角 (度・CCW) を theme から指定 (Phase 50 A3)。
--   共通 'themeAxisTextAngle' より優先。 per-axis 'xAxis (axisRotate …)' が更に優先。
themeAxisTextAngleX :: Double -> VisualSpec
themeAxisTextAngleX a = mempty { vsThemeOverride = mempty { toAxisTextAngleX = Last (Just a) } }

-- | axis.text の **y 軸のみ** の回転角 (度・CCW) を theme から指定 (Phase 50 A3)。
themeAxisTextAngleY :: Double -> VisualSpec
themeAxisTextAngleY a = mempty { vsThemeOverride = mempty { toAxisTextAngleY = Last (Just a) } }

-- | theme の x 軸 axis.text 回転角を解決 (軸別 'toAxisTextAngleX' > 共通 'toAxisTextAngle')。
--   'resolveAxisAngle' の theme fallback 引数に渡す (Phase 50 A3)。
axisTextAngleXOf :: ThemeOverride -> Last Double
axisTextAngleXOf o = toAxisTextAngle o <> toAxisTextAngleX o

-- | theme の y 軸 axis.text 回転角を解決 (軸別 'toAxisTextAngleY' > 共通 'toAxisTextAngle')。
axisTextAngleYOf :: ThemeOverride -> Last Double
axisTextAngleYOf o = toAxisTextAngle o <> toAxisTextAngleY o

-- | Phase 9 A-4: facet strip.background の塗り色を指定 (= 塗り on + 色)。
stripFill :: Text -> VisualSpec
stripFill c = mempty { vsThemeOverride = mempty { toStripBg = Last (Just c), toShowStrip = Last (Just True) } }

-- | facet strip 矩形の on/off。
themeStrip :: Bool -> VisualSpec
themeStrip b = mempty { vsThemeOverride = mempty { toShowStrip = Last (Just b) } }

-- | Phase 43 A4: プリセット専用だった 4 項目の theme 上書き setter (= 全プロパティ `<>` 上書き)。
--   `theme ThemeGrey <> titleHjust 0.5 <> legendKeyBg "#fff"` のように重ねる。
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

-- | Phase 8 C G7: facet_wrap(~c, ncol=n)。 c で分割し n 列で複数行に折り返す。
--   ncol 未使用 (= 'facet' のみ) なら従来の 1 行 N 列。
facetWrap :: ColRef -> Int -> VisualSpec
facetWrap c n = mempty { vsFacet = Last (Just c), vsFacetNcol = Last (Just n) }

-- | facet の列数のみ指定 (= 既存 'facet' と併用)。
facetCols :: Int -> VisualSpec
facetCols n = mempty { vsFacetNcol = Last (Just n) }

-- | Phase 11 A7-b: facet_wrap の scale 共有方式 (= ggplot facet_wrap(scales=))。
--   'FacetFixed' (既定) = 共通 domain、 'FacetFree'/'FacetFreeX'/'FacetFreeY' = 該当軸を
--   panel ごとに独立 domain に。 free な軸は全 panel に軸を表示する。 'facet' と併用。
facetScales :: FacetScales -> VisualSpec
facetScales fs = mempty { vsFacetScales = Last (Just fs) }

-- | Phase 11 A7-b: facet_grid の panel サイズ配分 (= ggplot facet_grid(space=))。
--   'SpaceFree' 等で free 軸の track 幅/高を data 範囲に比例配分する。 通常
--   'facetScales' と併用。 facet_grid のみ有効。
facetSpace :: FacetSpace -> VisualSpec
facetSpace fs = mempty { vsFacetSpace = Last (Just fs) }

-- | Phase 8 C G7 part-b: facet_grid(row ~ col)。 row 変数の levels で行、
--   col 変数の levels で列を作り 2 次元の cross 配置にする。 strip は上 (col 名)・
--   右 (row 名)、 軸は最下行 x・左端列 y のみ (ggplot facet_grid 既定)。
facetGrid :: ColRef -> ColRef -> VisualSpec
facetGrid rowC colC = mempty { vsFacetRow = Last (Just rowC)
                             , vsFacetCol = Last (Just colC) }

-- | Phase 26 S5-e-1: panel grid (= facet とは独立、 各 spec を独立 panel として並べる)。
-- |   facet は 1 列でデータを分割するのに対し、 subplots は完全に別 spec を並べる。
-- |   DoE の MainEffects (= 複数 factor を横並び) で使う。
subplots :: [VisualSpec] -> VisualSpec
subplots ss = mempty { vsSubplots = ss }

-- | P18: subplots の 2D grid 折り返し列数。
subplotCols :: Int -> VisualSpec
subplotCols n = mempty { vsSubplotCols = Last (Just n) }

-- | Phase 18 A1: subplot panel を **名前 (= 子 spec の 'vsTitle') で選択 + 並べ替え**。
-- 'repeatFields' (名前リスト → panel 群) の逆方向。 列挙順がそのまま表示順になる
-- (ggplot @scale_*_discrete(limits=)@ と同じ「選択 + 順序」 の意味論)。
-- 一致しない名前は無視、 title 無し panel は選択時には常に落ちる。
--
-- > subplots panels <> selectPanels ["b", "a"] <> subplotCols 2
selectPanels :: [Text] -> VisualSpec
selectPanels ws = mempty { vsPanelSel = Last (Just ws) }

-- | 'vsPanelSel' を適用した後の実効 subplot 列。 描画 ('renderSubplots') の正本で、
-- HS 外 (canvas / PS codec) へ spec を送る側も serialise 前にこれで解決すれば
-- PS 非改修で選択が効く。 選択未指定 ('Nothing') は全 panel をそのまま返す。
selectedSubplots :: VisualSpec -> [VisualSpec]
selectedSubplots s = case getLast (vsPanelSel s) of
  Nothing -> vsSubplots s
  Just ws -> [ p | nm <- ws, p <- vsSubplots s, getLast (vsTitle p) == Just nm ]

-- | Phase 18 A2: 離散 x 軸の limits (= ggplot @scale_x_discrete(limits=)@)。
-- x encoding が ColTxt の layer のカテゴリ行を **選択 + 列挙順に並べ替え**る。
-- aes 基準なので coord_flip と直交 (flip 後も x データ軸を指す)。
scaleXDiscreteLimits :: [Text] -> VisualSpec
scaleXDiscreteLimits ws = mempty { vsXDiscreteLimits = Last (Just ws) }

-- | Phase 18 A2: 離散 y 軸の limits (= ggplot @scale_y_discrete(limits=)@)。
-- 'forest' は cat ラベルが y encoding なのでこちらを使う。
scaleYDiscreteLimits :: [Text] -> VisualSpec
scaleYDiscreteLimits ws = mempty { vsYDiscreteLimits = Last (Just ws) }

-- | Phase 18 A2 の解決 (正本): 'vsXDiscreteLimits' / 'vsYDiscreteLimits' を layer の
-- 行 filter + 並べ替えとして適用する。 layout / render の入口で呼ぶ (冪等)。
--
-- * 当該軸の encoding が 'ColTxt' の layer のみ対象 (数値軸 layer は不変)。
-- * 行 filter は **全 row-aligned encoding** (encX/encY/encY2/errorX/errorY/shapeBy/
--   sizeBy/chain/linetypeBy/label/hover/color 列) を同 index で間引く (整合維持)。
-- * 'ColByName' (resolver 参照) を含む spec は先に 'bakeSpec' で inline 化してから
--   filter する (limits 未指定なら bake もしない = 従来経路完全不変)。
-- * limits は当該 spec 自身の layer にのみ効く (subplot 子へは伝播しない —
--   子は自分の limits を持てる)。
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

-- | layer の全 row-aligned encoding を同じ index 列で間引く ('applyDiscreteLimits' 用)。
-- 長さ @n@ (= cat 列長) と一致する inline 列のみ対象 (不一致・'ColByName' は据え置き)。
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

-- | Vega-Lite @repeat@ 相当: フィールド名のリストを反復し、 各フィールドから
-- |   1 つの view (VisualSpec) を生成して 'subplots' に並べる (= フィールド自動反復)。
-- |   @repeatFields ["a","b","c"] (\\f -> layer (hist f))@ は 3 パネルを作る。
-- |   列数は @<> subplotCols n@ で指定する。 Vega の @repeat@ が encoding 内の
-- |   @{repeat: ...}@ でフィールドを差し込むのに対し、 こちらは生成関数に
-- |   フィールド名を渡す明示形 (spec を値として組む方針ゆえ)。
repeatFields :: [Text] -> (Text -> VisualSpec) -> VisualSpec
repeatFields fields mk = subplots (map mk fields)

-- Phase 55: 以下 3 関数は mark 構築子だが 'VisualSpec' と 'layer' に依存するため
-- 'Spec.Constructors' には置けず、 top-level setter 群と同居する。

-- | Wide-form histogram (P1、 Phase 6 A10): 複数列を **同一 plot に半透明で重ねる**。
--
--   `histogramWide [c1, c2, c3]` は 'VisualSpec' を返し、 内部で各列を独立 layer 化:
--
--     * layer i = `histogram cᵢ <> color (fromHex (palette i)) <> alpha 0.4 <> binCount 20`
--
--   palette は ColorBrewer Set1 (= categorical 9-class、 wong / 独自 切替は今後)。
--   bin 数は全列で **共通** (= seaborn の `multiple="layer"` 同等)、 デフォ 20。
--
--   matplotlib との対応: `plt.hist([c1, c2, c3], alpha=0.5, label=names)` 相当。
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

-- | Phase 36 D3: 別列・別 mark を 1 パネルに併置 (= mixed-mark)。 @<+>@ の list 版
--   (@distCols xs = layer (foldl1 (<+>) xs)@)。 各マークの値列 (encY) が別なので別 slot
--   (列名) に横並び・y は全列の値域和・単一パネル (subplot とは別)。 lane は 1D 分布 mark 専用
--   (box/violin/strip/swarm/raincloud)。 raincloud は全マーク同一列ゆえ 1 slot に重畳する
--   ('compositeLanes' が列数を決める)。
--
-- > distCols [ boxplot "a", violin "c", boxplot "d" ]
distCols :: [Layer] -> VisualSpec
distCols []       = mempty
distCols (l : ls) = layer (foldl (<+>) l ls)

-- | ★ Phase 36 B1c: ridge レイヤを含み coord 未指定の spec に coord_flip を自動付与する。
--   ridge は「値→x(連続)・群→y(カテゴリ)」だが combinator は box/violin と統一 (値=encY・
--   群=encX via groupBy)。 coord_flip で encY(値)→x・encX(群)→y に回す (box-flip と同機構)。
--   computeLayout / renderToPrimitives の入口で適用する。
ridgeAutoFlip :: VisualSpec -> VisualSpec
ridgeAutoFlip spec
  | any (\l -> getFirst (lyKind l) == Just MRidge) (vsLayers spec)
  , Nothing <- getLast (vsCoord spec)
  = spec { vsCoord = Last (Just CoordFlip) }
  | otherwise = spec


-- | P6: annotation 1 個を追加。
annotate :: Annotation -> VisualSpec
annotate a = mempty { vsAnnotations = [a] }

-- | P6: data 座標で text label を打つ shortcut (= 'annotTextP' の PNative ラッパ)。
annotText :: Double -> Double -> Text -> VisualSpec
annotText x y t = annotTextP (PNative x) (PNative y) t

-- | ★ Phase 33 B6: 'Pos' で text を打つ (native/npc/絶対長を軸ごと混在可)。
-- 例: @annotTextP (PNpc 0.95) (PNative 3.0) "R²"@ (右端 npc・data y)。
annotTextP :: Pos -> Pos -> Text -> VisualSpec
annotTextP x y t = annotate $ AnnText
  { anX = x, anY = y, anText = t, anColor = "", anSize = 12 }

-- | P6: data 座標で arrow を引く shortcut。
annotArrow :: Double -> Double -> Double -> Double -> VisualSpec
annotArrow x1 y1 x2 y2 =
  annotArrowP (PNative x1) (PNative y1) (PNative x2) (PNative y2)

-- | ★ Phase 33 B6: 'Pos' で arrow を引く。
annotArrowP :: Pos -> Pos -> Pos -> Pos -> VisualSpec
annotArrowP x1 y1 x2 y2 = annotate $ AnnArrow
  { anX1 = x1, anY1 = y1, anX2 = x2, anY2 = y2
  , anColor = "#444", anWidth = 1.5 }

-- | P6: data 座標で rect を描く shortcut (x,y,w,h → 2 隅 Pos へ変換)。
annotRect :: Double -> Double -> Double -> Double -> Text -> VisualSpec
annotRect x y w h col =
  annotRectP (PNative x) (PNative y) (PNative (x + w)) (PNative (y + h)) col

-- | ★ Phase 33 B6: 'Pos' 2 隅で rect を描く。
-- 例: @annotRectP (PNpc 0.0) (PNative 1.0) (PNpc 1.0) (PNative 2.0) "grey"@
-- (帯: x 全幅 npc・y は data 1..2)。
annotRectP :: Pos -> Pos -> Pos -> Pos -> Text -> VisualSpec
annotRectP x1 y1 x2 y2 col = annotate $ AnnRect
  { anX1 = x1, anY1 = y1, anX2 = x2, anY2 = y2
  , anFill = col, anStroke = "", anStrokeWidth = 0, anFillOpacity = 0.2 }

-- | P6: data 座標で line を引く shortcut。
annotLine :: Double -> Double -> Double -> Double -> VisualSpec
annotLine x1 y1 x2 y2 =
  annotLineP (PNative x1) (PNative y1) (PNative x2) (PNative y2)

-- | ★ Phase 33 B6: 'Pos' で line を引く。
annotLineP :: Pos -> Pos -> Pos -> Pos -> VisualSpec
annotLineP x1 y1 x2 y2 = annotate $ AnnLine
  { anX1 = x1, anY1 = y1, anX2 = x2, anY2 = y2
  , anColor = "#444", anWidth = 1 }

-- | P13: inset 1 個追加 (= デフォルト位置 右上 30%×30%)。
inset :: VisualSpec -> VisualSpec
inset s = insetAt 0.65 0.05 0.3 0.3 s

-- | P13: 位置 + サイズ (plotArea 比率 0..1) 指定で inset を追加。
--   inX/inY は **左上原点・y 下向き** (= 描画系と同じ)。
insetAt :: Double -> Double -> Double -> Double -> VisualSpec -> VisualSpec
insetAt x y w h s = mempty
  { vsInsets = [ Inset { inSpec = s, inX = x, inY = y, inW = w, inH = h } ] }

-- | Phase 8 C G8: patchwork 'inset_element' 準拠の inset 追加。
--   left/bottom/right/top は plotArea 比率 0..1 で **左下原点・y 上向き** (patchwork 慣例)。
--   内部で従来 'insetAt' (左上原点・y 下向き) へ変換するだけの薄いラッパ (非破壊)。
--   patchwork 感覚で `inset_element(p, left, bottom, right, top)` と同じ向きに置ける。
insetElement :: Double -> Double -> Double -> Double -> VisualSpec -> VisualSpec
insetElement left bottom right top s =
  insetAt left (1 - top) (right - left) (top - bottom) s

-- | P17: categorical palette を指定。 default = hggMain (F-3)。
palette :: [Text] -> VisualSpec
palette colors = mempty { vsPalette = Last (Just colors) }

-- | Phase 7 A6: ggplot2 hue パレット (= @scales::hue_pal()@) を選ぶ。 群数 n は描画時に
-- 決まるため sentinel を渡し、 Layout で n 展開する (= 'Hgg.Plot.Palette.ggplotHue')。
paletteGGplot :: VisualSpec
paletteGGplot = mempty { vsPalette = Last (Just ["__ggplot_hue__"]) }

-- | P17: continuous (sequential) palette を指定。 default = viridis5。
continuousPalette :: [Text] -> VisualSpec
continuousPalette colors = mempty { vsContinuousPal = Last (Just colors) }

-- | A4-e: ggplot @scale_color_manual(values=)@。 カテゴリ名→色(hex) の辞書を指定。
--   'color' (ColorByCol) のカテゴリ名がここにあればその色を最優先で使う。 未登録名は
--   従来の positional palette ('palette'/theme) にフォールバック。
scaleColorManual :: [(Text, Text)] -> VisualSpec
scaleColorManual dict = mempty { vsColorManual = Last (Just dict) }

-- | A4-e: ggplot @scale_color_gradient2(low,mid,high,midpoint=)@。 発散 (diverging)
--   continuous palette。 'colorContinuousBy' (ColorByContinuous) のとき、 midpoint を中心
--   (0.5) に固定し lo..mid を [0,0.5]・mid..hi を [0.5,1] へ個別正規化して 3-stop 補間。
scaleColorGradient2 :: Text -> Text -> Text -> Double -> VisualSpec
scaleColorGradient2 low mid high midpoint =
  mempty { vsColorGradient2 = Last (Just (low, mid, high, midpoint)) }

-- | A4-e: ggplot @scale_size(range=c(min,max))@。 'sizeBy' (continuous size aesthetic) の
--   半径 px 範囲を指定 (default (3,10))。 sizeBy 未使用なら無影響。
scaleSize :: Double -> Double -> VisualSpec
scaleSize lo hi = mempty { vsSizeRange = Last (Just (lo, hi)) }

-- | P8: 凡例を有効化 (= 既定: 右側)。
legend :: VisualSpec
legend = mempty { vsLegend = Last (Just defaultLegendSpec) }

-- | P8: 凡例を抑制。
legendOff :: VisualSpec
legendOff = mempty
  { vsLegend = Last (Just (LegendSpec LegendNone mempty)) }

-- | P8: 凡例位置を指定。
legendPos :: LegendPosition -> VisualSpec
legendPos pos = mempty { vsLegend = Last (Just (LegendSpec pos mempty)) }

-- | Phase 11 A5-c: 色凡例を非表示 (= ggplot @guides(color="none")@)。 この系では凡例は
--   色 (color/fill) のみなので 'legendOff' と同義。 ggplot 慣習名の別名として提供。
guideColorNone :: VisualSpec
guideColorNone = legendOff

-- | Phase 11 A5-c: 凡例キーの表示順を逆に (= ggplot @guide_legend(reverse=TRUE)@)。
--   各キーの色は固定のまま順序のみ反転。 位置設定 ('legend'/'legendPos') と独立合成可。
legendReverse :: VisualSpec
legendReverse = mempty { vsLegendReverse = Last (Just True) }

-- | Phase 11 A5-c: 縦凡例 (Right/Inside) の列数 (= ggplot @guide_legend(ncol=)@)。
legendNcol :: Int -> VisualSpec
legendNcol n = mempty { vsLegendNcol = Last (Just n) }

-- | Phase 11 A5-c: 横凡例 (Bottom) の行数 (= ggplot @guide_legend(nrow=)@)。
legendNrow :: Int -> VisualSpec
legendNrow n = mempty { vsLegendNrow = Last (Just n) }

-- | 図サイズ ('Length'・Phase 34 A4)。 bare 数値リテラルは @Num Length@ 経由で
--   **pt** (@width 600@ = 600pt)。 mm で書きたいときは 'widthMm' / 'heightMm'、
--   その他の単位は @width (7 *~ inch)@ / 'widthUnit' を使う。
width, height :: Length -> VisualSpec
width  = widthUnit
height = heightUnit

-- | 図サイズ (mm 直接)。@widthMm 180@ = 180mm。 A4 で 'width' の bare が pt に
--   変わったので、 従来の mm 指定はこちらへ移行する。
widthMm, heightMm :: Double -> VisualSpec
widthMm  w = widthUnit  (w *~ mm)
heightMm h = heightUnit (h *~ mm)

-- | 図サイズ (単位明示)。@widthUnit (7 *~ inch)@ / @widthUnit (800 *~ px)@。
widthUnit, heightUnit :: Length -> VisualSpec
widthUnit  l = mempty { vsWidth  = Last (Just l) }
heightUnit l = mempty { vsHeight = Last (Just l) }

-- | 描画 dpi (px backend は px=pt×dpi/72)。@plot <> dpi 300@。既定 96。PDF は無視。
dpi :: Double -> VisualSpec
dpi d = mempty { vsDpi = Last (Just d) }

-- | Phase 8 A2 Step2: coord_fixed(ratio) 相当。 panel の 高/幅 比 (aspect) を固定。
-- 指定時は可用域内で aspect を保つ最大 panel を取り中央寄せ (ggplot Coord$aspect)。
aspectRatio :: Double -> VisualSpec
aspectRatio a = mempty { vsAspect = Last (Just a) }

-- | Phase 9 C: coord_flip。 x/y 軸を入れ替える (= 横棒グラフ等)。 ggplot coord_flip() 相当。
--
--   > bar "cat" "y" `layer'` purePlot <> coordFlip
coordFlip :: VisualSpec
coordFlip = mempty { vsCoord = Last (Just CoordFlip) }

-- | Phase 11 A7-c: 極座標 (= ggplot @coord_polar(theta="x")@)。 データ x を角度
--   (0..2π、 上始点・時計回り)、 データ y を半径に写す。 line/point は radar / spiral に。
coordPolar :: VisualSpec
coordPolar = mempty { vsCoord = Last (Just CoordPolarX) }

-- | Phase 11 A7-c: 極座標 (= ggplot @coord_polar(theta="y")@)。 データ y を角度、
--   データ x を半径に写す。 単一カテゴリの stacked bar と併せると円グラフになる。
coordPolarY :: VisualSpec
coordPolarY = mempty { vsCoord = Last (Just CoordPolarY) }

-- | Phase 11 A4-a: X 軸反転 (= ggplot @scale_x_reverse()@)。 大値が左、 小値が右へ。
--   coord_flip と独立合成可。
--
--   > scatter "x" "y" `layer'` purePlot <> reverseX
reverseX :: VisualSpec
reverseX = mempty { vsReverseX = Last (Just True) }

-- | Phase 11 A4-a: Y 軸反転 (= ggplot @scale_y_reverse()@)。 大値が下、 小値が上へ。
reverseY :: VisualSpec
reverseY = mempty { vsReverseY = Last (Just True) }

-- | Phase 11 A7-a: X 軸 zoom (= ggplot @coord_cartesian(xlim=c(lo,hi))@)。
--   'axisRange' (= scale limits、 範囲外データを切る) と異なり **データを落とさず**
--   表示範囲だけを [lo,hi] に上書きする。 stat (regression/density 等) は全データから
--   計算され、 範囲外の glyph は panel に clip される。 numeric 軸のみ有効。
coordCartesianX :: Double -> Double -> VisualSpec
coordCartesianX lo hi = mempty { vsCoordXLim = Last (Just (lo, hi)) }

-- | Phase 11 A7-a: Y 軸 zoom (= ggplot @coord_cartesian(ylim=c(lo,hi))@)。
coordCartesianY :: Double -> Double -> VisualSpec
coordCartesianY lo hi = mempty { vsCoordYLim = Last (Just (lo, hi)) }

-- | Phase 11 A7-a: X/Y 同時 zoom (= ggplot @coord_cartesian(xlim=,ylim=)@)。
--   'coordCartesianX' と 'coordCartesianY' の合成。
coordCartesian :: Double -> Double -> Double -> Double -> VisualSpec
coordCartesian xlo xhi ylo yhi = coordCartesianX xlo xhi <> coordCartesianY ylo yhi

-- | 軸 (X / Y) 設定の合成 helper。
--
-- > example = ... <> xAxis logAxis <> yAxis (linearAxis <> ...)
xAxis, yAxis :: AxisSpec -> VisualSpec
xAxis a = mempty { vsXAxis = Last (Just a) }
yAxis a = mempty { vsYAxis = Last (Just a) }

-- | P5: 右側 Y 軸の AxisSpec (= dual Y を有効化)。
yAxisRight :: AxisSpec -> VisualSpec
yAxisRight a = mempty { vsYAxisRight = Last (Just a) }

-- | P5: layer を右側 Y 軸に紐付ける。
toRightY :: Layer
toRightY = mempty { lyYAxisSide = Last (Just YAxisRight) }

-- | P5: layer を左側 Y 軸に紐付ける (= default なので通常不要)。
toLeftY :: Layer
toLeftY = mempty { lyYAxisSide = Last (Just YAxisLeft) }

-- | 参照線を 1 本追加 (= 重ねがけで複数本)。
--
-- > example = ... <> refLine RefIdentity <> refLine (RefHorizontalAt 0)
refLine :: ReferenceLine -> VisualSpec
refLine rl = mempty { vsRefLines = [rl] }

-- | shortcut。
refIdentity   :: VisualSpec
refIdentity   = refLine RefIdentity
refHorizontal :: Double -> VisualSpec
refHorizontal y = refLine (RefHorizontalAt y)
refVertical   :: Double -> VisualSpec
refVertical x   = refLine (RefVerticalAt x)

-- | Phase 26 §C-2 #10: scatter の周辺に X/Y 両方の histogram。
marginal :: VisualSpec
marginal = mempty { vsMarginal = Last (Just (defaultMarginalSpec { msShowX = True, msShowY = True })) }

-- | 周辺 histogram X 軸のみ。
marginalX :: VisualSpec
marginalX = mempty { vsMarginal = Last (Just (defaultMarginalSpec { msShowX = True })) }

-- | 周辺 histogram Y 軸のみ。
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

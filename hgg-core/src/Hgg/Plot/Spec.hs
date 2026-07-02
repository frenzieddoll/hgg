-- |
-- Module      : Hgg.Plot.Spec
-- Description : Layer 3 ─ VisualSpec / Layer / ColRef + Monoid (Phase 26 §A-2)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- 設計方針 (詳細: design/api-style-discussion-2.md + 続き):
--
--   * 2 階層 Monoid: 'Layer' (= 1 layer 内属性) と 'VisualSpec' (= 図全体)
--   * 全 helper が `<>` で paren 無し合成可能 (= plotnine 風)
--   * 'ColRef' で「文字列 col 参照」 と「Vector inline」 両対応、
--     ('OverloadedStrings' で `"weight" :: ColRef` が自動 'ColByName')
--   * core は DataFrame 型に非依存。 col 名 → Vector 解決は 'Resolver'
--     callback で render 時に行う (= core 内ではデータ source を持たない)
--   * Generic + ToJSON/FromJSON で Spec 全体が **JSON serializable**
--     (= hgg-canvas frontend ↔ backend 共有、 Phase 18 §4 Patch ADT
--     と直結)
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Hgg.Plot.Spec
  ( -- * ColRef + Resolver
    ColRef(..)
  , ColData(..)
  , Resolver
  , emptyResolver
  , resolveCol
  , bakeSpec
  , resolveNum
  , resolveTxt
  , colRefName
    -- * Inline column conversion
  , Numeric(..)
  , Categorical(..)
  , inline
  , inlineCat
    -- * Layer
  , Layer(..)
  , MarkKind(..)
  , ColorEnc(..)
    -- ** 固定色 'Color' 型 (re-export from "Hgg.Plot.Color")
  , Color(..)
  , rgb
  , fromHex
  , fromHexMaybe
  , fromHexA
  , fromHexAMaybe
  , ConnectSpec(..)
  , defaultConnectSpec
    -- * 2D 点 'Point2' (= 3D 'Hgg.Plot.ThreeD.Types.Point3' と対称)
  , Point2(..)
    -- * Layer constructors (= Layer 返却)
  , scatter
  , line
  , scatterPoints       -- ★ Phase 30 A7 inline [Point2] (= 3D scatter3DPoints と対称)
  , linePoints          -- ★ Phase 30 A7 inline [Point2] (= 3D line3DPoints と対称)
  , bar
  , quiver
  , arrowScale
  , arrowColorByMagnitude
  , text
  , label
  , qq
  , ecdf
  , lineRange
  , pointRange
  , crossbar
  , statFunction
  , histogram
  , freqpoly             -- ★ Ch10 EDA (Phase 28) geom_freqpoly (= 頻度多角形)
  , countXY              -- ★ Ch10 EDA (Phase 28) geom_count (= 2 カテゴリ件数)
  , histogramWide        -- ★ P1 (Phase 6 A10) wide-form 重ね
  , autocorr             -- ★ P19 (Phase 6 A4) MCMC autocorrelation
  , autocorrMaxLag       -- ★ Phase 6 A4 max lag override
  , ess                  -- ★ P20 (Phase 6 A5) Effective Sample Size
  , chain                -- ★ Phase 6 A5 chain group 設定
  , forest               -- ★ Phase 6 A2 Forest plot (= horizontal CI bar)
  , forestNull           -- ★ Phase 6 A2 null effect 位置 (= 縦 0 線、 default 0)
  , funnel               -- ★ Phase 6 A3 Funnel plot (= effect vs SE)
  , boxplot
  , groupBy             -- ★ Phase 36: 群配置チャネル (色なし・ggplot aes(group=))
  , density
  , densityNorm
  , pie
  , waterfall
  , heatmap
  , contour
  , contourFilled
  , contourLevels
  , contourBreaks
  , bin2d
  , bin2dCount           -- ★ Ch10 EDA (Phase 28) geom_bin2d (count 版)
  , hexbin               -- ★ Phase 40: 六角ビニング (geom_hex / matplotlib hexbin)
  , hexbinBins           -- ★ Phase 40: hexbin の x 方向セル分割数 (既定 30)
  , HexCell(..)          -- ★ Phase 40: 六角セル (中心+件数+頂点)
  , hexbinCells          -- ★ Phase 40: 六角ビニング純関数 (d3-hexbin)
  , hexbinLayerCells     -- ★ Phase 40: Layer 解決版 (render/colorbar 共有)
  , subplots
  , subplotCols
  , selectPanels
  , selectedSubplots
  , scaleXDiscreteLimits
  , scaleYDiscreteLimits
  , applyDiscreteLimits
  , hconcat
  , vconcat
  , (<->)
  , (<:>)
  , repeatFields
  , pairs
  , step
  , stem
  , statLm
  , statLmLevel
  , statSmooth
  , statSmoothCI
  , statPoly
  , statResid
  , band
  , stream
  , violin
  , strip
  , swarm
  , raincloud
  , (<+>)
  , distCols
  , compositeLanes
  , ridge
  , ridgeAutoFlip
  , jitterX
  , jitterY
  , sqrtAxis
  , timeAxis
  , AxisBreak(..)
  , axisBreak
  , axisBreaksAt
  , axisTickLabels
  , axisBreaksLabeled
  , hideTicks
  , Annotation(..)
  , annotate
  , annotText
  , annotTextP
  , annotArrow
  , annotArrowP
  , annotRect
  , annotRectP
  , annotLine
  , annotLineP
  , Inset(..)
  , inset
  , insetAt
  , insetElement
  , palette
  , paletteGGplot
  , continuousPalette
  , scaleColorManual
  , scaleColorGradient2
  , scaleSize
  , YAxisSide(..)
  , yAxisRight
  , toRightY
  , toLeftY
  , statMean
  , statMedian
  , parallelCoords
  , dag
  , dagFromLists
  , dagFromListsWithPlates
  , dagNode
  , dagNodeDist
  , dagEdge
  , trace
  , traceLines
    -- * Layer-local attribute (= Layer 返却)
  , color
  , colorRGBA
  , colorRGBAMaybe
  , colorBy
  , distGroupRef
  , distDodgeRef
  , colorContinuousBy
  , alpha
  , size
  , stroke
  , edgeOn
  , edge
  , edgeWidth
  , hoverCols
  , errorX
  , errorY
  , connect
  , connectOrder
  , connectGroup
  , connectColor
  , connectWidth
    -- * Axis (= Phase 26 §C-2 #1 / #2)
  , AxisSpec(..)
  , AxisKind(..)
  , AxisFormat(..)
  , axisKindOf
  , axisFormatOf
  , axTickValsOf
  , axTickLabelsOf
  , axisRotateOf
  , resolveAxisAngle
  , axisShowTicksOf
  , axisRotate
  , linearAxis
  , logAxis
  , axisFormat
  , axisMin
  , axisMax
  , axisRange
  , binCount
  , binWidth
  , histBinning
  , histogramDensity
  , histBorder
  , densityFill
  , hollow
    -- * 分布 mark の位置決め (= Phase 36 D1)
  , Side(..)
  , nudge
  , markWidth
  , side
    -- * Bar position adjustment (= Phase 9 B)
  , Position(..)
  , position
  , Coord(..)
  , coordFlip
  , coordPolar
  , coordPolarY
  , reverseX
  , reverseY
  , coordCartesianX
  , coordCartesianY
  , coordCartesian
    -- * Reference line (= Phase 26 §C-2 #3)
  , ReferenceLine(..)
    -- * Marginal histogram (= Phase 26 §C-2 #10)
  , MarginalSpec(..)
  , MarginalKind(..)
  , defaultMarginalSpec
  , LegendSpec(..)
  , LegendPosition(..)
  , defaultLegendSpec
  , legend
  , legendOff
  , legendPos
  , guideColorNone
  , legendReverse
  , legendNcol
  , legendNrow
    -- * DAG (= Phase 26 §E-6, HBM ModelGraph)
  , DAGSpec(..)
  , DAGNode(..)
  , DAGEdge(..)
  , RoutedEdge(..)
  , EdgeShapeKind(..)
  , DAGPlate(..)
  , DAGNodeKind(..)
  , DAGLayoutAlgorithm(..)
    -- * Top-level
  , VisualSpec(..)
  , ThemeName(..)
  , themeSeriesPalette
  , okabeIto, tolBright, brewerSet2, brewerDark2
  , ThemeOverride(..)
  , themeGrid, panelFill, panelBorder, themeAxisLine, gridColor, plotBg, axisColor, textColor
  , themeTitleFont, themeAxisLabelFont, themeTickFont, themeLegendFont, themeAxisTextAngle
  , stripFill, themeStrip
  , titleHjust, titleColor, tickColor, legendKeyBg   -- ★ Phase 43 A4
    -- * Top-level setters (= VisualSpec 返却)
  , purePlot
  , layer
  , title
  , theme
  , facet
  , facetWrap
  , facetCols
  , facetGrid
  , FacetScales(..)
  , freeScaleX
  , freeScaleY
  , facetScales
  , FacetSpace(..)
  , freeSpaceX
  , freeSpaceY
  , facetSpace
  , xLabel
  , yLabel
  , legendTitle
  , subtitle
  , caption
  , tag
  , Labs(..)
  , emptyLabs
  , labs
  , width
  , height
  , widthMm
  , heightMm
  , widthUnit
  , heightUnit
  , dpi
  , aspectRatio
  , xAxis
  , yAxis
  , refLine
  , refIdentity
  , refHorizontal
  , refVertical
  , marginal
  , marginalX
  , marginalY
    -- * C-6 Shape encoding
  , MarkShape(..)
  , ShapeMapEntry(..)
  , shape
  , shapeBy
  , shapeMapEntry
  , sizeBy
  , alphaBy              -- ★ Phase 30 A8 連続 alpha encoding (= ggplot scale_alpha)
  , colorCats
  , orderedCats
    -- * Phase 11 A4-b linetype encoding
  , LineType(..)
  , linetype
  , linetypeBy
  , lineTypeDash
  , lineTypeForIndex
    -- * Font customization (= hgg-frontend-settings-spec v0.1 §1.3)
  , FontSpec(..)
  , emptyFontSpec
  , fontSize
  , fontFamily
  , fontWeight
  , fontItalic
  , fontColor
  , titleFont
  , axisLabelFont
  , tickFont
  , legendFont
  ) where

import           Hgg.Plot.Color (Color (..), rgb, fromHex, fromHexMaybe,
                                     fromHexA, fromHexAMaybe, toCss)
import           Data.Aeson      (FromJSON, ToJSON, toJSON, parseJSON, toEncoding,
                                  Value (Object))
import qualified Data.Aeson      as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Char       as Char
import qualified Data.List
import           Data.Maybe      (catMaybes)
import           Data.Monoid     (First (..), Last (..))
import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           GHC.Generics    (Generic, Generically (..))

import           Hgg.Plot.Unit (Length, Pos (..), lengthToPt, mm, (*~))

-- ===========================================================================
-- ColRef + Resolver
-- ===========================================================================

-- | データ列の参照方法。 3 つの variant:
--
--   * 'ColByName' ─ 文字列 col 名。 'Resolver' で実 Vector に解決される。
--   * 'ColNum'    ─ 数値 Vector を inline (= 即値、 resolver 不要)
--   * 'ColTxt'    ─ 文字列 Vector を inline (= categorical encoding 用)
--
-- 'OverloadedStrings' で `"weight" :: ColRef` が `ColByName "weight"` に。
data ColRef
  = ColByName !Text
  | ColNum    !(Vector Double)
  | ColTxt    !(Vector Text)
  deriving (Generic, Show, Eq)

instance ToJSON   ColRef
instance FromJSON ColRef

instance IsString ColRef where
  fromString = ColByName . T.pack

-- | Resolver が返すデータ形 (= 数値 or 文字列)。
data ColData
  = NumData !(Vector Double)
  | TxtData !(Vector Text)
  deriving (Show, Eq)

-- | render 時に col 名を Vector に解決する callback。
-- 数値列 / 文字列列 どちらも返せるよう 'ColData' で union。
type Resolver = Text -> Maybe ColData

emptyResolver :: Resolver
emptyResolver _ = Nothing

-- | 'ColRef' を 'ColData' に解決。 inline は variant に応じて直接返す。
resolveCol :: Resolver -> ColRef -> Maybe ColData
resolveCol r (ColByName n) = r n
resolveCol _ (ColNum v)    = Just (NumData v)
resolveCol _ (ColTxt v)    = Just (TxtData v)

-- | 数値解決 (= 数値列 or 数値 inline のみ成功、 文字列は 'Nothing')。
resolveNum :: Resolver -> ColRef -> Maybe (Vector Double)
resolveNum r cr = case resolveCol r cr of
  Just (NumData v) -> Just v
  _                -> Nothing

-- | 文字列解決 (= 文字列 inline or 文字列列のみ成功)。
resolveTxt :: Resolver -> ColRef -> Maybe (Vector Text)
resolveTxt r cr = case resolveCol r cr of
  Just (TxtData v) -> Just v
  _                -> Nothing

-- ===========================================================================
-- Resolver の焼き込み (Phase 8 B16): ColByName を inline (ColNum/ColTxt) に解決
-- ===========================================================================
-- PS canvas backend は Resolver を持たず spec JSON だけ受け取るため、 ColByName
-- (列名参照) のままだと PS で解決できず描画されない (= pairs/facet/legend が空)。
-- JSON 出力前に bakeSpec で全 ColRef を inline 化すると PS でも描ける。

-- | ColByName を Resolver で解決し ColNum/ColTxt に置換 (解決不能なら元のまま)。
bakeColRef :: Resolver -> ColRef -> ColRef
bakeColRef r cr@(ColByName n) = case r n of
  Just (NumData v) -> ColNum v
  Just (TxtData v) -> ColTxt v
  Nothing          -> cr
bakeColRef _ cr = cr

bakeColorEnc :: Resolver -> ColorEnc -> ColorEnc
bakeColorEnc r (ColorByCol cr)        = ColorByCol (bakeColRef r cr)
-- Phase 9 A-5 fix: ColorByContinuous も inline 化しないと PS (emptyResolver) で色も
-- legend も出ない (= legend-continuous で発覚)。
bakeColorEnc r (ColorByContinuous cr) = ColorByContinuous (bakeColRef r cr)
bakeColorEnc _ ce                     = ce

bakeLayer :: Resolver -> Layer -> Layer
bakeLayer r l = l
  { lyEncX    = bakeColRef r <$> lyEncX l
  , lyEncY    = bakeColRef r <$> lyEncY l
  , lyEncY2   = bakeColRef r <$> lyEncY2 l
  , lyErrorX  = bakeColRef r <$> lyErrorX l
  , lyErrorY  = bakeColRef r <$> lyErrorY l
  , lyChain   = bakeColRef r <$> lyChain l
  , lyShapeBy = bakeColRef r <$> lyShapeBy l
  , lySizeBy  = bakeColRef r <$> lySizeBy l
  , lyAlphaBy = bakeColRef r <$> lyAlphaBy l
  , lyLinetypeBy = bakeColRef r <$> lyLinetypeBy l
  , lyLabel   = bakeColRef r <$> lyLabel l
  , lyColor   = bakeColorEnc r <$> lyColor l
  , lyOverlay = map (bakeLayer r) (lyOverlay l)   -- ★ Phase 36 D2: sub-mark の inline 列も bake
  }

-- | spec 内の全 ColByName を Resolver で inline 化 (layers + facet + subplots 再帰)。
-- JSON 出力前に呼ぶと PS でも Resolver 不要で描ける。
bakeSpec :: Resolver -> VisualSpec -> VisualSpec
bakeSpec r spec = spec
  { vsLayers   = map (bakeLayer r) (vsLayers spec)
  , vsFacet    = bakeColRef r <$> vsFacet spec
  , vsFacetRow = bakeColRef r <$> vsFacetRow spec
  , vsFacetCol = bakeColRef r <$> vsFacetCol spec
  , vsSubplots = map (bakeSpec r) (vsSubplots spec)
  }

-- | ColRef の表示名 (= hover tooltip / legend 等)。
colRefName :: ColRef -> Text
colRefName (ColByName n) = n
colRefName (ColNum _)    = "<inline-num>"
colRefName (ColTxt _)    = "<inline-txt>"

-- ===========================================================================
-- Inline column conversion
-- ===========================================================================

-- | 数値系 (Vector n / [n], n は Real instance を持つ任意型) を 'ColRef' に。
class Numeric a where
  toNumVec :: a -> Vector Double

instance Real n => Numeric (Vector n) where
  toNumVec = V.map realToFrac

instance Real n => Numeric [n] where
  toNumVec = V.fromList . map realToFrac

-- | 文字列系 (= categorical encoding 用)。
class Categorical a where
  toTxtVec :: a -> Vector Text

instance Categorical (Vector Text) where toTxtVec = id
instance Categorical [Text]        where toTxtVec = V.fromList
instance Categorical [String]      where toTxtVec = V.fromList . map T.pack

-- | 数値 (Vector / List) を inline 'ColRef' に。 'Int' / 'Double' / 'Float' /
-- 'Integer' / 'Word' 等 'Real' instance を持つ任意型に対応。
--
-- > scatter (inline xs) (inline ys)
-- > scatter (inline [1, 2, 3]) (inline [4.0, 5.0, 6.0])
inline :: Numeric a => a -> ColRef
inline = ColNum . toNumVec

-- | 文字列系を inline 'ColRef' に (= categorical encoding 用)。
--
-- > colorBy (inlineCat ["red", "blue", "green"])
inlineCat :: Categorical a => a -> ColRef
inlineCat = ColTxt . toTxtVec

-- ===========================================================================
-- Point2 (= 2D 点・3D 'Point3' と対称)
-- ===========================================================================

-- | 2D 点 (= world space)。 'Hgg.Plot.ThreeD.Types.Point3' と対称の直積型。
--   inline の点単位 API ('scatterPoints' / 'linePoints') で使う。
--
-- JSON: positional fields → array @[x, y]@ (= aeson Generic デフォルト挙動・
-- 'Point3' と同形式)。 ※ 'Hgg.Plot.Render' の @Point@ は screen 空間で別物。
data Point2 = Point2 !Double !Double
  deriving (Show, Eq, Generic)
instance ToJSON   Point2
instance FromJSON Point2

-- ===========================================================================
-- Layer (= 内側 Monoid)
-- ===========================================================================

-- | 1 layer の幾何種別。 Phase 26 §A-2 で 12 種列挙、 実 render は §A-5 で
-- 段階追加 (= Scatter / Line / Bar / Histogram を先行)。
data MarkKind
  = MScatter | MLine | MBar | MHistogram | MBox | MHeatmap
    -- 統計特化 (Phase 26 §E)
  | MTrace | MDensity | MForest | MFunnel
    -- Phase 28 (Ch10 EDA): 頻度多角形 (= ggplot geom_freqpoly)。 histogram と同じ
    -- bin 化 (histBinning) で各 bin の count を求め、 bin 中心を折れ線で結ぶ。
    -- KDE の MDensity とは別物 (= ビン頻度の折れ線、 滑らかでない)。 lyHistDensity
    -- True で after_stat(density) = count/(群N*binW) 正規化 (面積 1)。 color
    -- aesthetic で群分割すると群ごとに別色の折れ線を重ねる (MDensity 同方式)。
  | MFreqPoly
    -- 半導体特化 (Phase 26 §F)
  | MWaferMap | MControl
    -- 統計線 (Phase 26 §C-2 #8)
  | MStatMean | MStatMedian
    -- Parallel coordinates (Phase 26 §C-2 #13)
  | MParallel
    -- DAG (Phase 26 §E-6 HBM ModelGraph)
  | MDAG
    -- Pie chart (Phase 26 S4-d)
  | MPie
    -- Waterfall (Phase 26 S5-c)
  | MWaterfall
    -- Contour (連続 x/y/z → marching squares の等高線 iso-line)
  | MContour
    -- Filled contour (等値帯の塗り = matplotlib contourf / ggplot geom_contour_filled。 Phase 24 A4)
  | MContourFilled
    -- Bin2d (連続 x/y/z → grid binning + セル平均を連続色で塗る = ggplot geom_bin2d)
  | MBin2d
    -- MCMC 診断 (P19, P20)
  | MAutocorr | MEss
    -- P11 / P12: stem / step
  | MStep | MStem
    -- P2 / P3 / P22: distribution 系
  | MViolin | MStrip | MSwarm | MRaincloud
    -- P21: ridge / joyplot
  | MRidge
    -- TODO-11 (2026-05-27): area band (= 信頼区間 / 予測帯、 PPath fill 1 枚)
  | MBand
    -- 3D placeholder (Phase 26 §C-2 #15、 実装は別 Phase で hgg-3d)
  | MScatter3D
    -- Phase 11 A6: データ駆動テキストラベル (geom_text / geom_label)。 各 (x,y) 点に
    -- lyLabel 列の文字を描く。 MLabel は背景の角丸矩形付き (= ggplot geom_label)。
  | MText | MLabel
    -- Phase 11 A6-2: Q-Q plot (= ggplot stat_qq / geom_qq)。 encY = サンプル列。
    -- ソートした order statistic を y、 理論正規分位点 Φ⁻¹((i-0.5)/n) を x に取り
    -- scatter 系で描画する (= 正規性の視覚診断)。
  | MQQ
    -- Phase 11 A6-4: ECDF (= ggplot stat_ecdf)。 encX = サンプル列。 ソートして
    -- 右連続の階段 F(x)=#(≤x)/n を描く (y∈[0,1])。
  | MEcdf
    -- Phase 11 A6-4b: 区間 geom (= ggplot geom_linerange / geom_pointrange / geom_crossbar)。
    -- encX=x, encY=y(中心), errorY=半幅 (y±err)。 linerange=縦線のみ、 pointrange=縦線+中心点、
    -- crossbar=幅付き箱 (y±err) + 中央水平線。
  | MLineRange | MPointRange | MCrossbar
    -- Phase 16: stat-in (= ggplot stat_smooth(method="lm"/"…"))。 純タグ (回帰 fit は
    -- analyze-bridge の resolveStats が hanalyze で行い band+line layer に展開する)。
    -- encX=x, encY=y。 lyColor/lyStroke/lyAlpha 等の装飾はそのまま band/line に引き継がれる。
    -- MStatSmooth の knot 数は lyBinCount を流用。 renderer は MStat* を no-op (skip)。
  | MStatLM | MStatSmooth
    -- Phase 16 B3: 多項式回帰 (= ggplot stat_smooth(method="lm", formula=y~poly(x,deg)))。
    -- deg は lyBinCount を流用。 resolveStats が y~poly(x,deg) で fit し band+line に展開。
    -- MStatResid = 残差 vs fitted の診断散布図 (= base R plot(lm) #1)。 fit して
    -- (fitted, residual) を scatter に展開する。 いずれも renderer は MStat* を skip。
  | MStatPoly | MStatResid
    -- Phase 52.D2: Streamgraph (= 中心化積層 area、 ThemeRiver 風)。 encX=x, encY=y、
    -- color aes で系列分割。 各 x 点で系列 y を積層し baseline を -(Σy)/2 から開始する
    -- (silhouette 中心化)。 各系列を renderBand 同型の塗り polygon で描く。
  | MStream
    -- Phase 26 A2: vector field (quiver)。 encX=x, encY=y, lyEncU=u, lyEncV=v。
    -- 各 (x,y) に成分 (u,v) の矢印を描く (autoscale × lyArrowScale)。 magnitude
    -- 連続色は lyArrowMagnitude。 = matplotlib quiver / geom_segment(arrow=)。
  | MQuiver
    -- Phase 28 (Ch10 EDA): 2 カテゴリ変数の件数 (= ggplot geom_count / stat_sum)。
    -- encX/encY はともにカテゴリ列。 各 (x,y) セルの観測件数を集計し、 cell 中心に
    -- 面積 ∝ 件数 (= 半径 ∝ √件数) の点を打つ。 lySize で最大半径 px を上書き可。
  | MCount
    -- Phase 40: hexbin (六角ビニング = matplotlib hexbin / ggplot geom_hex)。 encX/encY は
    -- 連続列。 binwidth 正規化空間で d3-hexbin (Carr 1987) アルゴで点を六角セルに割当て count し、
    -- pointy-top 六角形を count→連続色 (Viridis) で塗る。 セル分割数は lyBinCount を流用 (既定 30)。
  | MHexbin
  deriving (Show, Eq, Generic)

instance ToJSON   MarkKind
instance FromJSON MarkKind

-- | Phase 26 §E-6: HBM ModelGraph DAG。
-- node 種別 (= 汎用、 HBM 慣例の latent/observed/deterministic/data を含む)。
-- 描画形状 (PyMC 慣例): NodeLatent = 白楕円、 NodeObserved = 灰楕円、
-- NodeDeterministic = 白四角 (Phase 52.A15)、 NodeData = 灰角丸四角、 NodeOther = 四角。
data DAGNodeKind = NodeLatent | NodeObserved | NodeDeterministic | NodeData | NodeOther
  deriving (Show, Eq, Ord, Generic)

instance ToJSON   DAGNodeKind
instance FromJSON DAGNodeKind

-- | DAG layout algorithm。
--   * 'LayoutManual'       ─ dnX / dnY をそのまま使う
--   * 'LayoutHierarchical' ─ topological sort + 同層 x 均等配置
--   * 'LayoutForce'        ─ 将来 (= force-directed、 §C-2 後続)
data DAGLayoutAlgorithm = LayoutManual | LayoutHierarchical
  deriving (Show, Eq, Generic)

instance ToJSON   DAGLayoutAlgorithm
instance FromJSON DAGLayoutAlgorithm

data DAGNode = DAGNode
  { dnId    :: !Text
  , dnLabel :: !Text
  , dnKind  :: !DAGNodeKind
  , dnDist  :: !(Maybe Text)  -- ★ 分布名 (= "Normal" / "HalfCauchy" 等、 PyMC 風 sub-label)
  , dnX     :: !Double        -- LayoutManual のみ参照、 他は layout で上書き
  , dnY     :: !Double
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   DAGNode
instance FromJSON DAGNode

-- | DAG edge。 Phase 1 A5 で 'dePath' (= dummy 経由の control 点列) を追加、
-- layout 計算後に埋まる。 JSON FromJSON はフィールド欠落時 'Nothing' default
-- (= aeson Generic 既定動作)、 旧 JSON との backward compat 維持。
-- | Phase 42 sub B: edge routing の形状種別 (= Render.EdgeRoute の constructor を
-- spec に焼き込むための非依存 tag)。 StraightArrow/SplinePath/BezierPath/CubicPath に対応。
data EdgeShapeKind = EShStraight | EShSpline | EShBezier | EShCubic
  deriving (Show, Eq, Generic)

instance ToJSON   EdgeShapeKind
instance FromJSON EdgeShapeKind

-- | Phase 42 sub B: HS が焼き込んだ routing 結果 (= pt 空間 = post-'toScreen'・pre-fit)。
-- HS 'routeEdge' が owner。 PS は描画 + 'fitPrimsToArea' のみ (option1 / DRY)。
-- 'rePts' の意味は 'reKind' 依存: Straight=[port0,port1]、 Spline/Bezier=制御点列、
-- Cubic=先頭が始点で以後 3 点ずつ (ctrl1,ctrl2,end) の cubic segment 列。
data RoutedEdge = RoutedEdge
  { reKind :: !EdgeShapeKind
  , rePts  :: ![(Double, Double)]
  } deriving (Show, Eq, Generic)

instance ToJSON   RoutedEdge
instance FromJSON RoutedEdge

data DAGEdge = DAGEdge
  { deFrom :: !Text
  , deTo   :: !Text
  , dePath :: !(Maybe [(Double, Double)])
    -- ^ Phase 1 A5: 中継 dummy 経由の制御点列 (= 始点と終点を含む 0..1 domain)。
    -- 'Nothing' なら短 edge (= 直線描画)、 'Just [..]' なら spline 描画。
  , deRoute :: !(Maybe RoutedEdge)
    -- ^ Phase 42 sub B: HS が layout 時に焼き込む pt 空間 routing (= PS と byte parity 用)。
    -- 'Nothing' なら未 bake (= HS は live routeEdge、 PS は straight fallback)。
    -- aeson Generic は欠落時 Nothing default で旧 JSON と backward compat。
  } deriving (Show, Eq, Generic)

instance ToJSON   DAGEdge
instance FromJSON DAGEdge

-- | Plate (= PyMC スタイルの "repeated" group 囲み)。
-- 含まれる node id 列を指定、 layout 時に bounding box を自動計算。
data DAGPlate = DAGPlate
  { dpLabel   :: !Text       -- e.g. "course (10)" / "record (2396)"
  , dpNodeIds :: ![Text]
  } deriving (Show, Eq, Generic)

instance ToJSON   DAGPlate
instance FromJSON DAGPlate

data DAGSpec = DAGSpec
  { dsNodes  :: ![DAGNode]
  , dsEdges  :: ![DAGEdge]
  , dsLayout :: !DAGLayoutAlgorithm
  , dsPlates :: ![DAGPlate]   -- ★ Plate 群 (= PyMC スタイル grouping)
  } deriving (Show, Eq, Generic)

instance ToJSON   DAGSpec
instance FromJSON DAGSpec

-- | Phase 26 §C-2 #5: scatter 点を線で結ぶ設定。
-- PlotConfig.connectPoints / connectOrderColumn / connectGroupColumn /
-- connectColor / connectWidth / connectBeforePoints 等価。
data ConnectSpec = ConnectSpec
  { csOrder  :: !(Last ColRef)   -- Nothing = データ順
  , csGroup  :: !(Last ColRef)   -- Nothing = 全点 1 本
  , csColor  :: !(Last Text)     -- Nothing = layer 色
  , csWidth  :: !(Last Double)   -- Nothing = 1.5
  , csBefore :: !Bool            -- True = 点より下に線、 False = 点より上
  } deriving (Show, Eq, Generic)

instance ToJSON   ConnectSpec
instance FromJSON ConnectSpec

-- ★ Phase 43 A3: レコードフィールド形式 (位置依存撲滅・挙動不変)。csBefore のみ
--   Bool 左勝ち (非 Monoid) なので名前付きで温存。残りは素直な per-field `<>`。
instance Semigroup ConnectSpec where
  a <> b = ConnectSpec
    { csOrder  = csOrder a <> csOrder b
    , csGroup  = csGroup a <> csGroup b
    , csColor  = csColor a <> csColor b
    , csWidth  = csWidth a <> csWidth b
    , csBefore = csBefore a   -- 左 (= 最初に setup されたもの) を優先
    }

instance Monoid ConnectSpec where
  mempty = defaultConnectSpec

defaultConnectSpec :: ConnectSpec
defaultConnectSpec = ConnectSpec mempty mempty mempty mempty False

-- | 色 encoding: 列指定 (categorical) か 静的色 か 連続値 gradient。
data ColorEnc
  = ColorByCol        !ColRef    -- categorical: Okabe-Ito palette
  | ColorStatic       !Text      -- "red" / "#ff0000"
  | ColorByContinuous !ColRef    -- ★ Phase 26 §C-2 #9 連続値 → Viridis 風 gradient
  deriving (Generic, Show, Eq)

instance ToJSON   ColorEnc
instance FromJSON ColorEnc

-- | P5: layer がどちらの Y 軸に属するか。
data YAxisSide = YAxisLeft | YAxisRight
  deriving (Show, Eq, Generic)

instance ToJSON   YAxisSide
instance FromJSON YAxisSide

-- | Phase 9 B: bar の position adjustment (= ggplot position_*)。
--   1 カテゴリに複数系列 (= color/group aesthetic = 'lyColor' の 'ColorByCol') の棒を
--   どう配置するか。 'PosIdentity' (既定) = 従来挙動 (= color を見ず単色棒)。
--     * 'PosDodge' = 系列を横に並べる (slot を系列数で等分)
--     * 'PosStack' = 系列を縦に積む (cumsum、 y domain は群和の max)
--     * 'PosFill'  = stack を各カテゴリ合計 1 に正規化 (y domain = [0,1])
--   JSON tag: "identity" / "dodge" / "stack" / "fill" (PS Codec と一致)。
data Position = PosIdentity | PosDodge | PosStack | PosFill
  deriving (Show, Eq, Generic)

positionJsonOptions :: Aeson.Options
positionJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'P':'o':'s':rest -> map Char.toLower rest
      other            -> other
  }

instance ToJSON Position where
  toJSON = Aeson.genericToJSON positionJsonOptions
  toEncoding = Aeson.genericToEncoding positionJsonOptions

instance FromJSON Position where
  parseJSON = Aeson.genericParseJSON positionJsonOptions

-- | Phase 36 D1: violin の片側化。 'SideBoth' (既定) = 左右対称、 'SideRight' / 'SideLeft' =
--   半 violin (片側のみ。 raincloud の「雲」 や非対称比較で使う)。
--   JSON tag: "both" / "left" / "right" (PS Codec と一致)。
data Side = SideBoth | SideLeft | SideRight
  deriving (Show, Eq, Generic)

sideJsonOptions :: Aeson.Options
sideJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'S':'i':'d':'e':rest -> map Char.toLower rest
      other                -> other }

instance ToJSON Side where
  toJSON = Aeson.genericToJSON sideJsonOptions
  toEncoding = Aeson.genericToEncoding sideJsonOptions

instance FromJSON Side where
  parseJSON = Aeson.genericParseJSON sideJsonOptions

-- | Phase 9 C / 11 A7-c: 座標系 (= ggplot coord_*)。 'CoordCartesian' (既定) = 通常の
--   直交座標。 'CoordFlip' = x/y 軸を入れ替える (= coord_flip、 横棒グラフ等)。
--   'CoordPolarX' / 'CoordPolarY' = 極座標 (= coord_polar(theta="x"|"y"))。 theta 軸を
--   角度 (0..2π、 上始点・時計回り)、 他軸を半径に写す。 PolarY + stacked bar = 円グラフ。
--   JSON tag: "cartesian" / "flip" / "polarx" / "polary" (PS Codec と一致)。
data Coord = CoordCartesian | CoordFlip | CoordPolarX | CoordPolarY
  deriving (Show, Eq, Generic)

coordJsonOptions :: Aeson.Options
coordJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'C':'o':'o':'r':'d':rest -> map Char.toLower rest
      other                    -> other
  }

instance ToJSON Coord where
  toJSON = Aeson.genericToJSON coordJsonOptions
  toEncoding = Aeson.genericToEncoding coordJsonOptions

instance FromJSON Coord where
  parseJSON = Aeson.genericParseJSON coordJsonOptions

-- | Phase 11 A7-b: facet の scale 共有方式 (= ggplot facet_wrap(scales=))。
--   'FacetFixed' (既定) = 全 panel 共通 domain (値比較可)。 'FacetFreeX' = x 軸のみ
--   panel ごとに独立 domain、 'FacetFreeY' = y のみ、 'FacetFree' = 両軸独立。 free な
--   軸は各 panel が自分のデータ範囲で scale を持ち、 全 panel に軸を表示する。
--   JSON tag: "fixed" / "freex" / "freey" / "free" (PS Codec と一致)。
data FacetScales = FacetFixed | FacetFreeX | FacetFreeY | FacetFree
  deriving (Show, Eq, Generic)

facetScalesJsonOptions :: Aeson.Options
facetScalesJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'F':'a':'c':'e':'t':rest -> map Char.toLower rest
      other                    -> other
  }

instance ToJSON FacetScales where
  toJSON = Aeson.genericToJSON facetScalesJsonOptions
  toEncoding = Aeson.genericToEncoding facetScalesJsonOptions

instance FromJSON FacetScales where
  parseJSON = Aeson.genericParseJSON facetScalesJsonOptions

-- | x 軸が free か (= 'FacetFreeX' または 'FacetFree')。
freeScaleX :: FacetScales -> Bool
freeScaleX fs = fs == FacetFreeX || fs == FacetFree

-- | y 軸が free か (= 'FacetFreeY' または 'FacetFree')。
freeScaleY :: FacetScales -> Bool
freeScaleY fs = fs == FacetFreeY || fs == FacetFree

-- | Phase 11 A7-b: facet_grid の panel サイズ配分 (= ggplot facet_grid(space=))。
--   'SpaceFixed' (既定) = 全 panel 同サイズ。 'SpaceFreeX' = 列幅を各列の x データ範囲に
--   比例、 'SpaceFreeY' = 行高を各行の y データ範囲に比例、 'SpaceFree' = 両方。 通常
--   scales="free" と併用する (= 各 panel の単位長を揃える)。 JSON tag: "fixed" / "freex"
--   / "freey" / "free"。
data FacetSpace = SpaceFixed | SpaceFreeX | SpaceFreeY | SpaceFree
  deriving (Show, Eq, Generic)

facetSpaceJsonOptions :: Aeson.Options
facetSpaceJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'S':'p':'a':'c':'e':rest -> map Char.toLower rest
      other                    -> other
  }

instance ToJSON FacetSpace where
  toJSON = Aeson.genericToJSON facetSpaceJsonOptions
  toEncoding = Aeson.genericToEncoding facetSpaceJsonOptions

instance FromJSON FacetSpace where
  parseJSON = Aeson.genericParseJSON facetSpaceJsonOptions

-- | 列幅が free か (= 'SpaceFreeX' または 'SpaceFree')。
freeSpaceX :: FacetSpace -> Bool
freeSpaceX fs = fs == SpaceFreeX || fs == SpaceFree

-- | 行高が free か (= 'SpaceFreeY' または 'SpaceFree')。
freeSpaceY :: FacetSpace -> Bool
freeSpaceY fs = fs == SpaceFreeY || fs == SpaceFree

-- | 1 layer の全 field。 各 field を 'First' (= kind は最初勝ち) または
-- 'Last' (= 属性は後勝ち) で包んで Monoid を field-wise に。
data Layer = Layer
  { lyKind    :: !(First MarkKind)
  , lyEncX    :: !(Last ColRef)
  , lyEncY    :: !(Last ColRef)
  , lyColor   :: !(Last ColorEnc)
  , lyAlpha   :: !(Last Double)
  , lySize    :: !(Last Double)
  , lyStroke  :: !(Last Double)
  , lyHover   :: ![ColRef]                  -- ★ Phase 26 §C-2 #4 multi-col tooltip
  , lyConnect :: !(Last ConnectSpec)        -- ★ Phase 26 §C-2 #5 connect points
  , lyErrorX  :: !(Last ColRef)             -- ★ Phase 26 §C-2 #6 ± 半幅 X
  , lyErrorY  :: !(Last ColRef)             -- ★ Phase 26 §C-2 #6 ± 半幅 Y
  , lyEncY2   :: !(Last ColRef)             -- ★ TODO-11: MBand 用 upper y
  , lyDAG     :: !(Last DAGSpec)            -- ★ Phase 26 §E-6 HBM ModelGraph
  , lyJitterX :: !(Last Double)             -- ★ P14 jitter X (plotArea 比率)
  , lyJitterY :: !(Last Double)             -- ★ P14 jitter Y
  , lyYAxisSide :: !(Last YAxisSide)        -- ★ P5 どちら Y 軸か
  , lyBinCount :: !(Last Int)               -- ★ frontend-settings v0.1 §2.4 hist bin 数
  , lyBinWidth :: !(Last Double)            -- ★ Phase 28: histogram の bin 幅 (= ggplot binwidth)。 binCount より優先
  , lyShape     :: !(Last MarkShape)        -- ★ Phase 30 A3: 固定 shape (bare=固定・lyShapeBy より優先)
  , lyShapeBy   :: !(Last ColRef)           -- ★ C-6 categorical shape encoding 列
  , lyShapeMap  :: ![ShapeMapEntry]          -- ★ C-6 cat → shape 上書き
  , lySizeBy    :: !(Last ColRef)           -- ★ C-6 continuous size encoding 列
  , lyAlphaBy   :: !(Last ColRef)           -- ★ Phase 30 A8 continuous alpha encoding 列
  , lyColorCats :: ![Text]                   -- ★ trellis 色一貫性 (= 全 data cat 順)
  , lyHistDensity :: !(Last Bool)             -- ★ TODO-3a (2026-05-29): histogram を density 正規化
  , lyHistBorder :: !(Last Bool)              -- ★ Phase 8 B7: histogram/bar の bin 境界線 (= default False)
  , lyDensityFill :: !(Last Bool)             -- ★ Phase 28: density 曲線下を塗る (= ggplot geom_density(aes(fill=)))。 alpha と併用
  , lyHollow    :: !(Last Bool)               -- ★ Phase 34: 中抜きマーカー (= ggplot shape="circle open"/fill=NA)。 塗り透明 + 点色 stroke
  , lyNudge     :: !(Last Double)             -- ★ Phase 36 D1: 分布 mark の slot 内横 offset (slot 幅比、 ggplot position_nudge 相当)
  , lyMarkWidth :: !(Last Double)             -- ★ Phase 36 D1: 分布 mark の幅 (slot 幅比・占有率)。 各 mark の既定占有率を上書き
  , lySide      :: !(Last Side)               -- ★ Phase 36 D1: violin の片側化 (= 半 violin)。 既定 Both
  , lyMaxLag    :: !(Last Int)                -- ★ Phase 6 A4 autocorr max lag (= default 40)
  , lyChain     :: !(Last ColRef)             -- ★ Phase 6 A5 chain group 列 (ESS / trace で chain 分け)
  , lyDensityNorm :: !(Last Bool)             -- ★ Phase 8 B16: pairs 対角用。 y 軸 = 値範囲、 KDE は panel 高さに独立正規化
  , lyPosition  :: !(Last Position)           -- ★ Phase 9 B: bar position adjustment (dodge/stack/fill、 既定 identity)
  , lyLinetype   :: !(Last LineType)           -- ★ Phase 11 A4-b: 固定 linetype (= ggplot linetype=)
  , lyLinetypeBy :: !(Last ColRef)             -- ★ Phase 11 A4-b: categorical linetype scale 列
  , lyLabel      :: !(Last ColRef)             -- ★ Phase 11 A6: geom_text/label のラベル列 (各点の文字)
  , lyStatLevel  :: !(Last Double)             -- ★ Phase 16 B1: stat 回帰の信頼水準 (= 既定 0.95)。 MStat* 解決時のみ意味を持つ
  , lyContourLevels :: !(Last Int)             -- ★ Phase 24 A4: 等高線の本数 (既定 8)。 MContour/MContourFilled 用
  , lyContourBreaks :: !(Last [Double])        -- ★ Phase 24 A4: 等高線レベルの明示指定 (本数指定より優先)
  , lyEncU        :: !(Last ColRef)            -- ★ Phase 26 A2: vector field (quiver) の u 成分列
  , lyEncV        :: !(Last ColRef)            -- ★ Phase 26 A2: vector field (quiver) の v 成分列
  , lyArrowScale  :: !(Last Double)            -- ★ Phase 26 A2: quiver 矢印長の倍率 (autoscale × この値・既定 1)
  , lyArrowMagnitude :: !(Last Bool)           -- ★ Phase 26 A2: quiver を magnitude (|u,v|) で連続色マップ (既定 False)
  , lyEdge         :: !(Last Bool)             -- ★ Phase 28: 散布点の縁を描くか (既定 False = 縁なし、 ggplot 塗り点 shape 19 相当)
  , lyEdgeColor    :: !(Last Text)             -- ★ Phase 28: 縁の色 (未指定なら点と同色)
  , lyEdgeWidth    :: !(Last Double)           -- ★ Phase 28: 縁の幅 px (既定 1.0)
  , lyOverlay      :: ![Layer]                  -- ★ Phase 36 D2: 同一 layer 内に重畳する追加 sub-mark
                                                --   (= '<+>' で蓄積)。 各 sub は自前の kind/nudge/markWidth/side
                                                --   を持ち、 親の群 (encX)・色 (colorBy)・値 (encY) を継承して描かれる。
                                                --   raincloud = (半 violin <+> box <+> strip) の preset。
  } deriving (Generic, Show, Eq)

-- | C-6: shape encoding 用 8 種。 PS Spec.purs MarkShape と一致 (= JSON round-trip)。
-- JSON: "circle" / "square" / ... ("MSh" prefix を constructorTagModifier で剥がす)。
data MarkShape
  = MShCircle | MShSquare | MShTriangle | MShDiamond | MShCross
  | MShSpade | MShHeart | MShClub
  deriving (Show, Eq, Generic)

markShapeJsonOptions :: Aeson.Options
markShapeJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'M':'S':'h':rest -> map Char.toLower rest
      other -> other
  }

instance ToJSON MarkShape where
  toJSON = Aeson.genericToJSON markShapeJsonOptions
  toEncoding = Aeson.genericToEncoding markShapeJsonOptions

instance FromJSON MarkShape where
  parseJSON = Aeson.genericParseJSON markShapeJsonOptions

-- | cat 名 → MarkShape の対応 1 件。 PS は `{ value, shape }` record で表現。
data ShapeMapEntry = ShapeMapEntry
  { smeValue :: !Text
  , smeShape :: !MarkShape
  } deriving (Show, Eq, Generic)

-- フィールドは sme- prefix (= 固定 shape combinator 'shape' とのセレクタ名衝突回避)。
-- JSON キーは従来通り value/shape を維持 (PS canvas round-trip 不変)。
shapeMapEntryJsonOptions :: Aeson.Options
shapeMapEntryJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = \s -> case s of
      "smeValue" -> "value"
      "smeShape" -> "shape"
      other      -> other }

instance ToJSON ShapeMapEntry where
  toJSON     = Aeson.genericToJSON shapeMapEntryJsonOptions
  toEncoding = Aeson.genericToEncoding shapeMapEntryJsonOptions
instance FromJSON ShapeMapEntry where
  parseJSON  = Aeson.genericParseJSON shapeMapEntryJsonOptions

-- | Phase 11 A4-b: linetype aesthetic 用 6 種 (= ggplot2 標準 linetype)。
-- JSON: "solid"/"dashed"/"dotted"/"dotdash"/"longdash"/"twodash"
-- ("Lt" prefix を constructorTagModifier で剥がし lowercase)。 PS Spec.purs LineType と一致。
data LineType
  = LtSolid | LtDashed | LtDotted | LtDotDash | LtLongDash | LtTwoDash
  deriving (Show, Eq, Enum, Bounded, Generic)

lineTypeJsonOptions :: Aeson.Options
lineTypeJsonOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \s -> case s of
      'L':'t':rest -> map Char.toLower rest
      other        -> other
  }

instance ToJSON LineType where
  toJSON = Aeson.genericToJSON lineTypeJsonOptions
  toEncoding = Aeson.genericToEncoding lineTypeJsonOptions

instance FromJSON LineType where
  parseJSON = Aeson.genericParseJSON lineTypeJsonOptions

-- | LineType → SVG/Canvas dash array (px)。 Solid のみ [] (= 実線・dasharray 無し)。
-- 値は ggplot2 既定の見た目に近い汎用パターン。 lsWidth に依存しない固定 px。
-- ※ Solid が [] を返すことが既存 SVG ゼロ diff の要 (dasharray attr を出さない)。
lineTypeDash :: LineType -> [Double]
lineTypeDash lt = case lt of
  LtSolid    -> []
  LtDashed   -> [4, 4]
  LtDotted   -> [1, 3]
  LtDotDash  -> [1, 3, 4, 3]
  LtLongDash -> [8, 4]
  LtTwoDash  -> [2, 2, 6, 2]

-- | categorical linetype scale: cat index → LineType (Solid から巡回)。
-- ggplot scale_linetype_discrete 同様、 index 0 = solid。 PS と同一順。
lineTypeForIndex :: Int -> LineType
lineTypeForIndex i = cycle [minBound .. maxBound] !! i

instance ToJSON   Layer
-- ★ Phase 36 D2: lyOverlay は後付けフィールドゆえ、 旧 JSON (= gallery specs/**.json 等) に
--   キーが無くても [] として decode できるよう、 generic parse の前に欠損キーを補う。
instance FromJSON Layer where
  parseJSON v = case v of
    Object o | not (KM.member "lyOverlay" o) ->
      Aeson.genericParseJSON Aeson.defaultOptions
        (Object (KM.insert "lyOverlay" (toJSON ([] :: [Layer])) o))
    _ -> Aeson.genericParseJSON Aeson.defaultOptions v

-- | 1 layer 内の属性合成。 'lyKind' のみ 'First' (= 最初の mark が勝ち、 後続の
-- mark は消える点に注意 ─ 重畳は 'layer' で包んで合成する。 @design/monoid-semantics.md@
-- §1 参照)。 lyHover/lyShapeMap は concat、 lyColorCats は last-nonempty、 残りは 'Last'。
-- Phase 26 A2: field 数が多く positional 列挙は取り違えやすいので record 構文で
-- per-field '(<>)' する (= Layer3D が Phase 25 A3 で行った変更と同方針)。 挙動は
-- 旧 positional 版と同一: 'lyKind' は First (= 最初の mark 勝ち)、 'lyHover'/
-- 'lyShapeMap' は list concat ('(<>)')、 'lyColorCats' は last-nonempty、 残りは Last。
instance Semigroup Layer where
  a <> b = Layer
    { lyKind        = lyKind a <> lyKind b
    , lyEncX        = lyEncX a <> lyEncX b
    , lyEncY        = lyEncY a <> lyEncY b
    , lyColor       = lyColor a <> lyColor b
    , lyAlpha       = lyAlpha a <> lyAlpha b
    , lySize        = lySize a <> lySize b
    , lyStroke      = lyStroke a <> lyStroke b
    , lyHover       = lyHover a <> lyHover b
    , lyConnect     = lyConnect a <> lyConnect b
    , lyErrorX      = lyErrorX a <> lyErrorX b
    , lyErrorY      = lyErrorY a <> lyErrorY b
    , lyEncY2       = lyEncY2 a <> lyEncY2 b
    , lyDAG         = lyDAG a <> lyDAG b
    , lyJitterX     = lyJitterX a <> lyJitterX b
    , lyJitterY     = lyJitterY a <> lyJitterY b
    , lyYAxisSide   = lyYAxisSide a <> lyYAxisSide b
    , lyBinCount    = lyBinCount a <> lyBinCount b
    , lyBinWidth    = lyBinWidth a <> lyBinWidth b
    , lyShape       = lyShape a <> lyShape b
    , lyShapeBy     = lyShapeBy a <> lyShapeBy b
    , lyShapeMap    = lyShapeMap a <> lyShapeMap b
    , lySizeBy      = lySizeBy a <> lySizeBy b
    , lyAlphaBy     = lyAlphaBy a <> lyAlphaBy b
    , lyColorCats   = if null (lyColorCats b) then lyColorCats a else lyColorCats b
    , lyHistDensity = lyHistDensity a <> lyHistDensity b
    , lyHistBorder  = lyHistBorder a <> lyHistBorder b
    , lyDensityFill = lyDensityFill a <> lyDensityFill b
    , lyHollow      = lyHollow a <> lyHollow b
    , lyNudge       = lyNudge a <> lyNudge b
    , lyMarkWidth   = lyMarkWidth a <> lyMarkWidth b
    , lySide        = lySide a <> lySide b
    , lyMaxLag      = lyMaxLag a <> lyMaxLag b
    , lyChain       = lyChain a <> lyChain b
    , lyDensityNorm = lyDensityNorm a <> lyDensityNorm b
    , lyPosition    = lyPosition a <> lyPosition b
    , lyLinetype    = lyLinetype a <> lyLinetype b
    , lyLinetypeBy  = lyLinetypeBy a <> lyLinetypeBy b
    , lyLabel       = lyLabel a <> lyLabel b
    , lyStatLevel   = lyStatLevel a <> lyStatLevel b
    , lyContourLevels = lyContourLevels a <> lyContourLevels b
    , lyContourBreaks = lyContourBreaks a <> lyContourBreaks b
    , lyEncU        = lyEncU a <> lyEncU b
    , lyEncV        = lyEncV a <> lyEncV b
    , lyArrowScale  = lyArrowScale a <> lyArrowScale b
    , lyArrowMagnitude = lyArrowMagnitude a <> lyArrowMagnitude b
    , lyEdge        = lyEdge a <> lyEdge b
    , lyEdgeColor   = lyEdgeColor a <> lyEdgeColor b
    , lyEdgeWidth   = lyEdgeWidth a <> lyEdgeWidth b
    , lyOverlay     = lyOverlay a <> lyOverlay b   -- ★ Phase 36 D2: sub-mark を concat
    }

instance Monoid Layer where
  mempty = Layer
    { lyKind = mempty, lyEncX = mempty, lyEncY = mempty, lyColor = mempty
    , lyAlpha = mempty, lySize = mempty, lyStroke = mempty, lyHover = []
    , lyConnect = mempty, lyErrorX = mempty, lyErrorY = mempty, lyEncY2 = mempty
    , lyDAG = mempty, lyJitterX = mempty, lyJitterY = mempty, lyYAxisSide = mempty
    , lyBinCount = mempty, lyBinWidth = mempty, lyShape = mempty, lyShapeBy = mempty, lyShapeMap = [], lySizeBy = mempty
    , lyAlphaBy = mempty
    , lyColorCats = [], lyHistDensity = mempty, lyHistBorder = mempty
    , lyDensityFill = mempty, lyHollow = mempty
    , lyNudge = mempty, lyMarkWidth = mempty, lySide = mempty
    , lyMaxLag = mempty, lyChain = mempty, lyDensityNorm = mempty
    , lyPosition = mempty, lyLinetype = mempty, lyLinetypeBy = mempty
    , lyLabel = mempty, lyStatLevel = mempty, lyContourLevels = mempty
    , lyContourBreaks = mempty, lyEncU = mempty, lyEncV = mempty
    , lyArrowScale = mempty, lyArrowMagnitude = mempty
    , lyEdge = mempty, lyEdgeColor = mempty, lyEdgeWidth = mempty
    , lyOverlay = []
    }

-- ===========================================================================
-- Layer constructors (= 各 mark の最小起点)
-- ===========================================================================

scatter, line, bar :: ColRef -> ColRef -> Layer
scatter x y = mempty
  { lyKind = First (Just MScatter), lyEncX = Last (Just x), lyEncY = Last (Just y) }
line    x y = mempty
  { lyKind = First (Just MLine),    lyEncX = Last (Just x), lyEncY = Last (Just y) }
bar     x y = mempty
  { lyKind = First (Just MBar),     lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 30 A7: 2D scatter ('Point2' 直入れ・3D 'Hgg.Plot.ThreeD.Spec.scatter3DPoints'
--   と対称)。 内部は @scatter (inline xs) (inline ys)@ に等価 (= x/y を inline 列に分解)
--   なので Render/JSON/PS 無改修。
--
-- > scatterPoints [Point2 1 2, Point2 3 4]
scatterPoints :: [Point2] -> Layer
scatterPoints pts = scatter (inline xs) (inline ys)
  where (xs, ys) = unzipPoint2 pts

-- | Phase 30 A7: 2D line ('Point2' 直入れ・3D 'Hgg.Plot.ThreeD.Spec.line3DPoints'
--   と対称)。 内部は @line (inline xs) (inline ys)@ に等価。
linePoints :: [Point2] -> Layer
linePoints pts = line (inline xs) (inline ys)
  where (xs, ys) = unzipPoint2 pts

-- | '[Point2]' を x / y の 'Double' リストに分解 ('scatterPoints' / 'linePoints' 用)。
unzipPoint2 :: [Point2] -> ([Double], [Double])
unzipPoint2 = unzip . map (\(Point2 x y) -> (x, y))

-- | Phase 26 A2: vector field (quiver)。 各 (x,y) に成分 (u,v) の矢印を描く
--   (= matplotlib @quiver@)。 矢印長は autoscale (= 最長矢印がデータ対角の ~8%)
--   に 'arrowScale' 倍を掛けた長さ。 列バインドは @df |>> quiver \"x\" \"y\" \"u\" \"v\"@。
--   矢印を magnitude (= √(u²+v²)) で連続色マップするには 'arrowColorByMagnitude'。
quiver :: ColRef -> ColRef -> ColRef -> ColRef -> Layer
quiver x y u v = mempty
  { lyKind = First (Just MQuiver)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyEncU = Last (Just u), lyEncV = Last (Just v) }

-- | Phase 26 A2: quiver 矢印長の倍率 (autoscale × この値・既定 1)。 値を上げると矢印が長く。
arrowScale :: Double -> Layer
arrowScale s = mempty { lyArrowScale = Last (Just s) }

-- | Phase 26 A2: quiver の矢印を magnitude (= √(u²+v²)) で連続色マップする (+ 既定 OFF)。
--   色は連続パレット (viridis 系)。 OFF 時は単色 ('color' / theme)。
arrowColorByMagnitude :: Layer
arrowColorByMagnitude = mempty { lyArrowMagnitude = Last (Just True) }

-- | Phase 11 A6: データ駆動テキストラベル (= ggplot @geom_text@)。 各 (x,y) 点に lab 列の
--   文字を描く。 'annotate' (固定 1 点) と違い列駆動で点数ぶん出る。
text :: ColRef -> ColRef -> ColRef -> Layer
text x y lab = mempty
  { lyKind = First (Just MText), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyLabel = Last (Just lab) }

-- | Phase 11 A6: 背景付きテキストラベル (= ggplot @geom_label@)。 'text' と同じだが
--   各文字の背後に角丸矩形を敷く (= 重なる点の上でも読みやすい)。
label :: ColRef -> ColRef -> ColRef -> Layer
label x y lab = mempty
  { lyKind = First (Just MLabel), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyLabel = Last (Just lab) }

-- | Phase 11 A6-2: Q-Q plot (= ggplot @stat_qq@ / @geom_qq@)。 sample 列のみを取り、
--   ソートした order statistic y_(i) を y、 理論正規分位点 Φ⁻¹((i-0.5)/n) を x に置いて
--   点を描く (= 正規性の視覚診断)。 理論分位点は render / range 側で算出するため、
--   ここでは sample を encY に保持するだけ (encX 列は持たない)。
qq :: ColRef -> Layer
qq sample = mempty
  { lyKind = First (Just MQQ), lyEncY = Last (Just sample) }

-- | Phase 11 A6-4: ECDF plot (= ggplot @stat_ecdf@)。 sample 列 (encX) をソートして
--   右連続の経験累積分布 F(x)=#(≤x)/n を階段状に描く (y∈[0,1])。
ecdf :: ColRef -> Layer
ecdf sample = mempty
  { lyKind = First (Just MEcdf), lyEncX = Last (Just sample) }

-- | Phase 11 A6-4b: linerange (= ggplot @geom_linerange@)。 各 (x,y) に縦線 y±err を描く。
lineRange :: ColRef -> ColRef -> ColRef -> Layer
lineRange x y err = mempty
  { lyKind = First (Just MLineRange), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | Phase 11 A6-4b: pointrange (= ggplot @geom_pointrange@)。 linerange + 中心点。
pointRange :: ColRef -> ColRef -> ColRef -> Layer
pointRange x y err = mempty
  { lyKind = First (Just MPointRange), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | Phase 11 A6-4b: crossbar (= ggplot @geom_crossbar@)。 幅付き箱 (y±err) + 中央水平線。
crossbar :: ColRef -> ColRef -> ColRef -> Layer
crossbar x y err = mempty
  { lyKind = First (Just MCrossbar), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | Phase 11 A6-4c: stat_function (= ggplot @stat_function@ / @geom_function@)。
--   関数 f を [xLo, xHi] で n 点サンプルし、 inline 列の line layer を生成する。
--   関数自体は JSON 化できないため **構成時にサンプル点へ焼き込む** (= spec には点列が入り、
--   canvas backend は通常の line として描く)。 n<2 は 2 に切り上げ。
statFunction :: (Double -> Double) -> Double -> Double -> Int -> Layer
statFunction f xLo xHi n =
  let m  = max 2 n
      xs = [ xLo + (xHi - xLo) * fromIntegral i / fromIntegral (m - 1) | i <- [0 .. m - 1] ]
      ys = map f xs
  in line (ColNum (V.fromList xs)) (ColNum (V.fromList ys))

histogram :: ColRef -> Layer
histogram x = mempty
  { lyKind = First (Just MHistogram), lyEncX = Last (Just x) }

-- | 頻度多角形 (Ch10 EDA, Phase 28): @geom_freqpoly(aes(x = …))@ 相当。 histogram と
--   同じ bin 化で各 bin の count を求め、 bin 中心を折れ線で結ぶ。 bin 幅は
--   'binWidth' / 'binCount'、 after_stat(density) は 'histogramDensity' True で
--   流用 (= histogram と同じフラグ)。 color aesthetic ('colorBy') で群分割すると
--   群ごとに別色の折れ線を重ねる (cut 別 price 分布の比較等)。
freqpoly :: ColRef -> Layer
freqpoly x = mempty
  { lyKind = First (Just MFreqPoly), lyEncX = Last (Just x) }

-- | Ch10 EDA (Phase 28): 2 カテゴリ変数の件数 (= ggplot @geom_count()@ / @stat_sum@)。
--   @countXY x y@ は (x,y) のカテゴリ組合せごとに観測件数を集計し、 各セル中心に
--   面積 ∝ 件数 (= 半径 ∝ √件数) の点を描く。 'size' で最大半径 px を上書き可。
countXY :: ColRef -> ColRef -> Layer
countXY x y = mempty
  { lyKind = First (Just MCount), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Wide-form histogram (P1、 Phase 6 A10): 複数列を **同一 plot に半透明で重ねる**。
--
--   `histogramWide [c1, c2, c3]` は 'VisualSpec' を返し、 内部で各列を独立 layer 化:
--
--     * layer i = `histogram cᵢ <> color (fromHex (palette i)) <> alpha 0.4 <> binCount 20`
--
--   palette は ColorBrewer Set1 (= categorical 9-class、 wong / Hgg 切替は今後)。
--   bin 数は全列で **共通** (= seaborn の `multiple="layer"` 同等)、 デフォ 20。
--
--   matplotlib との対応: `plt.hist([c1, c2, c3], alpha=0.5, label=names)` 相当。
-- | MCMC autocorrelation plot (P19、 Phase 6 A4): 1 列の時系列から lag-k 自己相関 r(τ)
--   を計算し bar chart で表示。 max lag は 'autocorrMaxLag'、 default は 40。
--   ±1.96/√N の significance band も同時描画。
--
--   r(τ) = Σ(x_t - μ)(x_{t+τ} - μ) / Σ(x_t - μ)²
--
--   matplotlib との対応: `plt.acorr(x, maxlags=40)` 相当 (= 但し片側のみ)。
autocorr :: ColRef -> Layer
autocorr c = mempty
  { lyKind = First (Just MAutocorr)
  , lyEncX = Last (Just c)
  }

-- | autocorr の max lag (= 'autocorr' と '<>' で組合せ)。 default 40。
autocorrMaxLag :: Int -> Layer
autocorrMaxLag n = mempty { lyMaxLag = Last (Just n) }

-- | Effective Sample Size plot (P20、 Phase 6 A5): chain ごとに ESS bar を描画。
--   chain group は 'chain' で指定 (= 'ess vals <> chain chainCol')。
--   chain 未指定なら全体を 1 chain として 1 bar。
--
--   ESS = N / (1 + 2 Σ |r(τ)|)  (= τ=1 から r(τ) > 0 まで)
--
--   matplotlib / arviz 対応: `az.plot_ess(idata)` の chain ごと bar (= 簡略版)。
-- | ESS 棒グラフ (Phase 8 B13): encX = パラメータ/chain 名 (categorical)、
-- encY = 計算済み ESS 値。 ESS の計算は統計ライブラリ (analyze 側) の責務で、
-- plot は値を棒にするだけ (= ggplot/bayesplot mcmc_neff 流の「計算と描画の分離」)。
ess :: ColRef -> ColRef -> Layer
ess nameCol essCol = mempty
  { lyKind = First (Just MEss)
  , lyEncX = Last (Just nameCol)
  , lyEncY = Last (Just essCol)
  }

-- | chain group 列を設定 (= 'autocorr' / 'ess' で chain 分け、 MTrace でも将来使用)。
chain :: ColRef -> Layer
chain c = mempty { lyChain = Last (Just c) }

-- | Forest plot (Phase 6 A2): 各 row が「label + 点推定 + CI」 の horizontal CI bar 群。
--
--   引数: label 列 (= categorical/text)、 point estimate 列、 ± 半幅 列 (= 対称 CI)。
--
--   * y 軸: label
--   * x 軸: estimate
--   * 中央 vertical 線: 'forestNull' (= default 0、 メタ解析慣例で OR は 1)
--
--   asymmetric CI (= lo / hi 個別) は将来。 現状は対称 CI のみ。
forest :: ColRef -> ColRef -> ColRef -> Layer
forest labelCol estCol errCol = mempty
  { lyKind   = First (Just MForest)
  , lyEncX   = Last (Just estCol)
  , lyEncY   = Last (Just labelCol)
  , lyErrorX = Last (Just errCol)
  }

-- | Forest plot の null effect 位置 (= 縦 0 線、 メタ解析の reference)。 default 0。
-- リスク比 / オッズ比 を log scale で扱う場合は 0 (= log 1)、 線形なら 0 (= 差)。
forestNull :: Double -> Layer
forestNull v = mempty { lyMaxLag = Last (Just (round v)) }
  -- 流用: lyMaxLag を null position の Int で再利用 (= round)。
  -- TODO: Double-precision null position field を別途 (= 当面 Int で十分)

-- | Funnel plot (Phase 6 A3): 効果量 vs 標準誤差の散布図 + 95% 信頼区間 envelope。
--
--   引数: 効果量 (effect) 列、 標準誤差 (SE) 列。 出版 bias 確認に使う。
--
--   * x 軸: effect (= estimate)
--   * y 軸: SE (= 上方が精度高、 下方が精度低)
--   * 中央 vertical 線: pooled mean (= データから算出)
--   * diagonal 線: pooled ± 1.96 * SE の envelope
funnel :: ColRef -> ColRef -> Layer
funnel effectCol seCol = mempty
  { lyKind = First (Just MFunnel)
  , lyEncX = Last (Just effectCol)
  , lyEncY = Last (Just seCol)
  }

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

-- | Box plot。 ★ Phase 36: 値 1 列を受ける。 群分けは @<> groupBy "g"@ (色一律) /
--   @<> colorBy "g"@ (群色+凡例) で付ける (ggplot 同型)。 群指定なしなら単一 box。
boxplot :: ColRef -> Layer
boxplot vals = mempty
  { lyKind = First (Just MBox), lyEncY = Last (Just vals) }

-- | ★ Phase 36: 群で分けて配置するチャネル (= ggplot @aes(group=)@)。 色は付けない
--   (一律。 色は 'color' / 'colorBy' で別途)。 distribution mark (boxplot/violin 等) では
--   群ごとに集約を作りカテゴリ x に並べる。 内部表現は encX (= 既存の群配置機構を流用)。
--   ⚠ @Data.List.groupBy@ と同名なので、 両方 import する場合は qualified 推奨。
groupBy :: ColRef -> Layer
groupBy g = mempty { lyEncX = Last (Just g) }

-- | Density plot: x 列の値ベクター で Gaussian KDE 曲線。
density :: ColRef -> Layer
density x = mempty
  { lyKind = First (Just MDensity), lyEncX = Last (Just x) }

-- | Phase 8 B16: pairs 対角用 density。 y 軸目盛りは値範囲 (= 行の変数値、 散布図行と
-- 共有)、 KDE 曲線は panel 高さに独立正規化して描く (= seaborn pairplot 対角の挙動)。
densityNorm :: ColRef -> Layer
densityNorm x = mempty
  { lyKind = First (Just MDensity), lyEncX = Last (Just x)
  , lyDensityNorm = Last (Just True) }

-- | Phase 26 S4-d: Pie chart (= encX cat, encY 値合計の扇)。
pie :: ColRef -> ColRef -> Layer
pie x y = mempty
  { lyKind = First (Just MPie), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 26 S5-c: Waterfall chart (= encX cat, encY delta、 累積 bar)。
waterfall :: ColRef -> ColRef -> Layer
waterfall x y = mempty
  { lyKind = First (Just MWaterfall), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 26 S4 / Phase 11 A6-3 (= Heatmap): x = カテゴリ, y = カテゴリ, value = 数値。
-- |   各 (x,y) セルを value の連続色 (Viridis) で塗る。 value は ColorByContinuous で表現。
-- |   PS heatmap と対応。
heatmap :: ColRef -> ColRef -> ColRef -> Layer
heatmap x y v = mempty
  { lyKind = First (Just MHeatmap)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous v))
  }

-- | Phase 26 S5-e-1: Contour / binned heatmap (= 連続 x/y/z、 grid 化して
-- |   セル平均を Viridis 色マッピング)。 ResponseSurface の基盤。
-- |   color は ColorByContinuous で z 列を表現。
contour :: ColRef -> ColRef -> ColRef -> Layer
contour x y z = mempty
  { lyKind = First (Just MContour)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | Phase 24 A4: filled contour (= matplotlib @contourf@ / ggplot
-- @geom_contour_filled@)。 等値帯を Viridis 連続色で塗る。 入力が規則 grid
-- (x 固有値 × y 固有値が全組存在) なら補間せず直入力、 散布なら k 近傍 IDW で
-- 格子化 ('Hgg.Plot.Math.Griddata')。 線の 'contour' と重畳すると
-- matplotlib の contourf+contour 同等。
contourFilled :: ColRef -> ColRef -> ColRef -> Layer
contourFilled x y z = mempty
  { lyKind = First (Just MContourFilled)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | Phase 24 A4: 等高線の本数 (既定 8)。 @contour x y z <> contourLevels 12@。
contourLevels :: Int -> Layer
contourLevels n = mempty { lyContourLevels = Last (Just n) }

-- | Phase 24 A4: 等高線レベルの明示指定 (本数指定より優先)。
contourBreaks :: [Double] -> Layer
contourBreaks bs = mempty { lyContourBreaks = Last (Just bs) }

-- | binned heatmap (= ggplot geom_bin2d / stat_summary_2d)。 連続 x/y/z を
-- nBins×nBins の grid に binning し、 各セルの z 平均を連続色 (Viridis) で塗る。
-- 'contour' (等高線) の塗り版。 ResponseSurface の塗り基盤。
bin2d :: ColRef -> ColRef -> ColRef -> Layer
bin2d x y z = mempty
  { lyKind = First (Just MBin2d)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | Ch10 EDA (Phase 28): 2D bin の**件数**を連続色で塗る (= ggplot @geom_bin2d()@ 既定)。
--   @bin2dCount x y@ は連続 x/y を 12×12 grid に binning し、 各セルの**観測件数**を
--   Viridis で塗る (z 列なし = 'bin2d' の count 版)。 'bin2d' (z 平均) は stat_summary_2d 相当。
bin2dCount :: ColRef -> ColRef -> Layer
bin2dCount x y = mempty
  { lyKind = First (Just MBin2d)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  }

-- | Phase 40: hexbin (= matplotlib @hexbin@ / ggplot @geom_hex@)。 連続 x/y を**六角格子**で
--   binning し、 各セルの**観測件数**を Viridis 連続色で塗る (= 散布過密の密度可視化)。
--   セル分割数は 'hexbinBins' で上書き (既定 30)。 矩形ビンの 'bin2dCount' の六角版。
--   アルゴは d3-hexbin (Carr 1987) を binwidth 正規化空間で適用 (pointy-top)。
hexbin :: ColRef -> ColRef -> Layer
hexbin x y = mempty
  { lyKind = First (Just MHexbin)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  }

-- | Phase 40: hexbin の x 方向セル分割数を指定 (= ggplot @bins@ / matplotlib @gridsize@)。
--   既定 30。 'hexbin' に @<>@ で重ねる: @layer (hexbin "x" "y" <> hexbinBins 40)@。
--   内部は 'lyBinCount' を流用 (histogram と共有フィールド)。
hexbinBins :: Int -> Layer
hexbinBins n = mempty { lyBinCount = Last (Just n) }

-- | P12: step plot (= 階段状 line)。
step :: ColRef -> ColRef -> Layer
step x y = mempty
  { lyKind = First (Just MStep), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 16: stat-in 線形回帰 (= ggplot @geom_smooth(method="lm")@)。 純タグ Layer。
--   回帰 fit は描画前に analyze-bridge の @resolveStats@ が hanalyze で行い、 信頼帯 (band) +
--   回帰線 (line) に展開する。 装飾は通常 geom と同じ: @statLm "x" "y" <> color N.red <> stroke 2@。
--   ★単体では描画されない (renderer は MStatLM を skip)。 必ず bridge の saveSVGBoundStats 等で解決する。
statLm :: ColRef -> ColRef -> Layer
statLm x y = mempty
  { lyKind = First (Just MStatLM), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 16 B1: 信頼水準を指定できる線形回帰 stat。 'statLm' は 0.95 固定だが、
--   こちらは @lvl@ (例 0.99) を 'lyStatLevel' に持たせる。 resolveStats が band 幅に反映する。
statLmLevel :: ColRef -> ColRef -> Double -> Layer
statLmLevel x y lvl = (statLm x y)
  { lyStatLevel = Last (Just lvl) }

-- | Phase 16: stat-in B-spline 平滑 (= ggplot @geom_smooth()@)。 knot 数 n。 曲線のみ (帯なし)。
--   resolveStats が hanalyze で fit し line に展開。 装飾は line に引き継がれる。
statSmooth :: ColRef -> ColRef -> Int -> Layer
statSmooth x y n = mempty
  { lyKind = First (Just MStatSmooth), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyBinCount = Last (Just n) }

-- | Phase 16 B1: 信頼帯つき B-spline 平滑。 'statSmooth' は曲線のみだが、 こちらは
--   'lyStatLevel' を Just にして「帯あり」を signal する。 resolveStats が bs 設計行列の
--   confidenceBand で band+line に展開する。 既定水準は 0.95 (@statSmoothCI x y n@)。
statSmoothCI :: ColRef -> ColRef -> Int -> Layer
statSmoothCI x y n = (statSmooth x y n)
  { lyStatLevel = Last (Just 0.95) }

-- | Phase 16 B3: 多項式回帰 stat (= ggplot @geom_smooth(method="lm", formula=y~poly(x,deg))@)。
--   次数 deg は 'lyBinCount' を流用。 resolveStats が @y ~ poly(x,deg)@ で fit し band+line に展開。
--   信頼帯の水準は 'lyStatLevel' (既定 0.95)。 ★単体では描画されない (renderer は MStatPoly を skip)。
statPoly :: ColRef -> ColRef -> Int -> Layer
statPoly x y deg = mempty
  { lyKind = First (Just MStatPoly), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyBinCount = Last (Just deg) }

-- | Phase 16 B3: 残差 vs fitted 診断散布 (= base R @plot(lm)@ #1)。 @y ~ x@ で fit し
--   各点を (fitted, residual) に写した scatter に展開する (回帰診断)。 装飾は scatter に引き継ぐ。
--   ★単体では描画されない (renderer は MStatResid を skip)。 bridge resolveStats が必要。
statResid :: ColRef -> ColRef -> Layer
statResid x y = mempty
  { lyKind = First (Just MStatResid), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | P11: stem / lollipop plot。
stem :: ColRef -> ColRef -> Layer
stem x y = mempty
  { lyKind = First (Just MStem), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | TODO-11 (2026-05-27): area band (= 信頼区間 / 予測帯)。
-- |   x       = 共通 x 軸
-- |   yLow    = 下境界 y
-- |   yHigh   = 上境界 y
-- | Render は PPath fill 1 枚 (= forward x-yLow + backward x-yHigh + close)。
-- | alpha は layer modifier の `alpha` で指定 (= default 0.2)。
band :: ColRef -> ColRef -> ColRef -> Layer
band x yLow yHigh = mempty
  { lyKind = First (Just MBand)
  , lyEncX = Last (Just x)
  , lyEncY = Last (Just yLow)
  , lyEncY2 = Last (Just yHigh)
  }

-- | Phase 52.D2: streamgraph (= 中心化積層 area)。
-- |   x = 共通 x 軸 (連続、 例: 時間)
-- |   y = 各系列の値
-- | 系列分割は color aesthetic で行う (= 'bar' の群分けと同型)。
-- |
-- |   > stream "t" "value" <> colorBy "series"
-- |
-- | 各 x 点で系列を積層し baseline を -(Σy)/2 から開始する (silhouette 中心化)。
-- | wiggle 最小化 (ThemeRiver) は行わない。
stream :: ColRef -> ColRef -> Layer
stream x y = mempty
  { lyKind = First (Just MStream), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | P2: violin plot。 ★ Phase 36 B1c: boxplot と同じく **値 1 列**を受ける。 群分けは
--   @<> groupBy "g"@ (色一律) / @<> colorBy "g"@ (群色+凡例) で付ける (ggplot 同型)。
--   群指定なしなら単一 violin。
violin :: ColRef -> Layer
violin v = mempty { lyKind = First (Just MViolin), lyEncY = Last (Just v) }

-- | P3: strip plot。 ★ Phase 36 B1c: 値 1 列 + groupBy/colorBy で群分け。
strip :: ColRef -> Layer
strip v = mempty { lyKind = First (Just MStrip), lyEncY = Last (Just v) }

-- | P3: swarm plot。 ★ Phase 36 B1c: 値 1 列 + groupBy/colorBy で群分け。
swarm :: ColRef -> Layer
swarm v = mempty { lyKind = First (Just MSwarm), lyEncY = Last (Just v) }

-- | P22: raincloud (= violin + box + strip 合成)。 ★ Phase 36 B1c: 値 1 列 + groupBy/colorBy。
-- | Phase 36 D2: mark 直結合成。 @a \<+\> b@ は a を base、 b を重畳 sub-mark とする
--   **単一 Layer** を返す (= 戻り型 Layer 維持ゆえ @raincloud v \<+\> ... \<\> groupBy g@ の
--   ような群修飾が従来どおり効く)。 b 側の overlay も平坦化して取り込む。 render は base +
--   各 overlay を「親の群 (encX)・色 (colorBy)・値 (encY) を継承・自前の kind/nudge/markWidth/side
--   で」 描く。 1D 分布 mark (box/violin/strip/swarm) の重畳を想定 (= raincloud / 自作 composite)。
infixl 7 <+>
(<+>) :: Layer -> Layer -> Layer
a <+> b = a { lyOverlay = lyOverlay a ++ [b { lyOverlay = [] }] ++ lyOverlay b }

-- | P22: raincloud (= 半 violin + box + jitter strip の合成)。 ★ Phase 36 D2: 専用 mark を廃し
--   '<+>' による 3 sub-mark 合成の preset に降格 (= 位置決めは D1 つまみ nudge/markWidth/side に委譲)。
--   戻り型は Layer なので @raincloud v \<\> groupBy g@ / @\<\> colorBy g@ は従来どおり群分けする。
raincloud :: ColRef -> Layer
raincloud v =
      (violin  v <> side SideRight <> nudge 0.15    <> markWidth 0.40)
  <+> (boxplot v               <> nudge 0.00    <> markWidth 0.10)
  <+> (strip   v               <> nudge (-0.25) <> markWidth 0.18)

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

-- | Phase 36 D3: 合成 Layer (base + overlay sub-mark) の値列レーン (= encY の distinct・base 先頭・
--   'colRefName' で重複除去)。 描画/Layout は各マークの slot を「自 encY が此のレーン列の何番目か」
--   で決める。 同一列なら 1 レーン (= raincloud の重畳)、 複数列なら横並び (= distCols)。
compositeLanes :: Layer -> [ColRef]
compositeLanes ly = foldl add [] [ c | l <- ly : lyOverlay ly, Just c <- [getLast (lyEncY l)] ]
  where add acc c = if any ((== colRefName c) . colRefName) acc then acc else acc ++ [c]

-- | P21: ridge / joyplot。 ★ Phase 36 B1c: 他 distribution mark と統一して **値 1 列**を
-- |   受ける。 群分けは @<> groupBy "g"@ / @<> colorBy "g"@ (= box/violin と同じ)。
-- |   群指定なしは単一 density 風。 ridge は値→x・群→y の向きが要るため、 ridge レイヤを
-- |   含む spec は 'ridgeAutoFlip' で coord_flip を自動適用する (値が x、 群が y に回る)。
-- |   内部表現は violin と同じ encY=値。
ridge :: ColRef -> Layer
ridge v = mempty { lyKind = First (Just MRidge), lyEncY = Last (Just v) }

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

-- | P14: scatter jitter (= plotArea 比率 0..1)。
jitterX, jitterY :: Double -> Layer
jitterX a = mempty { lyJitterX = Last (Just a) }
jitterY a = mempty { lyJitterY = Last (Just a) }

-- | frontend-settings v0.1 §2.4: histogram の bin 数 (= default 10)。
binCount :: Int -> Layer
binCount n = mempty { lyBinCount = Last (Just n) }

-- | Phase 28: histogram の bin 幅 (= ggplot @geom_histogram(binwidth = w)@)。
--   'binWidth' を指定すると 'binCount' より優先され、 'histBinning' が ggplot 流
--   (boundary = w/2 で bin 原点を定める) の bin 化を行う。
binWidth :: Double -> Layer
binWidth w = mempty { lyBinWidth = Last (Just w) }

-- | histogram の bin 化パラメタ (origin, binW, nBin) を決める単一情報源。
--   render (Render.Basic) と y/x range (Layout.RangeOf) の双方がこれを使い、
--   bin 境界・棒高・軸範囲を一致させる。
--
--   * 'lyBinWidth' 指定時: ggplot @bin_breaks_width@ と同式。 boundary = w/2 とし、
--     origin = boundary + floor((lo - boundary)/w) * w、 nBin = ceil((hi - origin)/w)。
--     これで R4DS の @binwidth=@ と同じ bin 境界・棒高になる。
--   * 未指定時: 従来どおり 'lyBinCount' (既定 30) で [lo,hi] を等分。
--
--   bin i は @[origin + i*binW, origin + (i+1)*binW)@、 値 v の所属は
--   @clamp 0 (nBin-1) (floor ((v - origin)/binW))@。
histBinning :: Layer -> (Double, Double) -> (Double, Double, Int)
histBinning ly (lo, hi) =
  case getLast (lyBinWidth ly) of
    Just w | w > 0 ->
      let boundary = w / 2
          shift    = fromIntegral (floor ((lo - boundary) / w) :: Int)
          origin   = boundary + shift * w
          nBin     = max 1 (ceiling ((hi - origin) / w))
      in (origin, w, nBin)
    _ ->
      let nBin = case getLast (lyBinCount ly) of
                   Just n | n > 0 -> n
                   _              -> 30
          binW = if hi > lo then (hi - lo) / fromIntegral nBin else 1
      in (lo, binW, nBin)

-- | Phase 40: hexbin の六角セル (中心 + 件数 + 6 頂点、 すべてデータ座標)。
data HexCell = HexCell
  { hexCx    :: !Double             -- ^ セル中心 x (データ座標)
  , hexCy    :: !Double             -- ^ セル中心 y
  , hexCount :: !Int                -- ^ セルに入った点数
  , hexVerts :: ![(Double, Double)] -- ^ 6 頂点 (pointy-top、 データ座標)
  } deriving (Show, Eq)

-- | Phase 40: 六角ビニング (d3-hexbin = Carr 1987)。 @bins@ = x 方向セル分割数。
--   (xmin,xmax)/(ymin,ymax) = データ範囲、 @pts@ = (x,y) 点列。 binwidth で正規化した
--   (u,v) 空間で点を六角セルに割当て件数を数え、 中心・6 頂点をデータ座標で返す
--   (= scale パイプラインでそのまま screen へ。 pointy-top)。
--   ★HS/PS で同式・JS Math.round (= @floor (z+0.5)@) を使い byte 一致させる。
hexbinCells :: Int -> (Double, Double) -> (Double, Double)
            -> [(Double, Double)] -> [HexCell]
hexbinCells bins (xmin, xmax) (ymin, ymax) pts
  | bins <= 0 || bwx <= 0 || bwy <= 0 || null pts = []
  | otherwise =
      [ mkCell (head grp) (length grp)
      | grp <- Data.List.group (Data.List.sort (map assign pts)) ]
  where
    bwx = (xmax - xmin) / fromIntegral bins
    bwy = (ymax - ymin) / fromIntegral bins
    dyv = sqrt 3 / 2                       -- = 1.5·r  (r = 1/√3、 dx=√3·r=1 に正規化)
    ruv = 1 / sqrt 3
    jsRound z = floor (z + 0.5) :: Int     -- JS Math.round (half-up)・HS=PS 一致用
    -- 点 → セルキー (pi, pj) (d3-hexbin verbatim)
    assign :: (Double, Double) -> (Int, Int)
    assign (x, y) =
      let u   = (x - xmin) / bwx
          v   = (y - ymin) / bwy
          py  = v / dyv
          pj  = jsRound py
          px  = u - (if odd pj then 0.5 else 0)     -- dx=1、 奇数行 0.5 シフト
          pii = jsRound px
          py1 = py - fromIntegral pj
      in if abs py1 * 3 > 1
           then let px1 = px - fromIntegral pii
                    pi2 = fromIntegral pii + (if px < fromIntegral pii then -1 else 1) / 2 :: Double
                    pj2 = pj + (if py < fromIntegral pj then -1 else 1)
                    px2 = px - pi2
                    py2 = py - fromIntegral pj2
                in if px1 * px1 + py1 * py1 > px2 * px2 + py2 * py2
                     then (jsRound (pi2 + (if odd pj then 1 else -1) / 2), pj2)
                     else (pii, pj)
           else (pii, pj)
    -- セルキー → HexCell (中心・頂点をデータ座標へ)
    mkCell :: (Int, Int) -> Int -> HexCell
    mkCell (pii, pj) n =
      let cu = fromIntegral pii + (if odd pj then 0.5 else 0)   -- × dx(=1)
          cv = fromIntegral pj * dyv
          cx = xmin + cu * bwx
          cy = ymin + cv * bwy
          vert k = let ang = fromIntegral k * pi / 3
                       vu  = sin ang * ruv
                       vv  = negate (cos ang) * ruv
                   in (xmin + (cu + vu) * bwx, ymin + (cv + vv) * bwy)
      in HexCell cx cy n (map vert [0 .. 5 :: Int])

-- | Phase 40: hexbin layer を解決して六角セルを返す (renderHexbin と count colorbar が共有)。
--   x/y を 'resolveNum' で取り NaN を除いて zip、 bins (既定 30) で 'hexbinCells'。
--   render と凡例で**同じ count 域**を得るために 1 本に集約する。
hexbinLayerCells :: Resolver -> Layer -> [HexCell]
hexbinLayerCells r ly =
  case (getLast (lyEncX ly), getLast (lyEncY ly)) of
    (Just xr, Just yr) ->
      case (resolveNum r xr, resolveNum r yr) of
        (Just xv, Just yv) ->
          let pts = [ (x, y) | (x, y) <- zip (V.toList xv) (V.toList yv)
                             , not (isNaN x), not (isNaN y) ]
              bins = case getLast (lyBinCount ly) of Just b | b > 0 -> b; _ -> 30
          in if null pts then []
             else let xs = map fst pts; ys = map snd pts
                  in hexbinCells bins (minimum xs, maximum xs) (minimum ys, maximum ys) pts
        _ -> []
    _ -> []

-- | TODO-3a (2026-05-29): histogram の y 軸を密度 (= count / (total * binW))
-- に正規化。 PS Spec.histogramDensity と同等。 SVG export でも動くように HS
-- 側にも実装 (= 旧来 HS は count のみで density mode が機能しなかった)。
histogramDensity :: Bool -> Layer
histogramDensity b = mempty { lyHistDensity = Last (Just b) }

-- | Phase 8 B7: histogram / bar の bin 境界線 (= 各バーの白枠) を表示するか。
-- デフォルトは False (= ggplot 流フラットバー、 枠なし)。 True で bin 区切りが見える。
histBorder :: Bool -> Layer
histBorder b = mempty { lyHistBorder = Last (Just b) }

-- | Phase 28: density 曲線の下を塗りつぶす (= ggplot @geom_density(aes(fill = …))@)。
--   群別 ('color') と 'alpha' を併用すると、 各群を群色 × alpha で塗る (R4DS Ch1 §1.5)。
--   既定 (未指定/False) は ggplot 同様 fill=NA = 線のみ。
densityFill :: Bool -> Layer
densityFill b = mempty { lyDensityFill = Last (Just b) }

-- | Phase 34: マーカーを中抜き (= ggplot @shape="circle open"@ / @geom_point(fill = NA)@)。
--   塗りを透明にし、 点色で輪郭 (stroke) のみ描く。 'size' で輪郭円の直径、
--   'stroke' で線幅 (既定 1pt)。 重畳して「点を輪で囲む」 強調に使う (R4DS Ch9 §9.6)。
hollow :: Layer
hollow = mempty { lyHollow = Last (Just True) }

-- | Phase 36 D1: 分布 mark (box/violin/strip/swarm) の slot 内横 offset。 値は **slot 幅比**
--   (= ggplot @position_nudge@)。 正で右、 負で左。 raincloud の「box を中央・strip を左・雲を右」
--   のような重畳配置を組むのに使う (= 旧 raincloud のハードコード offset を置換)。
nudge :: Double -> Layer
nudge x = mempty { lyNudge = Last (Just x) }

-- | Phase 36 D1: 分布 mark の幅 (= **slot 幅比・占有率**)。 各 mark の既定占有率
--   (box 0.5 / violin 0.7 / strip 0.4 / swarm 0.8) を上書きする。 raincloud では box を細く
--   (= 0.1 等) するのに使う。
markWidth :: Double -> Layer
markWidth w = mempty { lyMarkWidth = Last (Just w) }

-- | Phase 36 D1: violin の片側化 (= 半 violin)。 @violin "v" <> side SideRight@ で右半分のみ。
--   raincloud の「雲」 (= 片側 violin) に使う。 box/strip 等には影響しない。
side :: Side -> Layer
side s = mempty { lySide = Last (Just s) }

-- | Phase 9 B: bar の position adjustment (= ggplot `position`)。
--   群分け (= color/group aesthetic) があるとき 'PosDodge' / 'PosStack' / 'PosFill' で
--   並べ方を選ぶ。 既定 ('PosIdentity') は従来通り単色棒 (color を見ない)。
--
--   > bar "cat" "y" <> colorBy "grp" <> position PosDodge
position :: Position -> Layer
position p = mempty { lyPosition = Last (Just p) }

-- | Phase 30 A3: 固定 shape (= layer 全体に適用・ggplot @shape=@)。 bare=固定。
--   'shapeBy' (列で map) より優先される ('pointShapeAt' 参照)。
shape :: MarkShape -> Layer
shape s = mempty { lyShape = Last (Just s) }

-- | C-6: shape categorical encoding 列。
shapeBy :: ColRef -> Layer
shapeBy c = mempty { lyShapeBy = Last (Just c) }

-- | C-6: cat 名 → MarkShape 1 件追加 (= 複数 entry は <> で合成)。
shapeMapEntry :: Text -> MarkShape -> Layer
shapeMapEntry v s = mempty { lyShapeMap = [ ShapeMapEntry { smeValue = v, smeShape = s } ] }

-- | C-6: size continuous encoding 列。
sizeBy :: ColRef -> Layer
sizeBy c = mempty { lySizeBy = Last (Just c) }

-- | Phase 30 A8: alpha (= 不透明度) を連続値の列で encode する (= ggplot @scale_alpha@・
--   @aes(alpha = col)@)。 列値 min..max を alpha @[0.1, 1.0]@ に線形 map (ggplot 既定 range)。
--   固定 alpha は bare 'alpha' (案2 = bare 固定 / `*By` = map)。
--
-- > scatter "x" "y" <> alphaBy "weight"
alphaBy :: ColRef -> Layer
alphaBy c = mempty { lyAlphaBy = Last (Just c) }

-- | Phase 11 A4-b: 固定 linetype (= ggplot linetype="dashed")。 line 系 mark に適用。
--   例: @line "x" "y" <> linetype LtDashed@
linetype :: LineType -> Layer
linetype lt = mempty { lyLinetype = Last (Just lt) }

-- | Phase 11 A4-b: categorical linetype encoding 列 (= ggplot linetype=factor(g))。
--   line を群ごとに分割し各群へ巡回 LineType ('lineTypeForIndex') を割当。
--   例: @line "x" "y" <> linetypeBy (ColByName "grp")@
linetypeBy :: ColRef -> Layer
linetypeBy c = mempty { lyLinetypeBy = Last (Just c) }

-- | C-step trellis 色一貫性: 全データ cat 出現順を Layer に注入。
colorCats :: [Text] -> Layer
colorCats cs = mempty { lyColorCats = cs }

-- | Phase 28: categorical 水準の既定順 (= ggplot2 の factor 既定 = アルファベット順)。
--   色 / x 軸 / shape の distinct を取るときに使い、 R4DS と凡例・色・並びを一致させる。
--   明示順が要るとき (fct_infreq 等) は 'colorCats' / 'xCatOrder' で上書きする。
orderedCats :: [Text] -> [Text]
orderedCats = Data.List.sort . Data.List.nub

-- | Phase 26 §C-2 #8: 列の平均値を水平線として描画 (= PlotConfig.showMean)。
statMean :: ColRef -> Layer
statMean c = mempty
  { lyKind = First (Just MStatMean), lyEncY = Last (Just c) }

-- | Phase 26 §C-2 #8: 列の中央値を水平線として描画 (= PlotConfig.showMedian)。
statMedian :: ColRef -> Layer
statMedian c = mempty
  { lyKind = First (Just MStatMedian), lyEncY = Last (Just c) }

-- | Phase 26 §C-2 #13: parallel coordinates plot。 各 col が縦軸となり、
-- 各 row を全軸 cross する折線で表現。 hover で row 強調 (= 後追い)。
parallelCoords :: [ColRef] -> Layer
parallelCoords cols = mempty
  { lyKind = First (Just MParallel), lyHover = cols }

-- | Phase 26 §E-6: HBM ModelGraph DAG を描画する layer。
-- 内部 builder で使う直接 constructor。 ユーザは 'Hgg.Plot.DAG.dagPlot'
-- (= Graph a + ~> 経由) を使う方が良い。
dagFromLists :: [DAGNode] -> [DAGEdge] -> DAGLayoutAlgorithm -> Layer
dagFromLists nodes edges algo = mempty
  { lyKind = First (Just MDAG)
  , lyDAG  = Last (Just (DAGSpec nodes edges algo [])) }

-- | dsPlates も指定する版。
dagFromListsWithPlates
  :: [DAGNode] -> [DAGEdge] -> DAGLayoutAlgorithm -> [DAGPlate] -> Layer
dagFromListsWithPlates nodes edges algo plates = mempty
  { lyKind = First (Just MDAG)
  , lyDAG  = Last (Just (DAGSpec nodes edges algo plates)) }

-- | DAGNode constructor (= kind + 分布名なし)。
dagNode :: Text -> Text -> DAGNodeKind -> Double -> Double -> DAGNode
dagNode i l k x y = DAGNode i l k Nothing x y

-- | 分布名付き DAGNode constructor (= PyMC 風 "name ~ dist" 表示用)。
dagNodeDist :: Text -> Text -> DAGNodeKind -> Text -> Double -> Double -> DAGNode
dagNodeDist i l k dist x y = DAGNode i l k (Just dist) x y

-- | DAGEdge constructor。
dagEdge :: Text -> Text -> DAGEdge
dagEdge f t = DAGEdge f t Nothing Nothing

-- | 互換用 shortcut: 既存 demo / test 用 (= NodeLatent + LayoutManual)。
-- 新規 API は Hgg.Plot.DAG.dagPlot を使う。
dag :: [DAGNode] -> [DAGEdge] -> Layer
dag nodes edges = dagFromLists nodes edges LayoutManual

-- | Phase 26 §E-1: MCMC trace plot (single chain)。 iteration vs parameter
-- 値の line。 mark kind は MTrace (= alias for MLine、 frontend で区別可能)。
trace :: ColRef -> ColRef -> Layer
trace iterCol valCol = mempty
  { lyKind = First (Just MTrace)
  , lyEncX = Last (Just iterCol)
  , lyEncY = Last (Just valCol)
  }

-- | Phase 26 §E-1: multi-chain trace。 chain 列で色分け、 connect group も
-- chain 列 (= chain 内で連結、 chain 跨ぎ無し)。 PlotConfig.StreamingTracePlot 等価。
traceLines :: ColRef -> ColRef -> ColRef -> Layer
traceLines iterCol valCol chainCol =
  trace iterCol valCol
    <> colorBy chainCol
    <> connectGroup chainCol
    <> stroke 1.0

-- ===========================================================================
-- Layer-local attribute (= 直前の Layer に <>)
-- ===========================================================================

-- | 列で色分け encoding (= categorical / continuous は ColRef 種別による)。
--   Phase 30 案2: map 系は @*By@ 接尾辞 ('color' は固定色に明け渡し)。
colorBy :: ColRef -> Layer
colorBy c = mempty { lyColor = Last (Just (ColorByCol c)) }

-- | Phase 36 B1b: distribution mark の「群分け列」。 明示の 'lyEncX' があればそれを
--   群列とし、 無ければ 'colorBy' (= 'ColorByCol') の列を群列とみなす。 これにより
--   @boxplot "v" <> colorBy "g"@ が scatter と同様に群分割される (従来は encX 専用で
--   colorBy 単体だと単一群になっていた)。 distribution renderer と
--   'collectCategoricalLabels' (distribution 限定) が共有する。
distGroupRef :: Layer -> Maybe ColRef
distGroupRef ly = case getLast (lyEncX ly) of
  Just cr -> Just cr
  Nothing -> case getLast (lyColor ly) of
    Just (ColorByCol cr) -> Just cr
    _                    -> Nothing

-- | Phase 36 B2: distribution mark の dodge 検出。 @groupBy@ (= 'lyEncX' = 位置列) と
--   @colorBy@ (= 'lyColor' の 'ColorByCol' = 色列) が **両方** 指定され、 かつ別列の
--   とき @Just (位置列, 色列)@。 このとき各位置カテゴリ内で色サブグループを横並び
--   (= ggplot @position_dodge@) する。 同一列 (groupBy と colorBy が同じ) のときは
--   dodge せず単一群彩色のまま (= 'distGroupRef' 経路) なので 'Nothing'。
distDodgeRef :: Layer -> Maybe (ColRef, ColRef)
distDodgeRef ly = case (getLast (lyEncX ly), getLast (lyColor ly)) of
  (Just posC, Just (ColorByCol colC))
    -- ★ 同一列 (groupBy と colorBy が同じ列) は dodge せず単一群彩色のまま。 判定は
    --   ColRef の構造比較 (inline 列は 'colRefName' が両方 "<inline-*>" に潰れるため
    --   名前比較では別列を取り違える)。
    | posC /= colC -> Just (posC, colC)
  _ -> Nothing

-- | 静的色 (layer 全体に適用)。 Phase 30 案2: 固定色 aesthetic は bare 名 'color'。
--   'Color' 型 (RGB / 'fromHex' / R 657 名前付き定数) を受け、 ワイヤは 'toCss' で Text 化。
color :: Color -> Layer
color c = mempty { lyColor = Last (Just (ColorStatic (toCss c))) }

-- | 便利関数: 8 桁 RGBA hex (@"#rrggbbaa"@ / 4 桁 @"#rgba"@) を 1 つで受け、
--   @color (fromHex …) <> alpha …@ に展開する ('fromHexA' 経由)。 design ツール /
--   Web 由来の RGBA hex をそのまま貼れる。 ★@Color@ は RGB のみゆえ alpha は別 channel
--   に分離される (後続の @<> alphaBy "col"@ 等は 'Last' で後勝ち)。 不正入力は 'error'
--   (total 版は 'colorRGBAMaybe')。 6/3 桁 (alpha 無し) は不透明として扱う。
colorRGBA :: Text -> Layer
colorRGBA t = let (c, a) = fromHexA t in color c <> alpha a

-- | 'colorRGBA' の total 版。 不正な hex は 'Nothing'。
colorRGBAMaybe :: Text -> Maybe Layer
colorRGBAMaybe t = (\(c, a) -> color c <> alpha a) <$> fromHexAMaybe t

-- | Phase 26 §C-2 #9: 連続値 column を Viridis 風 gradient で色分け。
--   Phase 30 案2: map 系ゆえ @*By@ 接尾辞。
colorContinuousBy :: ColRef -> Layer
colorContinuousBy c = mempty { lyColor = Last (Just (ColorByContinuous c)) }

-- | 透過度 (0..1)。 これは無次元なので 'Double' のまま。
alpha :: Double -> Layer
alpha  a = mempty { lyAlpha  = Last (Just a) }

-- | マーカー径 ('size') / 線幅 ('stroke') を 'Length' で指定 (Phase 34 A4)。
-- bare 数値リテラルは @Num Length@ 経由で **pt** (@size 6@ = 6pt 直径)。 別単位は
-- @size (2 *~ mm)@。 内部は pt の 'Double' に解決して保持する (px は描画 dpi が
-- 確定する前なので、 例外的に 96dpi で pt 化する = マーカーに px 指定は非推奨)。
size, stroke :: Length -> Layer
size   s = mempty { lySize   = Last (Just (lengthToPt 96 s)) }
stroke s = mempty { lyStroke = Last (Just (lengthToPt 96 s)) }

-- | Phase 28: 散布点に縁 (edge) を付ける。 既定は縁なし (= ggplot の塗り点 shape 19)。
--   'edgeOn' は点と同色の 1px 縁、 'edge col' は色を指定、 'edgeWidth w' は幅を指定
--   (いずれも縁を有効化)。 縁の透過は色に alpha 付き hex (例 @edge "#00000044"@) で表せる。
edgeOn :: Layer
edgeOn = mempty { lyEdge = Last (Just True) }

edge :: Text -> Layer
edge c = mempty { lyEdge = Last (Just True), lyEdgeColor = Last (Just c) }

edgeWidth :: Double -> Layer
edgeWidth w = mempty { lyEdge = Last (Just True), lyEdgeWidth = Last (Just w) }

-- | hover tooltip に表示する追加列 (= multi-col)。
--
-- > scatter "x" "y" <> hoverCols ["group", "label"]
hoverCols :: [ColRef] -> Layer
hoverCols cs = mempty { lyHover = cs }

-- | Phase 26 §C-2 #6: 各点の X 方向 ± 半幅 (error bar)。
errorX :: ColRef -> Layer
errorX c = mempty { lyErrorX = Last (Just c) }

-- | Phase 26 §C-2 #6: 各点の Y 方向 ± 半幅 (error bar)。
errorY :: ColRef -> Layer
errorY c = mempty { lyErrorY = Last (Just c) }

-- | Phase 26 §C-2 #5: scatter 点を線で結ぶ ON。
--
-- > scatter "x" "y" <> connect
-- > scatter "x" "y" <> connect <> connectOrder "time" <> connectGroup "id"
connect :: Layer
connect = mempty { lyConnect = Last (Just defaultConnectSpec) }

connectOrder :: ColRef -> Layer
connectOrder c = mempty
  { lyConnect = Last (Just (defaultConnectSpec { csOrder = Last (Just c) })) }

connectGroup :: ColRef -> Layer
connectGroup c = mempty
  { lyConnect = Last (Just (defaultConnectSpec { csGroup = Last (Just c) })) }

connectColor :: Text -> Layer
connectColor c = mempty
  { lyConnect = Last (Just (defaultConnectSpec { csColor = Last (Just c) })) }

connectWidth :: Double -> Layer
connectWidth w = mempty
  { lyConnect = Last (Just (defaultConnectSpec { csWidth = Last (Just w) })) }

-- ===========================================================================
-- AxisSpec ─ 軸 1 本の設定 (Phase 26 §C-2 #1 LogScale + #2 軸 format)
-- ===========================================================================

-- | 軸 scale 種別。 線形 / 対数 / sqrt / time / ordinal / band を将来追加。
-- 現状は AxisLinear / AxisLog / AxisSqrt (P15) / AxisTime (P7)。
data AxisKind = AxisLinear | AxisLog | AxisSqrt | AxisTime
  deriving (Show, Eq, Generic)

instance ToJSON   AxisKind
instance FromJSON AxisKind

-- | 軸ラベルの数値表記。
data AxisFormat
  = AxisIntegerFmt
  | AxisDecimalFmt !Int      -- 小数桁数
  | AxisExponentFmt !Int     -- 指数表記 N 桁
  | AxisTimeFmt !Text        -- ★ P7 timestamp ms → date 文字列 (= "yyyy-MM-dd" 等)
  deriving (Show, Eq, Generic)

instance ToJSON   AxisFormat
instance FromJSON AxisFormat

-- | 軸 1 本の設定。 全 field を Maybe で Monoid 化、 後勝ち合成。
data AxisBreak = AxisBreak { abFrom :: !Double, abTo :: !Double }
  deriving (Show, Eq, Generic)

instance ToJSON   AxisBreak
instance FromJSON AxisBreak

data AxisSpec = AxisSpec
  { axKind   :: !(Last AxisKind)
  , axFormat :: !(Last AxisFormat)
  , axMin    :: !(Last Double)
  , axMax    :: !(Last Double)
  , axRotate :: !(Last Double)    -- ★ P10 軸 label 回転 (度)
  , axBreaks :: ![AxisBreak]       -- ★ P16 軸不連続範囲
  , axShowTicks :: !(Last Bool)   -- ★ tick 表示 (= default true、 pairs/facet 内側 false)
  , axShowGrid  :: !(Last Bool)   -- ★ C-5 grid line 表示 (= default false)
    -- ★ Phase 11 A4-d: 明示 tick 位置 (= ggplot scale_*_continuous(breaks=))。 非空なら
    --   自動 extendedBreaks を上書き。 numeric 軸のみ有効 (categorical は無視)。
  , axTickVals :: ![Double]
    -- ★ Phase 11 A4-d: 明示 tick ラベル (= ggplot labels=)。 axTickVals と 1:1 対応
    --   (短ければ "" 埋め)。 空なら値を format して使う。
  , axTickLabels :: ![Text]
  } deriving (Show, Eq, Generic)

instance ToJSON   AxisSpec
instance FromJSON AxisSpec

-- ★ Phase 43 A3: レコードフィールド形式 (位置依存撲滅・挙動不変)。axTickVals/axTickLabels
--   のみ「右が非空なら右」特殊合成 (list `<>` = 連結と別) を名前付きで温存。
instance Semigroup AxisSpec where
  a <> b = AxisSpec
    { axKind     = axKind a   <> axKind b
    , axFormat   = axFormat a <> axFormat b
    , axMin      = axMin a    <> axMin b
    , axMax      = axMax a    <> axMax b
    , axRotate   = axRotate a <> axRotate b
    , axBreaks   = axBreaks a <> axBreaks b
    , axShowTicks = axShowTicks a <> axShowTicks b
    , axShowGrid  = axShowGrid a  <> axShowGrid b
      -- ★ A4-d: 明示 break/label は「後勝ち」 (= 後から与えた breaks=/labels= が前の指定を
      --   完全に置換)。 空配列なら前の指定を温存。
    , axTickVals   = if null (axTickVals b)   then axTickVals a   else axTickVals b
    , axTickLabels = if null (axTickLabels b) then axTickLabels a else axTickLabels b
    }

instance Monoid AxisSpec where
  mempty = AxisSpec mempty mempty mempty mempty mempty [] mempty mempty [] []

-- | Last AxisSpec から AxisKind を取り出す (= default は AxisLinear)。
axisKindOf :: Last AxisSpec -> AxisKind
axisKindOf (Last Nothing)   = AxisLinear
axisKindOf (Last (Just as)) = case getLast (axKind as) of
  Just k  -> k
  Nothing -> AxisLinear

-- | Last AxisSpec から AxisFormat を取り出す (= default は AxisDecimalFmt 1
-- 相当の auto)。
axisFormatOf :: Last AxisSpec -> Maybe AxisFormat
axisFormatOf (Last Nothing)   = Nothing
axisFormatOf (Last (Just as)) = getLast (axFormat as)

-- | 'xAxis (linearAxis)' / 'xAxis (logAxis)' のような書き方の起点。
linearAxis, logAxis, sqrtAxis :: AxisSpec
linearAxis = mempty { axKind = Last (Just AxisLinear) }
logAxis    = mempty { axKind = Last (Just AxisLog) }
sqrtAxis   = mempty { axKind = Last (Just AxisSqrt) }

-- | P7: time axis (= 値 Unix timestamp ms、 pattern で date 文字列に format)。
timeAxis :: Text -> AxisSpec
timeAxis pat = mempty
  { axKind = Last (Just AxisTime)
  , axFormat = Last (Just (AxisTimeFmt pat)) }

-- | P16: 軸不連続範囲 1 つを追加。
axisBreak :: Double -> Double -> AxisSpec
axisBreak from to = mempty { axBreaks = [AxisBreak { abFrom = from, abTo = to }] }

-- | A4-d: 明示 tick 位置 (= ggplot scale_*_continuous(breaks=))。 自動 tick を
-- これで上書き (numeric 軸のみ。 categorical 軸では無視)。 範囲外の値は描画時に
-- censor される。 例: @xAxis (axisBreaksAt [0,25,50,75,100])@。
axisBreaksAt :: [Double] -> AxisSpec
axisBreaksAt vs = mempty { axTickVals = vs }

-- | A4-d: 明示 tick ラベル (= ggplot labels=)。 'axisBreaksAt' と組で使い、 i 番目の
-- break に i 番目のラベルを割り当てる (長さは breaks に揃える)。 単体指定でも
-- 自動 break の順に割り当たるが、 通常は 'axisBreaksLabeled' を推奨。
axisTickLabels :: [Text] -> AxisSpec
axisTickLabels ls = mempty { axTickLabels = ls }

-- | A4-d: break 位置とラベルを対で指定する便利関数 (= ggplot breaks=/labels= を一度に)。
-- 例: @xAxis (axisBreaksLabeled [(0,\"low\"),(50,\"mid\"),(100,\"high\")])@。
axisBreaksLabeled :: [(Double, Text)] -> AxisSpec
axisBreaksLabeled prs = mempty { axTickVals = map fst prs, axTickLabels = map snd prs }

-- | Last AxisSpec から明示 tick 位置を取り出す (未指定 = [])。
axTickValsOf :: Last AxisSpec -> [Double]
axTickValsOf (Last Nothing)   = []
axTickValsOf (Last (Just as)) = axTickVals as

-- | Last AxisSpec から明示 tick ラベルを取り出す (未指定 = [])。
axTickLabelsOf :: Last AxisSpec -> [Text]
axTickLabelsOf (Last Nothing)   = []
axTickLabelsOf (Last (Just as)) = axTickLabels as

-- | tick を隠す (= pairs / facet の内側 panel 用)。
hideTicks :: AxisSpec
hideTicks = mempty { axShowTicks = Last (Just False) }

-- | 'xAxis (axisFormat (AxisDecimalFmt 2))' で軸 format を指定。
axisFormat :: AxisFormat -> AxisSpec
axisFormat f = mempty { axFormat = Last (Just f) }

-- | P10: 軸 label 回転 (度)。 30 / 45 / 90 等。
axisRotate :: Double -> AxisSpec
axisRotate deg = mempty { axRotate = Last (Just deg) }

-- | frontend-settings v0.1 §1.5: 軸 min 値。
axisMin :: Double -> AxisSpec
axisMin v = mempty { axMin = Last (Just v) }

-- | frontend-settings v0.1 §1.5: 軸 max 値。
axisMax :: Double -> AxisSpec
axisMax v = mempty { axMax = Last (Just v) }

-- | frontend-settings v0.1 §1.5: 軸 min + max 同時指定。
axisRange :: Double -> Double -> AxisSpec
axisRange lo hi = mempty { axMin = Last (Just lo), axMax = Last (Just hi) }

-- | Last AxisSpec から軸 rotation を取り出す (= default 0)。
axisRotateOf :: Last AxisSpec -> Double
axisRotateOf (Last Nothing)   = 0
axisRotateOf (Last (Just as)) = case getLast (axRotate as) of
  Just d  -> d
  Nothing -> 0

-- | Phase 9 A-3: 軸目盛りラベルの回転角を解決。 per-axis 'axisRotate' (明示指定) を
-- 最優先、 無ければ theme override の axis.text angle、 どちらも無ければ 0。
resolveAxisAngle :: Last AxisSpec -> Last Double -> Double
resolveAxisAngle (Last (Just as)) themeAngle
  | Just d <- getLast (axRotate as) = d
  | otherwise                       = themeAngleOr0 themeAngle
resolveAxisAngle (Last Nothing) themeAngle = themeAngleOr0 themeAngle

themeAngleOr0 :: Last Double -> Double
themeAngleOr0 a = case getLast a of
  Just d  -> d
  Nothing -> 0

-- | Last AxisSpec から axShowTicks を取り出す (= default True、 Nothing も True 扱い)。
axisShowTicksOf :: Last AxisSpec -> Bool
axisShowTicksOf (Last Nothing)   = True
axisShowTicksOf (Last (Just as)) = case getLast (axShowTicks as) of
  Just b  -> b
  Nothing -> True

-- ===========================================================================
-- ReferenceLine (= Phase 26 §C-2 #3: 既存 PlotConfig.referenceLine 等価)
-- ===========================================================================

-- | plot area 内に重ねる参照線。
--   * 'RefIdentity'    ─ y = x の対角線 (= Actual vs Predicted)
--   * 'RefHorizontalAt c' ─ y = c
--   * 'RefVerticalAt c'   ─ x = c
--   * 'RefLinear slope intercept' ─ y = slope * x + intercept
data ReferenceLine
  = RefIdentity
  | RefHorizontalAt !Double
  | RefVerticalAt   !Double
  | RefLinear { rlSlope, rlIntercept :: !Double }
  deriving (Show, Eq, Generic)

instance ToJSON   ReferenceLine
instance FromJSON ReferenceLine

-- ===========================================================================
-- Annotation (= P6、 2026-05-25 任意 overlay)
-- ===========================================================================

-- ★ Phase 33 B6: 注釈の座標は 'Pos' (native/npc/絶対長を軸ごとに混在指定可)。
-- 旧 'AnnotCoord' (Data/Frac) は Pos に統一して撤去 (Frac は HS 描画で未実装だった)。
-- AnnRect は w/h でなく 2 隅 (x1,y1)-(x2,y2) の Pos で表す (座標一貫)。
data Annotation
  = AnnText
      { anX :: !Pos, anY :: !Pos
      , anText :: !Text
      , anColor :: !Text, anSize :: !Double }
  | AnnArrow
      { anX1 :: !Pos, anY1 :: !Pos
      , anX2 :: !Pos, anY2 :: !Pos
      , anColor :: !Text, anWidth :: !Double }
  | AnnRect
      { anX1 :: !Pos, anY1 :: !Pos
      , anX2 :: !Pos, anY2 :: !Pos
      , anFill :: !Text, anStroke :: !Text
      , anStrokeWidth :: !Double, anFillOpacity :: !Double }
  | AnnLine
      { anX1 :: !Pos, anY1 :: !Pos
      , anX2 :: !Pos, anY2 :: !Pos
      , anColor :: !Text, anWidth :: !Double }
  deriving (Show, Eq, Generic)

instance ToJSON   Annotation
instance FromJSON Annotation

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
-- MarginalSpec (= Phase 26 §C-2 #10 周辺 histogram)
-- ===========================================================================

-- | P9: marginal の種別 (hist / density / 重ね)。
data MarginalKind = MarginalHist | MarginalDensity | MarginalBoth
  deriving (Show, Eq, Generic)

instance ToJSON   MarginalKind
instance FromJSON MarginalKind

-- | scatter の周辺に X/Y histogram を sub-plot として配置するか。
data MarginalSpec = MarginalSpec
  { msShowX :: !Bool
  , msShowY :: !Bool
  , msBins  :: !Int   -- bin 数 (= default 20)
  , msKind  :: !MarginalKind  -- ★ P9 hist / density / 重ね
  } deriving (Show, Eq, Generic)

instance ToJSON   MarginalSpec
instance FromJSON MarginalSpec

-- ★ Phase 43 A3: レコードフィールド形式 (位置依存撲滅・挙動不変)。全 field が非 Monoid
--   (Bool/Int/enum) なので合成は名前付きで明示: show は OR・bins は max・kind は後勝ち。
instance Semigroup MarginalSpec where
  a <> b = MarginalSpec
    { msShowX = msShowX a || msShowX b
    , msShowY = msShowY a || msShowY b
    , msBins  = max (msBins a) (msBins b)
    , msKind  = msKind b   -- 後勝ち
    }

instance Monoid MarginalSpec where
  mempty = defaultMarginalSpec

defaultMarginalSpec :: MarginalSpec
defaultMarginalSpec = MarginalSpec False False 20 MarginalHist

-- ===========================================================================
-- LegendSpec (= P8、 2026-05-25 凡例設定)
-- ===========================================================================

data LegendPosition
  = LegendRight | LegendBottom | LegendNone
  | LegendInsideTopRight | LegendInsideTopLeft
  | LegendInsideBottomRight | LegendInsideBottomLeft
  -- ★ Phase 32 (re-apply): 外・右に置きつつ panel 高の縦中央に揃える (ggplot 既定の
  --   legend.position="right" は縦中央寄せ)。 LegendRight=上揃えは不変・これは opt-in。
  | LegendRightCenter
  deriving (Show, Eq, Generic)

instance ToJSON   LegendPosition
instance FromJSON LegendPosition

data LegendSpec = LegendSpec
  { lgPosition :: !LegendPosition
  , lgTitle    :: !(Last Text)
  } deriving (Show, Eq, Generic)

instance ToJSON   LegendSpec
instance FromJSON LegendSpec

-- ★ Phase 43 A3: レコードフィールド形式 (位置依存撲滅・挙動不変)。lgPosition は非 Monoid
--   enum なので後勝ち、 lgTitle は素直な `Last` 合成。
instance Semigroup LegendSpec where
  a <> b = LegendSpec
    { lgPosition = lgPosition b           -- 後勝ち (enum)
    , lgTitle    = lgTitle a <> lgTitle b
    }

instance Monoid LegendSpec where
  mempty = defaultLegendSpec

defaultLegendSpec :: LegendSpec
defaultLegendSpec = LegendSpec LegendRightCenter mempty  -- Phase 43: ggplot 既定 (右・縦中央)

-- ===========================================================================
-- FontSpec (= hgg-frontend-settings-spec v0.1 §1.3)
-- ===========================================================================

data FontSpec = FontSpec
  { fsFamily :: !(Last Text)
  , fsSize   :: !(Last Double)
  , fsWeight :: !(Last Text)
  , fsItalic :: !(Last Bool)   -- ★ TODO-10 (2026-05-29): PS parity
  , fsColor  :: !(Last Text)
  } deriving stock (Show, Eq, Generic)
    -- ★ Phase 43 A3: 全 field が `Last` の素直な per-field 合成なので generic 導出
    --   (= 手書き instance ゼロ・field 追加に強い)。挙動は旧手書きと完全同型。
    deriving (Semigroup, Monoid) via Generically FontSpec

instance ToJSON   FontSpec
instance FromJSON FontSpec

-- | 空 'FontSpec' (= 'mempty' alias、 generic 導出の mempty と同値)。
emptyFontSpec :: FontSpec
emptyFontSpec = mempty

fontSize :: Double -> FontSpec
fontSize n = emptyFontSpec { fsSize = Last (Just n) }

fontFamily :: Text -> FontSpec
fontFamily f = emptyFontSpec { fsFamily = Last (Just f) }

fontWeight :: Text -> FontSpec
fontWeight w = emptyFontSpec { fsWeight = Last (Just w) }

fontItalic :: Bool -> FontSpec
fontItalic b = emptyFontSpec { fsItalic = Last (Just b) }

fontColor :: Text -> FontSpec
fontColor c = emptyFontSpec { fsColor = Last (Just c) }

-- ===========================================================================
-- VisualSpec (= 外側 Monoid)
-- ===========================================================================

-- | 描画 theme (= 名前で参照、 関数を持たない = JSON serializable)。
-- Phase 9 A-1: ggplot 標準 preset (ThemeGrey) + HggCanvas ブランドテーマを追加。
--   * ThemeGrey            = ggplot 既定 theme_grey (灰背景 #EBEBEB・白 grid・枠なし・軸線なし)
--   * ThemeNoir     = ブランド (暗・上品・寒色アクセント、 コンペ用に残置)
--   * ThemeLumen    = ブランド (白基調・深い差し色・清潔、 コンペ用に残置)
--   * ThemeCanvas     = HggCanvas 正式テーマ (羊皮紙基調・明)。 配色は canvas 仕様書
--       hgg_canvas_dataVizPalettes.html v1.0 由来 (cream/gold/ink + Universal Categorical series)。
--   * ThemeCanvasDark = 同テーマの暗版 (焦茶インク背景・series は shade 300 で沈み防止)。
data ThemeName = ThemeDefault | ThemeMinimal | ThemeDark | ThemeLight
               | ThemeGrey | ThemeBW | ThemeClassic | ThemeVoid | ThemeLinedraw
               | ThemeNoir | ThemeLumen
               | ThemeCanvas | ThemeCanvasDark
  deriving (Show, Eq, Generic)

instance ToJSON   ThemeName
instance FromJSON ThemeName

-- | preset ごとの既定 series palette (= palette 未指定時に使う色順)。
-- ggplot 系 preset は従来通り hggMain (Hgg 配色)、 ブランド 3 種は専用 series。
-- Layout.computeLayout の catPal 既定がこれを参照する (= palette 指定で上書き可)。
themeSeriesPalette :: ThemeName -> [Text]
themeSeriesPalette t = case t of
  ThemeNoir  -> ["#7AA2F7", "#BB9AF7", "#7DCFFF", "#9ECE6A", "#E0AF68", "#F7768E"]
  ThemeLumen -> ["#4C5BD4", "#D6336C", "#2F9E44", "#E8590C", "#7048E8", "#1098AD"]
  -- HggCanvas 明: ユーザ案3 (#1 = White Rabbit Inner Ear Pink #F0A5A0、
  --   #3 = Dormouse 系 Warm Yellow #E8D58A、 他は Hgg 配色)。 2026-06-02 確定。
  ThemeCanvas     -> canvasPal
  -- 暗版 (Charcoal 背景): 案3 の暗色 (purple/teal/rose/wine) を明度調整し沈み防止。色相・順序は維持。
  ThemeCanvasDark -> [ "#F0A5A0", "#A98BD0", "#E8D58A", "#5FA0A8"
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
    -- ★ axis.text の回転角 (度)。 per-axis 'axisRotate' 未指定時の fallback。 x/y 両軸に適用。
  , toAxisTextAngle :: !(Last Double)
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
-- |   フィールド名を渡す明示形 (hgg は spec を値として組む方針ゆえ)。
repeatFields :: [Text] -> (Text -> VisualSpec) -> VisualSpec
repeatFields fields mk = subplots (map mk fields)

-- ===========================================================================
-- concat 合成 (Vega-Lite hconcat/vconcat 相当・patchwork 風演算子)
-- ===========================================================================
-- subplots + subplotCols の純粋な薄ラッパ。 レンダリングは既存 subplots 経路を
-- そのまま使う (render/parity への影響ゼロ)。
--
-- 演算子 '<->' (横) / '<:>' (縦) は同方向チェーンを **平坦化** する:
--   (a <-> b <-> c) は subplots [a,b,c] (3 等分列) になり、 二項ネスト
--   (= 2 列で左セルが a,b に分割) にはならない。 平坦化は「左辺が cols==列数の
--   水平グループなら末尾追加、 そうでなければ新規 2 要素」 で行う。 leaf プロットは
--   vsSubplots=[] ゆえ必ず新規開始 = 通常チャートを誤って取り込まない。
-- 例: @(a <-> b <-> c) <:> d@ = 1 行目 3 列 + 2 行目を全幅 (1 行目セルの 3 倍幅)。
--     これは @vconcat [hconcat [a,b,c], d]@ と同値。
--
-- ★演算子の選定: '<->'(横)・'<:>'(縦) は Prelude/標準ライブラリと衝突しない
--   (旧案 '<|>' は Control.Applicative の Alternative と衝突したため回避した)。

-- | 横並び (= Vega-Lite hconcat): n 要素を 1 行 n 列に。
hconcat :: [VisualSpec] -> VisualSpec
hconcat ss = subplots ss <> subplotCols (length ss)

-- | 縦並び (= Vega-Lite vconcat): n 要素を n 行 1 列に。
vconcat :: [VisualSpec] -> VisualSpec
vconcat ss = subplots ss <> subplotCols 1

infixl 6 <->
infixl 5 <:>

-- | 横結合演算子 (= hconcat の二項・同方向チェーンを平坦化)。
(<->) :: VisualSpec -> VisualSpec -> VisualSpec
a <-> b = case asHGroup a of
  Just xs -> hconcat (xs ++ [b])
  Nothing -> hconcat [a, b]

-- | 縦結合演算子 (= vconcat の二項・同方向チェーンを平坦化)。
(<:>) :: VisualSpec -> VisualSpec -> VisualSpec
a <:> b = case asVGroup a of
  Just xs -> vconcat (xs ++ [b])
  Nothing -> vconcat [a, b]

-- | spec が「純粋な水平グループ (subplots=xs (>1 要素)・cols==要素数)」 なら xs。
asHGroup :: VisualSpec -> Maybe [VisualSpec]
asHGroup s = case getLast (vsSubplotCols s) of
  Just c | let xs = vsSubplots s, length xs > 1, c == length xs -> Just (vsSubplots s)
  _ -> Nothing

-- | spec が「純粋な垂直グループ (subplots=xs (>1 要素)・cols==1)」 なら xs。
asVGroup :: VisualSpec -> Maybe [VisualSpec]
asVGroup s = case getLast (vsSubplotCols s) of
  Just 1 | length (vsSubplots s) > 1 -> Just (vsSubplots s)
  _ -> Nothing

-- | P18: pairs plot (= N 列の posterior 等を N×N grid で対角は density、
-- |   非対角は scatter)。
pairs :: [ColRef] -> VisualSpec
pairs cols =
  let n = length cols
      -- Phase 7 A6: 内側パネルの軸目盛りを抑制 (= seaborn/ggpairs 流)。
      --   x tick は最下段 (i == n-1) のみ、 y tick は左端列 (j == 0) のみ表示。
      --   対角 (i == j) は density で y = count スケールのため、 左端の (0,0) も y は抑制。
      -- ※ axShowTicks (Bool) は HS が真の JSON boolean で出力する。 PS Codec の
      --   decodeBoolean を boolean 両対応にして HS→PS decode を通るようにした。
      mkPanel i j =
        let showXAxis = i == n - 1
            -- 左端列 (j==0) は対角も含め y 軸目盛りを表示 (= その行の変数値スケール)。
            showYAxis = j == 0
            axisCfg = (if showXAxis then mempty else xAxis hideTicks)
                   <> (if showYAxis then mempty else yAxis hideTicks)
            -- seaborn/ggpairs 流: 軸ラベル (変数名) は最下段 x・左端 y のみ。
            -- inline 列は名前を持たない (placeholder) ので、 その場合はラベルを付けない
            -- (= xLabel/yLabel を呼ばず mempty。 空ラベルの margin 予約も避けて詰める)。
            axName c = let nm = colRefName c
                       in if nm == "<inline-num>" || nm == "<inline-txt>" then "" else nm
            xLab c = if i == n - 1 && axName c /= "" then xLabel (axName c) else mempty
            yLab c = if j == 0     && axName c /= "" then yLabel (axName c) else mempty
            base
              -- 対角 (i==j): densityNorm (= y 軸 = 値範囲、 KDE は panel 高さに正規化、
              -- seaborn pairplot 対角)。 左端列なら y タイトルも。
              | i == j    = case cols !? i of
                  Just c  -> purePlot <> layer (densityNorm c) <> xLab c <> yLab c
                  Nothing -> purePlot
              | otherwise = case (cols !? j, cols !? i) of
                  (Just xc, Just yc) -> purePlot
                    <> layer (scatter xc yc <> alpha 0.3 <> size 2.5)
                    <> xLab xc <> yLab yc
                  _ -> purePlot
        in base <> axisCfg
      panels = [ mkPanel i j | i <- [0..n-1], j <- [0..n-1] ]
  in subplots panels <> subplotCols n <> title "Pairs plot"
  where
    (!?) :: [a] -> Int -> Maybe a
    xs !? i = if i < 0 || i >= length xs then Nothing else Just (xs !! i)

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

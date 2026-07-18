-- |
-- Module      : Hgg.Plot.Spec.Mark
-- Description : mark 種別 (MarkKind) + DAG 型群 + layer 補助 enum (Spec の leaf)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Hgg.Plot.Spec' の module 分割で切り出した leaf。 幾何種別
-- 'MarkKind'、 DAG 描画の型群 ('DAGSpec' 一式)、 layer 属性の enum
-- ('ColorEnc' / 'Position' / 'Side' / 'Coord' / facet 系 / 'MarkShape' /
-- 'LineType' 等) を持つ。 依存は 'Hgg.Plot.Spec.Column' ('ColRef') のみ。
-- 公開 API は従来どおり 'Hgg.Plot.Spec' (facade) が re-export する。
-- 挙動・出力 (JSON tag 含む) は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Hgg.Plot.Spec.Mark
  ( -- * MarkKind
    MarkKind(..)
    -- * DAG
  , DAGNodeKind(..)
  , DAGLayoutAlgorithm(..)
  , DAGNode(..)
  , EdgeShapeKind(..)
  , RoutedEdge(..)
  , DAGEdge(..)
  , DAGPlate(..)
  , DAGSpec(..)
    -- * layer 補助 enum / spec
  , ConnectSpec(..)
  , defaultConnectSpec
  , ColorEnc(..)
  , YAxisSide(..)
  , Position(..)
  , Side(..)
  , Coord(..)
  , FacetScales(..)
  , freeScaleX
  , freeScaleY
  , FacetSpace(..)
  , freeSpaceX
  , freeSpaceY
    -- * shape / linetype
  , MarkShape(..)
  , ShapeMapEntry(..)
  , LineType(..)
  , lineTypeDash
  , lineTypeForIndex
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.Aeson      as Aeson
import qualified Data.Char       as Char
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           Hgg.Plot.Spec.Column (ColRef)

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
    -- Tile (連続 x/y のセルを fill 値でベタ塗り = ggplot geom_tile/geom_raster。 1 行=1 セル・
    -- 再ビニングせず格子間隔から幅自動。 決定境界の連続軸塗りが主用途。 Phase 60)
  | MTile
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
    -- Phase 51: custom mark (拡張可能な描画語彙)。 core を触らず 'customMark' で新プロット型を
    -- 足す拡張点。 payload (id/options/draw closure) は 'lyCustom' に持つ。 renderer は
    -- 'lyCustom' の 'cmDraw' を呼んで primitive を emit する (= registry 不要・closure が源)。
  | MCustom
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

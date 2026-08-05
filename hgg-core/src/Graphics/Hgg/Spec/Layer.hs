-- |
-- Module      : Graphics.Hgg.Spec.Layer
-- Description : Layer (inner Monoid) record and layer-local attribute setters
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'Graphics.Hgg.Spec' の module 分割で切り出し。 1 layer の全 field を
-- 持つ 'Layer' record と field-wise Monoid ('lyKind' のみ First・後は Last/concat、
-- @design/monoid-semantics.md@ §1)、 および「直前の 'Layer' に @<>@ する」
-- layer-local setter ('colorBy' / 'alpha' / 'size' / 'connect' 系等) を持つ。
-- mark ごとの構築子は 'Graphics.Hgg.Spec.Constructors' 側。 公開 API は従来どおり
-- 'Graphics.Hgg.Spec' (facade) が re-export する。 挙動・出力 (JSON 形含む) は
-- 完全に不変。
--
-- [English]: Split out from 'Graphics.Hgg.Spec' during a module split. Holds
-- the 'Layer' record with every field for a single layer, together with a
-- field-wise Monoid instance (only 'lyKind' uses First; the rest use Last or
-- concatenation — see @design/monoid-semantics.md@ §1), plus the layer-local
-- setters (colorBy / alpha / size / connect, and friends) that each combine
-- with @<>@ onto the preceding 'Layer'. Mark-specific constructors live in
-- 'Graphics.Hgg.Spec.Constructors'. The public API is unchanged:
-- 'Graphics.Hgg.Spec' (the facade) still re-exports everything. Behavior and
-- output (including JSON shape) are completely unchanged.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Layer
  ( -- * Layer (内側 Monoid)
    Layer(..)
    -- * layer-local attribute setter
  , colorBy
  , distGroupRef
  , distDodgeRef
  , color
  , colorRGBA
  , colorRGBAMaybe
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
  ) where

import           Data.Aeson      (FromJSON, ToJSON, toJSON, parseJSON,
                                  Value (Object))
import qualified Data.Aeson      as Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.Monoid     (First (..), Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           Graphics.Hgg.Color (Color, fromHexA, fromHexAMaybe, toCss)
import           Graphics.Hgg.Unit (Length, lengthToPt)
import           Graphics.Hgg.Spec.Column (ColRef)
import           Graphics.Hgg.Spec.CustomMark (CustomMark)
import           Graphics.Hgg.Spec.Mark

-- ===========================================================================
-- Layer (= 内側 Monoid)
-- ===========================================================================

-- | [日本語]: 1 layer の全 field。 各 field を 'First' (= kind は最初勝ち) または
--   'Last' (= 属性は後勝ち) で包んで Monoid を field-wise に。
--   [English]: All fields for a single layer. Each field is wrapped in
--   'First' (kind, where the first value wins) or 'Last' (attributes, where
--   the last value wins) to make the Monoid instance field-wise.
data Layer = Layer
  { lyKind    :: !(First MarkKind)
  , lyEncX    :: !(Last ColRef)
  , lyEncY    :: !(Last ColRef)
  , lyColor   :: !(Last ColorEnc)
  , lyAlpha   :: !(Last Double)
  , lySize    :: !(Last Double)
  , lyStroke  :: !(Last Double)
  , lyHover   :: ![ColRef]                  -- ★ multi-col tooltip
  , lyConnect :: !(Last ConnectSpec)        -- ★ connect points
  , lyErrorX  :: !(Last ColRef)             -- ★ ± 半幅 X
  , lyErrorY  :: !(Last ColRef)             -- ★ ± 半幅 Y
  , lyEncY2   :: !(Last ColRef)             -- ★ MBand 用 upper y
  , lyDAG     :: !(Last DAGSpec)            -- ★ HBM ModelGraph
  , lyJitterX :: !(Last Double)             -- ★ jitter X (plotArea 比率)
  , lyJitterY :: !(Last Double)             -- ★ jitter Y
  , lyYAxisSide :: !(Last YAxisSide)        -- ★ どちら Y 軸か
  , lyBinCount :: !(Last Int)               -- ★ frontend-settings v0.1 §2.4 hist bin 数
  , lyBinWidth :: !(Last Double)            -- ★ histogram の bin 幅 (= ggplot binwidth)。 binCount より優先
  , lyShape     :: !(Last MarkShape)        -- ★ 固定 shape (bare=固定・lyShapeBy より優先)
  , lyShapeBy   :: !(Last ColRef)           -- ★ C-6 categorical shape encoding 列
  , lyShapeMap  :: ![ShapeMapEntry]          -- ★ C-6 cat → shape 上書き
  , lySizeBy    :: !(Last ColRef)           -- ★ C-6 continuous size encoding 列
  , lyAlphaBy   :: !(Last ColRef)           -- ★ continuous alpha encoding 列
  , lyColorCats :: ![Text]                   -- ★ trellis 色一貫性 (= 全 data cat 順)
  , lyHistDensity :: !(Last Bool)             -- ★ histogram を density 正規化
  , lyHistBorder :: !(Last Bool)              -- ★ histogram/bar の bin 境界線 (= default False)
  , lyDensityFill :: !(Last Bool)             -- ★ density 曲線下を塗る (= ggplot geom_density(aes(fill=)))。 alpha と併用
  , lyHollow    :: !(Last Bool)               -- ★ 中抜きマーカー (= ggplot shape="circle open"/fill=NA)。 塗り透明 + 点色 stroke
  , lyNudge     :: !(Last Double)             -- ★ 分布 mark の slot 内横 offset (slot 幅比、 ggplot position_nudge 相当)
  , lyMarkWidth :: !(Last Double)             -- ★ 分布 mark の幅 (slot 幅比・占有率)。 各 mark の既定占有率を上書き
  , lySide      :: !(Last Side)               -- ★ violin の片側化 (= 半 violin)。 既定 Both
  , lyMaxLag    :: !(Last Int)                -- ★ autocorr max lag (= default 40)
  , lyChain     :: !(Last ColRef)             -- ★ chain group 列 (ESS / trace で chain 分け)
  , lyDensityNorm :: !(Last Bool)             -- ★ pairs 対角用。 y 軸 = 値範囲、 KDE は panel 高さに独立正規化
  , lyPosition  :: !(Last Position)           -- ★ bar position adjustment (dodge/stack/fill、 既定 identity)
  , lyLinetype   :: !(Last LineType)           -- ★ A4-b: 固定 linetype (= ggplot linetype=)
  , lyLinetypeBy :: !(Last ColRef)             -- ★ A4-b: categorical linetype scale 列
  , lyLabel      :: !(Last ColRef)             -- ★ geom_text/label のラベル列 (各点の文字)
  , lyStatLevel  :: !(Last Double)             -- ★ stat 回帰の信頼水準 (= 既定 0.95)。 MStat* 解決時のみ意味を持つ
  , lyContourLevels :: !(Last Int)             -- ★ 等高線の本数 (既定 8)。 MContour/MContourFilled 用
  , lyContourBreaks :: !(Last [Double])        -- ★ 等高線レベルの明示指定 (本数指定より優先)
  , lyEncU        :: !(Last ColRef)            -- ★ vector field (quiver) の u 成分列
  , lyEncV        :: !(Last ColRef)            -- ★ vector field (quiver) の v 成分列
  , lyArrowScale  :: !(Last Double)            -- ★ quiver 矢印長の倍率 (autoscale × この値・既定 1)
  , lyArrowMagnitude :: !(Last Bool)           -- ★ quiver を magnitude (|u,v|) で連続色マップ (既定 False)
  , lyEdge         :: !(Last Bool)             -- ★ 散布点の縁を描くか (既定 False = 縁なし、 ggplot 塗り点 shape 19 相当)
  , lyEdgeColor    :: !(Last Text)             -- ★ 縁の色 (未指定なら点と同色)
  , lyEdgeWidth    :: !(Last Double)           -- ★ 縁の幅 px (既定 1.0)
  , lyOverlay      :: ![Layer]                  -- ★ 同一 layer 内に重畳する追加 sub-mark
                                                --   (= '<+>' で蓄積)。 各 sub は自前の kind/nudge/markWidth/side
                                                --   を持ち、 親の群 (encX)・色 (colorBy)・値 (encY) を継承して描かれる。
                                                --   raincloud = (半 violin <+> box <+> strip) の preset。
  , lyCustom       :: !(Last CustomMark)         -- ★ custom mark payload (MCustom 用・id/options/draw closure)
  } deriving (Generic, Show, Eq)

instance ToJSON   Layer
-- ★ lyOverlay は後付けフィールドゆえ、 旧 JSON (= gallery specs/**.json 等) に
--   キーが無くても [] として decode できるよう、 generic parse の前に欠損キーを補う。
instance FromJSON Layer where
  parseJSON v = case v of
    -- ★ 後付けフィールド (lyOverlay/lyCustom) が旧 JSON に無くても
    --   decode できるよう、 generic parse の前に欠損キーを既定値で補う。
    Object o ->
      let o1 = if KM.member "lyOverlay" o then o
               else KM.insert "lyOverlay" (toJSON ([] :: [Layer])) o
          o2 = if KM.member "lyCustom" o1 then o1
               else KM.insert "lyCustom" Aeson.Null o1
      in Aeson.genericParseJSON Aeson.defaultOptions (Object o2)
    _ -> Aeson.genericParseJSON Aeson.defaultOptions v

-- | [日本語]: 1 layer 内の属性合成。 'lyKind' のみ 'First' (= 最初の mark が勝ち、 後続の
--   mark は消える点に注意 ─ 重畳は @layer@ で包んで合成する。 @design/monoid-semantics.md@
--   §1 参照)。 lyHover/lyShapeMap は concat、 lyColorCats は last-nonempty、 残りは 'Last'。
--   field 数が多く positional 列挙は取り違えやすいので record 構文で
--   per-field '(<>)' する (= Layer3D が同方針で行った変更)。 挙動は
--   旧 positional 版と同一: 'lyKind' は First (= 最初の mark 勝ち)、 'lyHover'/
--   'lyShapeMap' は list concat ('(<>)')、 'lyColorCats' は last-nonempty、 残りは Last。
--   [English]: Combines attributes within a single layer. Only 'lyKind' uses
--   'First' (the first mark wins; note that later marks are dropped —
--   overlaying multiple marks should instead go through @layer@; see
--   @design/monoid-semantics.md@ §1). lyHover/lyShapeMap concatenate,
--   lyColorCats keeps the last non-empty value, and everything else uses
--   'Last'. With this many fields, a positional field list is easy to get
--   wrong, so this uses record syntax with a per-field '(<>)' instead (the
--   same approach taken for Layer3D). The behavior matches the old
--   positional version exactly: 'lyKind' is First (the first mark wins),
--   'lyHover'/'lyShapeMap' use list concatenation ('(<>)'), 'lyColorCats'
--   keeps the last non-empty value, and the rest use Last.
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
    , lyOverlay     = lyOverlay a <> lyOverlay b   -- ★ sub-mark を concat
    , lyCustom      = lyCustom a <> lyCustom b      -- ★ custom mark payload (Last)
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
    , lyCustom = mempty
    }

-- ===========================================================================
-- Layer-local attribute (= 直前の Layer に <>)
-- ===========================================================================

-- | [日本語]: 列で色分け encoding (= categorical / continuous は ColRef 種別による)。
--   map 系は @*By@ 接尾辞 ('color' は固定色に明け渡し)。
--   [English]: Column-based color encoding (categorical or continuous,
--   depending on the ColRef kind). Mapping-style aesthetics use the @*By@
--   suffix ('color' is reserved for a fixed color).
colorBy :: ColRef -> Layer
colorBy c = mempty { lyColor = Last (Just (ColorByCol c)) }

-- | [日本語]: distribution mark の「群分け列」。 明示の 'lyEncX' があればそれを
--   群列とし、 無ければ 'colorBy' (= 'ColorByCol') の列を群列とみなす。 これにより
--   @boxplot "v" <> colorBy "g"@ が scatter と同様に群分割される (従来は encX 専用で
--   colorBy 単体だと単一群になっていた)。 distribution renderer と
--   @collectCategoricalLabels@ (distribution 限定) が共有する。
--   [English]: The "grouping column" for a distribution mark. If 'lyEncX' is
--   set explicitly, that is the grouping column; otherwise the column behind
--   'colorBy' (a 'ColorByCol') is treated as the grouping column. This lets
--   @boxplot "v" <> colorBy "g"@ split into groups the same way scatter does
--   (previously only encX did this, and colorBy alone produced a single
--   group). Shared by the distribution renderer and
--   @collectCategoricalLabels@ (distribution-only).
distGroupRef :: Layer -> Maybe ColRef
distGroupRef ly = case getLast (lyEncX ly) of
  Just cr -> Just cr
  Nothing -> case getLast (lyColor ly) of
    Just (ColorByCol cr) -> Just cr
    _                    -> Nothing

-- | [日本語]: distribution mark の dodge 検出。 @groupBy@ (= 'lyEncX' = 位置列) と
--   @colorBy@ (= 'lyColor' の 'ColorByCol' = 色列) が __両方__ 指定され、 かつ別列の
--   とき @Just (位置列, 色列)@。 このとき各位置カテゴリ内で色サブグループを横並び
--   (= ggplot @position_dodge@) する。 同一列 (groupBy と colorBy が同じ) のときは
--   dodge せず単一群彩色のまま (= 'distGroupRef' 経路) なので 'Nothing'。
--   [English]: Detects dodging for a distribution mark. When @groupBy@ (that
--   is, 'lyEncX', the position column) and @colorBy@ (the 'ColorByCol' inside
--   'lyColor', the color column) are __both__ set and refer to different
--   columns, returns @Just (position column, color column)@. In that case the
--   color subgroups within each position category are laid out side by side
--   (ggplot's @position_dodge@). When the two columns are the same (groupBy
--   and colorBy match), there is no dodging — coloring stays a single group
--   (the 'distGroupRef' path), so this returns 'Nothing'.
distDodgeRef :: Layer -> Maybe (ColRef, ColRef)
distDodgeRef ly = case (getLast (lyEncX ly), getLast (lyColor ly)) of
  (Just posC, Just (ColorByCol colC))
    -- ★ 同一列 (groupBy と colorBy が同じ列) は dodge せず単一群彩色のまま。 判定は
    --   ColRef の構造比較 (inline 列は 'colRefName' が両方 "<inline-*>" に潰れるため
    --   名前比較では別列を取り違える)。
    | posC /= colC -> Just (posC, colC)
  _ -> Nothing

-- | [日本語]: 静的色 (layer 全体に適用)。 固定色 aesthetic は bare 名 'color'。
--   'Color' 型 (RGB / @fromHex@ / R 657 名前付き定数) を受け、 ワイヤは 'toCss' で Text 化。
--   [English]: A static color (applied to the whole layer). The fixed-color
--   aesthetic uses the bare name 'color'. Takes a 'Color' value (RGB /
--   @fromHex@ / one of the 657 R named colors) and converts it to Text on the
--   wire via 'toCss'.
color :: Color -> Layer
color c = mempty { lyColor = Last (Just (ColorStatic (toCss c))) }

-- | [日本語]: 便利関数: 8 桁 RGBA hex (@"#rrggbbaa"@ / 4 桁 @"#rgba"@) を 1 つで受け、
--   @color (fromHex …) <> alpha …@ に展開する ('fromHexA' 経由)。 design ツール /
--   Web 由来の RGBA hex をそのまま貼れる。 ★@Color@ は RGB のみゆえ alpha は別 channel
--   に分離される (後続の @<> alphaBy "col"@ 等は 'Last' で後勝ち)。 不正入力は 'error'
--   (total 版は 'colorRGBAMaybe')。 6/3 桁 (alpha 無し) は不透明として扱う。
--   [English]: A convenience function: takes an 8-digit RGBA hex
--   (@"#rrggbbaa"@, or a 4-digit @"#rgba"@) as a single value and expands it
--   into @color (fromHex …) <> alpha …@ (via 'fromHexA'). Lets you paste RGBA
--   hex values straight from design tools or the web. Since @Color@ only
--   holds RGB, the alpha is split off into a separate channel (a later
--   @<> alphaBy "col"@ etc. still wins, per 'Last'). Invalid input calls
--   'error' (see 'colorRGBAMaybe' for the total version). 6- and 3-digit
--   forms (no alpha) are treated as fully opaque.
colorRGBA :: Text -> Layer
colorRGBA t = let (c, a) = fromHexA t in color c <> alpha a

-- | [日本語]: 'colorRGBA' の total 版。 不正な hex は 'Nothing'。
--   [English]: The total version of 'colorRGBA'. Invalid hex input yields
--   'Nothing'.
colorRGBAMaybe :: Text -> Maybe Layer
colorRGBAMaybe t = (\(c, a) -> color c <> alpha a) <$> fromHexAMaybe t

-- | [日本語]: 連続値 column を Viridis 風 gradient で色分け。
--   map 系ゆえ @*By@ 接尾辞。
--   [English]: Colors by a continuous column using a Viridis-like gradient.
--   Uses the @*By@ suffix since it is a mapping-style aesthetic.
colorContinuousBy :: ColRef -> Layer
colorContinuousBy c = mempty { lyColor = Last (Just (ColorByContinuous c)) }

-- | [日本語]: 透過度 (0..1)。 これは無次元なので 'Double' のまま。
--   [English]: Opacity (0..1). This is dimensionless, so it stays a 'Double'.
alpha :: Double -> Layer
alpha  a = mempty { lyAlpha  = Last (Just a) }

-- | [日本語]: マーカー径 ('size') / 線幅 ('stroke') を 'Length' で指定。
--   bare 数値リテラルは @Num Length@ 経由で __pt__ (@size 6@ = 6pt 直径)。 別単位は
--   @size (2 *~ mm)@。 内部は pt の 'Double' に解決して保持する (px は描画 dpi が
--   確定する前なので、 例外的に 96dpi で pt 化する = マーカーに px 指定は非推奨)。
--   [English]: Sets marker diameter ('size') / stroke width ('stroke') via a
--   'Length'. A bare numeric literal is __pt__ via @Num Length@ (@size 6@ =
--   a 6pt diameter). Other units use @size (2 *~ mm)@. Internally this
--   resolves and stores a 'Double' in pt (since the rendering dpi is not yet
--   known, px is converted at a fixed 96dpi as an exception — specifying
--   markers in px is discouraged).
size, stroke :: Length -> Layer
size   s = mempty { lySize   = Last (Just (lengthToPt 96 s)) }
stroke s = mempty { lyStroke = Last (Just (lengthToPt 96 s)) }

-- | [日本語]: 散布点に縁 (edge) を付ける。 既定は縁なし (= ggplot の塗り点 shape 19)。
--   'edgeOn' は点と同色の 1px 縁、 'edge col' は色を指定、 'edgeWidth w' は幅を指定
--   (いずれも縁を有効化)。 縁の透過は色に alpha 付き hex (例 @edge "#00000044"@) で表せる。
--   [English]: Adds an edge (outline) to scatter points. The default is no
--   edge (ggplot's filled-point shape 19). 'edgeOn' gives a 1px edge the same
--   color as the point, 'edge col' sets the edge color, and 'edgeWidth w'
--   sets the edge width (each of these also enables the edge). Edge
--   transparency can be expressed with an alpha-bearing color hex (for
--   example @edge "#00000044"@).
edgeOn :: Layer
edgeOn = mempty { lyEdge = Last (Just True) }

edge :: Text -> Layer
edge c = mempty { lyEdge = Last (Just True), lyEdgeColor = Last (Just c) }

edgeWidth :: Double -> Layer
edgeWidth w = mempty { lyEdge = Last (Just True), lyEdgeWidth = Last (Just w) }

-- | [日本語]: hover tooltip に表示する追加列 (= multi-col)。
--   [English]: Additional columns to show in the hover tooltip (multi-column).
--
-- > scatter "x" "y" <> hoverCols ["group", "label"]
hoverCols :: [ColRef] -> Layer
hoverCols cs = mempty { lyHover = cs }

-- | [日本語]: 各点の X 方向 ± 半幅 (error bar)。
--   [English]: The X-direction ± half-width for each point (an error bar).
errorX :: ColRef -> Layer
errorX c = mempty { lyErrorX = Last (Just c) }

-- | [日本語]: 各点の Y 方向 ± 半幅 (error bar)。
--   [English]: The Y-direction ± half-width for each point (an error bar).
errorY :: ColRef -> Layer
errorY c = mempty { lyErrorY = Last (Just c) }

-- | [日本語]: scatter 点を線で結ぶ ON。
--   [English]: Turns on connecting scatter points with a line.
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


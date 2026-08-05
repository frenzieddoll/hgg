-- |
-- Module      : Graphics.Hgg.Spec.Decoration
-- Description : Decoration specs — reference lines, annotations, marginals, legends, fonts
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: @Graphics.Hgg.Spec@ の module 分割で切り出し。 図に載せる装飾の
-- 宣言型 spec 群 ('ReferenceLine' / 'Annotation' / 'MarginalSpec' /
-- 'LegendSpec' / 'FontSpec') を持つ。 ※@Inset@ は @VisualSpec@ と相互参照の
-- ため @Graphics.Hgg.Spec.Visual@ 側。 公開 API は従来どおり
-- @Graphics.Hgg.Spec@ (facade) が re-export する。 挙動・出力は完全に不変。
-- [English]: Split out from @Graphics.Hgg.Spec@ via module decomposition.
-- Holds the declarative spec types for decorations placed on a figure
-- ('ReferenceLine' / 'Annotation' / 'MarginalSpec' / 'LegendSpec' /
-- 'FontSpec'). Note: @Inset@ instead lives alongside @VisualSpec@ in
-- @Graphics.Hgg.Spec.Visual@, since the two cross-reference each other.
-- The public API is unchanged — @Graphics.Hgg.Spec@ (the facade) still
-- re-exports everything, and behavior/output are fully preserved.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Decoration
  ( -- * ReferenceLine / Annotation
    ReferenceLine(..)
  , Annotation(..)
    -- * Marginal / Legend
  , MarginalKind(..)
  , MarginalSpec(..)
  , defaultMarginalSpec
  , LegendPosition(..)
  , LegendSpec(..)
  , defaultLegendSpec
    -- * FontSpec
  , FontSpec(..)
  , emptyFontSpec
  , fontSize, fontFamily, fontWeight, fontItalic, fontColor
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic, Generically (..))

import           Graphics.Hgg.Unit (Pos (..))

-- ===========================================================================
-- ReferenceLine (= 既存 PlotConfig.referenceLine 等価)
-- ===========================================================================

-- | [日本語]: plot area 内に重ねる参照線。
--   * 'RefIdentity'    ─ y = x の対角線 (= Actual vs Predicted)
--   * @RefHorizontalAt c@ ─ y = c
--   * @RefVerticalAt c@   ─ x = c
--   * @RefLinear slope intercept@ ─ y = slope * x + intercept
--   [English]: A reference line overlaid on the plot area.
--   * 'RefIdentity' — the y = x diagonal (Actual vs Predicted)
--   * @RefHorizontalAt c@ — y = c
--   * @RefVerticalAt c@   — x = c
--   * @RefLinear slope intercept@ — y = slope * x + intercept
data ReferenceLine
  = RefIdentity
  | RefHorizontalAt !Double
  | RefVerticalAt   !Double
  | RefLinear { rlSlope, rlIntercept :: !Double }
  deriving (Show, Eq, Generic)

instance ToJSON   ReferenceLine
instance FromJSON ReferenceLine

-- ===========================================================================
-- Annotation (= 2026-05-25 任意 overlay)
-- ===========================================================================

-- ★ 注釈の座標は 'Pos' (native/npc/絶対長を軸ごとに混在指定可)。
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
-- MarginalSpec (= 周辺 histogram)
-- ===========================================================================

-- | [日本語]: marginal の種別 (hist / density / 重ね)。
--   [English]: The kind of marginal panel (histogram / density / overlaid).
data MarginalKind = MarginalHist | MarginalDensity | MarginalBoth
  deriving (Show, Eq, Generic)

instance ToJSON   MarginalKind
instance FromJSON MarginalKind

-- | [日本語]: scatter の周辺に X/Y histogram を sub-plot として配置するか。
--   [English]: Whether to place X/Y histograms as sub-plots around the scatter.
data MarginalSpec = MarginalSpec
  { msShowX :: !Bool
  , msShowY :: !Bool
  , msBins  :: !Int   -- bin 数 (= default 20)
  , msKind  :: !MarginalKind  -- ★ hist / density / 重ね
  } deriving (Show, Eq, Generic)

instance ToJSON   MarginalSpec
instance FromJSON MarginalSpec

-- ★ レコードフィールド形式 (位置依存撲滅・挙動不変)。全 field が非 Monoid
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
-- LegendSpec (= 2026-05-25 凡例設定)
-- ===========================================================================

data LegendPosition
  = LegendRight | LegendBottom | LegendNone
  | LegendInsideTopRight | LegendInsideTopLeft
  | LegendInsideBottomRight | LegendInsideBottomLeft
  -- ★ 外・右に置きつつ panel 高の縦中央に揃える (ggplot 既定の
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

-- ★ レコードフィールド形式 (位置依存撲滅・挙動不変)。lgPosition は非 Monoid
--   enum なので後勝ち、 lgTitle は素直な `Last` 合成。
instance Semigroup LegendSpec where
  a <> b = LegendSpec
    { lgPosition = lgPosition b           -- 後勝ち (enum)
    , lgTitle    = lgTitle a <> lgTitle b
    }

instance Monoid LegendSpec where
  mempty = defaultLegendSpec

defaultLegendSpec :: LegendSpec
defaultLegendSpec = LegendSpec LegendRightCenter mempty  -- ggplot 既定 (右・縦中央)

-- ===========================================================================
-- FontSpec (= hgg-frontend-settings-spec v0.1 §1.3)
-- ===========================================================================

data FontSpec = FontSpec
  { fsFamily :: !(Last Text)
  , fsSize   :: !(Last Double)
  , fsWeight :: !(Last Text)
  , fsItalic :: !(Last Bool)   -- ★ PS parity
  , fsColor  :: !(Last Text)
  } deriving stock (Show, Eq, Generic)
    -- ★ 全 field が `Last` の素直な per-field 合成なので generic 導出
    --   (= 手書き instance ゼロ・field 追加に強い)。挙動は旧手書きと完全同型。
    deriving (Semigroup, Monoid) via Generically FontSpec

instance ToJSON   FontSpec
instance FromJSON FontSpec

-- | [日本語]: 空 'FontSpec' (= 'mempty' alias、 generic 導出の mempty と同値)。
--   [English]: The empty 'FontSpec' (an alias for 'mempty', identical to the
--   value produced by the generic derivation).
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


-- |
-- Module      : Graphics.Hgg.Spec.Axis
-- Description : AxisSpec ─ 軸 1 本の設定 (scale 種別 / format / break / 回転)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Graphics.Hgg.Spec' の module 分割で切り出し。 軸 1 本の宣言型設定
-- 'AxisSpec' (log/sqrt/time scale・範囲・tick・回転・break) とその setter /
-- accessor を持つ。 Spec 内の他 module に依存しない leaf。 公開 API は従来どおり
-- 'Graphics.Hgg.Spec' (facade) が re-export する。 挙動・出力は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Axis
  ( AxisKind(..)
  , AxisFormat(..)
  , AxisBreak(..)
  , AxisSpec(..)
  , linearAxis, logAxis, sqrtAxis, timeAxis
  , axisMin, axisMax, axisRange
  , axisFormat, axisRotate, axisTickLabels, hideTicks
  , axisBreak, axisBreaksAt, axisBreaksLabeled
  , axisKindOf, axisFormatOf, axisRotateOf, axisShowTicksOf
  , axTickValsOf, axTickLabelsOf
  , resolveAxisAngle, themeAngleOr0
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

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

-- | 軸 label 回転 (度・**CCW = 反時計回りが正**、 R / matplotlib / ggplot と同じ規約・Phase 50 A1)。
--   30 / 45 / 90 等。 例: @xAxis (axisRotate 90)@ で x 目盛ラベルを CCW 90°
--   (縦書き・下→上読み・y 軸タイトルと同じ向き)。 内部の SVG/canvas rotate は CW 正なので
--   'resolveAxisAngle' で符号反転して描画に渡す (公開 API は R 準拠の CCW に統一)。
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

-- | Phase 9 A-3 / Phase 50 A1: 軸目盛りラベルの回転角を解決 (**CCW 正・canonical**)。
-- 'axisRotate' / theme axis.text angle も内部 'tsRotate' も **CCW 正** (R/matplotlib/ggplot 準拠)
-- で一貫。 CW の device (SVG/canvas/rasterific) への変換は **各 backend の emit で 1 回だけ** 行う
-- (PDF は y-up=CCW ゆえ恒等)。 per-axis 明示指定を最優先、 無ければ theme override、 無ければ 0。
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


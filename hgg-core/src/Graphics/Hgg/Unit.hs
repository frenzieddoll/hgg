-- |
-- Module      : Graphics.Hgg.Unit
-- Description : 長さの単位系 (pt オーサリング + dpi 描画境界、Phase 33)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg は SVG / Canvas / PNG / PDF の複数 backend を持つ。PDF は point
-- (1/72 inch) ネイティブなので、オーサリングは物理単位 (mm/cm/inch/pt) を主とし、
-- px 出力境界で一度だけ @px = pt × dpi/72@ を掛ける。本 module は最下層の純 value
-- 層で、Spec / Layout から参照される (Spec には依存しない = 循環回避)。
--
-- 単位は値と一体 ('Length')。混在は許さず、各値が自分の単位を持つ。px は dpi 依存
-- なので 'toPt' では変換できず ('Nothing')、dpi を受け取る 'lengthToPt' で解決する。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Hgg.Unit
  ( LUnit(..)
  , Length(..)
  , mm, cm, inch, pt', px
  , (*~)
  , mmToPt
  , toPt
  , lengthToPt
    -- * 座標 (Phase 33 B3): 相対単位込みの位置型 + resolver 別名
  , Pos(..)
  , resolveLen
  ) where

import           Data.Aeson  (FromJSON (..), ToJSON (..), Value (..), object,
                              pairs, withObject, (.:), (.=))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           GHC.Generics (Generic)

-- === 型 ===

-- | 長さの単位。Mm/Cm/In/Pt は dpi 非依存の物理単位、Px は device 依存。
data LUnit = Mm | Cm | In | Pt | Px
  deriving (Eq, Show, Generic)

-- | 値と単位を一体に保持する長さ。
data Length = Length !Double !LUnit
  deriving (Eq, Show, Generic)

-- | 軸に沿った「座標」(サイズ 'Length' ではない)。注釈・参照線・inset の自由配置
-- に使う。相対単位 (npc/native) の意味は panel rect / scale が決めるので、解決は
-- 'UCtx' を受け取る Layout 側 resolver (@resolvePosX/Y@) が担う (本 module には
-- 型と Codec だけ置き、Rect/Scale への依存を避ける = 循環回避)。
data Pos
  = PAbs    !Length   -- ^ 物理長オフセット (pt/mm/in/px)。panel 原点基準。
  | PNpc    !Double    -- ^ panel 正規化座標 0..1 (0=左/下端, 1=右/上端)。
  | PNative !Double    -- ^ data 座標 (scale 経由で pt 化)。
  deriving (Eq, Show, Generic)

-- === 構築 (単位量 + スカラ倍) ===

-- | 各単位の「1 単位」を表す単位量。@7 *~ inch@ のように使う。
mm, cm, inch, pt', px :: Length
mm   = Length 1 Mm
cm   = Length 1 Cm
inch = Length 1 In
pt'  = Length 1 Pt
px   = Length 1 Px

-- | スカラ倍 (単位保存)。@k *~ (n 単位) = (k*n) 単位@。
infixl 7 *~
(*~) :: Double -> Length -> Length
k *~ Length n u = Length (k * n) u

-- | 数値リテラルを 'Length' として解釈するための 'Num' / 'Fractional' instance
-- (Phase 34 A2)。狙いは @width 624@ のような **bare 数値リテラル = pt** を成立させ、
-- かつ @width (7 *~ inch)@ の単位付きも同じ引数型で受けること。
--
-- ★ なぜ型クラス ('ToLength' 案) でなくこちら: @ToLength a => a -> _@ だと
-- @width 624@ が @(Num a, ToLength a) => a@ で曖昧化し、ToLength が標準クラスでない
-- ため Haskell2010 の defaulting が効かず**コンパイル不可** (Phase 34 A2 で実測検証)。
-- @Num Length@ なら @624 :: Length = fromInteger 624 = Length 624 Pt@ と確定し曖昧化しない
-- (CSS length ライブラリ = clay/diagrams と同じ慣用)。
--
-- 算術 (@+@/@-@/@*@) は **同一単位の被演算子**を想定し、左辺の単位を保存して数値だけ
-- 合成する (主用途はリテラル overloading なので cross-unit 演算は非対象)。
instance Num Length where
  fromInteger n           = Length (fromInteger n) Pt
  Length a u + Length b _ = Length (a + b) u
  Length a u - Length b _ = Length (a - b) u
  Length a u * Length b _ = Length (a * b) u
  abs    (Length a u)     = Length (abs a) u
  signum (Length a u)     = Length (signum a) u
  negate (Length a u)     = Length (negate a) u

instance Fractional Length where
  fromRational r          = Length (fromRational r) Pt
  Length a u / Length b _ = Length (a / b) u

-- === pt への正規化 ===

-- | mm → pt 変換定数 (72pt / 25.4mm ≈ 2.8346)。ggplot の @.pt=72.27/25.4@ /
-- @.stroke=96/25.4@ の基準混在は採らず、全部 72pt/inch に統一する。
mmToPt :: Double
mmToPt = 72 / 25.4

-- | dpi 非依存単位を pt 化。'Px' は dpi が要るので 'Nothing' (型で表現)。
toPt :: Length -> Maybe Double
toPt (Length n u) = case u of
  Pt -> Just n
  In -> Just (n * 72)
  Cm -> Just (n * 10 * mmToPt)
  Mm -> Just (n * mmToPt)
  Px -> Nothing

-- | dpi を受け取り全単位を pt 化。'Px' のみ @n * 72/dpi@。
-- computeLayout 入口で figure size を解決する本命関数。
lengthToPt :: Double -> Length -> Double
lengthToPt dpi (Length n u) = case u of
  Pt -> n
  In -> n * 72
  Cm -> n * 10 * mmToPt
  Mm -> n * mmToPt
  Px -> n * 72 / dpi

-- | 'Length' を pt 化する resolver 別名 ('lengthToPt' と同一)。Pos resolver
-- (@resolvePosX/Y@) と対で「単位を pt へ解く」API を一様に呼ぶための名前。
resolveLen :: Double -> Length -> Double
resolveLen = lengthToPt

-- === JSON Codec ===
-- @{ "v": Double, "u": String }@。key 順 v→u を toEncoding で固定し、PS argonaut と
-- byte 一致させる。tag は小文字 "mm"|"cm"|"in"|"pt"|"px"。

lunitTag :: LUnit -> Text
lunitTag u = case u of
  Mm -> "mm"; Cm -> "cm"; In -> "in"; Pt -> "pt"; Px -> "px"

instance ToJSON Length where
  toJSON (Length v u) = object ["v" .= v, "u" .= lunitTag u]
  toEncoding (Length v u) = pairs ("v" .= v <> "u" .= lunitTag u)

instance FromJSON Length where
  -- 後方互換 (Phase 33 移行): 旧来の px Int/Number 形式を px Length として読む。
  parseJSON (Number n) = pure (Length (realToFrac n) Px)
  parseJSON other      = flip (withObject "Length") other $ \o -> do
    v    <- o .: "v"
    uStr <- o .: "u"
    u <- case (uStr :: Text) of
      "mm" -> pure Mm
      "cm" -> pure Cm
      "in" -> pure In
      "pt" -> pure Pt
      "px" -> pure Px
      _    -> fail ("Graphics.Hgg.Unit: unknown LUnit tag " <> T.unpack uStr)
    pure (Length v u)

-- | 'Pos' の Codec。tag 付き @{ "t": "abs"|"npc"|"native", ... }@。
-- "abs" は @"l"@ に 'Length'、"npc"/"native" は @"p"@ に Double。key 順は
-- toEncoding (t→payload) で固定し PS argonaut と byte 一致させる。
instance ToJSON Pos where
  toJSON p = case p of
    PAbs l    -> object ["t" .= ("abs" :: Text),    "l" .= l]
    PNpc x    -> object ["t" .= ("npc" :: Text),    "p" .= x]
    PNative x -> object ["t" .= ("native" :: Text), "p" .= x]
  toEncoding p = case p of
    PAbs l    -> pairs ("t" .= ("abs" :: Text)    <> "l" .= l)
    PNpc x    -> pairs ("t" .= ("npc" :: Text)    <> "p" .= x)
    PNative x -> pairs ("t" .= ("native" :: Text) <> "p" .= x)

instance FromJSON Pos where
  parseJSON = withObject "Pos" $ \o -> do
    t <- o .: "t"
    case (t :: Text) of
      "abs"    -> PAbs    <$> o .: "l"
      "npc"    -> PNpc    <$> o .: "p"
      "native" -> PNative <$> o .: "p"
      _        -> fail ("Graphics.Hgg.Unit: unknown Pos tag " <> T.unpack t)

-- |
-- Module      : Graphics.Hgg.Unit
-- Description : The length unit system — pt authoring with a dpi rendering boundary
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: hgg は SVG / Canvas / PNG / PDF の複数 backend を持つ。PDF は
--   point (1/72 inch) ネイティブなので、オーサリングは物理単位 (mm/cm/inch/pt) を
--   主とし、px 出力境界で一度だけ @px = pt × dpi/72@ を掛ける。本 module は最下層の
--   純 value 層で、Spec / Layout から参照される (Spec には依存しない = 循環回避)。
--
--   単位は値と一体 ('Length')。混在は許さず、各値が自分の単位を持つ。px は dpi
--   依存なので 'toPt' では変換できず ('Nothing')、dpi を受け取る 'lengthToPt' で
--   解決する。
--   [English]: hgg has multiple backends — SVG / Canvas / PNG / PDF.
--   Since PDF is native in points (1/72 inch), authoring primarily uses
--   physical units (mm/cm/inch/pt), and @px = pt × dpi/72@ is applied exactly
--   once at the px output boundary. This module is the lowest, pure-value
--   layer; it is referenced from Spec / Layout but does not depend on Spec
--   itself, to avoid a cycle.
--
--   A unit travels together with its value ('Length'); mixing is not
--   allowed, so each value carries its own unit. Since px is dpi-dependent,
--   'toPt' cannot convert it ('Nothing'); resolving it requires 'lengthToPt',
--   which takes a dpi.
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
    -- * 座標: 相対単位込みの位置型 + resolver 別名
  , Pos(..)
  , resolveLen
  ) where

import           Data.Aeson  (FromJSON (..), ToJSON (..), Value (..), object,
                              pairs, withObject, (.:), (.=))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           GHC.Generics (Generic)

-- === 型 ===

-- | [日本語]: 長さの単位。Mm/Cm/In/Pt は dpi 非依存の物理単位、Px は device 依存。
--   [English]: A length unit. Mm/Cm/In/Pt are dpi-independent physical
--   units; Px is device-dependent.
data LUnit = Mm | Cm | In | Pt | Px
  deriving (Eq, Show, Generic)

-- | [日本語]: 値と単位を一体に保持する長さ。
--   [English]: A length that carries its value and unit together.
data Length = Length !Double !LUnit
  deriving (Eq, Show, Generic)

-- | [日本語]: 軸に沿った「座標」(サイズ 'Length' ではない)。注釈・参照線・inset
--   の自由配置に使う。相対単位 (npc/native) の意味は panel rect / scale が
--   決めるので、解決は @UCtx@ を受け取る Layout 側 resolver (@resolvePosX/Y@)
--   が担う (本 module には型と Codec だけ置き、Rect/Scale への依存を避ける =
--   循環回避)。
--   [English]: A "coordinate" along an axis (not a size, unlike 'Length').
--   Used for freely positioning annotations, reference lines, and insets.
--   The meaning of the relative units (npc/native) is determined by the
--   panel rect / scale, so resolution is handled by the Layout-side
--   resolver (@resolvePosX/Y@), which takes a @UCtx@. This module holds only
--   the type and its Codec, avoiding a dependency on Rect/Scale (to prevent
--   a cycle).
data Pos
  = PAbs    !Length   -- ^ [日本語]: 物理長オフセット (pt/mm/in/px)。panel 原点基準。
                      --   [English]: A physical-length offset (pt/mm/in/px), relative to the panel origin.
  | PNpc    !Double    -- ^ [日本語]: panel 正規化座標 0..1 (0=左/下端, 1=右/上端)。
                       --   [English]: A panel-normalised coordinate in 0..1 (0 = left/bottom edge, 1 = right/top edge).
  | PNative !Double    -- ^ [日本語]: data 座標 (scale 経由で pt 化)。
                       --   [English]: A data coordinate (converted to pt via the scale).
  deriving (Eq, Show, Generic)

-- === 構築 (単位量 + スカラ倍) ===

-- | [日本語]: 各単位の「1 単位」を表す単位量。@7 *~ inch@ のように使う。
--   [English]: A unit quantity representing "one unit" of each unit; used as
--   in @7 *~ inch@.
mm, cm, inch, pt', px :: Length
mm   = Length 1 Mm
cm   = Length 1 Cm
inch = Length 1 In
pt'  = Length 1 Pt
px   = Length 1 Px

-- | [日本語]: スカラ倍 (単位保存)。@k *~ (n 単位) = (k*n) 単位@。
--   [English]: Scalar multiplication (unit-preserving). @k *~ (n units) =
--   (k*n) units@.
infixl 7 *~
(*~) :: Double -> Length -> Length
k *~ Length n u = Length (k * n) u

-- | [日本語]: 数値リテラルを 'Length' として解釈するための 'Num' / 'Fractional'
--   instance。狙いは @width 624@ のような __bare 数値リテラル = pt__ を成立させ、
--   かつ @width (7 *~ inch)@ の単位付きも同じ引数型で受けること。
--
--   ★ なぜ型クラス (@ToLength@ 案) でなくこちら: @ToLength a => a -> _@ だと
--   @width 624@ が @(Num a, ToLength a) => a@ で曖昧化し、ToLength が標準クラスで
--   ないため Haskell2010 の defaulting が効かず__コンパイル不可__ (実測検証済)。
--   @Num Length@ なら @624 :: Length = fromInteger 624 = Length 624 Pt@ と確定し
--   曖昧化しない (CSS length ライブラリ = clay/diagrams と同じ慣用)。
--
--   算術 (@+@/@-@/@*@) は __同一単位の被演算子__を想定し、左辺の単位を保存して
--   数値だけ合成する (主用途はリテラル overloading なので cross-unit 演算は
--   非対象)。
--   [English]: The 'Num' / 'Fractional' instances that let a numeric literal
--   be interpreted as a 'Length'. The goal is to make __a bare numeric literal mean pt__,
--   as in @width 624@, while also accepting a unit
--   annotation such as @width (7 *~ inch)@ at the same argument type.
--
--   ★ Why this rather than a type class (the @ToLength@ idea): with
--   @ToLength a => a -> _@, @width 624@ becomes ambiguous at
--   @(Num a, ToLength a) => a@, and since ToLength is not a standard class,
--   Haskell2010 defaulting does not kick in, so it __fails to compile__
--   (verified by measurement). With @Num Length@, @624 :: Length@ resolves
--   unambiguously to @fromInteger 624 = Length 624 Pt@ (the same idiom used
--   by CSS-length libraries such as clay/diagrams).
--
--   Arithmetic (@+@/@-@/@*@) assumes __operands of the same unit__: it keeps
--   the left operand's unit and only combines the numbers (the primary use
--   case is literal overloading, so cross-unit arithmetic is out of scope).
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

-- | [日本語]: mm → pt 変換定数 (72pt / 25.4mm ≈ 2.8346)。ggplot の
--   @.pt=72.27/25.4@ / @.stroke=96/25.4@ の基準混在は採らず、全部 72pt/inch
--   に統一する。
--   [English]: The mm to pt conversion constant (72pt / 25.4mm ≈ 2.8346).
--   Rather than mixing bases like ggplot's @.pt=72.27/25.4@ /
--   @.stroke=96/25.4@, everything here is unified to 72pt/inch.
mmToPt :: Double
mmToPt = 72 / 25.4

-- | [日本語]: dpi 非依存単位を pt 化。'Px' は dpi が要るので 'Nothing' (型で表現)。
--   [English]: Converts dpi-independent units to pt. 'Px' requires a dpi, so
--   it yields 'Nothing' (expressed at the type level).
toPt :: Length -> Maybe Double
toPt (Length n u) = case u of
  Pt -> Just n
  In -> Just (n * 72)
  Cm -> Just (n * 10 * mmToPt)
  Mm -> Just (n * mmToPt)
  Px -> Nothing

-- | [日本語]: dpi を受け取り全単位を pt 化。'Px' のみ @n * 72/dpi@。
--   computeLayout 入口で figure size を解決する本命関数。
--   [English]: Converts every unit to pt, given a dpi; only 'Px' uses
--   @n * 72/dpi@. This is the primary function that resolves figure size at
--   the computeLayout entry point.
lengthToPt :: Double -> Length -> Double
lengthToPt dpi (Length n u) = case u of
  Pt -> n
  In -> n * 72
  Cm -> n * 10 * mmToPt
  Mm -> n * mmToPt
  Px -> n * 72 / dpi

-- | [日本語]: 'Length' を pt 化する resolver 別名 ('lengthToPt' と同一)。Pos
--   resolver (@resolvePosX/Y@) と対で「単位を pt へ解く」API を一様に呼ぶための
--   名前。
--   [English]: A resolver alias for converting 'Length' to pt (identical to
--   'lengthToPt'). Paired with the Pos resolver (@resolvePosX/Y@) to give a
--   uniform name for "resolve a unit to pt" APIs.
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

-- | [日本語]: 'Pos' の Codec。tag 付き @{ "t": "abs"|"npc"|"native", ... }@。
--   "abs" は @"l"@ に 'Length'、"npc"/"native" は @"p"@ に Double。key 順は
--   toEncoding (t→payload) で固定し PS argonaut と byte 一致させる。
--   [English]: The Codec for 'Pos': tagged @{ "t": "abs"|"npc"|"native", ... }@.
--   "abs" carries a 'Length' at @"l"@; "npc"/"native" carry a Double at
--   @"p"@. Key order is fixed by toEncoding (t then payload) to byte-match
--   PureScript's argonaut.
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

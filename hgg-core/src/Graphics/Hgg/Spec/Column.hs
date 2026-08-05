-- |
-- Module      : Graphics.Hgg.Spec.Column
-- Description : Column references (ColRef/Resolver), inline conversion, and Point2 — a Spec leaf
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'Graphics.Hgg.Spec' (3420 行) の module 分割で切り出した leaf。
-- 列参照の 3 variant ('ColByName' / 'ColNum' / 'ColTxt') と render 時解決
-- ('Resolver')、 inline 列変換 ('Numeric' / 'Categorical')、 2D 点 ('Point2') を
-- 持つ。 Spec 内の他 module に依存しない最下層。 公開 API は従来どおり
-- 'Graphics.Hgg.Spec' (facade) が re-export する。 挙動・出力は完全に不変。
--
-- [English]: A leaf split out of 'Graphics.Hgg.Spec' (3420 lines) during
-- its module split. Carries the 3 column-reference variants ('ColByName' /
-- 'ColNum' / 'ColTxt'), their render-time resolution ('Resolver'), inline
-- column conversion ('Numeric' / 'Categorical'), and the 2D point
-- ('Point2'). The bottom layer, depending on no other module within Spec.
-- The public API is still re-exported by the 'Graphics.Hgg.Spec' facade as
-- before; behavior and output are completely unchanged.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Column
  ( -- * ColRef + Resolver
    ColRef(..)
  , ColData(..)
  , Resolver
  , emptyResolver
  , resolveCol
  , resolveNum
  , resolveTxt
  , colRefName
    -- * Inline column conversion
  , Numeric(..)
  , Categorical(..)
  , inline
  , inlineCat
    -- * Point2
  , Point2(..)
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           GHC.Generics    (Generic)

-- ===========================================================================
-- ColRef + Resolver
-- ===========================================================================

-- | [日本語]: データ列の参照方法。 3 つの variant:
--
--     * 'ColByName' ─ 文字列 col 名。 'Resolver' で実 Vector に解決される。
--     * 'ColNum'    ─ 数値 Vector を inline (= 即値、 resolver 不要)
--     * 'ColTxt'    ─ 文字列 Vector を inline (= categorical encoding 用)
--
--   @OverloadedStrings@ で `"weight" :: ColRef` が `ColByName "weight"` に。
--   [English]: How a data column is referenced. Three variants:
--
--     * 'ColByName' — a string column name, resolved to an actual Vector
--       by 'Resolver'.
--     * 'ColNum'    — an inline numeric Vector (an immediate value, no
--       resolver needed)
--     * 'ColTxt'    — an inline text Vector (for categorical encoding)
--
--   With 'OverloadedStrings', `"weight" :: ColRef` becomes `ColByName
--   "weight"` automatically.
data ColRef
  = ColByName !Text
  | ColNum    !(Vector Double)
  | ColTxt    !(Vector Text)
  deriving (Generic, Show, Eq)

instance ToJSON   ColRef
instance FromJSON ColRef

instance IsString ColRef where
  fromString = ColByName . T.pack

-- | [日本語]: Resolver が返すデータ形 (= 数値 or 文字列)。
--   [English]: The data shape returned by a Resolver (numeric or text).
data ColData
  = NumData !(Vector Double)
  | TxtData !(Vector Text)
  deriving (Show, Eq)

-- | [日本語]: render 時に col 名を Vector に解決する callback。
--   数値列 / 文字列列 どちらも返せるよう 'ColData' で union。
--   [English]: A callback that resolves a column name to a Vector at
--   render time. Unions over 'ColData' so it can return either a numeric
--   or a text column.
type Resolver = Text -> Maybe ColData

emptyResolver :: Resolver
emptyResolver _ = Nothing

-- | [日本語]: 'ColRef' を 'ColData' に解決。 inline は variant に応じて直接返す。
--   [English]: Resolves a 'ColRef' to 'ColData'. Inline variants are
--   returned directly according to their variant.
resolveCol :: Resolver -> ColRef -> Maybe ColData
resolveCol r (ColByName n) = r n
resolveCol _ (ColNum v)    = Just (NumData v)
resolveCol _ (ColTxt v)    = Just (TxtData v)

-- | [日本語]: 数値解決 (= 数値列 or 数値 inline のみ成功、 文字列は 'Nothing')。
--   [English]: Numeric resolution (succeeds only for a numeric column or
--   numeric inline data; text yields 'Nothing').
resolveNum :: Resolver -> ColRef -> Maybe (Vector Double)
resolveNum r cr = case resolveCol r cr of
  Just (NumData v) -> Just v
  _                -> Nothing

-- | [日本語]: 文字列解決 (= 文字列 inline or 文字列列のみ成功)。
--   [English]: Text resolution (succeeds only for text inline data or a
--   text column).
resolveTxt :: Resolver -> ColRef -> Maybe (Vector Text)
resolveTxt r cr = case resolveCol r cr of
  Just (TxtData v) -> Just v
  _                -> Nothing

-- | [日本語]: ColRef の表示名 (= hover tooltip / legend 等)。
--   [English]: The display name of a 'ColRef' (for hover tooltips, the
--   legend, etc.).
colRefName :: ColRef -> Text
colRefName (ColByName n) = n
colRefName (ColNum _)    = "<inline-num>"
colRefName (ColTxt _)    = "<inline-txt>"

-- ===========================================================================
-- Inline column conversion
-- ===========================================================================

-- | [日本語]: 数値系 (Vector n / [n]、 n は Real instance を持つ任意型) を
--   'ColRef' に。
--   [English]: Converts numeric data (Vector n / [n], where n is any type
--   with a Real instance) into a 'ColRef'.
class Numeric a where
  toNumVec :: a -> Vector Double

instance Real n => Numeric (Vector n) where
  toNumVec = V.map realToFrac

instance Real n => Numeric [n] where
  toNumVec = V.fromList . map realToFrac

-- | [日本語]: 文字列系 (= categorical encoding 用)。
--   [English]: Text data (for categorical encoding).
class Categorical a where
  toTxtVec :: a -> Vector Text

instance Categorical (Vector Text) where toTxtVec = id
instance Categorical [Text]        where toTxtVec = V.fromList
instance Categorical [String]      where toTxtVec = V.fromList . map T.pack

-- | [日本語]: 数値 (Vector / List) を inline 'ColRef' に。 'Int' / 'Double' /
--   'Float' / 'Integer' / 'Word' 等 'Real' instance を持つ任意型に対応。
--   [English]: Converts numeric data (a Vector or list) into an inline
--   'ColRef'. Works for any type with a 'Real' instance, such as 'Int',
--   'Double', 'Float', 'Integer', or 'Word'.
--
-- > scatter (inline xs) (inline ys)
-- > scatter (inline [1, 2, 3]) (inline [4.0, 5.0, 6.0])
inline :: Numeric a => a -> ColRef
inline = ColNum . toNumVec

-- | [日本語]: 文字列系を inline 'ColRef' に (= categorical encoding 用)。
--   [English]: Converts text data into an inline 'ColRef' (for categorical
--   encoding).
--
-- > colorBy (inlineCat ["red", "blue", "green"])
inlineCat :: Categorical a => a -> ColRef
inlineCat = ColTxt . toTxtVec

-- ===========================================================================
-- Point2 (= 2D 点・3D 'Point3' と対称)
-- ===========================================================================

-- | [日本語]: 2D 点 (= world space)。 @Graphics.Hgg.ThreeD.Types.Point3@ と対称の
--   直積型。 inline の点単位 API ('scatterPoints' / 'linePoints') で使う。
--   [English]: A 2D point (in world space); a product type symmetric to
--   @Graphics.Hgg.ThreeD.Types.Point3@. Used by the inline point-wise API
--   ('scatterPoints' / 'linePoints').
--
--   [日本語]: JSON: positional field は array @[x, y]@ になる (= aeson の
--   Generic 既定挙動・@Point3@ と同形式)。 ※ @Graphics.Hgg.Render@ の @Point@
--   は screen 空間の別物。
--   [English]: JSON: positional fields become the array @[x, y]@ (the
--   default aeson Generic behavior, in the same shape as @Point3@). Note:
--   the @Point@ type in @Graphics.Hgg.Render@ is a distinct, screen-space
--   type.
data Point2 = Point2 !Double !Double
  deriving (Show, Eq, Generic)
instance ToJSON   Point2
instance FromJSON Point2


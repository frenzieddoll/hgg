-- |
-- Module      : Hgg.Plot.Spec.Column
-- Description : データ列参照 (ColRef/Resolver) + inline 変換 + Point2 (Spec の leaf)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Hgg.Plot.Spec' (3420 行) の module 分割で切り出した leaf。
-- 列参照の 3 variant ('ColByName' / 'ColNum' / 'ColTxt') と render 時解決
-- ('Resolver')、 inline 列変換 ('Numeric' / 'Categorical')、 2D 点 ('Point2') を
-- 持つ。 Spec 内の他 module に依存しない最下層。 公開 API は従来どおり
-- 'Hgg.Plot.Spec' (facade) が re-export する。 挙動・出力は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Hgg.Plot.Spec.Column
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


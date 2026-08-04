-- |
-- Module      : Graphics.Hgg.Custom.Dendrogram
-- Description : Draws a hierarchical-clustering dendrogram as a custom mark
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: dendrogram を __custom mark__ で本実装する。 core (@MarkKind@) は
-- 触らない (add-on package)。
--
-- [English]: Implements the dendrogram proper as a __custom mark__. It does
-- not touch core (@MarkKind@) — this is an add-on package.
--
-- [日本語]: 設計 = __焼き込み (baked segments)__: 樹形図の U 字リンクを「計算済みの線分列 ('DendroSeg')」
-- として 'DendroPayload' に持ち、 それを @cmOptions@ (JSON) に焼き込む。 HS の draw closure は
-- payload を直接使い、 PS (canvas) の registry は同じ payload JSON を読む — どちらも「線分を
-- proj して 'PLine' で描くだけ」なので HS=PS parity が自明。 clustering/レイアウト算法は
-- __呼び出し側 (analyze の @dendrogramOf@ 等)__ が行い、 その結果の線分だけをここへ渡す。
--
-- [English]: The design is __baked segments__: the U-shaped links of the tree
-- are held in 'DendroPayload' as a precomputed segment list ('DendroSeg'),
-- which is then baked into @cmOptions@ (JSON). The HS draw closure consumes
-- the payload directly, and the PS (canvas) registry reads the same payload
-- JSON — since both merely project the segments and draw them with 'PLine',
-- HS=PS parity is self-evident. The clustering and layout algorithms are run
-- by __the caller (for example analyze's @dendrogramOf@)__, which passes only
-- the resulting segments here.
--
-- [日本語]: 葉ラベル・軸は本 mark の外 (呼び出し側が数値 x 軸 + @axisBreaksLabeled@ で付ける)。 本 mark は
-- __U 字リンクの線分のみ__を描く。
--
-- [English]: Leaf labels and axes live outside this mark (the caller adds them
-- with a numeric x axis plus @axisBreaksLabeled@). This mark draws
-- __only the U-link segments__.
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Custom.Dendrogram
  ( -- * 焼き込みペイロード
    DendroSeg(..)
  , DendroPayload(..)
    -- * mark
  , dendrogramMark
  , drawDendro
    -- * PS registry で使う mark id
  , dendrogramMarkId
  ) where

import           Data.Aeson    (FromJSON (..), ToJSON (..))
import qualified Data.Aeson    as Aeson
import qualified Data.Char     as Char
import           Data.Text     (Text)
import qualified Data.Vector   as V
import           GHC.Generics  (Generic)

import           Graphics.Hgg.Primitive (Point (..), Primitive (..), solid)
import           Graphics.Hgg.Spec      (ColRef (ColNum), Layer, RenderCtx (..),
                                        customMarkWith, encX, encY)

-- | [日本語]: dendrogram の 1 線分 (data 座標)。 x = 葉 slot / node 中点、 y = マージ高 (height)。
--   JSON キーは prefix @seg@ を落とす: @x1/y1/x2/y2/color/width@。
--   [English]: A single dendrogram segment (in data coordinates). x is the
--   leaf slot / node midpoint, y is the merge height. The JSON keys drop the
--   @seg@ prefix: @x1/y1/x2/y2/color/width@.
data DendroSeg = DendroSeg
  { segX1    :: !Double
  , segY1    :: !Double
  , segX2    :: !Double
  , segY2    :: !Double
  , segColor :: !Text
  , segWidth :: !Double
  } deriving (Show, Eq, Generic)

segOptions :: Aeson.Options
segOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = lowerFirst . drop 3 }

instance ToJSON DendroSeg where
  toJSON     = Aeson.genericToJSON segOptions
  toEncoding = Aeson.genericToEncoding segOptions
instance FromJSON DendroSeg where
  parseJSON  = Aeson.genericParseJSON segOptions

-- | [日本語]: dendrogram 全体の焼き込みペイロード。 線分列 + 軸 range (葉方向 / height 方向)。
--   JSON キーは prefix @dp@ を落とす: @segments/xRange/yRange@ (range は @[lo,hi]@ 配列)。
--   [English]: The baked payload for the whole dendrogram: the segment list
--   plus the axis ranges (leaf direction / height direction). The JSON keys
--   drop the @dp@ prefix: @segments/xRange/yRange@ (each range is a
--   @[lo,hi]@ array).
data DendroPayload = DendroPayload
  { dpSegments :: ![DendroSeg]
  , dpXRange   :: !(Double, Double)
    -- ^ [日本語]: 葉方向 range (例 -0.6 .. n-0.4)。
    --   [English]: Leaf-direction range (e.g. -0.6 .. n-0.4).
  , dpYRange   :: !(Double, Double)
    -- ^ [日本語]: height 方向 range (例 0 .. maxH*1.05)。
    --   [English]: Height-direction range (e.g. 0 .. maxH*1.05).
  } deriving (Show, Eq, Generic)

payloadOptions :: Aeson.Options
payloadOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = lowerFirst . drop 2 }

instance ToJSON DendroPayload where
  toJSON     = Aeson.genericToJSON payloadOptions
  toEncoding = Aeson.genericToEncoding payloadOptions
instance FromJSON DendroPayload where
  parseJSON  = Aeson.genericParseJSON payloadOptions

lowerFirst :: String -> String
lowerFirst (c : cs) = Char.toLower c : cs
lowerFirst []       = []

-- | [日本語]: PS registry (canvas) と共有する安定 mark id。
--   [English]: The stable mark id shared with the PS registry (canvas).
dendrogramMarkId :: Text
dendrogramMarkId = "dendrogram"

-- | [日本語]: dendrogram を 'Layer' として返す (= 普通の mark 同様 @layer (...)@ に入れて使う)。
--   線分を @cmOptions@ に焼き込み、 'encX'/'encY' で軸 range を束ねる (不可視 anchor 不要)。
--   [English]: Returns the dendrogram as a 'Layer' (used inside @layer (...)@
--   just like an ordinary mark). Bakes the segments into @cmOptions@ and
--   binds the axis range via 'encX'/'encY' (no invisible anchor needed).
dendrogramMark :: DendroPayload -> Layer
dendrogramMark p =
     customMarkWith dendrogramMarkId (toJSON p) (drawDendro p)
  <> encX (rangeCol (dpXRange p))
  <> encY (rangeCol (dpYRange p))
  where
    rangeCol (lo, hi) = ColNum (V.fromList [lo, hi])

-- | [日本語]: payload の線分を 'RenderCtx' で proj して 'PLine' を emit する draw 関数
--   (HS closure が源。 PS registry も同型の draw を手登録する = parity)。
--   [English]: The draw function that projects the payload's segments via
--   'RenderCtx' and emits 'PLine's. The HS closure is the source of truth;
--   the PS registry hand-registers an equivalent draw function (for parity).
drawDendro :: DendroPayload -> RenderCtx -> [Primitive]
drawDendro p ctx =
  [ PLine (proj (segX1 s) (segY1 s)) (proj (segX2 s) (segY2 s))
          (solid (segColor s) (segWidth s))
  | s <- dpSegments p ]
  where
    proj x y = uncurry Point (rcProjectXY ctx x y)

-- |
-- Module      : Hgg.Plot.Spec.CustomMark
-- Description : custom mark の payload 型 (RenderCtx / CustomMark、 Phase 51)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Hgg.Plot.Spec' の module 分割で切り出した leaf。 Phase 51 の
-- custom mark 拡張点のうち **型** ('RenderCtx' / 'CustomMark') のみを持つ
-- (smart constructor 'customMark' 等は 'Hgg.Plot.Spec.Constructors' 側)。
-- 依存は 'Hgg.Plot.Spec.Column' ('Resolver') と 'Hgg.Plot.Primitive'。
-- 公開 API は従来どおり 'Hgg.Plot.Spec' (facade) が re-export する。
-- 挙動・出力 (JSON 形含む) は完全に不変。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Spec.CustomMark
  ( RenderCtx(..)
  , CustomMark(..)
  ) where

import           Data.Aeson      (FromJSON, ToJSON, toJSON, parseJSON,
                                  Value)
import qualified Data.Aeson      as Aeson
import           Data.Text       (Text)

import           Hgg.Plot.Primitive (Primitive, Rect (..))
import           Hgg.Plot.Spec.Column (Resolver)

-- ===========================================================================
-- Phase 51: custom mark (拡張可能な描画語彙)
-- ===========================================================================

-- | custom mark の draw closure に渡す描画文脈。 backend 非依存。 scale 適用済の
-- projection・plot 領域 (px)・データ resolver・theme 既定色を提供する。 これと
-- ("Hgg.Plot.Render" が re-export する) 'Primitive' 構築子が custom mark の
-- authoring API。
data RenderCtx = RenderCtx
  { rcProjectXY :: !(Double -> Double -> (Double, Double))  -- ^ データ座標 (x,y) → device px
  , rcPlotArea  :: !Rect                                    -- ^ plot 描画領域 (px)
  , rcResolver  :: !Resolver                                -- ^ 列名 → データ (layer 束縛列を引く)
  , rcColor     :: !Text                                    -- ^ theme 既定の線/点色
  , rcFill      :: !Text                                    -- ^ theme 既定の塗り色
  , rcTextColor :: !Text                                    -- ^ theme 既定の文字色
  , rcAxisColor :: !Text                                    -- ^ theme 既定の軸色
  }

-- | custom mark の payload。 'lyCustom' に載る。
--
--   * 'cmDraw' は HS の描画 closure。 データは closure に閉じ込め可。 __serialize 不能__
--     ゆえ JSON では落ち、 decode 時は no-op (@const []@) に戻る。 PS は 'cmId' で自前
--     registry を引いて描く (parity 手登録)。
--   * 'cmOptions' は PS へ渡す必要のある serializable option (任意)。
--
-- 'Eq' / 'Show' は closure を無視し 'cmId' + 'cmOptions' で比較 (function は比較不能ゆえ)。
data CustomMark = CustomMark
  { cmId      :: !Text                        -- ^ 安定 mark 識別子 (PS dispatch の鍵・serialize される)
  , cmOptions :: !Value                       -- ^ PS へ渡す option (JSON・任意)
  , cmDraw    :: !(RenderCtx -> [Primitive])  -- ^ HS 描画 closure (JSON 非対象)
  }

instance Show CustomMark where
  show cm = "CustomMark " <> show (cmId cm)

instance Eq CustomMark where
  a == b = cmId a == cmId b && cmOptions a == cmOptions b

-- closure は落とし 'cmId' + 'cmOptions' のみ serialize。
instance ToJSON CustomMark where
  toJSON cm = Aeson.object [ "cmId" Aeson..= cmId cm, "cmOptions" Aeson..= cmOptions cm ]

-- decode では closure を復元できないので no-op に戻す (HS は live 値を使い、 PS は registry)。
instance FromJSON CustomMark where
  parseJSON = Aeson.withObject "CustomMark" $ \o ->
    CustomMark <$> o Aeson..:  "cmId"
               <*> o Aeson..:? "cmOptions" Aeson..!= Aeson.Null
               <*> pure (const [])


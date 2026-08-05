-- |
-- Module      : Graphics.Hgg.Spec.CustomMark
-- Description : Payload types for custom marks (RenderCtx / CustomMark)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'Graphics.Hgg.Spec' の module 分割で切り出した leaf。 custom mark
-- 拡張点のうち __型__ ('RenderCtx' / 'CustomMark') のみを持つ (smart
-- constructor @customMark@ 等は 'Graphics.Hgg.Spec.Constructors' 側)。 依存は
-- 'Graphics.Hgg.Spec.Column' ('Resolver') と 'Graphics.Hgg.Primitive'。 公開
-- API は従来どおり 'Graphics.Hgg.Spec' (facade) が re-export する。 挙動・出力
-- (JSON 形含む) は完全に不変。
--
-- [English]: A leaf split out of 'Graphics.Hgg.Spec' during its module
-- split. Carries only the __types__ ('RenderCtx' / 'CustomMark') for the
-- custom-mark extension point (the smart constructor @customMark@ and
-- friends live in 'Graphics.Hgg.Spec.Constructors'). Depends only on
-- 'Graphics.Hgg.Spec.Column' ('Resolver') and 'Graphics.Hgg.Primitive'. The
-- public API is still re-exported by the 'Graphics.Hgg.Spec' facade as
-- before; behavior and output (including the JSON shape) are completely
-- unchanged.
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Spec.CustomMark
  ( RenderCtx(..)
  , CustomMark(..)
  ) where

import           Data.Aeson      (FromJSON, ToJSON, toJSON, parseJSON,
                                  Value)
import qualified Data.Aeson      as Aeson
import           Data.Text       (Text)

import           Graphics.Hgg.Primitive (Primitive, Rect (..))
import           Graphics.Hgg.Spec.Column (Resolver)

-- ===========================================================================
-- Phase 51: custom mark (拡張可能な描画語彙)
-- ===========================================================================

-- | [日本語]: custom mark の draw closure に渡す描画文脈。 backend 非依存。
--   scale 適用済の projection・plot 領域 (px)・データ resolver・theme 既定色を
--   提供する。 これと ("Graphics.Hgg.Render" が re-export する) 'Primitive'
--   構築子が custom mark の authoring API。
--   [English]: The drawing context passed to a custom mark's draw closure.
--   Backend-agnostic. Supplies the scale-applied projection, the plot area
--   (px), the data resolver, and the theme's default colors. Together with
--   the 'Primitive' constructors (re-exported by "Graphics.Hgg.Render"),
--   this forms the custom-mark authoring API.
data RenderCtx = RenderCtx
  { rcProjectXY :: !(Double -> Double -> (Double, Double))  -- ^ [日本語]: データ座標 (x,y) → device px
                                                             --   [English]: Converts data coordinates (x,y) to device px.
  , rcPlotArea  :: !Rect                                    -- ^ [日本語]: plot 描画領域 (px)
                                                             --   [English]: The plot drawing area (px).
  , rcResolver  :: !Resolver                                -- ^ [日本語]: 列名 → データ (layer 束縛列を引く)
                                                             --   [English]: Column name to data (looks up columns bound by the layer).
  , rcColor     :: !Text                                    -- ^ [日本語]: theme 既定の線/点色
                                                             --   [English]: The theme's default line/point color.
  , rcFill      :: !Text                                    -- ^ [日本語]: theme 既定の塗り色
                                                             --   [English]: The theme's default fill color.
  , rcTextColor :: !Text                                    -- ^ [日本語]: theme 既定の文字色
                                                             --   [English]: The theme's default text color.
  , rcAxisColor :: !Text                                    -- ^ [日本語]: theme 既定の軸色
                                                             --   [English]: The theme's default axis color.
  }

-- | [日本語]: custom mark の payload。 @lyCustom@ に載る。
--
--     * 'cmDraw' は HS の描画 closure。 データは closure に閉じ込め可。
--       __serialize 不能__ ゆえ JSON では落ち、 decode 時は no-op (@const []@)
--       に戻る。 PS は 'cmId' で自前 registry を引いて描く (parity 手登録)。
--     * 'cmOptions' は PS へ渡す必要のある serializable option (任意)。
--
--   'Eq' / 'Show' は closure を無視し 'cmId' + 'cmOptions' で比較 (function は
--   比較不能ゆえ)。
--   [English]: The payload of a custom mark, carried in @lyCustom@.
--
--     * 'cmDraw' is the Haskell draw closure. Data may be captured inside
--       the closure, so it is __not serializable__: it is dropped from
--       JSON, and decoding restores a no-op (@const []@). PS draws by
--       looking up its own registry via 'cmId' (manually registered for
--       parity).
--     * 'cmOptions' is the serializable option (optional) that needs to be
--       passed to PS.
--
--   'Eq' / 'Show' ignore the closure and compare by 'cmId' + 'cmOptions'
--   (since functions cannot be compared).
data CustomMark = CustomMark
  { cmId      :: !Text                        -- ^ [日本語]: 安定 mark 識別子 (PS dispatch の鍵・serialize される)
                                                --   [English]: The stable mark identifier (the key for PS dispatch; serialized).
  , cmOptions :: !Value                       -- ^ [日本語]: PS へ渡す option (JSON・任意)
                                               --   [English]: The option passed to PS (JSON, optional).
  , cmDraw    :: !(RenderCtx -> [Primitive])  -- ^ [日本語]: HS 描画 closure (JSON 非対象)
                                               --   [English]: The Haskell draw closure (not part of the JSON).
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


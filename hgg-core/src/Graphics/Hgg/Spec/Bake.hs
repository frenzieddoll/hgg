-- |
-- Module      : Graphics.Hgg.Spec.Bake
-- Description : Resolver の焼き込み (ColByName → inline 解決、 Phase 8 B16)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Graphics.Hgg.Spec' の module 分割で切り出し。 spec 内の全
-- 'ColByName' を 'Resolver' で inline ('ColNum' / 'ColTxt') 化する 'bakeSpec'
-- を持つ。 'VisualSpec' / 'Layer' 全体を走査する sink (被参照ゼロ・Phase 55 A1
-- 実測) ゆえ分割 module 群の最後段。 公開 API は従来どおり 'Graphics.Hgg.Spec'
-- (facade) が re-export する。 挙動・出力は完全に不変。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Spec.Bake
  ( bakeSpec
  ) where

import           Graphics.Hgg.Spec.Column
import           Graphics.Hgg.Spec.Layer (Layer (..))
import           Graphics.Hgg.Spec.Mark (ColorEnc (..))
import           Graphics.Hgg.Spec.Visual (VisualSpec (..))

-- ===========================================================================
-- Resolver の焼き込み (Phase 8 B16): ColByName を inline (ColNum/ColTxt) に解決
-- ===========================================================================
-- PS canvas backend は Resolver を持たず spec JSON だけ受け取るため、 ColByName
-- (列名参照) のままだと PS で解決できず描画されない (= pairs/facet/legend が空)。
-- JSON 出力前に bakeSpec で全 ColRef を inline 化すると PS でも描ける。

-- | ColByName を Resolver で解決し ColNum/ColTxt に置換 (解決不能なら元のまま)。
bakeColRef :: Resolver -> ColRef -> ColRef
bakeColRef r cr@(ColByName n) = case r n of
  Just (NumData v) -> ColNum v
  Just (TxtData v) -> ColTxt v
  Nothing          -> cr
bakeColRef _ cr = cr

bakeColorEnc :: Resolver -> ColorEnc -> ColorEnc
bakeColorEnc r (ColorByCol cr)        = ColorByCol (bakeColRef r cr)
-- Phase 9 A-5 fix: ColorByContinuous も inline 化しないと PS (emptyResolver) で色も
-- legend も出ない (= legend-continuous で発覚)。
bakeColorEnc r (ColorByContinuous cr) = ColorByContinuous (bakeColRef r cr)
bakeColorEnc _ ce                     = ce

bakeLayer :: Resolver -> Layer -> Layer
bakeLayer r l = l
  { lyEncX    = bakeColRef r <$> lyEncX l
  , lyEncY    = bakeColRef r <$> lyEncY l
  , lyEncY2   = bakeColRef r <$> lyEncY2 l
  , lyErrorX  = bakeColRef r <$> lyErrorX l
  , lyErrorY  = bakeColRef r <$> lyErrorY l
  , lyChain   = bakeColRef r <$> lyChain l
  , lyShapeBy = bakeColRef r <$> lyShapeBy l
  , lySizeBy  = bakeColRef r <$> lySizeBy l
  , lyAlphaBy = bakeColRef r <$> lyAlphaBy l
  , lyLinetypeBy = bakeColRef r <$> lyLinetypeBy l
  , lyLabel   = bakeColRef r <$> lyLabel l
  , lyColor   = bakeColorEnc r <$> lyColor l
  , lyOverlay = map (bakeLayer r) (lyOverlay l)   -- ★ Phase 36 D2: sub-mark の inline 列も bake
  }

-- | spec 内の全 ColByName を Resolver で inline 化 (layers + facet + subplots 再帰)。
-- JSON 出力前に呼ぶと PS でも Resolver 不要で描ける。
bakeSpec :: Resolver -> VisualSpec -> VisualSpec
bakeSpec r spec = spec
  { vsLayers   = map (bakeLayer r) (vsLayers spec)
  , vsFacet    = bakeColRef r <$> vsFacet spec
  , vsFacetRow = bakeColRef r <$> vsFacetRow spec
  , vsFacetCol = bakeColRef r <$> vsFacetCol spec
  , vsSubplots = map (bakeSpec r) (vsSubplots spec)
  }

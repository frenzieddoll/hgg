-- |
-- Module      : Hgg.Plot.Palette
-- Description : Categorical / Sequential / Diverging palette カタログ
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
--   P17 (2026-05-26 改訂):
--     * default `hggMain` = F-3 Balanced Mix (= 中程度彩度で重み均等)
--     * sub `hggPastel`   = F-2 Pastel Mix (= 淡色 secondary)
--     * 全色は 7 キャラ設定画 Color Palette セクションの公式 hex
{-# LANGUAGE OverloadedStrings #-}

module Hgg.Plot.Palette
  ( CategoricalPalette
  , SequentialPalette
  , DivergingPalette
  , okabeIto
  , viridis5
  , spotfire           -- 標準 UI default palette
  , hggMain        -- F-3 (default)
  , hggMainVivid   -- F-1
  , hggPastel      -- F-2
  , ggplotHue          -- ggplot2 hue_pal (= n 依存 HCL 等間隔)
  , hggExtended
  , whiteRabbit
  , cheshireCat
  , dormouse
  , marchHare
  , queenOfHearts
  , whiteQueen
  , redQueen
  , whiteRabbitSeq
  , cheshireCatSeq
  , dormouseSeq
  , marchHareSeq
  , queenOfHeartsSeq
  , whiteQueenSeq
  , redQueenSeq
  , hggBlueSeq
  , hggRedSeq
  , hggGoldSeq
  , hggSageSeq
  , hggSiennaSeq
  , hggPurpleSeq
  , hggRoseSeq
  , hggDivQueens
  , hggDivYellowPurple
  , hggDivPinkGreen
  , hggCyclical5
  , hggWarn
  , hggHighlight
  , hggSuccess
  , hggInfo
    -- * ColorBrewer 2.0 palette (Phase 6 A9、 P17)
    -- $colorbrewer
    -- ** Categorical (qualitative)
  , brewerSet1
  , brewerSet2
  , brewerSet3
  , brewerPaired
  , brewerDark2
    -- ** Diverging
  , brewerRdYlBu
  , brewerRdBu
  , brewerSpectral
  , brewerPuOr
  , brewerBrBG
  , brewerRdGy
  , brewerPiYG
  ) where

import Data.Text (Text)

type CategoricalPalette = [Text]
type SequentialPalette = [Text]
type DivergingPalette = [Text]

okabeIto :: CategoricalPalette
okabeIto =
  [ "#E69F00", "#56B4E9", "#009E73", "#F0E442"
  , "#0072B2", "#D55E00", "#CC79A7", "#000000" ]

viridis5 :: SequentialPalette
viridis5 = ["#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725"]

-- | Spotfire 風 (= 標準 UI default colorPalette と一致)。
spotfire :: CategoricalPalette
spotfire =
  [ "#93c5fd", "#fca5a5", "#fde047", "#86efac", "#f9a8d4"
  , "#67e8f9", "#fdba74", "#e5e5e5"
  , "#2563eb", "#dc2626", "#ca8a04", "#16a34a"
  , "#db2777", "#0891b2", "#ea580c"
  ]

-- ★ F-3 Balanced (default)
hggMain :: CategoricalPalette
hggMain =
  [ "#C9A968", "#7A5C92", "#B79DB8", "#3E6A6F"
  , "#C7445D", "#B8C7D9", "#7E1F23" ]

-- F-1 Vivid Main
hggMainVivid :: CategoricalPalette
hggMainVivid =
  [ "#C9A968", "#5B2E7C", "#8A6B4F", "#2A4E54"
  , "#A61E2A", "#C7D7E6", "#5D0F12" ]

-- ★ F-2 Pastel Mix
hggPastel :: CategoricalPalette
hggPastel =
  [ "#F0A5A0", "#C0B8E6", "#B79DB8", "#A7D7DE"
  , "#D7A1A6", "#C7D7E6", "#A45353" ]

-- | ggplot2 既定 discrete パレット (= @scales::hue_pal()@)。
-- HCL 色空間で等間隔 hue (L=65, C=100, hue = seq(15,375,length=n+1)[1:n])。
-- 色数 n に依存して hue が再配分されるため、 R @hue_pal()(n)@ の出力を n=1..8 で
-- テーブル化 (= 実行時 HCL→sRGB 変換を避ける)。 n>8 は 8 色版を循環、 n<1 は 8 色版。
ggplotHue :: Int -> CategoricalPalette
ggplotHue n
  | n <= 0    = ggplotHue8
  | n == 1    = ["#F8766D"]
  | n == 2    = ["#F8766D", "#00BFC4"]
  | n == 3    = ["#F8766D", "#00BA38", "#619CFF"]
  | n == 4    = ["#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"]
  | n == 5    = ["#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"]
  | n == 6    = ["#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"]
  | n == 7    = ["#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7"]
  | n == 8    = ggplotHue8
  | otherwise = take n (cycle ggplotHue8)
  where
    ggplotHue8 =
      [ "#F8766D", "#CD9600", "#7CAE00", "#00BE67"
      , "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC" ]

hggExtended :: CategoricalPalette
hggExtended =
  [ "#C9A968", "#2A3050", "#D63A4A"   -- WR
  , "#5B2E7C", "#C7A666", "#7A5C92"   -- CC
  , "#8A6B4F", "#B79DB8", "#F2E8C9"   -- DM
  , "#2A4E54", "#B08B4A", "#7A4A2E"   -- MH
  , "#A61E2A", "#C7A46A", "#3B263B"   -- QH
  , "#C7D7E6", "#B8C7D9", "#C9CDD3"   -- WQ
  , "#5D0F12", "#C3A46E", "#4A3322"   -- RQ
  ]

-- 各キャラ 4 色 (= 公式 hex のみ)
whiteRabbit   = [ "#C9A968", "#2A3050", "#D63A4A", "#F0A5A0" ] :: CategoricalPalette
cheshireCat   = [ "#5B2E7C", "#C7A666", "#C0B8E6", "#7A5C92" ] :: CategoricalPalette
dormouse      = [ "#8A6B4F", "#B79DB8", "#F2E8C9", "#C9A968" ] :: CategoricalPalette
marchHare     = [ "#2A4E54", "#B08B4A", "#7A4A2E", "#A7D7DE" ] :: CategoricalPalette
queenOfHearts = [ "#A61E2A", "#C7A46A", "#3B263B", "#C7445D" ] :: CategoricalPalette
whiteQueen    = [ "#C7D7E6", "#B8C7D9", "#C9CDD3", "#DDE8F2" ] :: CategoricalPalette
redQueen      = [ "#5D0F12", "#C3A46E", "#4A3322", "#7E1F23" ] :: CategoricalPalette

-- 各キャラ sequential (= 公式 hex 由来)
whiteRabbitSeq, cheshireCatSeq, dormouseSeq, marchHareSeq
  , queenOfHeartsSeq, whiteQueenSeq, redQueenSeq :: SequentialPalette
whiteRabbitSeq   = ["#F2E8D0", "#E8E5DD", "#C9A968", "#8B6F3A", "#2A3050"]
whiteQueenSeq    = ["#FBFCFD", "#E6EEF6", "#DDE8F2", "#C9CDD3", "#B8C7D9"]
cheshireCatSeq   = ["#E6E3EA", "#C0B8E6", "#C0C0F0", "#7A5C92", "#5B2E7C"]
dormouseSeq      = ["#FBF6EE", "#F3E6C6", "#F2E8C9", "#C9A968", "#8A6B4F"]
marchHareSeq     = ["#F8F6F1", "#F3E7D6", "#A7D7DE", "#3E6A6F", "#2A4E54"]
queenOfHeartsSeq = ["#F7F4F1", "#D7A1A6", "#C7445D", "#A61E2A", "#6E0F23"]
redQueenSeq      = ["#F5EFE6", "#C3A46E", "#A45353", "#7E1F23", "#5D0F12"]

-- 汎用 sequential (= ColorBrewer 風)
hggBlueSeq, hggRedSeq, hggGoldSeq, hggSageSeq
  , hggSiennaSeq, hggPurpleSeq, hggRoseSeq :: SequentialPalette
hggBlueSeq   = ["#C0C8D5", "#7A839A", "#2A3050", "#1A1F38", "#0F1424"]
hggRedSeq    = ["#F4C7CD", "#E68390", "#D63A4A", "#A02838", "#6E1A28"]
hggGoldSeq   = ["#F0E3BD", "#DDC68C", "#C9A968", "#8B6F3A", "#5A4520"]
hggSageSeq   = ["#C8D3C0", "#92A689", "#5A7958", "#3D573D", "#253726"]
hggSiennaSeq = ["#F0C9AC", "#DD9B6F", "#C76A3A", "#8B4523", "#5A2A13"]
hggPurpleSeq = ["#C4B3CE", "#8E68A8", "#5B2E7C", "#3F1F58", "#25113A"]
hggRoseSeq   = ["#F2C9D1", "#DD8A9B", "#C7445D", "#8E2D44", "#561A2A"]

-- Diverging (= 公式 hex)
hggDivQueens :: DivergingPalette
hggDivQueens =
  [ "#5D0F12", "#7E1F23", "#A45353"
  , "#F5EFE6"
  , "#DDE8F2", "#B8C7D9", "#C7D7E6"
  ]

hggDivYellowPurple :: DivergingPalette
hggDivYellowPurple =
  [ "#C9A968", "#DDC68C", "#F0E3BD"
  , "#F2E8D0"
  , "#C4B3CE", "#8E68A8", "#5B2E7C" ]

hggDivPinkGreen :: DivergingPalette
hggDivPinkGreen =
  [ "#C7445D", "#DD8A9B", "#F2C9D1"
  , "#F2E8D0"
  , "#C8D3C0", "#92A689", "#5A7958" ]

hggCyclical5 :: CategoricalPalette
hggCyclical5 =
  [ "#C9A968", "#2A4E54", "#3A4A66", "#5B2E7C", "#5D0F12" ]

hggWarn, hggHighlight, hggSuccess, hggInfo :: Text
hggWarn      = "#D63A4A"
hggHighlight = "#F0A5A0"
hggSuccess   = "#6BB07A"
hggInfo      = "#8AA0BA"

-- ===========================================================================
-- ColorBrewer 2.0 (= Cynthia Brewer、 Penn State、 Apache 2.0 license)
-- ===========================================================================

-- $colorbrewer
-- ColorBrewer は地図・科学可視化向け 35 palette のセット。
-- ここでは categorical 5 種 + diverging 7 種を import (= 9-class が中心)。
-- 公式: <https://colorbrewer2.org/>
-- License: Apache 2.0 (= attribution required)

-- | Categorical Set1 (9-class)。 強い primary 色、 区別性高い。
brewerSet1 :: CategoricalPalette
brewerSet1 =
  [ "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"
  , "#FFFF33", "#A65628", "#F781BF", "#999999" ]

-- | Categorical Set2 (8-class)。 やや pastel、 印刷に向く。
brewerSet2 :: CategoricalPalette
brewerSet2 =
  [ "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"
  , "#FFD92F", "#E5C494", "#B3B3B3" ]

-- | Categorical Set3 (12-class)。 多 categorical に向く、 薄め。
brewerSet3 :: CategoricalPalette
brewerSet3 =
  [ "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3"
  , "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD"
  , "#CCEBC5", "#FFED6F" ]

-- | Paired (12-class)。 light/dark のペア (= 2 グループ × 6 色)。
brewerPaired :: CategoricalPalette
brewerPaired =
  [ "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99"
  , "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"
  , "#FFFF99", "#B15928" ]

-- | Dark2 (8-class)。 dark 系、 強い contrast。
brewerDark2 :: CategoricalPalette
brewerDark2 =
  [ "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"
  , "#E6AB02", "#A6761D", "#666666" ]

-- | Diverging RdYlBu (9-class、 中心 #FFFFBF)。 赤 ↔ 黄 ↔ 青。
brewerRdYlBu :: DivergingPalette
brewerRdYlBu =
  [ "#D73027", "#F46D43", "#FDAE61", "#FEE090"
  , "#FFFFBF"
  , "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4" ]

-- | Diverging RdBu (9-class)。 赤 ↔ 青、 中央 #F7F7F7。
brewerRdBu :: DivergingPalette
brewerRdBu =
  [ "#B2182B", "#D6604D", "#F4A582", "#FDDBC7"
  , "#F7F7F7"
  , "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC" ]

-- | Diverging Spectral (11-class)。 虹 (= 赤→橙→黄→緑→青→紫)、 中心 #FFFFBF。
brewerSpectral :: DivergingPalette
brewerSpectral =
  [ "#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B"
  , "#FFFFBF"
  , "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2" ]

-- | Diverging PuOr (9-class)。 紫 ↔ 橙、 中央 #F7F7F7。
brewerPuOr :: DivergingPalette
brewerPuOr =
  [ "#B35806", "#E08214", "#FDB863", "#FEE0B6"
  , "#F7F7F7"
  , "#D8DAEB", "#B2ABD2", "#8073AC", "#542788" ]

-- | Diverging BrBG (9-class)。 茶 ↔ 緑、 中央 #F5F5F5。
brewerBrBG :: DivergingPalette
brewerBrBG =
  [ "#8C510A", "#BF812D", "#DFC27D", "#F6E8C3"
  , "#F5F5F5"
  , "#C7EAE5", "#80CDC1", "#35978F", "#01665E" ]

-- | Diverging RdGy (9-class)。 赤 ↔ 灰、 中央 #FFFFFF。
brewerRdGy :: DivergingPalette
brewerRdGy =
  [ "#B2182B", "#D6604D", "#F4A582", "#FDDBC7"
  , "#FFFFFF"
  , "#E0E0E0", "#BABABA", "#878787", "#4D4D4D" ]

-- | Diverging PiYG (9-class)。 ピンク ↔ 黄緑、 中央 #F7F7F7。
brewerPiYG :: DivergingPalette
brewerPiYG =
  [ "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF"
  , "#F7F7F7"
  , "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221" ]

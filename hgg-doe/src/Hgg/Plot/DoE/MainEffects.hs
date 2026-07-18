-- |
-- Module      : Hgg.Plot.DoE.MainEffects
-- Description : 主効果プロット
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 各 factor (= 1 categorical 列) の level ごとに response (= numeric 列) の
-- mean を縦軸に line で結ぶ。 grand mean を水平線で示す。
-- 複数 factor 指定時は subplots で横並び。
module Hgg.Plot.DoE.MainEffects
  ( mainEffects
  , mainEffectsWith
  ) where

import qualified Data.Text as T

import Hgg.Plot.Spec

-- | 単一 factor の主効果。
-- |
-- > mainEffects (col "temp") (col "yield")
mainEffects :: ColRef -> ColRef -> VisualSpec
mainEffects factor response =
  purePlot
    <> layer (line factor response <> stroke 2.0)
    <> layer (statMean response)  -- grand mean horizontal
    <> xLabel (colRefName factor)
    <> yLabel ("mean " <> colRefName response)
    <> title ("Main effect of " <> colRefName factor)

-- | 複数 factor の主効果を横並び panel で。
mainEffectsWith :: [ColRef] -> ColRef -> VisualSpec
mainEffectsWith factors response =
  let panels = [ mainEffects f response | f <- factors ]
      titleText = "Main effects (" <> T.intercalate ", " (map colRefName factors) <> ")"
  in subplots panels <> title titleText

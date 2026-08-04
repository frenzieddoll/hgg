-- |
-- Module      : Graphics.Hgg.DoE.MainEffects
-- Description : Main effects plot
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 各 factor (= 1 categorical 列) の level ごとに response (= numeric 列) の
-- mean を縦軸に line で結ぶ。 grand mean を水平線で示す。
-- 複数 factor 指定時は subplots で横並び。
--
-- [English]: For each factor (a single categorical column), connects the mean
-- of the response (a numeric column) per level with a line on the vertical
-- axis, and shows the grand mean as a horizontal line. When several factors
-- are given, they are laid out side by side as subplots.
module Graphics.Hgg.DoE.MainEffects
  ( mainEffects
  , mainEffectsWith
  ) where

import qualified Data.Text as T

import Graphics.Hgg.Spec

-- | [日本語]: 単一 factor の主効果。
--   [English]: The main effect for a single factor.
--
-- > mainEffects (col "temp") (col "yield")
mainEffects :: ColRef -> ColRef -> VisualSpec
mainEffects factor response =
  purePlot
    <> layer (line factor response <> stroke 2.0)
    <> layer (statMean response)  -- grand mean horizontal
    <> xLabel (colRefName factor)
    <> yLabel ("mean " <> colRefName response)
    <> title ("Main effect of " <> colRefName factor)

-- | [日本語]: 複数 factor の主効果を横並び panel で。
--   [English]: The main effects for multiple factors, side by side as panels.
mainEffectsWith :: [ColRef] -> ColRef -> VisualSpec
mainEffectsWith factors response =
  let panels = [ mainEffects f response | f <- factors ]
      titleText = "Main effects (" <> T.intercalate ", " (map colRefName factors) <> ")"
  in subplots panels <> title titleText

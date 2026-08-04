-- |
-- Module      : Graphics.Hgg.DoE.ResponseSurface
-- Description : 応答曲面プロット
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 2 連続因子 (x, y) と response (z) で binned heatmap (= contour) を描画。
-- core の 'contour' helper を semantic に wrap (= "応答曲面" 概念を明示)。
--
-- [English]: Draws a binned heatmap (a contour plot) from two continuous
-- factors (x, y) and the response (z). Wraps core's 'contour' helper
-- semantically, making the "response surface" concept explicit.
module Graphics.Hgg.DoE.ResponseSurface
  ( responseSurface
  ) where

import Graphics.Hgg.Spec

responseSurface :: ColRef -> ColRef -> ColRef -> VisualSpec
responseSurface factor1 factor2 response =
  purePlot
    <> layer (contour factor1 factor2 response)
    <> xLabel (colRefName factor1)
    <> yLabel (colRefName factor2)
    <> title ("Response surface: " <> colRefName response
                 <> " ~ " <> colRefName factor1
                 <> " × " <> colRefName factor2)

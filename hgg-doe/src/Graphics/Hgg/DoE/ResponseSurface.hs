-- |
-- Module      : Graphics.Hgg.DoE.ResponseSurface
-- Description : 応答曲面プロット
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 2 連続因子 (x, y) と response (z) で binned heatmap (= contour) を描画。
-- core の 'contour' helper を semantic に wrap (= "応答曲面" 概念を明示)。
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

-- |
-- Module      : Graphics.Hgg.DoE.Interaction
-- Description : Two-factor interaction plot
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: factor1 (x cat) と factor2 (color) の組合せごとに response の mean を
-- line で結ぶ。 factor2 の各 level が 1 本の line として色分け表示。
--
-- [English]: Connects the mean of the response with a line for each
-- combination of factor1 (the categorical x axis) and factor2 (color). Every
-- level of factor2 is shown as its own color-coded line.
module Graphics.Hgg.DoE.Interaction
  ( interaction
  ) where

import Graphics.Hgg.Spec

interaction :: ColRef -> ColRef -> ColRef -> VisualSpec
interaction factor1 factor2 response =
  purePlot
    <> layer (line factor1 response <> colorBy factor2 <> stroke 2.0)
    <> xLabel (colRefName factor1)
    <> yLabel ("mean " <> colRefName response)
    <> title ("Interaction: " <> colRefName factor1
                 <> " × " <> colRefName factor2)

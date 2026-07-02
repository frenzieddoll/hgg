-- | Hgg.Plot.DoE.Interaction — 2 因子交互作用プロット。
-- |
-- | factor1 (x cat) と factor2 (color) の組合せごとに response の mean を
-- | line で結ぶ。 factor2 の各 level が 1 本の line として色分け表示。
module Hgg.Plot.DoE.Interaction
  ( interaction
  ) where

import Hgg.Plot.Spec

interaction :: ColRef -> ColRef -> ColRef -> VisualSpec
interaction factor1 factor2 response =
  purePlot
    <> layer (line factor1 response <> colorBy factor2 <> stroke 2.0)
    <> xLabel (colRefName factor1)
    <> yLabel ("mean " <> colRefName response)
    <> title ("Interaction: " <> colRefName factor1
                 <> " × " <> colRefName factor2)

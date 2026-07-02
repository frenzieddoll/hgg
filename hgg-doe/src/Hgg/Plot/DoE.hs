-- | Hgg.Plot.DoE — DoE (Design of Experiments) chart helpers の集約 re-export。
-- |
-- | Phase 26 S5-e (2026-05-25): C+ 案。 基盤は hgg-core (= MContour /
-- |   vsSubplots) に置き、 ここでは DoE の意味論 (factor / level / response) を
-- |   持つ薄い helper のみ。
module Hgg.Plot.DoE
  ( module Hgg.Plot.DoE.MainEffects
  , module Hgg.Plot.DoE.Interaction
  , module Hgg.Plot.DoE.ResponseSurface
  ) where

import Hgg.Plot.DoE.MainEffects
import Hgg.Plot.DoE.Interaction
import Hgg.Plot.DoE.ResponseSurface

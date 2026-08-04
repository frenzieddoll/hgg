-- |
-- Module      : Graphics.Hgg.DoE
-- Description : DoE (Design of Experiments) chart helpers の集約 re-export
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: C+ 案。 基盤は hgg-core (= MContour / vsSubplots) に置き、
-- ここでは DoE の意味論 (factor / level / response) を持つ薄い helper のみ。
--
-- [English]: The C+ approach. The foundations live in hgg-core
-- (@MContour@ / @vsSubplots@); this module holds only the thin helpers that
-- carry DoE semantics (factor / level / response).
module Graphics.Hgg.DoE
  ( module Graphics.Hgg.DoE.MainEffects
  , module Graphics.Hgg.DoE.Interaction
  , module Graphics.Hgg.DoE.ResponseSurface
  ) where

import Graphics.Hgg.DoE.MainEffects
import Graphics.Hgg.DoE.Interaction
import Graphics.Hgg.DoE.ResponseSurface

-- |
-- Module      : Graphics.Hgg.DoE
-- Description : DoE (Design of Experiments) chart helpers の集約 re-export
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 26 S5-e (2026-05-25): C+ 案。 基盤は hgg-core (= MContour /
-- |   vsSubplots) に置き、 ここでは DoE の意味論 (factor / level / response) を
--   持つ薄い helper のみ。
module Graphics.Hgg.DoE
  ( module Graphics.Hgg.DoE.MainEffects
  , module Graphics.Hgg.DoE.Interaction
  , module Graphics.Hgg.DoE.ResponseSurface
  ) where

import Graphics.Hgg.DoE.MainEffects
import Graphics.Hgg.DoE.Interaction
import Graphics.Hgg.DoE.ResponseSurface

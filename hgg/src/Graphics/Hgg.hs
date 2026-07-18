-- |
-- Module      : Graphics.Hgg
-- Description : Batteries-included entry point (Easy API + grammar + df binding + SVG save)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- One import for the whole default experience:
--
-- @
-- import Graphics.Hgg
--
-- main :: IO ()
-- main = quickScatter "scatter.svg" [1,2,3,4,5] [1,4,9,16,25]
-- @
--
-- Re-exports, thinnest to richest:
--
-- * "Graphics.Hgg.Quick" — one-line IO save helpers ('quickScatter',
--   'quickPlot', …), which itself re-exports the Easy API
--   ("Graphics.Hgg.Easy": 'points', 'overlay', …) and the full grammar API
--   ("Graphics.Hgg.Spec": 'scatter', 'layer', 'title', …).
-- * "Graphics.Hgg.Frame" — the @df '|>>' spec@ binding ('PlotData',
--   'BoundPlot').
-- * "Graphics.Hgg.Backend.SVG" — 'saveSVG', 'saveSVGBound' and friends.
-- * "Graphics.Hgg.Unit" — physical lengths ('px', 'mm', '*~') for
--   'widthUnit' \/ 'heightUnit' and friends.
--
-- Other backends (PDF \/ PNG \/ LaTeX \/ 3D) are not re-exported here;
-- enable them with the package's manual cabal flags (@pdf@ \/ @png@ \/
-- @latex@ \/ @3d@) and import e.g. "Graphics.Hgg.Backend.PDF" directly.
module Graphics.Hgg
  ( -- * Easy API + grammar (via Quick)
    module Graphics.Hgg.Quick
    -- * DataFrame binding
  , module Graphics.Hgg.Frame
    -- * SVG output
  , module Graphics.Hgg.Backend.SVG
    -- * Units
  , module Graphics.Hgg.Unit
  ) where

import           Graphics.Hgg.Backend.SVG
import           Graphics.Hgg.Frame
import           Graphics.Hgg.Quick
import           Graphics.Hgg.Unit

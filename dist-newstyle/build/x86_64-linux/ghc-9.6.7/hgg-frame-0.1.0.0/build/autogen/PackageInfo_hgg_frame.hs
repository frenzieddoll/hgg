{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hgg_frame (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hgg_frame"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "DataFrame \25277\35937 (class PlotData) + df |>> spec \12496\12452\12531\12489 for hgg"
copyright :: String
copyright = "2026 Hgg"
homepage :: String
homepage = ""

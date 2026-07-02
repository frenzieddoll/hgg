{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hgg_svg (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hgg_svg"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "SVG backend for hgg (\32020 Haskell\12289 OSS \12398\20027\29992\36884)"
copyright :: String
copyright = "2026 Hgg"
homepage :: String
homepage = ""

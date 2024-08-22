{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}

module PackageInfo_fretboard_thoery
  ( name,
    version,
    synopsis,
    copyright,
    homepage,
  )
where

import Data.Version (Version (..))
import Prelude

name :: String
name = "fretboard_thoery"

version :: Version
version = Version [0, 1, 0, 0] []

synopsis :: String
synopsis = ""

copyright :: String
copyright = ""

homepage :: String
homepage = ""

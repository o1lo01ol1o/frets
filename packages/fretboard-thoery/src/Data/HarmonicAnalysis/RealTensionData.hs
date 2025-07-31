{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.RealTensionData
  ( -- * Tension Table Data from Java Reference
    tonalityTensionDefault,
    modeTensionMajorMinor,
    modeTensionGreekModes,
    functionTensionRiemann,
    functionTensionDiatonic,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Default tonality tension table from Java reference.
-- Row 0: Circle of fifths distances (0, 2, 4, 6, 8, 10, 12)
-- Row 1: Circle of fourths distances (0, 2, 4, 6, 8, 10, 12)
tonalityTensionDefault :: Vector (Vector Double)
tonalityTensionDefault =
  V.fromList $
    map
      V.fromList
      [ -- Circle of fifths row
        [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0],
        -- Circle of fourths row
        [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0]
      ]

-- | Mode tension for "Major, Minor" configuration.
-- Rows/Cols: 0=Major, 1=Minor
modeTensionMajorMinor :: Vector (Vector Double)
modeTensionMajorMinor =
  V.fromList $
    map
      V.fromList
      [ [0.0, 2.0],
        [1.5, 0.0]
      ]

-- | Mode tension for "All 7 Greek Modes" configuration.
-- Rows/Cols: 0=Ionian..6=Locrian
modeTensionGreekModes :: Vector (Vector Double)
modeTensionGreekModes =
  V.fromList $
    map
      V.fromList
      [ [0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
        [1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0],
        [1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0],
        [1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0],
        [1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0],
        [1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0],
        [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0]
      ]

-- | Function tension for "T,S,D" (Riemann) configuration.
-- We assume T=0, S=1, D=2 for this table.
functionTensionRiemann :: Vector (Vector Double)
functionTensionRiemann =
  V.fromList $
    map
      V.fromList
      [ -- T     S     D
        [0.0, 1.5, 0.5], -- from T
        [-0.2, 0.0, 0.8], -- from S
        [-0.5, 0.2, 0.0] -- from D
      ]

-- | Function tension for "All 7 Degrees" configuration.
-- Rows/Cols: 0=Tonic..6=Leading Tone
functionTensionDiatonic :: Vector (Vector Double)
functionTensionDiatonic =
  V.fromList $
    map
      V.fromList
      [ --    T     ST    M     S     D     SM    LT
        [0.0, 1.5, 1.5, 1.5, 0.5, 1.5, 0.5], -- from T
        [-0.2, 1.0, 1.0, 1.0, 0.8, 1.0, 0.8], -- from ST
        [-0.2, 1.0, 1.0, 1.0, 0.8, 1.0, 0.8], -- from M
        [-0.2, 1.0, 1.0, 1.0, 0.8, 1.0, 0.8], -- from S
        [-0.5, 0.2, 0.2, 0.2, 1.0, 1.0, 0.2], -- from D
        [-0.2, 0.0, 0.0, 0.0, 0.8, 0.8, 0.0], -- from SM
        [-0.5, 0.2, 0.2, 0.2, 0.0, 0.2, 1.0] -- from LT
      ]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.HarmonicAnalysis.Tension
  ( -- * Core Tension Functions
    transitionWeight,
    tonalDistance,
    modalDistance,
    functionalDistance,

    -- * Tension Table Construction
    makeTensionTable,
    defaultTonalTensions,
    defaultModalTensions,
    defaultFunctionalTensions,
  )
where

import Control.Monad (guard)
import Data.HarmonicAnalysis.Types
import Data.Mod
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

-- | Calculate transition weight between two matrix positions
-- Uses formula from paper: exp(-|tonalDist + modalDist + funcDist|)
transitionWeight ::
  TensionTable ->
  -- | Source position
  (Row, Col) ->
  -- | Target position
  (Row, Col) ->
  Double
transitionWeight TensionTable {..} (row1, Col c1) (row2, Col c2) =
  let tDist = tonalDistance tonalTension c1 c2
      (mode1, func1) = rowToModeFunction numFunctions row1
      (mode2, func2) = rowToModeFunction numFunctions row2
      mDist = modalDistance modalTension mode1 mode2
      fDist = functionalDistance functionalTension func1 func2
      !totalDist = tDist + mDist + fDist
   in exp (negate $ abs totalDist)
  where
    numFunctions = fromIntegral $ V.length functionalTension

-- | Calculate tonal distance between two tonalities
tonalDistance :: Vector (Vector Double) -> Mod 12 -> Mod 12 -> Double
tonalDistance tensions t1 t2 =
  let diff = unMod (t2 - t1)
      dist = lookupTension tensions (fromIntegral diff)
   in dist * dist * signum dist -- Square and preserve sign

-- | Calculate modal distance between two modes
modalDistance :: Vector (Vector Double) -> Mode -> Mode -> Double
modalDistance tensions m1 m2 =
  let dist = lookupTension tensions (fromEnum m1) (fromEnum m2)
   in dist * dist * signum dist

-- | Calculate functional distance between two functions
functionalDistance :: Vector (Vector Double) -> Function -> Function -> Double
functionalDistance tensions f1 f2 =
  let dist = lookupTension tensions (fromEnum f1) (fromEnum f2)
   in dist * dist * signum dist

-- | Safely lookup tension value from table
lookupTension :: Vector (Vector Double) -> Int -> Int -> Double
lookupTension tensions i j =
  case tensions V.!? i >>= (V.!? j) of
    Just v -> v
    Nothing -> 0.0

-- | Create a tension table with provided values
makeTensionTable ::
  -- | Tonal tensions
  [(Int, Int, Double)] ->
  -- | Modal tensions
  [(Int, Int, Double)] ->
  -- | Functional tensions
  [(Int, Int, Double)] ->
  TensionTable
makeTensionTable tonal modal functional =
  TensionTable
    { tonalTension = makeSymmetricMatrix 12 tonal,
      modalTension = makeSymmetricMatrix (fromEnum (maxBound :: Mode) + 1) modal,
      functionalTension = makeSymmetricMatrix (fromEnum (maxBound :: Function) + 1) functional
    }

-- | Create a symmetric matrix from (row,col,value) triples
makeSymmetricMatrix :: Int -> [(Int, Int, Double)] -> Vector (Vector Double)
makeSymmetricMatrix size entries =
  V.generate size $ \i ->
    V.generate size $ \j ->
      case lookup (i, j) valueMap of
        Just v -> v
        Nothing -> case lookup (j, i) valueMap of
          Just v -> v
          Nothing -> 0.0
  where
    valueMap = [((r, c), v) | (r, c, v) <- entries]

-- | Default tonal tension values based on circle of fifths distances
defaultTonalTensions :: [(Int, Int, Double)]
defaultTonalTensions =
  -- Positive values for fifth relationships
  [ (0, 7, 0.8), -- Perfect fifth
    (0, 5, 0.6), -- Perfect fourth
    -- Negative values for tritone and distant relationships
    (0, 6, -0.9), -- Tritone
    (0, 8, -0.7), -- Minor sixth
    (0, 9, -0.5) -- Major sixth
    -- Other relationships filled by symmetry
  ]

-- | Default modal tension values
defaultModalTensions :: [(Int, Int, Double)]
defaultModalTensions =
  [ (fromEnum Major, fromEnum Minor, 0.7),
    (fromEnum Major, fromEnum Mixolydian, 0.8),
    (fromEnum Minor, fromEnum Dorian, 0.8),
    (fromEnum Minor, fromEnum Phrygian, 0.7),
    -- More distant relationships are negative
    (fromEnum Major, fromEnum Locrian, -0.9)
  ]

-- | Default functional tension values
defaultFunctionalTensions :: [(Int, Int, Double)]
defaultFunctionalTensions =
  [ (fromEnum Tonic, fromEnum Dominant, 0.9),
    (fromEnum Tonic, fromEnum Subdominant, 0.8),
    (fromEnum Dominant, fromEnum Tonic, 0.9),
    (fromEnum Subdominant, fromEnum Dominant, 0.7),
    -- Negative values for distant functions
    (fromEnum Tonic, fromEnum LeadingTone, -0.8),
    (fromEnum Mediant, fromEnum LeadingTone, -0.7)
  ]

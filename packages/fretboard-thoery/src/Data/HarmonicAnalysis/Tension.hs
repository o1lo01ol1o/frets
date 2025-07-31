{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.HarmonicAnalysis.Tension
  ( -- * Core Tension Functions
    transitionWeight,
    tonalDistance,
    modalDistance,
    functionalDistance,
    lookupTension,

    -- * Tension Table Construction
    makeDefaultTensionTable,
    makeTSDTensionTable,
    makeDiatonicTensionTable,
    makeMajorMinorTensionTable,
  )
where

import Data.HarmonicAnalysis.RealTensionData
import Data.HarmonicAnalysis.Types
import Data.Mod
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Calculate transition weight between two matrix positions
-- Uses formula from paper: exp(-|tonalDist + modalDist + funcDist|)
transitionWeight ::
  TensionTable ->
  -- | Source position
  (Row, Col) ->
  -- | Target position
  (Row, Col) ->
  Double
transitionWeight TensionTable {..} (Row row1, Col c1) (Row row2, Col c2) =
  let tDist = tonalDistance tonalTension c1 c2
      (mode1, func1) = rowToModeFunction row1
      (mode2, func2) = rowToModeFunction row2
      mDist = modalDistance modalTension mode1 mode2
      fDist = functionalDistance functionalTension func1 func2
      !totalDist = tDist + mDist + fDist
   in exp (negate $ abs totalDist)

-- | Calculate tonal distance between two tonalities
-- Matches Java implementation using circle of fifths/fourths
tonalDistance :: Vector (Vector Double) -> Mod 12 -> Mod 12 -> Double
tonalDistance tensions t1 t2 =
  let tDiff = posMod (fromIntegral (unMod t2) - fromIntegral (unMod t1)) 12

      -- Try to find tDiff in circle of fifths (row 0) or fourths (row 1)
      findInCircle rowIdx circleStep =
        let findInRow colIdx currentDiff
              | colIdx >= V.length (tensions V.! 0) = Nothing
              | currentDiff == tDiff =
                  case tensions V.!? rowIdx >>= (V.!? colIdx) of
                    Just val -> Just val
                    Nothing -> Nothing
              | otherwise =
                  let nextDiff = posMod (currentDiff + circleStep) 12
                   in findInRow (colIdx + 1) nextDiff
         in findInRow 0 0

      -- Try circle of fifths first (row 0, step +7), then fourths (row 1, step +5)
      result = case findInCircle 0 7 of
        Just val -> val
        Nothing -> case findInCircle 1 5 of
          Just val -> val
          Nothing -> 0.0
   in result * result * signum result

-- | Positive modulus helper (matches Java posMod)
posMod :: Int -> Int -> Int
posMod i n =
  let r = i `mod` n
   in if r < 0 then r + n else r

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

-- | Create a default tension table using the real data from the Java reference.
-- This uses the "All 7 Greek Modes" and "All 7 Degrees" configurations as default.
makeDefaultTensionTable :: TensionTable
makeDefaultTensionTable =
  TensionTable
    { tonalTension = tonalityTensionDefault,
      modalTension = modeTensionGreekModes,
      functionalTension = functionTensionDiatonic
    }

-- | Create a tension table for TSD (Tonic, Subdominant, Dominant) analysis.
-- Uses Major/Minor modes and T-S-D functions.
makeTSDTensionTable :: TensionTable
makeTSDTensionTable =
  TensionTable
    { tonalTension = tonalityTensionDefault,
      modalTension = modeTensionMajorMinor,
      functionalTension = functionTensionRiemann
    }

-- | Create a tension table for full diatonic analysis.
-- Uses all 7 Greek modes and all 7 diatonic functions.
makeDiatonicTensionTable :: TensionTable
makeDiatonicTensionTable =
  TensionTable
    { tonalTension = tonalityTensionDefault,
      modalTension = modeTensionGreekModes,
      functionalTension = functionTensionDiatonic
    }

-- | Create a tension table for Major/Minor analysis with TSD functions.
-- Simplified analysis with just major/minor modes and T-S-D functions.
makeMajorMinorTensionTable :: TensionTable
makeMajorMinorTensionTable =
  TensionTable
    { tonalTension = tonalityTensionDefault,
      modalTension = modeTensionMajorMinor,
      functionalTension = functionTensionRiemann
    }

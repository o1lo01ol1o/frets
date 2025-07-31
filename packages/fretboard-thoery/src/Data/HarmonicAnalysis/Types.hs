{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.HarmonicAnalysis.Types
-- Description : Core types for Neo-Riemannian harmonic analysis
-- Copyright   : (c) 2024
-- License     : BSD3
--
-- This module provides the fundamental types needed for harmonic analysis using
-- Neo-Riemannian theory. The analysis works by:
--
-- 1. Constructing Riemann matrices for each chord
-- 2. Computing transition weights between matrix cells
-- 3. Finding optimal paths through these matrices using Viterbi algorithm
--
-- The optimal path represents the most "natural" harmonic interpretation of
-- a chord progression.
module Data.HarmonicAnalysis.Types
  ( -- * Core Analysis Types
    RMPoint (..),
    HarmonicPath (..),
    HarmonicAnalysisResult (..),
    RiemannMatrix (..),

    -- * Runtime Configuration
    RuntimeConfig (..),
    makeRuntimeConfig,

    -- * Matrix Indices
    Row (..),
    Col (..),

    -- * Harmonic Components
    Mode (..),
    Function (..),
    ChainOfThirds (..),

    -- * Transition Weights
    TensionTable (..),

    -- * Index Conversion
    rowToModeFunction,
    modeFunctionToRow,
    rowToModeFunctionForConfig,
    modeFunctionToRowForConfig,
    rowToModeFunctionWithConfig,
    modeFunctionToRowWithConfig,

    -- * Configuration Functions
    numModesForConfig,
    numFunctionsForConfig,
    modesForConfig0,
    modesForConfig1,
    modesForConfig2,
    modesForConfig3,
    functionsForConfig0,
    functionsForConfig1,
    functionsForConfig2,
    functionsForConfig3,

    -- * Matrix Construction
    emptyRiemannMatrix,
    emptyRiemannMatrixForConfig,
    emptyRiemannMatrixWithConfig,
    defaultTensionTable,
    numModes,
    numFunctions,
    numTonalities,

    -- * Backward Compatibility
    major,
    minor,
  )
where

import Chord
import Control.Parallel.Strategies (NFData)
import Data.Mod
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import Modulation hiding (Aeolian, Dorian, Ionian, Locrian, Lydian, Mixolydian, Phrygian, chord)

-- | Row index in a Riemann matrix, encoding both mode and function
newtype Row = Row {unRow :: Int}
  deriving (Eq, Show, Ord, Generic, NFData, Num)

-- | Column index in a Riemann matrix, representing tonality
newtype Col = Col {unCol :: Mod 12}
  deriving (Eq, Show, Ord, Generic, NFData)

-- | A point in the Riemann Matrix representing a specific harmonic interpretation
-- of a pitch set. The interpretation includes the pitch set's tonality (col) and its
-- combined mode/function (row).
data RMPoint = RMPoint
  { -- | Position in sequence of matrices
    matrixIndex :: Int,
    -- | Row index (mode + function)
    row :: Row,
    -- | Column index (tonality)
    col :: Col,
    -- | Weight/strength of interpretation
    value :: Double
  }
  deriving (Eq, Show, Generic)

-- | A complete harmonic analysis, consisting of a sequence of interpretations
-- forming an optimal path through the Riemann matrices.
newtype HarmonicPath = HarmonicPath
  { getPath :: [RMPoint]
  }
  deriving (Eq, Show, Generic)

-- | Result of harmonic analysis that may contain multiple equally weighted candidates
data HarmonicAnalysisResult = HarmonicAnalysisResult
  { -- | All paths with maximum weight (may be multiple for ties)
    candidatePaths :: [HarmonicPath],
    -- | The maximum weight achieved by all candidates
    maxWeight :: Double,
    -- | Whether there were ties (multiple equally weighted paths)
    hasTies :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Available modes for harmonic analysis.
-- Supports both the simple Major/Minor system and full 7 Greek modes.
data Mode
  = -- | Ionian mode (standard major scale) - Java mode 0
    Ionian
  | -- | Dorian mode - Java mode 1
    Dorian
  | -- | Phrygian mode - Java mode 2
    Phrygian
  | -- | Lydian mode - Java mode 3
    Lydian
  | -- | Mixolydian mode - Java mode 4
    Mixolydian
  | -- | Aeolian mode (natural minor scale) - Java mode 5
    Aeolian
  | -- | Locrian mode - Java mode 6
    Locrian
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, NFData)

-- | Backward compatibility aliases
type Major = Ionian

type Minor = Aeolian

-- | Helper functions for backward compatibility
major :: Mode
major = Ionian

minor :: Mode
minor = Aeolian

-- | Configuration-specific mode selectors for Java compatibility
modesForConfig0 :: [Mode]
modesForConfig0 = [Ionian, Aeolian] -- Major, Minor

modesForConfig1 :: [Mode]
modesForConfig1 = [Ionian, Aeolian] -- Major, Minor

modesForConfig2 :: [Mode]
modesForConfig2 = [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian] -- All 7 Greek modes

modesForConfig3 :: [Mode]
modesForConfig3 = [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian] -- All 7 Greek modes

-- | Harmonic functions representing a chord's role in a progression.
-- Supports both the simple T-S-D system and full 7 diatonic functions.
data Function
  = -- | Main tonal center (I) - Java function 0
    Tonic
  | -- | Supertonic function (ii) - Java function 1
    Supertonic
  | -- | Mediant function (iii) - Java function 2
    Mediant
  | -- | Fourth-related function (IV) - Java function 3
    Subdominant
  | -- | Fifth-related function (V) - Java function 4
    Dominant
  | -- | Submediant function (vi) - Java function 5
    Submediant
  | -- | Leading tone function (vii°) - Java function 6
    LeadingTone
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, NFData)

-- | Configuration-specific function selectors for Java compatibility
functionsForConfig0 :: [Function]
functionsForConfig0 = [Tonic, Subdominant, Dominant] -- T-S-D

functionsForConfig1 :: [Function]
functionsForConfig1 = [Tonic, Supertonic, Mediant, Subdominant, Dominant, Submediant, LeadingTone] -- All 7 functions

functionsForConfig2 :: [Function]
functionsForConfig2 = [Tonic, Subdominant, Dominant] -- T-S-D

functionsForConfig3 :: [Function]
functionsForConfig3 = [Tonic, Supertonic, Mediant, Subdominant, Dominant, Submediant, LeadingTone] -- All 7 functions

-- | Representation of a chord's structure as a chain of thirds.
-- This is used in Neo-Riemannian analysis to identify standard
-- harmonic structures.
newtype ChainOfThirds = ChainOfThirds
  { -- | Pitch classes in the chain
    chainPitches :: Set (Mod 12)
  }
  deriving (Eq, Show, Ord, Generic, NFData)

-- | Matrix representing all possible harmonic interpretations of a chord.
-- Each cell contains a weight indicating the strength of that interpretation.
data RiemannMatrix = RiemannMatrix
  { -- | Number of possible tonalities
    colCount :: Int,
    -- | Number of mode/function pairs
    rowCount :: Int,
    -- | Weights matrix
    matrix :: Vector (Vector (Maybe Double))
  }
  deriving (Eq, Show, Generic, NFData)

-- | Runtime configuration parameters for harmonic analysis
-- Follows the Java reference pattern of passing dimensions and tables as parameters
data RuntimeConfig = RuntimeConfig
  { -- | Number of modes in this configuration
    configNumModes :: Int,
    -- | Number of functions in this configuration
    configNumFunctions :: Int,
    -- | Number of tonalities (usually 12 for chromatic system)
    configNumTonalities :: Int,
    -- | Modes available in this configuration
    configModes :: [Mode],
    -- | Functions available in this configuration
    configFunctions :: [Function],
    -- | Tension tables for this configuration
    configTensionTable :: TensionTable,
    -- | Weight table for this configuration
    configWeightTable :: Vector Double
  }
  deriving (Eq, Show, Generic, NFData)

-- | Tables storing transition weights between different harmonic aspects.
-- These weights are used to evaluate the "naturalness" of harmonic progressions.
data TensionTable = TensionTable
  { -- | Between tonalities
    tonalTension :: Vector (Vector Double),
    -- | Between modes
    modalTension :: Vector (Vector Double),
    -- | Between functions
    functionalTension :: Vector (Vector Double)
  }
  deriving (Eq, Show, Generic, NFData)

-- | Get the number of modes available (legacy - returns all 7 modes)
numModes :: Int
numModes = fromEnum (maxBound :: Mode) + 1

-- | Get the number of functions available (legacy - returns all 7 functions)
numFunctions :: Int
numFunctions = fromEnum (maxBound :: Function) + 1

-- | Configuration-specific dimension functions
numModesForConfig :: Int -> Int
numModesForConfig 0 = 2 -- Major, Minor
numModesForConfig 1 = 2 -- Major, Minor
numModesForConfig 2 = 7 -- All 7 Greek modes
numModesForConfig 3 = 7 -- All 7 Greek modes
numModesForConfig _ = 2 -- Default to simple system

numFunctionsForConfig :: Int -> Int
numFunctionsForConfig 0 = 3 -- T-S-D
numFunctionsForConfig 1 = 7 -- All 7 functions
numFunctionsForConfig 2 = 3 -- T-S-D
numFunctionsForConfig 3 = 7 -- All 7 functions
numFunctionsForConfig _ = 3 -- Default to T-S-D

-- | Get the number of tonalities (always 12 for chromatic system)
numTonalities :: Int
numTonalities = 12

-- | Create a runtime configuration from basic parameters
makeRuntimeConfig ::
  -- | Number of modes
  Int ->
  -- | Number of functions
  Int ->
  -- | Available modes
  [Mode] ->
  -- | Available functions
  [Function] ->
  -- | Tension table
  TensionTable ->
  -- | Weight table
  Vector Double ->
  RuntimeConfig
makeRuntimeConfig numModes numFunctions modes functions tensionTable weightTable =
  RuntimeConfig
    { configNumModes = numModes,
      configNumFunctions = numFunctions,
      configNumTonalities = 12, -- Always 12 for chromatic system
      configModes = modes,
      configFunctions = functions,
      configTensionTable = tensionTable,
      configWeightTable = weightTable
    }

-- | Convert a row index to its constituent mode and function (legacy version)
rowToModeFunction :: Int -> (Mode, Function)
rowToModeFunction row =
  let mode = toEnum $ row `div` numFunctions
      function = toEnum $ row `mod` numFunctions
   in (mode, function)

-- | Convert a row index to mode and function using runtime configuration
rowToModeFunctionWithConfig :: RuntimeConfig -> Int -> (Mode, Function)
rowToModeFunctionWithConfig config row =
  let numFuncs = configNumFunctions config
      modes = configModes config
      functions = configFunctions config
      modeIdx = row `div` numFuncs
      funcIdx = row `mod` numFuncs
      mode = if modeIdx < length modes then modes !! modeIdx else head modes
      function = if funcIdx < length functions then functions !! funcIdx else head functions
   in (mode, function)

-- | Convert a row index to mode and function for specific configuration
rowToModeFunctionForConfig :: Int -> Int -> (Mode, Function)
rowToModeFunctionForConfig configNum row =
  let numFuncs = numFunctionsForConfig configNum
      modeIdx = row `div` numFuncs
      funcIdx = row `mod` numFuncs
      modes = case configNum of
        0 -> modesForConfig0
        1 -> modesForConfig1
        2 -> modesForConfig2
        3 -> modesForConfig3
        _ -> modesForConfig0
      functions = case configNum of
        0 -> functionsForConfig0
        1 -> functionsForConfig1
        2 -> functionsForConfig2
        3 -> functionsForConfig3
        _ -> functionsForConfig0
      mode = if modeIdx < length modes then modes !! modeIdx else head modes
      function = if funcIdx < length functions then functions !! funcIdx else head functions
   in (mode, function)

-- | Convert a mode and function to the corresponding row index
-- | Convert mode and function to matrix row index (legacy version)
modeFunctionToRow :: Mode -> Function -> Row
modeFunctionToRow mode function =
  Row $
    fromEnum mode * numFunctions
      + fromEnum function

-- | Convert mode and function to row index using runtime configuration
modeFunctionToRowWithConfig :: RuntimeConfig -> Mode -> Function -> Row
modeFunctionToRowWithConfig config mode function =
  let modes = configModes config
      functions = configFunctions config
      numFuncs = configNumFunctions config
      modeIdx = case findInList mode (zip modes [0 ..]) of
        Just idx -> idx
        Nothing -> 0
      funcIdx = case findInList function (zip functions [0 ..]) of
        Just idx -> idx
        Nothing -> 0
   in Row $ modeIdx * numFuncs + funcIdx

-- | Convert mode and function to row index for specific configuration
modeFunctionToRowForConfig :: Int -> Mode -> Function -> Row
modeFunctionToRowForConfig configNum mode function =
  let modes = case configNum of
        0 -> modesForConfig0
        1 -> modesForConfig1
        2 -> modesForConfig2
        3 -> modesForConfig3
        _ -> modesForConfig0
      functions = case configNum of
        0 -> functionsForConfig0
        1 -> functionsForConfig1
        2 -> functionsForConfig2
        3 -> functionsForConfig3
        _ -> functionsForConfig0
      modeIdx = case findInList mode (zip modes [0 ..]) of
        Just idx -> idx
        Nothing -> 0 -- Default to first mode
      funcIdx = case findInList function (zip functions [0 ..]) of
        Just idx -> idx
        Nothing -> 0 -- Default to first function
      numFuncs = length functions
   in Row $ modeIdx * numFuncs + funcIdx

-- Helper function for finding items in association lists
findInList :: (Eq a) => a -> [(a, b)] -> Maybe b
findInList _ [] = Nothing
findInList key ((k, v) : rest)
  | key == k = Just v
  | otherwise = findInList key rest

-- | Create an empty Riemann matrix (legacy version)
emptyRiemannMatrix :: RiemannMatrix
emptyRiemannMatrix =
  let numRows = numModes * numFunctions
      numCols = numTonalities
   in RiemannMatrix
        { rowCount = numRows,
          colCount = numCols,
          matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
        }

-- | Create an empty Riemann matrix using runtime configuration
emptyRiemannMatrixWithConfig :: RuntimeConfig -> RiemannMatrix
emptyRiemannMatrixWithConfig config =
  let numRows = configNumModes config * configNumFunctions config
      numCols = configNumTonalities config
   in RiemannMatrix
        { rowCount = numRows,
          colCount = numCols,
          matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
        }

-- | Create an empty Riemann matrix for specific configuration
emptyRiemannMatrixForConfig :: Int -> RiemannMatrix
emptyRiemannMatrixForConfig configNum =
  let numRows = numModesForConfig configNum * numFunctionsForConfig configNum
      numCols = numTonalities
   in RiemannMatrix
        { rowCount = numRows,
          colCount = numCols,
          matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
        }

-- | Create a tension table with default (zero) values
defaultTensionTable :: TensionTable
defaultTensionTable =
  TensionTable
    { tonalTension = V.replicate numTonalities (V.replicate numTonalities 0.0),
      modalTension = V.replicate numModes (V.replicate numModes 0.0),
      functionalTension = V.replicate numFunctions (V.replicate numFunctions 0.0)
    }

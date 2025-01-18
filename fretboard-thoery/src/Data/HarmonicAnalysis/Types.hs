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
    RiemannMatrix (..),

    -- * Matrix Indices
    Row (..),
    Col (..),

    -- * Harmonic Componentsd
    Mode (..),
    Function (..),
    ChainOfThirds (..),

    -- * Transition Weights
    TensionTable (..),

    -- * Index Conversion
    rowToModeFunction,
    modeFunctionToRow,

    -- * Matrix Construction
    emptyRiemannMatrix,
    defaultTensionTable,
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
import Modulation

-- | Row index in a Riemann matrix, encoding both mode and function
newtype Row = Row {unRow :: Word}
  deriving (Eq, Show, Ord, Generic, NFData, Num)

-- | Column index in a Riemann matrix, representing tonality
newtype Col = Col {unCol :: Mod 12}
  deriving (Eq, Show, Ord, Generic, NFData)

-- | A point in the Riemann Matrix representing a specific harmonic interpretation
-- of a chord. The interpretation includes the chord's tonality (col) and its
-- combined mode/function (row).
data RMPoint = RMPoint
  { -- | Position in sequence of matrices
    matrixIndex :: Word,
    -- | The chord being analyzed
    rmChord :: Chord,
    -- | Row index (mode + function)
    row :: Row,
    -- | Column index (tonality)
    col :: Col,
    -- | Weight/strength of interpretation
    value :: Double
  }
  deriving (Eq, Show, Ord, Generic, NFData)

-- | A complete harmonic analysis, consisting of a sequence of interpretations
-- forming an optimal path through the Riemann matrices.
newtype HarmonicPath = HarmonicPath
  { getPath :: [RMPoint]
  }
  deriving (Eq, Show, Generic, NFData)

-- | Available modes for harmonic analysis. These represent different
-- rotations of the diatonic scale.
data Mode
  = -- | Ionian mode (standard major scale)
    Major
  | -- | Aeolian mode (natural minor scale)
    Minor
  | -- | Minor scale with raised 6th
    Dorian
  | -- | Minor scale with lowered 2nd
    Phrygian
  | -- | Major scale with raised 4th
    Lydian
  | -- | Major scale with lowered 7th
    Mixolydian
  | -- | Minor scale with lowered 2nd and 5th
    Locrian
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, NFData)

-- | Harmonic functions representing a chord's role in a progression
data Function
  = -- | Main tonal center (I)
    Tonic
  | -- | Fourth-related function (IV)
    Subdominant
  | -- | Fifth-related function (V)
    Dominant
  | -- | Third-related function (III)
    Mediant
  | -- | Sixth-related function (VI)
    Submediant
  | -- | Seventh-related function (VII)
    Subtonic
  | -- | Leading tone function (vii)
    LeadingTone
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, NFData)

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
  { -- | Chord being analyzed
    chord :: Chord,
    -- | Number of possible tonalities
    colCount :: Word,
    -- | Number of mode/function pairs
    rowCount :: Word,
    -- | Weights matrix
    matrix :: Vector (Vector (Maybe Double))
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

-- | Convert a row index to its constituent mode and function
rowToModeFunction :: Word -> Word -> (Mode, Function)
rowToModeFunction numFunctions row =
  let mode = toEnum $ fromIntegral $ row `div` numFunctions
      function = toEnum $ fromIntegral $ row `mod` numFunctions
   in (mode, function)

-- | Convert a mode and function to the corresponding row index
modeFunctionToRow :: Word -> Mode -> Function -> Row
modeFunctionToRow numFunctions mode function =
  Row $
    (fromIntegral (fromEnum mode) * numFunctions)
      + fromIntegral (fromEnum function)

-- | Create an empty Riemann matrix for a given chord
-- Initialize with Just 0.0 instead of 0.0
emptyRiemannMatrix :: Chord -> RiemannMatrix
emptyRiemannMatrix c =
  RiemannMatrix
    { chord = c,
      colCount = 12,
      rowCount =
        fromIntegral $
          (fromEnum (maxBound :: Mode) + 1)
            * (fromEnum (maxBound :: Function) + 1),
      matrix =
        V.replicate
          (fromIntegral $ rowCount defaultMatrix)
          (V.replicate 12 (Just 0.0))
    }

-- | Create a tension table with default (zero) values
defaultTensionTable :: TensionTable
defaultTensionTable =
  TensionTable
    { tonalTension = V.replicate 12 (V.replicate 12 0.0),
      modalTension =
        V.replicate
          (fromEnum (maxBound :: Mode) + 1)
          (V.replicate (fromEnum (maxBound :: Mode) + 1) 0.0),
      functionalTension =
        V.replicate
          (fromEnum (maxBound :: Function) + 1)
          (V.replicate (fromEnum (maxBound :: Function) + 1) 0.0)
    }

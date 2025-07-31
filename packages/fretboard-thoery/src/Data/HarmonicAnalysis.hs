{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis
  ( -- * Core Analysis
    harmonicAnalysis,
    harmonicAnalysisWithConfig,
    harmonicAnalysisWithRuntimeConfig,
    HarmonicAnalysisConfig (..),
    defaultConfig,

    -- * Musical Configuration Presets
    majorMinorTSDConfig,
    majorMinorDiatonicConfig,
    modalTSDConfig,
    modalDiatonicConfig,
    majorMinorTSDWeights,
    majorMinorDiatonicWeights,
    modalTSDWeights,
    modalDiatonicWeights,

    -- * Runtime Configuration Functions
    makeHarmonicConfig,
    makeMajorMinorTSDConfig,
    makeMajorMinorDiatonicConfig,
    makeModalTSDConfig,
    makeModalDiatonicConfig,

    -- * Convenient Analysis Functions
    analyzeMajorMinorTSD,
    analyzeMajorMinorDiatonic,
    analyzeModalTSD,
    analyzeModalDiatonic,

    -- * Multi-Candidate Analysis Functions
    analyzeMajorMinorTSDMultiCandidate,
    analyzeMajorMinorDiatonicMultiCandidate,
    analyzeModalTSDMultiCandidate,
    analyzeModalDiatonicMultiCandidate,

    -- * Windowed Path Finding
    windowedHarmonicAnalysis,
    windowedAnalyzeMajorMinorTSD,
    windowedAnalyzeMajorMinorDiatonic,
    windowedAnalyzeModalTSD,
    windowedAnalyzeModalDiatonic,

    -- * Windowed Configuration (re-exported)
    Windowed.WindowedConfig (..),
    Windowed.defaultWindowedConfig,

    -- * Extended Configuration Functions
    makeMajorMinorExtendedTensionTable,
    makeGreekModesTSDTensionTable,

    -- * Backward Compatibility
    defaultWeightTable,

    -- * Re-exports
    module Data.HarmonicAnalysis.Types,
  )
where

import Data.HarmonicAnalysis.ChainOfThirds
import Data.HarmonicAnalysis.PathFinding (viterbiPath, viterbiPathMultiCandidate, viterbiPathMultiCandidateWithConfig, viterbiPathWithConfig)
import Data.HarmonicAnalysis.RealTensionData (functionTensionDiatonic, functionTensionRiemann, modeTensionGreekModes, modeTensionMajorMinor, tonalityTensionDefault)
import Data.HarmonicAnalysis.RiemannMatrix
import Data.HarmonicAnalysis.RiemannMatrix (computeDirectWithConfig)
import Data.HarmonicAnalysis.Tension (makeDiatonicTensionTable, makeMajorMinorTensionTable)
import Data.HarmonicAnalysis.Types
import Data.HarmonicAnalysis.Types (HarmonicAnalysisResult (..))
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Configuration for harmonic analysis
data HarmonicAnalysisConfig = HarmonicAnalysisConfig
  { tensionTable :: TensionTable,
    -- | Weight-of-thirds table (1D indexing: row*12+col, double values)
    weightTable :: Vector Double
  }

-- | Default configuration using basic Major/Minor × T-S-D analysis
-- Major/Minor modes × Tonic/Subdominant/Dominant functions
defaultConfig :: HarmonicAnalysisConfig
defaultConfig = majorMinorTSDConfig

-- | Major/Minor modes with Tonic-Subdominant-Dominant functions
-- Simple harmonic analysis focusing on basic tonal functions (2×3 system)
majorMinorTSDConfig :: HarmonicAnalysisConfig
majorMinorTSDConfig =
  HarmonicAnalysisConfig
    { tensionTable = makeMajorMinorTensionTable,
      weightTable = majorMinorTSDWeights
    }

-- | Major/Minor modes with all seven diatonic functions
-- Full functional analysis with basic major/minor modes (2×7 system)
majorMinorDiatonicConfig :: HarmonicAnalysisConfig
majorMinorDiatonicConfig =
  HarmonicAnalysisConfig
    { tensionTable = makeMajorMinorExtendedTensionTable,
      weightTable = majorMinorDiatonicWeights
    }

-- | All seven Greek modes with Tonic-Subdominant-Dominant functions
-- Modal analysis with basic tonal functions (7×3 system)
modalTSDConfig :: HarmonicAnalysisConfig
modalTSDConfig =
  HarmonicAnalysisConfig
    { tensionTable = makeGreekModesTSDTensionTable,
      weightTable = modalTSDWeights
    }

-- | All seven Greek modes with all seven diatonic functions
-- Complete modal and functional harmonic analysis (7×7 system)
modalDiatonicConfig :: HarmonicAnalysisConfig
modalDiatonicConfig =
  HarmonicAnalysisConfig
    { tensionTable = makeDiatonicTensionTable,
      weightTable = modalDiatonicWeights
    }

-- | Weight table for Major/Minor modes × T-S-D functions
-- Size: 72 elements (2 modes × 3 functions × 12 pitch classes)
majorMinorTSDWeights :: Vector Double
majorMinorTSDWeights =
  V.fromList
    [ -- Major Tonic (row 0)
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Major Subdominant (row 1)
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Major Dominant (row 2)
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Minor Tonic (row 3)
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Minor Subdominant (row 4)
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      -- Minor Dominant (row 5)
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1
    ]

-- | Weight table for Major/Minor modes × all diatonic functions
-- Size: 168 elements (2 modes × 7 functions × 12 pitch classes)
majorMinorDiatonicWeights :: Vector Double
majorMinorDiatonicWeights =
  V.fromList
    [ -- Major mode functions (rows 0-6)
      -- Row 0: Major Tonic
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Row 1: Major Supertonic
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Row 2: Major Mediant
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Row 3: Major Subdominant
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Row 4: Major Dominant
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Row 5: Major Submediant
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      -- Row 6: Major Leading Tone
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Minor mode functions (rows 7-13)
      -- Row 7: Minor Tonic
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Row 8: Minor Supertonic
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Row 9: Minor Mediant
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Row 10: Minor Subdominant
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Row 11: Minor Dominant
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      -- Row 12: Minor Submediant
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Row 13: Minor Leading Tone
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1
    ]

-- | Weight table for all Greek modes × T-S-D functions
-- Size: 252 elements (7 modes × 3 functions × 12 pitch classes)
modalTSDWeights :: Vector Double
modalTSDWeights =
  V.fromList
    [ -- Ionian mode (Major)
      -- Tonic
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Subdominant
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Dominant
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Dorian mode
      -- Tonic
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Subdominant
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Dominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Phrygian mode
      -- Tonic
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      0,
      1,
      0,
      1,
      -- Subdominant
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      -- Dominant
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Lydian mode
      -- Tonic
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      -- Subdominant
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      -- Dominant
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      -- Mixolydian mode
      -- Tonic
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      -- Subdominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Dominant
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      -- Aeolian mode (Minor)
      -- Tonic
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Subdominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Dominant
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      -- Locrian mode
      -- Tonic
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Subdominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Dominant
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0
    ]

-- | Weight table for all Greek modes × all diatonic functions
-- Size: 588 elements (7 modes × 7 functions × 12 pitch classes)
modalDiatonicWeights :: Vector Double
modalDiatonicWeights =
  V.fromList
    [ -- Ionian mode (Major) - 7 functions × 12 pitch classes = 84 elements
      -- Tonic
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Supertonic
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      1,
      -- Mediant
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Subdominant
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Dominant
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Submediant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Leading Tone
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      0,
      1,
      0,
      1,
      -- Dorian mode - 84 elements
      -- Tonic
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      -- Supertonic
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Mediant
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      -- Subdominant
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      -- Dominant
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      -- Submediant
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      -- Leading Tone
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Phrygian mode - 84 elements
      -- Tonic
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      -- Supertonic
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Mediant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Subdominant
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      -- Dominant
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      -- Submediant
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      -- Leading Tone
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      -- Lydian mode - 84 elements
      -- Tonic
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      -- Supertonic
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Mediant
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      -- Subdominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Dominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Submediant
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      -- Leading Tone
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      -- Mixolydian mode - 84 elements
      -- Tonic
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      -- Supertonic
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      -- Mediant
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      -- Subdominant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Dominant
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      -- Submediant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Leading Tone
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Aeolian mode (Minor) - 84 elements
      -- Tonic
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      -- Supertonic
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      -- Mediant
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      -- Subdominant
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      -- Dominant
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      -- Submediant
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      -- Leading Tone
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      -- Locrian mode - 84 elements
      -- Tonic
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      -- Supertonic
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      -- Mediant
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      -- Subdominant
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      -- Dominant
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      -- Submediant
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      -- Leading Tone
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1
    ]

-- Additional tension table constructors for extended configurations
makeMajorMinorExtendedTensionTable :: TensionTable
makeMajorMinorExtendedTensionTable =
  TensionTable
    { tonalTension = tonalityTensionDefault,
      modalTension = modeTensionMajorMinor,
      functionalTension = functionTensionDiatonic
    }

makeGreekModesTSDTensionTable :: TensionTable
makeGreekModesTSDTensionTable =
  TensionTable
    { tonalTension = tonalityTensionDefault,
      modalTension = modeTensionGreekModes,
      functionalTension = functionTensionRiemann
    }

-- Backward compatibility aliases
defaultWeightTable :: Vector Double
defaultWeightTable = majorMinorTSDWeights

-- | Main harmonic analysis function (backward compatible)
harmonicAnalysis ::
  HarmonicAnalysisConfig ->
  -- | Input pitch set sequence
  [Set.Set (Mod 12)] ->
  -- | Analyzed harmonic path
  HarmonicPath
harmonicAnalysis config pitchSets = harmonicAnalysisWithConfig 0 config pitchSets

-- | Harmonic analysis using runtime configuration
harmonicAnalysisWithRuntimeConfig ::
  -- | Runtime configuration
  RuntimeConfig ->
  -- | Input pitch set sequence
  [Set.Set (Mod 12)] ->
  -- | Analyzed harmonic path
  HarmonicPath
harmonicAnalysisWithRuntimeConfig config pitchSets =
  let -- Generate chains of thirds for each pitch set
      chains = map findChainsOfThirds pitchSets

      -- Create Riemann matrices using runtime configuration
      matrices = zipWith (createMatrixWithConfig config) chains pitchSets

      -- Find optimal path using runtime configuration
      result = viterbiPathWithConfig config matrices
   in result
  where
    createMatrixWithConfig cfg chainList pitchSet =
      if null chainList
        then emptyRiemannMatrixWithConfig cfg
        else compute3rdChain cfg chainList pitchSet

-- | Configuration-aware harmonic analysis function
-- Uses specific Java configuration for matrix dimensions and weight interpretation
harmonicAnalysisWithConfig ::
  -- | Java configuration number (0-3)
  Int ->
  HarmonicAnalysisConfig ->
  -- | Input pitch set sequence
  [Set.Set (Mod 12)] ->
  -- | Analyzed harmonic path
  HarmonicPath
harmonicAnalysisWithConfig configNum config pitchSets =
  let -- Find chains of thirds for each pitch set
      chains = map findChainsOfThirds pitchSets

      -- Compute Riemann matrices with configuration-specific dimensions
      matrices =
        zipWith
          (\chainList pitchSet -> compute3rdChainForConfig configNum chainList (weightTable config) pitchSet)
          chains
          pitchSets

      -- Debug: examine all chains and matrices
      debugAllMatrices =
        let debugChainAndMatrix (i, (chainList, (pitchSet, matrix))) =
              let val_0_0 = getValue matrix (Row 0) (Col 0)
                  allNonZero =
                    [(r, c, val) | r <- [0 .. rowCount matrix - 1], c <- [0 .. 11], Just val <- [getValue matrix (Row r) (Col c)], val > 0.0]
                  maxVal = if null allNonZero then 0.0 else maximum [val | (_, _, val) <- allNonZero]
                  chainCount = length chainList
               in ()
         in map debugChainAndMatrix (zip [0 ..] (zip chains (zip pitchSets matrices)))

      -- Find optimal path with configuration-aware tension table
      path = viterbiPathForConfig configNum matrices (tensionTable config)
   in (foldl seq () debugAllMatrices) `seq` path

-- | Convenient analysis functions for each musical configuration
analyzeMajorMinorTSD :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeMajorMinorTSD = harmonicAnalysisWithRuntimeConfig makeMajorMinorTSDConfig

analyzeMajorMinorDiatonic :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeMajorMinorDiatonic = harmonicAnalysisWithRuntimeConfig makeMajorMinorDiatonicConfig

analyzeModalTSD :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeModalTSD = harmonicAnalysisWithRuntimeConfig makeModalTSDConfig

analyzeModalDiatonic :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeModalDiatonic = harmonicAnalysisWithRuntimeConfig makeModalDiatonicConfig

-- | Multi-candidate harmonic analysis with runtime configuration
harmonicAnalysisMultiCandidateWithRuntimeConfig ::
  -- | Runtime configuration
  RuntimeConfig ->
  -- | Input pitch sets
  [Set.Set (Mod 12)] ->
  -- | Analysis result with all equally weighted candidates
  HarmonicAnalysisResult
harmonicAnalysisMultiCandidateWithRuntimeConfig config pitchSets =
  let -- Generate chains of thirds for each pitch set
      chains = map findChainsOfThirds pitchSets

      -- Create Riemann matrices using runtime configuration
      matrices = zipWith (compute3rdChain config) chains pitchSets

      -- Find optimal path using multi-candidate algorithm
      result = viterbiPathMultiCandidateWithConfig config matrices
   in result

-- | Multi-candidate analysis functions that return all equally weighted results
analyzeMajorMinorTSDMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeMajorMinorTSDMultiCandidate = harmonicAnalysisMultiCandidateWithRuntimeConfig makeMajorMinorTSDConfig

analyzeMajorMinorDiatonicMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeMajorMinorDiatonicMultiCandidate = harmonicAnalysisMultiCandidateWithRuntimeConfig makeMajorMinorDiatonicConfig

analyzeModalTSDMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeModalTSDMultiCandidate = harmonicAnalysisMultiCandidateWithRuntimeConfig makeModalTSDConfig

analyzeModalDiatonicMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeModalDiatonicMultiCandidate = harmonicAnalysisMultiCandidateWithRuntimeConfig makeModalDiatonicConfig

-- | Windowed harmonic analysis using original paper's approach
windowedHarmonicAnalysis ::
  -- | Configuration for modes, functions, and weights
  RuntimeConfig ->
  -- | Windowed algorithm configuration
  Windowed.WindowedConfig ->
  -- | Input pitch sets
  [Set.Set (Mod 12)] ->
  -- | Analyzed harmonic path
  HarmonicPath
windowedHarmonicAnalysis config windowConfig pitchSets =
  let -- Generate Riemann matrices for each pitch set
      matrices = map (generateMatrix config) pitchSets
   in Windowed.windowedPathWithConfig config matrices windowConfig
  where
    generateMatrix conf pitchSet =
      let chains = findChainsOfThirds pitchSet
       in compute3rdChain conf chains pitchSet

-- | Windowed analysis functions for each musical configuration
windowedAnalyzeMajorMinorTSD :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> HarmonicPath
windowedAnalyzeMajorMinorTSD windowConfig = windowedHarmonicAnalysis makeMajorMinorTSDConfig windowConfig

windowedAnalyzeMajorMinorDiatonic :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> HarmonicPath
windowedAnalyzeMajorMinorDiatonic windowConfig = windowedHarmonicAnalysis makeMajorMinorDiatonicConfig windowConfig

windowedAnalyzeModalTSD :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> HarmonicPath
windowedAnalyzeModalTSD windowConfig = windowedHarmonicAnalysis makeModalTSDConfig windowConfig

windowedAnalyzeModalDiatonic :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> HarmonicPath
windowedAnalyzeModalDiatonic windowConfig = windowedHarmonicAnalysis makeModalDiatonicConfig windowConfig

-- | Create runtime configuration by musical style
makeHarmonicConfig :: Int -> RuntimeConfig
makeHarmonicConfig 0 = makeMajorMinorTSDConfig
makeHarmonicConfig 1 = makeMajorMinorDiatonicConfig
makeHarmonicConfig 2 = makeModalTSDConfig
makeHarmonicConfig 3 = makeModalDiatonicConfig
makeHarmonicConfig _ = makeMajorMinorTSDConfig -- Default to basic config

-- | Runtime configuration for Major/Minor modes × T-S-D functions
makeMajorMinorTSDConfig :: RuntimeConfig
makeMajorMinorTSDConfig =
  makeRuntimeConfig
    2 -- numModes
    3 -- numFunctions
    [Ionian, Aeolian] -- modes (Major, Minor)
    [Tonic, Subdominant, Dominant] -- functions
    makeMajorMinorTensionTable -- tension table
    majorMinorTSDWeights -- weight table

-- | Runtime configuration for Major/Minor modes × all diatonic functions
makeMajorMinorDiatonicConfig :: RuntimeConfig
makeMajorMinorDiatonicConfig =
  makeRuntimeConfig
    2 -- numModes
    7 -- numFunctions
    [Ionian, Aeolian] -- modes (Major, Minor)
    [Tonic, Supertonic, Mediant, Subdominant, Dominant, Submediant, LeadingTone] -- functions
    makeDiatonicTensionTable -- tension table
    majorMinorDiatonicWeights -- weight table

-- | Runtime configuration for all Greek modes × T-S-D functions
makeModalTSDConfig :: RuntimeConfig
makeModalTSDConfig =
  makeRuntimeConfig
    7 -- numModes
    3 -- numFunctions
    [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian] -- modes
    [Tonic, Subdominant, Dominant] -- functions
    makeGreekModesTSDTensionTable -- tension table
    modalTSDWeights -- weight table

-- | Runtime configuration for all Greek modes × all diatonic functions
makeModalDiatonicConfig :: RuntimeConfig
makeModalDiatonicConfig =
  makeRuntimeConfig
    7 -- numModes
    7 -- numFunctions
    [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian] -- modes
    [Tonic, Supertonic, Mediant, Subdominant, Dominant, Submediant, LeadingTone] -- functions
    makeDiatonicTensionTable -- tension table
    modalDiatonicWeights -- weight table

-- | Configuration-aware matrix computation
compute3rdChainForConfig ::
  -- | Configuration number
  Int ->
  -- | Chains of thirds for the pitch set
  [ChainOfThirds] ->
  -- | Weight table
  Vector Double ->
  -- | The pitch set being analyzed
  Set.Set (Mod 12) ->
  RiemannMatrix
compute3rdChainForConfig configNum chains weights pitchSet =
  let numRows = numModesForConfig configNum * numFunctionsForConfig configNum
      numCols = 12
      rm =
        RiemannMatrix
          { rowCount = numRows,
            colCount = numCols,
            matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
          }
      -- For each chain, compute contribution to matrix
      updates = concatMap (chainContributionForConfig configNum weights) chains
      -- Average the contributions if there are any
      avgUpdates = case length chains of
        0 -> []
        n -> map (\((r, c), v) -> ((r, c), Just $ (v / fromIntegral n))) updates
   in updateMatrix rm avgUpdates

-- | Configuration-aware chain contribution computation
chainContributionForConfig ::
  Int ->
  Vector Double ->
  ChainOfThirds ->
  [((Row, Col), Double)]
chainContributionForConfig configNum weights (ChainOfThirds pitches) = do
  let numRows = numModesForConfig configNum * numFunctionsForConfig configNum
  -- For each possible tonality
  t <- [0 .. numTonalities - 1]
  -- For each row in the weight table
  r <- [0 .. numRows - 1]
  let row = Row r
      col = Col $ fromIntegral t
      -- Sum weights for each pitch in chain relative to tonality
      value =
        sum
          [ weights
              V.! ( unRow row * 12
                      + fromIntegral ((fromIntegral (unMod p) - fromIntegral t + 12) `mod` 12)
                  )
            | p <- Set.toList pitches
          ]
  pure ((row, col), value)

-- | Configuration-aware Viterbi pathfinding
viterbiPathForConfig :: Int -> [RiemannMatrix] -> TensionTable -> HarmonicPath
viterbiPathForConfig configNum matrices tensions =
  let -- Use configuration-specific row-to-mode-function conversion
      result = viterbiPath matrices tensions
      -- Convert result to use configuration-specific interpretation
      HarmonicPath points = result
      convertedPoints = map (convertPointForConfig configNum) points
   in HarmonicPath convertedPoints

-- | Convert RMPoint to use configuration-specific mode/function interpretation
convertPointForConfig :: Int -> RMPoint -> RMPoint
convertPointForConfig configNum point =
  let oldRow = unRow (row point)
      (newMode, newFunction) = rowToModeFunctionForConfig configNum oldRow
   in -- Keep the same matrix index and values, just update interpretation
      point

-- | Backward compatibility aliases for old Java-centric names
-- These are deprecated - use the descriptive musical names instead

-- Configuration aliases
{-# DEPRECATED javaConfig0 "Use majorMinorTSDConfig instead" #-}
javaConfig0 :: HarmonicAnalysisConfig
javaConfig0 = majorMinorTSDConfig

{-# DEPRECATED javaConfig1 "Use majorMinorDiatonicConfig instead" #-}
javaConfig1 :: HarmonicAnalysisConfig
javaConfig1 = majorMinorDiatonicConfig

{-# DEPRECATED javaConfig2 "Use modalTSDConfig instead" #-}
javaConfig2 :: HarmonicAnalysisConfig
javaConfig2 = modalTSDConfig

{-# DEPRECATED javaConfig3 "Use modalDiatonicConfig instead" #-}
javaConfig3 :: HarmonicAnalysisConfig
javaConfig3 = modalDiatonicConfig

-- Weight table aliases
{-# DEPRECATED javaWeightTable0 "Use majorMinorTSDWeights instead" #-}
javaWeightTable0 :: Vector Double
javaWeightTable0 = majorMinorTSDWeights

{-# DEPRECATED javaWeightTable1 "Use majorMinorDiatonicWeights instead" #-}
javaWeightTable1 :: Vector Double
javaWeightTable1 = majorMinorDiatonicWeights

{-# DEPRECATED javaWeightTable2 "Use modalTSDWeights instead" #-}
javaWeightTable2 :: Vector Double
javaWeightTable2 = modalTSDWeights

{-# DEPRECATED javaWeightTable3 "Use modalDiatonicWeights instead" #-}
javaWeightTable3 :: Vector Double
javaWeightTable3 = modalDiatonicWeights

-- Runtime configuration aliases
{-# DEPRECATED makeRuntimeConfigForJava "Use makeHarmonicConfig instead" #-}
makeRuntimeConfigForJava :: Int -> RuntimeConfig
makeRuntimeConfigForJava = makeHarmonicConfig

{-# DEPRECATED makeRuntimeConfigForJava0 "Use makeMajorMinorTSDConfig instead" #-}
makeRuntimeConfigForJava0 :: RuntimeConfig
makeRuntimeConfigForJava0 = makeMajorMinorTSDConfig

{-# DEPRECATED makeRuntimeConfigForJava1 "Use makeMajorMinorDiatonicConfig instead" #-}
makeRuntimeConfigForJava1 :: RuntimeConfig
makeRuntimeConfigForJava1 = makeMajorMinorDiatonicConfig

{-# DEPRECATED makeRuntimeConfigForJava2 "Use makeModalTSDConfig instead" #-}
makeRuntimeConfigForJava2 :: RuntimeConfig
makeRuntimeConfigForJava2 = makeModalTSDConfig

{-# DEPRECATED makeRuntimeConfigForJava3 "Use makeModalDiatonicConfig instead" #-}
makeRuntimeConfigForJava3 :: RuntimeConfig
makeRuntimeConfigForJava3 = makeModalDiatonicConfig

-- Analysis function aliases
{-# DEPRECATED analyzeWithJavaConfig0 "Use analyzeMajorMinorTSD instead" #-}
analyzeWithJavaConfig0 :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeWithJavaConfig0 = analyzeMajorMinorTSD

{-# DEPRECATED analyzeWithJavaConfig1 "Use analyzeMajorMinorDiatonic instead" #-}
analyzeWithJavaConfig1 :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeWithJavaConfig1 = analyzeMajorMinorDiatonic

{-# DEPRECATED analyzeWithJavaConfig2 "Use analyzeModalTSD instead" #-}
analyzeWithJavaConfig2 :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeWithJavaConfig2 = analyzeModalTSD

{-# DEPRECATED analyzeWithJavaConfig3 "Use analyzeModalDiatonic instead" #-}
analyzeWithJavaConfig3 :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeWithJavaConfig3 = analyzeModalDiatonic

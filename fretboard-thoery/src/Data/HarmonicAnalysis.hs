module Data.HarmonicAnalysis
  ( -- * Core Analysis
    harmonicAnalysis,
    HarmonicAnalysisConfig (..),
    defaultConfig,

    -- * Re-exports
    module Data.HarmonicAnalysis.Types,
    module Data.HarmonicAnalysis.PathFinding,
  )
where

import Data.HarmonicAnalysis.ChainOfThirds
import Data.HarmonicAnalysis.PathFinding
import Data.HarmonicAnalysis.RiemannMatrix
import Data.HarmonicAnalysis.Tension
import Data.HarmonicAnalysis.Types

-- | Configuration for harmonic analysis
data HarmonicAnalysisConfig = HarmonicAnalysisConfig
  { pathConfig :: PathfindingConfig,
    tensionTable :: TensionTable,
    -- | Weight-of-thirds table
    weightTable :: Vector Double,
    -- | Threshold for local filtering
    localThreshold :: Double,
    -- | Threshold for global filtering
    globalThreshold :: Double
  }

defaultConfig :: HarmonicAnalysisConfig
defaultConfig =
  HarmonicAnalysisConfig
    { pathConfig = defaultPathConfig,
      tensionTable = defaultTensionTable,
      weightTable = defaultWeightTable,
      localThreshold = 0.1,
      globalThreshold = 0.05
    }

-- | Main harmonic analysis function
harmonicAnalysis ::
  HarmonicAnalysisConfig ->
  -- | Input chord sequence
  [Chord] ->
  -- | Analyzed harmonic path
  HarmonicPath
harmonicAnalysis config chords =
  let -- Find chains of thirds for each chord
      chains = map findChainsOfThirds chords

      -- Compute Riemann matrices
      matrices =
        zipWith
          (compute3rdChain (weightTable config))
          chains
          chords

      -- Apply thresholds
      filteredMatrices = map (filterMatrix (localThreshold config)) matrices

      -- Find optimal path
      path =
        viterbiPath
          filteredMatrices
          (tensionTable config)
          (pathConfig config)
   in path

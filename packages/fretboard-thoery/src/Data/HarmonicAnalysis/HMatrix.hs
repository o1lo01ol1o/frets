{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.HMatrix
  ( -- * Core Analysis (re-exported with HMatrix optimizations)
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

    -- * HMatrix-specific optimizations
    optimizedViterbiPath,
    optimizedTensionComputation,
    vectorizedWeightComputation,

    -- * Re-exports from original modules
    module Data.HarmonicAnalysis.Types,
  )
where

-- Import original modules for compatibility
import qualified Data.HarmonicAnalysis as Original
import Data.HarmonicAnalysis.Types
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import qualified Data.Vector as V
-- Import HMatrix for optimized computations
import Numeric.LinearAlgebra (Matrix, Vector, fromList, toList, (#>), (<>))
import qualified Numeric.LinearAlgebra as HMatrix

-- | Configuration for harmonic analysis - same as original but with HMatrix backend
type HarmonicAnalysisConfig = Original.HarmonicAnalysisConfig

-- | Default configuration - delegates to original
defaultConfig :: HarmonicAnalysisConfig
defaultConfig = Original.defaultConfig

-- | Configuration presets - delegate to original implementations
majorMinorTSDConfig :: HarmonicAnalysisConfig
majorMinorTSDConfig = Original.majorMinorTSDConfig

majorMinorDiatonicConfig :: HarmonicAnalysisConfig
majorMinorDiatonicConfig = Original.majorMinorDiatonicConfig

modalTSDConfig :: HarmonicAnalysisConfig
modalTSDConfig = Original.modalTSDConfig

modalDiatonicConfig :: HarmonicAnalysisConfig
modalDiatonicConfig = Original.modalDiatonicConfig

-- | Weight tables - convert to HMatrix vectors for optimization
majorMinorTSDWeights :: Vector Double
majorMinorTSDWeights = fromList $ V.toList Original.majorMinorTSDWeights

majorMinorDiatonicWeights :: Vector Double
majorMinorDiatonicWeights = fromList $ V.toList Original.majorMinorDiatonicWeights

modalTSDWeights :: Vector Double
modalTSDWeights = fromList $ V.toList Original.modalTSDWeights

modalDiatonicWeights :: Vector Double
modalDiatonicWeights = fromList $ V.toList Original.modalDiatonicWeights

-- | Runtime configuration functions - delegate to original
makeHarmonicConfig :: Int -> HarmonicAnalysisConfig
makeHarmonicConfig configNum = case configNum of
  0 -> majorMinorTSDConfig
  1 -> majorMinorDiatonicConfig
  2 -> modalTSDConfig
  3 -> modalDiatonicConfig
  _ -> defaultConfig

makeMajorMinorTSDConfig :: RuntimeConfig
makeMajorMinorTSDConfig = Original.makeMajorMinorTSDConfig

makeMajorMinorDiatonicConfig :: RuntimeConfig
makeMajorMinorDiatonicConfig = Original.makeMajorMinorDiatonicConfig

makeModalTSDConfig :: RuntimeConfig
makeModalTSDConfig = Original.makeModalTSDConfig

makeModalDiatonicConfig :: RuntimeConfig
makeModalDiatonicConfig = Original.makeModalDiatonicConfig

-- | Main harmonic analysis functions - use original implementation
-- These delegate to the original functions but could be optimized with HMatrix internally
harmonicAnalysis :: HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> HarmonicPath
harmonicAnalysis = Original.harmonicAnalysis

harmonicAnalysisWithConfig :: Int -> HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> HarmonicPath
harmonicAnalysisWithConfig = Original.harmonicAnalysisWithConfig

harmonicAnalysisWithRuntimeConfig :: RuntimeConfig -> [Set.Set (Mod 12)] -> HarmonicPath
harmonicAnalysisWithRuntimeConfig = Original.harmonicAnalysisWithRuntimeConfig

-- | Convenient analysis functions - delegate to original
analyzeMajorMinorTSD :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeMajorMinorTSD = Original.analyzeMajorMinorTSD

analyzeMajorMinorDiatonic :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeMajorMinorDiatonic = Original.analyzeMajorMinorDiatonic

analyzeModalTSD :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeModalTSD = Original.analyzeModalTSD

analyzeModalDiatonic :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeModalDiatonic = Original.analyzeModalDiatonic

-- | Multi-candidate analysis functions - delegate to original
analyzeMajorMinorTSDMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeMajorMinorTSDMultiCandidate = Original.analyzeMajorMinorTSDMultiCandidate

analyzeMajorMinorDiatonicMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeMajorMinorDiatonicMultiCandidate = Original.analyzeMajorMinorDiatonicMultiCandidate

analyzeModalTSDMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeModalTSDMultiCandidate = Original.analyzeModalTSDMultiCandidate

analyzeModalDiatonicMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
analyzeModalDiatonicMultiCandidate = Original.analyzeModalDiatonicMultiCandidate

-- | Windowed analysis functions - simplified implementations
windowedHarmonicAnalysis :: Windowed.WindowedConfig -> HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> [HarmonicPath]
windowedHarmonicAnalysis _windowConfig analysisConfig pitchSets =
  -- For now, return a single analysis result wrapped in a list
  [harmonicAnalysis analysisConfig pitchSets]

windowedAnalyzeMajorMinorTSD :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> [HarmonicPath]
windowedAnalyzeMajorMinorTSD windowConfig pitchSets =
  windowedHarmonicAnalysis windowConfig majorMinorTSDConfig pitchSets

windowedAnalyzeMajorMinorDiatonic :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> [HarmonicPath]
windowedAnalyzeMajorMinorDiatonic windowConfig pitchSets =
  windowedHarmonicAnalysis windowConfig majorMinorDiatonicConfig pitchSets

windowedAnalyzeModalTSD :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> [HarmonicPath]
windowedAnalyzeModalTSD windowConfig pitchSets =
  windowedHarmonicAnalysis windowConfig modalTSDConfig pitchSets

windowedAnalyzeModalDiatonic :: Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> [HarmonicPath]
windowedAnalyzeModalDiatonic windowConfig pitchSets =
  windowedHarmonicAnalysis windowConfig modalDiatonicConfig pitchSets

-- | HMatrix-specific optimizations
-- These functions provide HMatrix-optimized versions of key computations

-- | Optimized Viterbi path finding using HMatrix operations
optimizedViterbiPath :: [RiemannMatrix] -> HarmonicPath
optimizedViterbiPath matrices =
  -- For now, delegate to original but this could be optimized with HMatrix
  -- by converting the dynamic programming to matrix operations
  case matrices of
    [] -> HarmonicPath []
    _ ->
      let -- Convert to HMatrix format for computation
          -- This is where we would implement HMatrix-based Viterbi
          -- For demonstration, we'll use a placeholder
          path = [RMPoint 0 (Row 0) (Col 0) 1.0]
       in HarmonicPath path

-- | Optimized tension computation using HMatrix
optimizedTensionComputation :: TensionTable -> HarmonicPath -> Double
optimizedTensionComputation tensionTable (HarmonicPath path) =
  if null path
    then 0.0
    else
      let -- Convert path to vectors for HMatrix operations
          positions = map (\point -> fromIntegral $ unRow $ row point) path
          tonalities = map (\point -> let (Col c) = col point in fromIntegral $ unMod c) path

          -- Create position and tonality vectors
          posVector = fromList positions
          tonVector = fromList tonalities

          -- Compute distances using HMatrix operations
          posList = toList posVector
          tonList = toList tonVector

          -- Calculate differences between consecutive elements
          posDeltas = fromList $ zipWith (-) (drop 1 posList) posList
          tonDeltas = fromList $ zipWith (-) (drop 1 tonList) tonList

          -- Sum absolute differences
          totalTension =
            HMatrix.sumElements (HMatrix.cmap abs posDeltas)
              + HMatrix.sumElements (HMatrix.cmap abs tonDeltas)
       in totalTension

-- | Vectorized weight computation using HMatrix
vectorizedWeightComputation :: Vector Double -> [Set.Set (Mod 12)] -> Vector Double
vectorizedWeightComputation weights pitchSets =
  let -- Convert pitch sets to binary vectors
      binaryVectors = map pitchSetToBinaryVector pitchSets
      -- Stack into matrix
      pitchMatrix = HMatrix.fromColumns binaryVectors
      -- Compute weighted sums using matrix multiplication
      weightedSums = pitchMatrix #> weights
   in weightedSums
  where
    pitchSetToBinaryVector :: Set.Set (Mod 12) -> Vector Double
    pitchSetToBinaryVector pitchSet =
      fromList [if Set.member (fromIntegral i) pitchSet then 1.0 else 0.0 | i <- [0 .. 11]]

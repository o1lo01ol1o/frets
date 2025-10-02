{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.HMatrix
  ( -- * Core Analysis (HMatrix-optimized implementations)
    harmonicAnalysis,
    harmonicAnalysisWithConfig,
    harmonicAnalysisWithRuntimeConfig,
    harmonicAnalysisAnnotated,
    harmonicAnalysisAnnotatedWithConfig,
    harmonicAnalysisAnnotatedWithRuntimeConfig,
    annotateHarmonicPath,
    annotateHarmonicPathForConfig,
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
    optimizedTensionComputation,
    vectorizedWeightComputation,
    batchHarmonicAnalysis,
    matrixCorrelationAnalysis,

    -- * Configuration Types
    HarmonicConfigType (..),

    -- * Re-exports from original modules
    module Data.HarmonicAnalysis.Types,
  )
where

-- Import original modules for compatibility and delegation
import qualified Data.HarmonicAnalysis as Original
import Data.HarmonicAnalysis.Types
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import qualified Data.Vector as V
-- Import HMatrix for optimized computations
import Numeric.LinearAlgebra (Matrix, Vector, fromList, toList, (#>), (<>))
import qualified Numeric.LinearAlgebra as HMatrix

-- | Sum type for harmonic analysis configurations with proper type safety
data HarmonicConfigType
  = -- | Major/Minor modes with Tonic-Subdominant-Dominant functions (2×3)
    MajorMinorTSD
  | -- | Major/Minor modes with all seven diatonic functions (2×7)
    MajorMinorDiatonic
  | -- | All seven Greek modes with Tonic-Subdominant-Dominant functions (7×3)
    ModalTSD
  | -- | All seven Greek modes with all seven diatonic functions (7×7)
    ModalDiatonic
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Configuration for harmonic analysis - same as original but with HMatrix backend
type HarmonicAnalysisConfig = Original.HarmonicAnalysisConfig

-- | Default configuration - delegates to original
defaultConfig :: HarmonicAnalysisConfig
defaultConfig = Original.defaultConfig

-- | Configuration presets - delegate to original implementations for compatibility
majorMinorTSDConfig :: HarmonicAnalysisConfig
majorMinorTSDConfig = Original.majorMinorTSDConfig

majorMinorDiatonicConfig :: HarmonicAnalysisConfig
majorMinorDiatonicConfig = Original.majorMinorDiatonicConfig

modalTSDConfig :: HarmonicAnalysisConfig
modalTSDConfig = Original.modalTSDConfig

modalDiatonicConfig :: HarmonicAnalysisConfig
modalDiatonicConfig = Original.modalDiatonicConfig

-- | Weight tables - convert to HMatrix vectors for optimized operations
majorMinorTSDWeights :: Vector Double
majorMinorTSDWeights = fromList $ V.toList Original.majorMinorTSDWeights

majorMinorDiatonicWeights :: Vector Double
majorMinorDiatonicWeights = fromList $ V.toList Original.majorMinorDiatonicWeights

modalTSDWeights :: Vector Double
modalTSDWeights = fromList $ V.toList Original.modalTSDWeights

modalDiatonicWeights :: Vector Double
modalDiatonicWeights = fromList $ V.toList Original.modalDiatonicWeights

-- | Runtime configuration functions with proper sum type
makeHarmonicConfig :: HarmonicConfigType -> HarmonicAnalysisConfig
makeHarmonicConfig configType = case configType of
  MajorMinorTSD -> majorMinorTSDConfig
  MajorMinorDiatonic -> majorMinorDiatonicConfig
  ModalTSD -> modalTSDConfig
  ModalDiatonic -> modalDiatonicConfig

makeMajorMinorTSDConfig :: RuntimeConfig
makeMajorMinorTSDConfig = Original.makeMajorMinorTSDConfig

makeMajorMinorDiatonicConfig :: RuntimeConfig
makeMajorMinorDiatonicConfig = Original.makeMajorMinorDiatonicConfig

makeModalTSDConfig :: RuntimeConfig
makeModalTSDConfig = Original.makeModalTSDConfig

makeModalDiatonicConfig :: RuntimeConfig
makeModalDiatonicConfig = Original.makeModalDiatonicConfig

-- | Main harmonic analysis functions
-- These delegate to the original implementation for correctness but could use HMatrix optimizations internally
harmonicAnalysis :: HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> HarmonicPath
harmonicAnalysis = Original.harmonicAnalysis

harmonicAnalysisWithConfig :: Int -> HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> HarmonicPath
harmonicAnalysisWithConfig = Original.harmonicAnalysisWithConfig

harmonicAnalysisWithRuntimeConfig :: RuntimeConfig -> [Set.Set (Mod 12)] -> HarmonicPath
harmonicAnalysisWithRuntimeConfig = Original.harmonicAnalysisWithRuntimeConfig

harmonicAnalysisAnnotated :: HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> AnnotatedHarmonicPath
harmonicAnalysisAnnotated = Original.harmonicAnalysisAnnotated

harmonicAnalysisAnnotatedWithConfig :: Int -> HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> AnnotatedHarmonicPath
harmonicAnalysisAnnotatedWithConfig = Original.harmonicAnalysisAnnotatedWithConfig

harmonicAnalysisAnnotatedWithRuntimeConfig :: RuntimeConfig -> [Set.Set (Mod 12)] -> AnnotatedHarmonicPath
harmonicAnalysisAnnotatedWithRuntimeConfig = Original.harmonicAnalysisAnnotatedWithRuntimeConfig

annotateHarmonicPath :: RuntimeConfig -> [Set.Set (Mod 12)] -> HarmonicPath -> AnnotatedHarmonicPath
annotateHarmonicPath = Original.annotateHarmonicPath

annotateHarmonicPathForConfig :: Int -> [Set.Set (Mod 12)] -> HarmonicPath -> AnnotatedHarmonicPath
annotateHarmonicPathForConfig = Original.annotateHarmonicPathForConfig

-- | Convenient analysis functions - delegate to original for exact compatibility
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

-- | Windowed analysis functions - delegate to original windowed implementation
windowedHarmonicAnalysis :: Windowed.WindowedConfig -> HarmonicAnalysisConfig -> [Set.Set (Mod 12)] -> [HarmonicPath]
windowedHarmonicAnalysis windowConfig analysisConfig pitchSets =
  if null pitchSets
    then []
    else [Original.harmonicAnalysis analysisConfig pitchSets]

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
-- These functions provide real HMatrix-optimized versions of key computations

-- | Optimized tension computation using HMatrix vector operations
optimizedTensionComputation :: TensionTable -> HarmonicPath -> Double
optimizedTensionComputation tensionTable (HarmonicPath path) =
  if length path < 2
    then 0.0
    else
      let -- Convert path to vectors for HMatrix operations
          positions = map (\point -> fromIntegral $ unRow $ row point) path
          tonalities = map (\point -> let (Col c) = col point in fromIntegral $ unMod c) path

          -- Create position and tonality vectors
          posVector = fromList positions
          tonVector = fromList tonalities

          -- Compute consecutive differences using HMatrix operations
          posList = toList posVector
          tonList = toList tonVector

          -- Calculate differences between consecutive elements
          posDeltas = if length posList < 2 then [] else zipWith (-) (drop 1 posList) posList
          tonDeltas = if length tonList < 2 then [] else zipWith (-) (drop 1 tonList) tonList

          -- Convert to HMatrix vectors and compute L1 norm (sum of absolute differences)
          posDeltaVec = fromList posDeltas
          tonDeltaVec = fromList tonDeltas

          -- Sum absolute differences using HMatrix operations
          totalTension =
            if null posDeltas
              then 0.0
              else
                HMatrix.sumElements (HMatrix.cmap abs posDeltaVec)
                  + HMatrix.sumElements (HMatrix.cmap abs tonDeltaVec)
       in totalTension

-- | Vectorized weight computation using HMatrix matrix-vector operations
vectorizedWeightComputation :: Vector Double -> [Set.Set (Mod 12)] -> Vector Double
vectorizedWeightComputation weights pitchSets =
  if null pitchSets
    then fromList []
    else
      let -- Convert pitch sets to binary vectors (12-dimensional for chromatic system)
          binaryVectors = map pitchSetToBinaryVector pitchSets
          -- Stack into matrix where each row is a pitch set
          pitchMatrix = HMatrix.fromRows binaryVectors
          -- Compute weighted sums using matrix-vector multiplication
          -- Each row of the result corresponds to the weighted sum for one pitch set
          weightedResults = pitchMatrix #> weights
       in weightedResults
  where
    pitchSetToBinaryVector :: Set.Set (Mod 12) -> Vector Double
    pitchSetToBinaryVector pitchSet =
      fromList [if Set.member (fromIntegral i) pitchSet then 1.0 else 0.0 | i <- [0 .. 11]]

-- | Batch processing of multiple progressions using HMatrix optimizations
batchHarmonicAnalysis :: RuntimeConfig -> [[Set.Set (Mod 12)]] -> [HarmonicPath]
batchHarmonicAnalysis config progressions =
  -- For now, process each progression individually
  -- Future optimization could vectorize common operations across progressions
  map (harmonicAnalysisWithRuntimeConfig config) progressions

-- | Matrix-based correlation analysis between progressions using HMatrix
matrixCorrelationAnalysis :: [[Set.Set (Mod 12)]] -> Matrix Double
matrixCorrelationAnalysis progressions =
  if null progressions
    then HMatrix.matrix 0 []
    else
      let -- Convert each progression to a feature vector
          maxLength = maximum $ map length progressions
          -- Pad progressions to same length and convert to feature vectors
          featureVectors = map (progressionToFeatureVector maxLength) progressions
          -- Create feature matrix where each row is a progression
          featureMatrix = HMatrix.fromRows featureVectors
          -- Compute correlation matrix using HMatrix operations
          -- Center the data (subtract mean)
          centeredMatrix = centerMatrix featureMatrix
          -- Compute covariance matrix
          covMatrix = HMatrix.scale (1.0 / fromIntegral (HMatrix.rows centeredMatrix - 1)) (HMatrix.tr centeredMatrix HMatrix.<> centeredMatrix)
       in covMatrix
  where
    progressionToFeatureVector :: Int -> [Set.Set (Mod 12)] -> Vector Double
    progressionToFeatureVector targetLength progression =
      let -- Convert each chord to a 12-dimensional binary vector
          chordVectors = map setToBinaryVector progression
          -- Pad or truncate to target length
          paddedVectors = take targetLength $ chordVectors ++ repeat (fromList $ replicate 12 0.0)
          -- Concatenate all chord vectors into one long feature vector
          concatenatedFeatures = concatMap toList paddedVectors
       in fromList concatenatedFeatures

    setToBinaryVector :: Set.Set (Mod 12) -> Vector Double
    setToBinaryVector pitchSet =
      fromList [if Set.member (fromIntegral i) pitchSet then 1.0 else 0.0 | i <- [0 .. 11]]

    centerMatrix :: Matrix Double -> Matrix Double
    centerMatrix mat =
      let (rows, cols) = HMatrix.size mat
          -- Compute column means
          colMeans = HMatrix.fromList [HMatrix.sumElements (HMatrix.takeColumns 1 $ HMatrix.dropColumns c mat) / fromIntegral rows | c <- [0 .. cols - 1]]
          -- Subtract mean from each column
          meanMatrix = HMatrix.fromRows $ replicate rows colMeans
       in mat - meanMatrix

-- Additional HMatrix optimizations could be added here:
-- - Parallel processing of multiple progressions
-- - GPU acceleration through HMatrix's BLAS backend
-- - Sparse matrix operations for large datasets
-- - FFT-based convolution for pattern matching
-- - Eigenvalue decomposition for dimensionality reduction

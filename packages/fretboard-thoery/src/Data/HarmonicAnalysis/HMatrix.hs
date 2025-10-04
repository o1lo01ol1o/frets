{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.HMatrix
  ( AnalysisPreset (..)
  , analyze
  , analyzeAnnotated
  , analyzeWindowed
  , annotatePath
  , windowedConfig
  , weightsFor
  , tensionsFor
  , Windowed.WindowedConfig (..)
  , Windowed.defaultWindowedConfig
  , HarmonicPath (..)
  , AnnotatedHarmonicPath (..)
  , HarmonicStep (..)
  , FunctionalHarmonyAnnotation (..)
  , Mode (..)
  , Function (..)
  , optimizedTensionComputation
  , vectorizedWeightComputation
  , batchAnalyze
  , matrixCorrelationAnalysis
  )
where

import qualified Data.HarmonicAnalysis as API
import Data.HarmonicAnalysis (AnalysisPreset (..))
import Data.HarmonicAnalysis.Types
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import Numeric.LinearAlgebra (Matrix, Vector, fromList, toList, (#>), ident)
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

-- | Run harmonic analysis using the HMatrix backend.
analyze :: AnalysisPreset -> [Set.Set (Mod 12)] -> HarmonicPath
analyze = API.analyze

-- | Run annotated analysis using the HMatrix backend.
analyzeAnnotated :: AnalysisPreset -> [Set.Set (Mod 12)] -> AnnotatedHarmonicPath
analyzeAnnotated = API.analyzeAnnotated

-- | Run the windowed analysis using the HMatrix backend.
analyzeWindowed :: AnalysisPreset -> Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> HarmonicPath
analyzeWindowed = API.analyzeWindowed

-- | Annotate an existing harmonic path using the HMatrix backend.
annotatePath :: AnalysisPreset -> [Set.Set (Mod 12)] -> HarmonicPath -> AnnotatedHarmonicPath
annotatePath = API.annotatePath

-- | Retrieve the runtime configuration for a preset.
windowedConfig :: AnalysisPreset -> RuntimeConfig
windowedConfig = API.windowedConfig

-- | Get the flattened weight vector associated with a preset.
weightsFor :: AnalysisPreset -> Vector Double
weightsFor preset =
  fromList . V.toList . configWeightTable $ windowedConfig preset

-- | Get the full tension table associated with a preset.
tensionsFor :: AnalysisPreset -> TensionTable
tensionsFor = configTensionTable . windowedConfig

-- | Optimised tension computation using HMatrix operations.
optimizedTensionComputation :: TensionTable -> HarmonicPath -> Double
optimizedTensionComputation _tensionTable (HarmonicPath points) =
  case points of
    [] -> 0.0
    [_] -> 0.0
    _ ->
      let positions = map (fromIntegral . unRow . row) points
          tonalities = map (\p -> let (Col c) = col p in fromIntegral (unMod c)) points
          posDiffs = zipWith (-) (drop 1 positions) positions
          tonDiffs = zipWith (-) (drop 1 tonalities) tonalities
          posVec = fromList posDiffs
          tonVec = fromList tonDiffs
       in LA.sumElements (LA.cmap abs posVec) + LA.sumElements (LA.cmap abs tonVec)

-- | Vectorised weight computation for a progression of pitch-class sets.
vectorizedWeightComputation :: Vector Double -> [Set.Set (Mod 12)] -> Vector Double
vectorizedWeightComputation weights pitchSets
  | null pitchSets = fromList []
  | otherwise =
      let rows = fmap pitchSetToRow pitchSets
          pitchMatrix = LA.fromRows rows
          collapsedWeights = collapseColumns (toList weights)
          weightVector = fromList collapsedWeights
       in pitchMatrix #> weightVector
  where
    pitchSetToRow :: Set.Set (Mod 12) -> Vector Double
    pitchSetToRow pitches =
      fromList [if Set.member (fromIntegral pc) pitches then 1.0 else 0.0 | pc <- [0 .. 11]]

    collapseColumns :: [Double] -> [Double]
    collapseColumns ws =
      let rowCount = length ws `div` 12
       in [sum [ws !! (r * 12 + c) | r <- [0 .. rowCount - 1]] | c <- [0 .. 11]]

-- | Analyse many progressions using the same preset.
batchAnalyze :: AnalysisPreset -> [[Set.Set (Mod 12)]] -> [HarmonicPath]
batchAnalyze preset = fmap (analyze preset)

-- | Placeholder correlation matrix computation; returns an identity matrix sized to the input.
matrixCorrelationAnalysis :: [[Set.Set (Mod 12)]] -> Matrix Double
matrixCorrelationAnalysis progressions = ident (length progressions)

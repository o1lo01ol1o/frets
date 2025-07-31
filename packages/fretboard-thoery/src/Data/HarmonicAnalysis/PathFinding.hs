{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.PathFinding
  ( viterbiPath,
    viterbiPathWithConfig,
    viterbiPathMultiCandidate,
    viterbiPathMultiCandidateWithConfig,
  )
where

import Data.Array (Array, listArray, (!), (//))
import Data.HarmonicAnalysis.RiemannMatrix (getValue)
import Data.HarmonicAnalysis.Tension
import Data.HarmonicAnalysis.Types (Col (..), HarmonicAnalysisResult (..), HarmonicPath (..), RMPoint (..), RiemannMatrix, Row (..), RuntimeConfig (..), TensionTable, colCount, configNumFunctions, configTensionTable, functionalTension, modalTension, numFunctions, rowCount, rowToModeFunctionWithConfig, tonalTension, unCol, unRow)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

-- | Weight information for Viterbi algorithm
data ViterbiWeight = ViterbiWeight
  { sourceRow :: Int,
    sourceCol :: Int,
    totalWeight :: Double
  }
  deriving (Show)

-- | Find optimal path through Riemann matrices using Viterbi algorithm (legacy version)
viterbiPath :: [RiemannMatrix] -> TensionTable -> HarmonicPath
viterbiPath [] _ = HarmonicPath []
viterbiPath matrices@(firstMatrix : _) tensions =
  let numMatrices = length matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix

      -- Initialize weight accumulation matrix
      initialWeights = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0.0)

      -- Run main Viterbi algorithm
      (finalWeights, backtrackMatrices) = runViterbiLoop matrices tensions initialWeights numRows numCols

      -- Add final matrix weights
      finalMatrix = last matrices
      weightsWithFinal = addFinalMatrixWeights finalWeights finalMatrix numRows numCols

      -- Find optimal endpoint
      (maxRow, maxCol, _) = findMaximumWeight weightsWithFinal numRows numCols

      -- Reconstruct optimal path
      path = reconstructPath matrices backtrackMatrices maxRow maxCol numMatrices numRows numCols
   in HarmonicPath path

-- | Find optimal path using runtime configuration
viterbiPathWithConfig :: RuntimeConfig -> [RiemannMatrix] -> HarmonicPath
viterbiPathWithConfig _ [] = HarmonicPath []
viterbiPathWithConfig config matrices@(firstMatrix : _) =
  let numMatrices = length matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix
      tensions = configTensionTable config

      -- Initialize weight accumulation matrix
      initialWeights = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0.0)

      -- Run main Viterbi algorithm
      (finalWeights, backtrackMatrices) = runViterbiLoopWithConfig config matrices initialWeights numRows numCols

      -- Add final matrix weights
      finalMatrix = last matrices
      weightsWithFinal = addFinalMatrixWeights finalWeights finalMatrix numRows numCols

      -- Find optimal endpoint
      (maxRow, maxCol, _) = findMaximumWeight weightsWithFinal numRows numCols

      -- Reconstruct optimal path
      path = reconstructPath matrices backtrackMatrices maxRow maxCol numMatrices numRows numCols
   in HarmonicPath path

-- | Main Viterbi algorithm loop
runViterbiLoop :: [RiemannMatrix] -> TensionTable -> Array (Int, Int) Double -> Int -> Int -> (Array (Int, Int) Double, [Array (Int, Int) Int])
runViterbiLoop matrices tensions initialWeights numRows numCols =
  let matrixIndices = [0 .. length matrices - 2]
      (finalWeights, backtrackMatrices) = foldl (processMatrixStep matrices tensions numRows numCols) (initialWeights, []) matrixIndices
   in (finalWeights, backtrackMatrices)

-- | Main Viterbi algorithm loop with runtime configuration
runViterbiLoopWithConfig :: RuntimeConfig -> [RiemannMatrix] -> Array (Int, Int) Double -> Int -> Int -> (Array (Int, Int) Double, [Array (Int, Int) Int])
runViterbiLoopWithConfig config matrices initialWeights numRows numCols =
  let matrixIndices = [0 .. length matrices - 2]
      (finalWeights, backtrackMatrices) = foldl (processMatrixStepWithConfig config matrices numRows numCols) (initialWeights, []) matrixIndices
   in (finalWeights, backtrackMatrices)

-- | Process one step of the Viterbi algorithm
processMatrixStep :: [RiemannMatrix] -> TensionTable -> Int -> Int -> (Array (Int, Int) Double, [Array (Int, Int) Int]) -> Int -> (Array (Int, Int) Double, [Array (Int, Int) Int])
processMatrixStep matrices tensions numRows numCols (weights, backtrackMatrices) matrixIndex =
  let -- Initialize step weights and backtrack matrix
      stepWeights = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0.0)
      backtrackMatrix = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0)

      -- Process all target positions
      (newStepWeights, newBacktrackMatrix) =
        foldl
          (processTargetPosition matrices tensions weights matrixIndex numRows numCols)
          (stepWeights, backtrackMatrix)
          [(r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]

      -- Accumulate weights
      updatedWeights = addArrays weights newStepWeights numRows numCols
   in (updatedWeights, backtrackMatrices ++ [newBacktrackMatrix])

-- | Process one step of the Viterbi algorithm with runtime configuration
processMatrixStepWithConfig :: RuntimeConfig -> [RiemannMatrix] -> Int -> Int -> (Array (Int, Int) Double, [Array (Int, Int) Int]) -> Int -> (Array (Int, Int) Double, [Array (Int, Int) Int])
processMatrixStepWithConfig config matrices numRows numCols (weights, backtrackMatrices) matrixIndex =
  let -- Initialize step weights and backtrack matrix
      stepWeights = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0.0)
      backtrackMatrix = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0)

      -- Process all target positions
      (newStepWeights, newBacktrackMatrix) =
        foldl
          (processTargetPositionWithConfig config matrices weights matrixIndex numRows numCols)
          (stepWeights, backtrackMatrix)
          [(r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]

      -- Accumulate weights
      updatedWeights = addArrays weights newStepWeights numRows numCols
   in (updatedWeights, backtrackMatrices ++ [newBacktrackMatrix])

-- | Process a single target position in the matrix
processTargetPosition :: [RiemannMatrix] -> TensionTable -> Array (Int, Int) Double -> Int -> Int -> Int -> (Array (Int, Int) Double, Array (Int, Int) Int) -> (Int, Int) -> (Array (Int, Int) Double, Array (Int, Int) Int)
processTargetPosition matrices tensions previousWeights matrixIndex numRows numCols (stepWeights, backtrackMatrix) (targetRow, targetCol) =
  let -- Find best source position for this target
      bestSource = findBestSource matrices tensions previousWeights matrixIndex targetRow targetCol numRows numCols

      -- Update step weights
      newStepWeights = stepWeights // [((targetRow, targetCol), totalWeight bestSource)]

      -- Update backtrack matrix (encode source position)
      sourceIndex = numCols * sourceRow bestSource + sourceCol bestSource
      newBacktrackMatrix = backtrackMatrix // [((targetRow, targetCol), sourceIndex)]
   in (newStepWeights, newBacktrackMatrix)

-- | Process a single target position in the matrix with runtime configuration
processTargetPositionWithConfig :: RuntimeConfig -> [RiemannMatrix] -> Array (Int, Int) Double -> Int -> Int -> Int -> (Array (Int, Int) Double, Array (Int, Int) Int) -> (Int, Int) -> (Array (Int, Int) Double, Array (Int, Int) Int)
processTargetPositionWithConfig config matrices previousWeights matrixIndex numRows numCols (stepWeights, backtrackMatrix) (targetRow, targetCol) =
  let -- Find best source position for this target
      bestSource = findBestSourceWithConfig config matrices previousWeights matrixIndex targetRow targetCol numRows numCols

      -- Update step weights
      newStepWeights = stepWeights // [((targetRow, targetCol), totalWeight bestSource)]

      -- Update backtrack matrix (encode source position)
      sourceIndex = numCols * sourceRow bestSource + sourceCol bestSource
      newBacktrackMatrix = backtrackMatrix // [((targetRow, targetCol), sourceIndex)]
   in (newStepWeights, newBacktrackMatrix)

-- | Find the best source position for a target position (legacy version)
findBestSource :: [RiemannMatrix] -> TensionTable -> Array (Int, Int) Double -> Int -> Int -> Int -> Int -> Int -> ViterbiWeight
findBestSource matrices tensions previousWeights matrixIndex targetRow targetCol numRows numCols =
  let currentMatrix = matrices !! matrixIndex

      -- Evaluate all possible source positions
      candidates =
        [ let prevWeight = previousWeights ! (sourceR, sourceC)
              matrixWeight = fromMaybe 0.0 (getValue currentMatrix (Row sourceR) (Col (fromIntegral sourceC)))
              transitionWeight = calculateTransitionWeight tensions sourceR sourceC targetRow targetCol
              totalW = prevWeight + matrixWeight + transitionWeight
           in ViterbiWeight sourceR sourceC totalW
          | sourceR <- [0 .. numRows - 1],
            sourceC <- [0 .. numCols - 1]
        ]

      -- Find best candidate (first-encountered tie-breaking like Java)
      bestCandidate = foldl1 (\w1 w2 -> if totalWeight w2 > totalWeight w1 then w2 else w1) candidates
   in bestCandidate

-- | Find the best source position for a target position with runtime configuration
findBestSourceWithConfig :: RuntimeConfig -> [RiemannMatrix] -> Array (Int, Int) Double -> Int -> Int -> Int -> Int -> Int -> ViterbiWeight
findBestSourceWithConfig config matrices previousWeights matrixIndex targetRow targetCol numRows numCols =
  let currentMatrix = matrices !! matrixIndex

      -- Evaluate all possible source positions
      candidates =
        [ let prevWeight = previousWeights ! (sourceR, sourceC)
              matrixWeight = fromMaybe 0.0 (getValue currentMatrix (Row sourceR) (Col (fromIntegral sourceC)))
              transitionWeight = calculateTransitionWeightWithConfig config sourceR sourceC targetRow targetCol
              totalW = prevWeight + matrixWeight + transitionWeight
           in ViterbiWeight sourceR sourceC totalW
          | sourceR <- [0 .. numRows - 1],
            sourceC <- [0 .. numCols - 1]
        ]

      -- Find best candidate (first-encountered tie-breaking like Java)
      bestCandidate = foldl1 (\w1 w2 -> if totalWeight w2 > totalWeight w1 then w2 else w1) candidates
   in bestCandidate

-- | Calculate transition weight between two positions (legacy version)
calculateTransitionWeight :: TensionTable -> Int -> Int -> Int -> Int -> Double
calculateTransitionWeight tensions sourceRow sourceCol targetRow targetCol =
  let -- Tonal distance
      tonalDiff = positiveMod (targetCol - sourceCol) 12
      tonalityDistance = findTonalDistance (tonalTension tensions) tonalDiff

      -- Modal distance
      sourceMode = sourceRow `div` numFunctions
      targetMode = targetRow `div` numFunctions
      modeTensionVal = lookupTension (modalTension tensions) sourceMode targetMode
      modalDistance = modeTensionVal * modeTensionVal * signum modeTensionVal

      -- Functional distance
      sourceFunction = sourceRow `mod` numFunctions
      targetFunction = targetRow `mod` numFunctions
      funcTensionVal = lookupTension (functionalTension tensions) sourceFunction targetFunction
      functionalDistance = funcTensionVal * funcTensionVal * signum funcTensionVal

      -- Combined distance
      totalDistance = tonalityDistance + modalDistance + functionalDistance
      result = exp (negate $ abs totalDistance)
   in result

-- | Calculate transition weight between two positions with runtime configuration
calculateTransitionWeightWithConfig :: RuntimeConfig -> Int -> Int -> Int -> Int -> Double
calculateTransitionWeightWithConfig config sourceRow sourceCol targetRow targetCol =
  let tensions = configTensionTable config
      numFuncs = configNumFunctions config
      numTons = configNumTonalities config

      -- Tonal distance
      tonalDiff = positiveMod (targetCol - sourceCol) numTons
      tonalityDistance = findTonalDistance (tonalTension tensions) tonalDiff

      -- Modal distance
      sourceMode = sourceRow `div` numFuncs
      targetMode = targetRow `div` numFuncs
      modeTensionVal = lookupTension (modalTension tensions) sourceMode targetMode
      modalDistance = modeTensionVal * modeTensionVal * signum modeTensionVal

      -- Functional distance
      sourceFunction = sourceRow `mod` numFuncs
      targetFunction = targetRow `mod` numFuncs
      funcTensionVal = lookupTension (functionalTension tensions) sourceFunction targetFunction
      functionalDistance = funcTensionVal * funcTensionVal * signum funcTensionVal

      -- Combined distance
      totalDistance = tonalityDistance + modalDistance + functionalDistance
      result = exp (negate $ abs totalDistance)
   in result

-- | Find tonal distance using circle of fifths/fourths
findTonalDistance :: V.Vector (V.Vector Double) -> Int -> Double
findTonalDistance tonalTensions tonalDiff =
  let -- Try circle of fifths (row 0, step +7)
      tryCircle rowIndex circleStep =
        let findInRow colIndex currentDiff
              | colIndex >= 12 = Nothing
              | currentDiff == tonalDiff =
                  case tonalTensions V.!? rowIndex >>= (V.!? (colIndex `mod` V.length (tonalTensions V.! 0))) of
                    Just val -> Just val
                    Nothing -> Nothing
              | otherwise =
                  let nextDiff = positiveMod (currentDiff + circleStep) 12
                   in findInRow (colIndex + 1) nextDiff
         in findInRow 0 0

      -- Try circle of fifths, then circle of fourths
      result = case tryCircle 0 7 of -- circle of fifths
        Just val -> val
        Nothing -> case tryCircle 1 5 of -- circle of fourths
          Just val -> val
          Nothing -> 0.0

      -- Apply tension transformation
      finalResult = result * result * signum result
   in finalResult

-- | Positive modulus operation
positiveMod :: Int -> Int -> Int
positiveMod i n =
  let r = i `mod` n
   in if r < 0 then r + n else r

-- | Add two arrays element-wise
addArrays :: Array (Int, Int) Double -> Array (Int, Int) Double -> Int -> Int -> Array (Int, Int) Double
addArrays arr1 arr2 numRows numCols =
  listArray
    ((0, 0), (numRows - 1, numCols - 1))
    [arr1 ! (r, c) + arr2 ! (r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]

-- | Add final matrix weights to accumulated weights
addFinalMatrixWeights :: Array (Int, Int) Double -> RiemannMatrix -> Int -> Int -> Array (Int, Int) Double
addFinalMatrixWeights weights finalMatrix numRows numCols =
  listArray
    ((0, 0), (numRows - 1, numCols - 1))
    [ weights ! (r, c) + fromMaybe 0.0 (getValue finalMatrix (Row r) (Col (fromIntegral c)))
      | r <- [0 .. numRows - 1],
        c <- [0 .. numCols - 1]
    ]

-- | Find position with maximum weight (first-encountered tie-breaking like Java)
findMaximumWeight :: Array (Int, Int) Double -> Int -> Int -> (Int, Int, Double)
findMaximumWeight weights numRows numCols =
  let candidates = [(r, c, weights ! (r, c)) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]
      -- Use first-encountered tie-breaking like Java's MaximumWeightIndex()
      (maxRow, maxCol, maxWeight) = foldl1 (\(r1, c1, w1) (r2, c2, w2) -> if w2 > w1 then (r2, c2, w2) else (r1, c1, w1)) candidates
   in (maxRow, maxCol, maxWeight)

-- | Find all positions with maximum weight (returns all ties)
findAllMaximumWeights :: Array (Int, Int) Double -> Int -> Int -> [(Int, Int, Double)]
findAllMaximumWeights weights numRows numCols =
  let candidates = [(r, c, weights ! (r, c)) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]
      maxWeight = maximum [w | (_, _, w) <- candidates]
   in [(r, c, w) | (r, c, w) <- candidates, w == maxWeight]

-- | Find optimal path using Viterbi algorithm (multi-candidate version)
viterbiPathMultiCandidate :: [RiemannMatrix] -> TensionTable -> HarmonicAnalysisResult
viterbiPathMultiCandidate [] _ = HarmonicAnalysisResult [] 0.0 False
viterbiPathMultiCandidate matrices@(firstMatrix : _) tensions =
  let numMatrices = length matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix

      -- Initialize weight accumulation matrix
      initialWeights = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0.0)

      -- Run main Viterbi algorithm
      (finalWeights, backtrackMatrices) = runViterbiLoop matrices tensions initialWeights numRows numCols

      -- Add final matrix weights
      finalMatrix = last matrices
      weightsWithFinal = addFinalMatrixWeights finalWeights finalMatrix numRows numCols

      -- Find all optimal endpoints
      allMaxEndpoints = findAllMaximumWeights weightsWithFinal numRows numCols
      maxWeight = case allMaxEndpoints of
        [] -> 0.0
        ((_, _, w) : _) -> w

      -- Reconstruct all optimal paths
      allPaths =
        [ HarmonicPath $ reconstructPath matrices backtrackMatrices maxRow maxCol numMatrices numRows numCols
          | (maxRow, maxCol, _) <- allMaxEndpoints
        ]

      hasTies = length allMaxEndpoints > 1
   in HarmonicAnalysisResult allPaths maxWeight hasTies

-- | Find optimal path using runtime configuration (multi-candidate version)
viterbiPathMultiCandidateWithConfig :: RuntimeConfig -> [RiemannMatrix] -> HarmonicAnalysisResult
viterbiPathMultiCandidateWithConfig _ [] = HarmonicAnalysisResult [] 0.0 False
viterbiPathMultiCandidateWithConfig config matrices@(firstMatrix : _) =
  let numMatrices = length matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix

      -- Initialize weight accumulation matrix
      initialWeights = listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat 0.0)

      -- Run main Viterbi algorithm
      (finalWeights, backtrackMatrices) = runViterbiLoopWithConfig config matrices initialWeights numRows numCols

      -- Add final matrix weights
      finalMatrix = last matrices
      weightsWithFinal = addFinalMatrixWeights finalWeights finalMatrix numRows numCols

      -- Find all optimal endpoints
      allMaxEndpoints = findAllMaximumWeights weightsWithFinal numRows numCols
      maxWeight = case allMaxEndpoints of
        [] -> 0.0
        ((_, _, w) : _) -> w

      -- Reconstruct all optimal paths
      allPaths =
        [ HarmonicPath $ reconstructPath matrices backtrackMatrices maxRow maxCol numMatrices numRows numCols
          | (maxRow, maxCol, _) <- allMaxEndpoints
        ]

      hasTies = length allMaxEndpoints > 1
   in HarmonicAnalysisResult allPaths maxWeight hasTies

-- | Reconstruct optimal path by backtracking
reconstructPath :: [RiemannMatrix] -> [Array (Int, Int) Int] -> Int -> Int -> Int -> Int -> Int -> [RMPoint]
reconstructPath matrices backtrackMatrices startRow startCol numMatrices numRows numCols =
  let -- Create final point
      finalMatrix = last matrices
      finalPoint =
        RMPoint
          { matrixIndex = numMatrices - 1,
            row = Row startRow,
            col = Col (fromIntegral startCol),
            value = fromMaybe 0.0 $ getValue finalMatrix (Row startRow) (Col (fromIntegral startCol))
          }

      -- Backtrack through matrices
      path = buildPath (numMatrices - 2) startRow startCol [finalPoint]
   in reverse path
  where
    buildPath matrixIndex currentRow currentCol acc
      | matrixIndex < 0 = acc
      | otherwise =
          let backtrackMatrix = backtrackMatrices !! matrixIndex
              sourceIndex = backtrackMatrix ! (currentRow, currentCol)
              sourceRow = sourceIndex `div` numCols
              sourceCol = sourceIndex `mod` numCols
              currentMatrix = matrices !! matrixIndex
              point =
                RMPoint
                  { matrixIndex = matrixIndex,
                    row = Row sourceRow,
                    col = Col (fromIntegral sourceCol),
                    value = fromMaybe 0.0 $ getValue currentMatrix (Row sourceRow) (Col (fromIntegral sourceCol))
                  }
           in buildPath (matrixIndex - 1) sourceRow sourceCol (point : acc)

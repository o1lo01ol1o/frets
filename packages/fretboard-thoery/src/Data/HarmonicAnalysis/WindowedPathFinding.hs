{-# LANGUAGE DataKinds #-}

-- | Windowed harmonic path finding implementation
-- Based on the original paper's approach using causal and final depth
-- This is different from the Viterbi algorithm used in Noll & Garbers (2004)
module Data.HarmonicAnalysis.WindowedPathFinding
  ( -- * Windowed Path Finding
    windowedPath,
    windowedPathWithConfig,

    -- * Configuration
    WindowedConfig (..),
    defaultWindowedConfig,
  )
where

import Data.Array (Array, listArray, (!))
import Data.HarmonicAnalysis.RiemannMatrix (getValue)
import Data.HarmonicAnalysis.Tension
import Data.HarmonicAnalysis.Types
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Mod (unMod)
import Data.Ord (comparing)
import qualified Data.Vector as V

-- | Configuration for windowed path finding
data WindowedConfig = WindowedConfig
  { -- | Number of chords to look backward (causal depth)
    causalDepth :: Int,
    -- | Number of chords to look forward (final depth)
    finalDepth :: Int,
    -- | Global threshold percentage (0-100)
    globalThreshold :: Int,
    -- | Local threshold percentage (0-100)
    localThreshold :: Int,
    -- | Tension table for transitions
    windowTensionTable :: TensionTable
  }
  deriving (Eq, Show)

-- | Default windowed configuration (matches Java BestLocalGerms defaults)
defaultWindowedConfig :: WindowedConfig
defaultWindowedConfig =
  WindowedConfig
    { causalDepth = 3,
      finalDepth = 3,
      globalThreshold = 8,
      localThreshold = 8,
      windowTensionTable = defaultTensionTable
    }

-- | Find optimal path using windowed approach (legacy version)
windowedPath :: [RiemannMatrix] -> WindowedConfig -> HarmonicPath
windowedPath [] _ = HarmonicPath []
windowedPath matrices config =
  let tensions = windowTensionTable config
      numMatrices = length matrices
      firstMatrix = head matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix

      -- Process each position with a sliding window
      points = processWindows matrices config 0 []
   in HarmonicPath points

-- | Find optimal path using runtime configuration and windowed approach
windowedPathWithConfig :: RuntimeConfig -> [RiemannMatrix] -> WindowedConfig -> HarmonicPath
windowedPathWithConfig _ [] _ = HarmonicPath []
windowedPathWithConfig runtimeConfig matrices windowConfig =
  let tensions = configTensionTable runtimeConfig
      updatedConfig = windowConfig {windowTensionTable = tensions}
      result = windowedPath matrices updatedConfig
      HarmonicPath points = result
      -- Convert points to use runtime configuration interpretation
      convertedPoints = map (convertPointWithConfig runtimeConfig) points
   in HarmonicPath convertedPoints

-- | Process sliding windows across the matrix sequence
processWindows :: [RiemannMatrix] -> WindowedConfig -> Int -> [RMPoint] -> [RMPoint]
processWindows matrices config currentPos accPoints
  | currentPos >= length matrices = accPoints
  | otherwise =
      let causalD = min (causalDepth config) currentPos
          finalD = min (finalDepth config) (length matrices - currentPos - 1)
          windowStart = currentPos - causalD
          windowEnd = currentPos + finalD
          windowMatrices = take (windowEnd - windowStart + 1) $ drop windowStart matrices

          -- Find best path in this window
          windowPath = findMaxWeightInWindow windowMatrices config windowStart

          -- Extract the point for current position (at index causalD in window)
          currentPoint =
            if causalD < length windowPath
              then windowPath !! causalD
              else constructDefaultPoint matrices currentPos
       in -- Continue with next position
          processWindows matrices config (currentPos + 1) (accPoints ++ [currentPoint])

-- | Find the path with maximum weight within a window
findMaxWeightInWindow :: [RiemannMatrix] -> WindowedConfig -> Int -> [RMPoint]
findMaxWeightInWindow [] _ _ = []
findMaxWeightInWindow matrices config startIndex =
  let windowSize = length matrices
      firstMatrix = head matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix

      -- Apply threshold filtering as in the Java implementation
      filteredMatrices =
        applyLocalThreshold (localThreshold config) $
          applyGlobalThreshold (globalThreshold config) matrices

      -- Generate all possible paths through the filtered window
      allPaths = generateAllWindowPaths filteredMatrices startIndex

      -- Calculate weight for each path
      pathWeights = map (calculatePathWeight filteredMatrices config) allPaths

      -- Find path with maximum weight
      bestPathIndex = findMaxIndex pathWeights
   in if null allPaths then [] else allPaths !! bestPathIndex

-- | Generate all possible paths through a window of matrices
generateAllWindowPaths :: [RiemannMatrix] -> Int -> [[RMPoint]]
generateAllWindowPaths [] _ = []
generateAllWindowPaths matrices startIndex =
  let windowSize = length matrices
      firstMatrix = head matrices
      numRows = rowCount firstMatrix
      numCols = colCount firstMatrix

      -- All possible points for each matrix
      allPoints =
        [ [ RMPoint
              { matrixIndex = startIndex + i,
                row = Row r,
                col = Col (fromIntegral c),
                value = fromMaybe 0.0 $ getValue matrix (Row r) (Col (fromIntegral c))
              }
            | r <- [0 .. numRows - 1],
              c <- [0 .. numCols - 1]
          ]
          | (i, matrix) <- zip [0 ..] matrices
        ]
   in cartesianProduct allPoints

-- | Calculate total weight of a path through matrices
calculatePathWeight :: [RiemannMatrix] -> WindowedConfig -> [RMPoint] -> Double
calculatePathWeight matrices config path =
  let matrixWeights = sum $ map value path
      transitionWeights = sum $ zipWith (calculateTransition config) path (tail path)
   in matrixWeights + transitionWeights

-- | Calculate transition weight between two adjacent points
calculateTransition :: WindowedConfig -> RMPoint -> RMPoint -> Double
calculateTransition config point1 point2 =
  let tensions = windowTensionTable config
      r1 = unRow $ row point1
      c1 = fromIntegral $ unMod $ unCol $ col point1
      r2 = unRow $ row point2
      c2 = fromIntegral $ unMod $ unCol $ col point2

      -- Calculate tonal, modal, and functional distances
      tonalDistance = calculateTonalDistance tensions c1 c2
      modalDistance = calculateModalDistance tensions r1 r2
      functionalDistance = calculateFunctionalDistance tensions r1 r2

      totalDistance = tonalDistance + modalDistance + functionalDistance
   in exp (-abs totalDistance) -- Negative exponential as mentioned in Java comments

-- | Calculate tonal distance using tension table
calculateTonalDistance :: TensionTable -> Int -> Int -> Double
calculateTonalDistance tensions c1 c2 =
  let tDiff = (c2 - c1 + 12) `mod` 12
      tonalTensions = tonalTension tensions
   in if not (V.null tonalTensions) && not (V.null (V.head tonalTensions))
        then (V.head tonalTensions) V.! (tDiff `mod` V.length (V.head tonalTensions))
        else 0.0

-- | Calculate modal distance using tension table
calculateModalDistance :: TensionTable -> Int -> Int -> Double
calculateModalDistance tensions r1 r2 =
  let mode1 = r1 `div` 3 -- T-S-D functions
      mode2 = r2 `div` 3
      modalTensions = modalTension tensions
   in if not (V.null modalTensions) && V.length modalTensions > mode1 && V.length (modalTensions V.! mode1) > mode2
        then (modalTensions V.! mode1) V.! mode2
        else 0.0

-- | Calculate functional distance using tension table
calculateFunctionalDistance :: TensionTable -> Int -> Int -> Double
calculateFunctionalDistance tensions r1 r2 =
  let func1 = r1 `mod` 3 -- T-S-D functions
      func2 = r2 `mod` 3
      funcTensions = functionalTension tensions
   in if not (V.null funcTensions) && V.length funcTensions > func1 && V.length (funcTensions V.! func1) > func2
        then (funcTensions V.! func1) V.! func2
        else 0.0

-- | Apply threshold filtering to a matrix (sets values below threshold to negative infinity)
filterMatrix :: Double -> RiemannMatrix -> RiemannMatrix
filterMatrix threshold matrix =
  let numRows = rowCount matrix
      numCols = colCount matrix
      updates = do
        r <- [0 .. numRows - 1]
        c <- [0 .. numCols - 1]
        case getValue matrix (Row r) (Col (fromIntegral c)) of
          Just val | val < threshold -> [((Row r, Col (fromIntegral c)), Just (negate (1.0 / 0.0)))] -- -Infinity
          _ -> []
   in updateMatrix matrix updates

-- | Apply global threshold filtering (percentage of global maximum)
applyGlobalThreshold :: Int -> [RiemannMatrix] -> [RiemannMatrix]
applyGlobalThreshold 0 matrices = matrices
applyGlobalThreshold percentage matrices =
  let globalMax = maximum $ concatMap getAllMatrixValues matrices
      threshold = (fromIntegral percentage / 100.0) * globalMax
   in map (filterMatrix threshold) matrices

-- | Apply local threshold filtering (percentage of each matrix's maximum)
applyLocalThreshold :: Int -> [RiemannMatrix] -> [RiemannMatrix]
applyLocalThreshold 0 matrices = matrices
applyLocalThreshold percentage matrices =
  map
    ( \matrix ->
        let localMax = maximum $ getAllMatrixValues matrix
            threshold = (fromIntegral percentage / 100.0) * localMax
         in filterMatrix threshold matrix
    )
    matrices

-- | Get all values from a matrix
getAllMatrixValues :: RiemannMatrix -> [Double]
getAllMatrixValues matrix =
  let numRows = rowCount matrix
      numCols = colCount matrix
   in [ fromMaybe 0.0 $ getValue matrix (Row r) (Col (fromIntegral c))
        | r <- [0 .. numRows - 1],
          c <- [0 .. numCols - 1]
      ]

-- | Update a matrix with new values
updateMatrix :: RiemannMatrix -> [((Row, Col), Maybe Double)] -> RiemannMatrix
updateMatrix originalMatrix updates =
  let matrixData = matrix originalMatrix
      updatedMatrix =
        foldl
          ( \m ((Row r, Col c), maybeVal) ->
              let currentRow = m V.! r
                  newRow = case maybeVal of
                    Just val -> currentRow V.// [(fromIntegral (unMod c), Just val)]
                    Nothing -> currentRow V.// [(fromIntegral (unMod c), Nothing)]
               in m V.// [(r, newRow)]
          )
          matrixData
          updates
   in originalMatrix {matrix = updatedMatrix}

-- | Cartesian product of lists of lists
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [x : ys | x <- xs, ys <- cartesianProduct xss]

-- | Find index of maximum element in list (first-encountered tie-breaking like Java)
findMaxIndex :: [Double] -> Int
findMaxIndex [] = 0
findMaxIndex xs =
  let indexed = zip [0 ..] xs
      -- Use first-encountered tie-breaking like Java's MaximumWeightIndex()
      (maxIdx, _) = foldl1 (\(idx1, val1) (idx2, val2) -> if val2 > val1 then (idx2, val2) else (idx1, val1)) indexed
   in maxIdx

-- | Construct a default point when window processing fails
constructDefaultPoint :: [RiemannMatrix] -> Int -> RMPoint
constructDefaultPoint matrices index =
  if index < length matrices
    then
      let matrix = matrices !! index
          -- Find the cell with maximum value as default
          bestPoint = findBestPointInMatrix matrix index
       in bestPoint
    else
      RMPoint
        { matrixIndex = index,
          row = Row 0,
          col = Col 0,
          value = 0.0
        }

-- | Find the point with maximum value in a matrix
findBestPointInMatrix :: RiemannMatrix -> Int -> RMPoint
findBestPointInMatrix matrix index =
  let numRows = rowCount matrix
      numCols = colCount matrix
      allPoints =
        [ (r, c, fromMaybe 0.0 $ getValue matrix (Row r) (Col (fromIntegral c)))
          | r <- [0 .. numRows - 1],
            c <- [0 .. numCols - 1]
        ]
      (bestR, bestC, bestVal) = maximumBy (comparing (\(_, _, v) -> v)) allPoints
   in RMPoint
        { matrixIndex = index,
          row = Row bestR,
          col = Col (fromIntegral bestC),
          value = bestVal
        }

-- | Convert point using runtime configuration
convertPointWithConfig :: RuntimeConfig -> RMPoint -> RMPoint
convertPointWithConfig config point =
  -- For now, just return the point as-is
  -- This could be extended to handle configuration-specific transformations
  point

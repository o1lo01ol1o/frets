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

import Data.HarmonicAnalysis.RiemannMatrix (getValue)
import qualified Data.HarmonicAnalysis.Tension as Tension
import Data.HarmonicAnalysis.Types
import Data.List (foldl, maximumBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Mod (Mod, unMod)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Debug.Trace (trace)

-- | Context required to evaluate transition penalties
data TransitionContext = TransitionContext
  { tcTensionTable :: TensionTable,
    tcNumFunctions :: Int,
    tcNumTonalities :: Int
  }

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
    windowTensionTable :: TensionTable,
    -- | Optional inclusion mask (row × column) restricting the search space
    windowInclusion :: Maybe (V.Vector (V.Vector Bool))
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
      windowTensionTable = Tension.makeDefaultTensionTable,
      windowInclusion = Nothing
    }

-- | Find optimal path using windowed approach (legacy version)
windowedPath :: [RiemannMatrix] -> WindowedConfig -> HarmonicPath
windowedPath [] _ = HarmonicPath []
windowedPath matrices config =
  let points = processWindows matrices config 0 []
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
  let filteredMatrices =
        applyLocalThreshold (localThreshold config) $
          applyGlobalThreshold (globalThreshold config) matrices
   in case filteredMatrices of
        [] -> []
        (firstMatrix : _) ->
          let tensions = windowTensionTable config
              context = makeTransitionContext tensions firstMatrix
              totalCols = colCount firstMatrix
              totalRows = rowCount firstMatrix
              inclusionMask = windowInclusion config

              negInf = negate (1 / 0)

              allPoints =
                [ [ RMPoint
                      { matrixIndex = startIndex + offset,
                        row = Row r,
                        col = Col (fromIntegral c),
                        value =
                          fromMaybe negInf $ getValue matrix (Row r) (Col (fromIntegral c))
                      }
                  | r <- [0 .. totalRows - 1],
                    c <- [0 .. totalCols - 1],
                    cellIncluded inclusionMask r c
                  ]
                | (offset, matrix) <- zip [0 ..] filteredMatrices
                ]

              allPaths = cartesianProduct allPoints
              weightedPaths =
                [ let weight = calculatePathWeight context path
                   in (path, weight)
                  | path <- allPaths
                ]
              sortedPaths = reverse (sortOn snd weightedPaths)
              _ =
                if startIndex == 0
                  then
                    trace
                      ("Window 0 candidate weights: "
                        ++ show
                          ( take 10
                              [ ( [ (unRow (row p), unMod (unCol (col p)), value p)
                                  | p <- path
                                  ], weight)
                                | (path, weight) <- sortedPaths
                              ]
                          )
                      )
                      ()
                  else ()
              chooseBest Nothing candidate = Just candidate
              chooseBest best@(Just (_, bestWeight)) candidate@(_, candWeight)
                | candWeight > bestWeight = Just candidate
                | otherwise = best
              bestCandidate = foldl chooseBest Nothing weightedPaths
              bestPathResult =
                case bestCandidate of
                  Just (bestPath, bestWeight) ->
                    let _ =
                          if startIndex == 0
                            then
                              trace
                                ( "Window 0 best path: "
                                    ++ show
                                      [ ( unRow (row p),
                                          unMod (unCol (col p)),
                                          value p
                                        )
                                      | p <- bestPath
                                      ]
                                    ++ ", weight="
                                    ++ show bestWeight
                                )
                                ()
                            else ()
                     in bestPath
                  Nothing -> []
            in bestPathResult

-- | Calculate total weight of a path through matrices
calculatePathWeight :: TransitionContext -> [RMPoint] -> Double
calculatePathWeight _ [] = 0
calculatePathWeight context path =
  let matrixWeights = sum $ map value path
      transitionWeights = sum $ zipWith (calculateTransition context) path (drop 1 path)
   in matrixWeights + transitionWeights

-- | Priority for breaking ties between equally weighted paths.
-- | Cartesian product helper (preserves order like Java).
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [x : ys | x <- xs, ys <- cartesianProduct xss]

-- | Check whether a matrix cell is allowed by the inclusion mask.
cellIncluded :: Maybe (V.Vector (V.Vector Bool)) -> Int -> Int -> Bool
cellIncluded Nothing _ _ = True
cellIncluded (Just mask) r c =
  case mask V.!? r >>= (\rowVec -> rowVec V.!? c) of
    Just allowed -> allowed
    Nothing -> False

-- | Calculate transition weight between two adjacent points
calculateTransition :: TransitionContext -> RMPoint -> RMPoint -> Double
calculateTransition context point1 point2 =
  let tensions = tcTensionTable context
      numFunctions = tcNumFunctions context
      numTonalities = tcNumTonalities context
      r1 = unRow $ row point1
      c1 = fromIntegral $ unMod $ unCol $ col point1
      r2 = unRow $ row point2
      c2 = fromIntegral $ unMod $ unCol $ col point2

      tonalDiff = positiveMod (c2 - c1) numTonalities
      tonalityDistance = findTonalDistance numTonalities (tonalTension tensions) tonalDiff

      safeNumFunctions = max 1 numFunctions
      sourceMode = r1 `div` safeNumFunctions
      targetMode = r2 `div` safeNumFunctions
      modeTensionVal = Tension.lookupTension (modalTension tensions) sourceMode targetMode
      modalDistance = squareWithSign modeTensionVal

      sourceFunction = r1 `mod` safeNumFunctions
      targetFunction = r2 `mod` safeNumFunctions
      funcTensionVal = Tension.lookupTension (functionalTension tensions) sourceFunction targetFunction
      functionalDistance = squareWithSign funcTensionVal

      totalDistance = tonalityDistance + modalDistance + functionalDistance
   in exp (negate $ abs totalDistance)

-- | Construct transition context from tension table and matrix dimensions
makeTransitionContext :: TensionTable -> RiemannMatrix -> TransitionContext
makeTransitionContext tensions matrix =
  let inferredFunctions =
        let ft = functionalTension tensions
         in if V.null ft
              then max 1 (rowCount matrix)
              else V.length ft
      inferredTonalities = colCount matrix
    in TransitionContext tensions inferredFunctions inferredTonalities

-- | Convert a tension value into a squared distance while preserving sign
squareWithSign :: Double -> Double
squareWithSign val =
  let sq = val * val
   in if val < 0 then negate sq else sq

-- | Tonal distance lookup mirroring the Java reference implementation
findTonalDistance :: Int -> V.Vector (V.Vector Double) -> Int -> Double
findTonalDistance numTonalities tonalTensions tonalDiff
  | V.null tonalTensions = 0
  | otherwise =
      let tryCircle rowIndex circleStep
            | rowIndex >= V.length tonalTensions = Nothing
            | otherwise =
                let rowVec = tonalTensions V.! rowIndex
                    rowLen = V.length rowVec
                    rowLenSafe = max 1 rowLen
                    limit = max 1 numTonalities
                    findInRow colIndex currentDiff
                      | colIndex >= limit = Nothing
                      | currentDiff == tonalDiff = rowVec V.!? (colIndex `mod` rowLenSafe)
                      | otherwise =
                          let nextDiff = positiveMod (currentDiff + circleStep) limit
                           in findInRow (colIndex + 1) nextDiff
                 in do
                      guardValue <- findInRow 0 0
                      pure guardValue

          result =
            case tryCircle 0 7 of
              Just val -> val
              Nothing ->
                case tryCircle 1 5 of
                  Just val -> val
                  Nothing -> 0

       in squareWithSign result

-- | Positive modulus helper replicating the Java logic
positiveMod :: Int -> Int -> Int
positiveMod i n =
  let modulus = if n <= 0 then 1 else n
      r = i `mod` modulus
   in if r < 0 then r + modulus else r

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

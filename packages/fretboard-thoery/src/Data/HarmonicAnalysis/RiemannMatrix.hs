{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.RiemannMatrix
  ( compute3rdChain,
    computeDirect,
    computeDirectWithConfig,
    chainContributionWithConfig,
    filterMatrix,
    getValue,
    setValue,
    updateMatrix,
    normalizeMatrix,
  )
where

import Data.HarmonicAnalysis.ChainOfThirds
import Data.HarmonicAnalysis.Types
import qualified Data.Map as Map
import Data.Mod (Mod, unMod)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Epsilon value to prevent arithmetic underflow
eps :: Double
eps = 1e-6

-- | Get value from a specific cell in the Riemann matrix
getValue :: RiemannMatrix -> Row -> Col -> Maybe Double
getValue rm (Row r) (Col c) =
  (matrix rm) V.! r V.! fromIntegral (unMod c)

-- | Set value in a specific cell of the Riemann matrix
setValue :: RiemannMatrix -> Row -> Col -> Maybe Double -> RiemannMatrix
setValue rm (Row r) (Col c) val =
  rm
    { matrix =
        matrix rm
          V.// [ ( r,
                   (matrix rm V.! r)
                     V.// [(fromIntegral (unMod c), val)]
                 )
               ]
    }

-- | Update multiple cells in the matrix
updateMatrix :: RiemannMatrix -> [((Row, Col), Maybe Double)] -> RiemannMatrix
updateMatrix = foldl (\rm ((r, c), v) -> setValue rm r c v)

-- | Normalize matrix values so they sum to 1
normalizeMatrix :: RiemannMatrix -> RiemannMatrix
normalizeMatrix rm =
  let allValues = do
        r <- [0 .. rowCount rm - 1]
        c <- [0 .. colCount rm - 1]
        case getValue rm (Row r) (Col (fromIntegral c)) of
          Just val -> [val]
          Nothing -> []
      total = sum allValues
      updates = do
        r <- [0 .. rowCount rm - 1]
        c <- [0 .. colCount rm - 1]
        case getValue rm (Row r) (Col (fromIntegral c)) of
          Just val -> [((Row r, Col (fromIntegral c)), Just (val / total))]
          Nothing -> [((Row r, Col (fromIntegral c)), Nothing)]
   in if total == 0 then rm else updateMatrix rm updates

-- | Compute Riemann matrix using chain of thirds method with runtime configuration
compute3rdChain ::
  -- | Runtime configuration
  RuntimeConfig ->
  -- | Chains of thirds for the pitch set
  [ChainOfThirds] ->
  -- | The pitch set being analyzed
  Set (Mod 12) ->
  RiemannMatrix
compute3rdChain config chains pitchSet =
  let numRows = configNumModes config * configNumFunctions config
      numCols = configNumTonalities config
      weights = configWeightTable config
      rm =
        RiemannMatrix
          { rowCount = numRows,
            colCount = numCols,
            matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
          }
      -- For each chain, compute contribution to matrix
      allUpdates = concatMap (chainContributionWithConfig config) chains
      -- Group contributions by position and sum them (Java-style accumulation)
      groupedUpdates = Map.toList $ Map.fromListWith (+) allUpdates
      -- Average the accumulated contributions by number of chains
      avgUpdates = case length chains of
        0 -> []
        n -> map (\((r, c), v) -> ((r, c), Just $ (v / fromIntegral n))) groupedUpdates
   in updateMatrix rm avgUpdates

-- | Compute matrix contributions from a single chain using runtime configuration
chainContributionWithConfig ::
  RuntimeConfig ->
  ChainOfThirds ->
  [((Row, Col), Double)]
chainContributionWithConfig config (ChainOfThirds pitches) = do
  let weights = configWeightTable config
      numRows = configNumModes config * configNumFunctions config
      numCols = configNumTonalities config
  -- For each possible tonality
  t <- [0 .. numCols - 1]
  -- For each row in the weight table
  r <- [0 .. numRows - 1]
  let row = Row r
      col = Col $ fromIntegral t
      -- Sum weights for each pitch in chain relative to tonality
      value =
        sum
          [ weights
              V.! ( unRow row * numCols
                      + fromIntegral ((fromIntegral (unMod p) - fromIntegral t + numCols) `mod` numCols)
                  )
            | p <- Set.toList pitches
          ]
  pure ((row, col), value)

-- | Legacy wrapper for backward compatibility
chainContribution ::
  Vector Double ->
  ChainOfThirds ->
  [((Row, Col), Double)]
chainContribution weights (ChainOfThirds pitches) = do
  let numRows = V.length weights `div` 12
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

-- | Compute Riemann matrix using direct method with runtime configuration
computeDirectWithConfig ::
  -- | Runtime configuration
  RuntimeConfig ->
  -- | The pitch set being analyzed
  Set (Mod 12) ->
  RiemannMatrix
computeDirectWithConfig config pitchSet =
  let weights = configWeightTable config
      numRows = configNumModes config * configNumFunctions config
      numCols = configNumTonalities config
      rm =
        RiemannMatrix
          { rowCount = numRows,
            colCount = numCols,
            matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
          }
      pitches = Set.map (fromIntegral . unMod) pitchSet
      updates = do
        -- For each possible tonality
        t <- [0 .. numCols - 1]
        -- For each row in the weight table
        r <- [0 .. numRows - 1]
        let row = Row r
            col = Col $ fromIntegral t
            -- Sum weights for each pitch relative to tonality
            value =
              sum
                [ weights
                    V.! ( unRow row * numCols
                            + fromIntegral ((fromIntegral p - fromIntegral t + numCols) `mod` numCols)
                        )
                  | p <- Set.toList pitches
                ]
        pure ((row, col), Just value)
   in updateMatrix rm updates

-- | Legacy direct computation method
computeDirect ::
  -- | Weight table
  Vector Double ->
  -- | The pitch set being analyzed
  Set (Mod 12) ->
  RiemannMatrix
computeDirect weights pitchSet =
  let numRows = V.length weights `div` 12
      numCols = 12
      rm =
        RiemannMatrix
          { rowCount = numRows,
            colCount = numCols,
            matrix = V.replicate numRows (V.replicate numCols (Just 0.0))
          }
      pitches = Set.map (fromIntegral . unMod) pitchSet
      updates = do
        -- For each possible tonality
        t <- [0 .. numTonalities - 1]
        -- For each row in the weight table
        r <- [0 .. numRows - 1]
        let row = Row r
            col = Col $ fromIntegral t
            -- Sum weights for each pitch relative to tonality
            value =
              sum
                [ weights
                    V.! ( unRow row * 12
                            + fromIntegral ((fromIntegral p - fromIntegral t + 12) `mod` 12)
                        )
                  | p <- Set.toList pitches
                ]
        pure ((row, col), Just value)
   in updateMatrix rm updates

-- | Filter matrix values below threshold to Nothing
filterMatrix :: Double -> RiemannMatrix -> RiemannMatrix
filterMatrix threshold rm =
  let updates = do
        r <- [0 .. rowCount rm - 1]
        c <- [0 .. colCount rm - 1]
        let row = Row r
            col = Col $ fromIntegral c
        case getValue rm row col of
          Just val | val < threshold -> pure ((row, col), Nothing)
          _ -> []
   in updateMatrix rm updates

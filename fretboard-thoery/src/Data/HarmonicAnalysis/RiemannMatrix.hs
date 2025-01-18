{-# LANGUAGE TupleSections #-}

module Data.HarmonicAnalysis.RiemannMatrix
  ( compute3rdChain,
    computeDirect,
    filterMatrix,
    getValue,
    setValue,
    updateMatrix,
  )
where

import Chord (Chord (..), chordToChromatics)
import Control.Monad (forM_, guard)
import Data.HarmonicAnalysis.ChainOfThirds
import Data.HarmonicAnalysis.Types
import Data.Mod
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Modulation (Chromatic (..), toLocalInterpretation)

-- | Get value from a specific cell in the Riemann matrix
getValue :: RiemannMatrix -> Row -> Col -> Maybe Double
getValue rm (Row r) (Col c) =
  (matrix rm) V.! fromIntegral r V.! fromIntegral (unMod c)

-- | Set value in a specific cell of the Riemann matrix
setValue :: RiemannMatrix -> Row -> Col -> Maybe Double -> RiemannMatrix
setValue rm (Row r) (Col c) val =
  rm
    { matrix =
        matrix rm
          V.// [ ( fromIntegral r,
                   (matrix rm V.! fromIntegral r)
                     V.// [(fromIntegral (unMod c), val)]
                 )
               ]
    }

-- | Update multiple cells in the matrix
updateMatrix :: RiemannMatrix -> [((Row, Col), Maybe Double)] -> RiemannMatrix
updateMatrix = foldl (\rm ((r, c), v) -> setValue rm r c v)

-- | Compute Riemann matrix using chain of thirds method
compute3rdChain ::
  -- | Chains found for this chord
  [ChainOfThirds] ->
  -- | Weight table
  Vector Double ->
  -- | Number of tonalities (usually 12)
  Word ->
  -- | Number of modes
  Word ->
  -- | Number of functions
  Word ->
  -- | The chord being analyzed
  Chord ->
  RiemannMatrix
compute3rdChain chains weights tonSize modeSize funSize ch =
  let rm = emptyRiemannMatrix ch
      -- For each chain, compute contribution to matrix
      updates = concatMap (chainContribution weights tonSize modeSize funSize) chains
      -- Average the contributions if there are any
      avgUpdates = case length chains of
        0 -> []
        n -> map (\((r, c), v) -> ((r, c), Just $ v / fromIntegral n)) updates
   in updateMatrix rm avgUpdates

-- | Compute matrix contributions from a single chain
chainContribution ::
  Vector Double ->
  Word ->
  Word ->
  Word ->
  ChainOfThirds ->
  [((Row, Col), Double)]
chainContribution weights tonSize modeSize funSize (ChainOfThirds pitches) = do
  -- For each possible tonality
  t <- [0 .. tonSize - 1]
  -- For each mode/function combination
  m <- [0 .. modeSize - 1]
  f <- [0 .. funSize - 1]
  let row = modeFunctionToRow funSize (toEnum $ fromIntegral m) (toEnum $ fromIntegral f)
      col = Col $ fromIntegral t
      -- Sum weights for each pitch in chain relative to tonality
      value =
        sum
          [ weights
              V.! ( fromIntegral (m * funSize + f) * fromIntegral tonSize
                      + fromIntegral ((p - fromIntegral t) `mod` 12)
                  )
            | p <- Set.toList pitches
          ]
  pure ((row, col), value)

-- | Compute Riemann matrix using direct method
computeDirect ::
  -- | Weight table
  Vector Double ->
  -- | Number of tonalities
  Word ->
  -- | Number of modes
  Word ->
  -- | Number of functions
  Word ->
  -- | The chord being analyzed
  Chord ->
  RiemannMatrix
computeDirect weights tonSize modeSize funSize ch =
  let rm = emptyRiemannMatrix ch
      pitches = Set.map (fromIntegral . unMod) $ chordToChromatics ch
      updates = do
        -- For each possible tonality
        t <- [0 .. tonSize - 1]
        -- For each mode/function combination
        m <- [0 .. modeSize - 1]
        f <- [0 .. funSize - 1]
        let row = modeFunctionToRow funSize (toEnum $ fromIntegral m) (toEnum $ fromIntegral f)
            col = Col $ fromIntegral t
            -- Sum weights for each pitch relative to tonality
            value =
              sum
                [ weights
                    V.! ( fromIntegral (m * funSize + f) * fromIntegral tonSize
                            + fromIntegral ((p - fromIntegral t) `mod` 12)
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

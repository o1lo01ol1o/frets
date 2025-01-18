
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.HarmonicAnalysis.PathFinding
  ( viterbiPath
  , bestLocalGerms
  , PathfindingConfig(..)
  , defaultConfig
  ) where

import Data.HarmonicAnalysis.Types
import Data.HarmonicAnalysis.RiemannMatrix (getValue)
import Data.HarmonicAnalysis.Tension

import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe (catMaybes)
import Control.Monad (guard)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word

-- | Configuration for pathfinding algorithms
data PathfindingConfig = PathfindingConfig
  { causalDepth :: Word  -- ^ How far back to look for causal connections
  , finalDepth :: Word   -- ^ How far forward to look for final connections
  , useMetricProximity :: Bool  -- ^ Whether to consider metric relationships
  } deriving (Show, Eq)

defaultConfig :: PathfindingConfig
defaultConfig = PathfindingConfig
  { causalDepth = 1
  , finalDepth = 2
  , useMetricProximity = False
  }

-- | Find optimal path using Viterbi algorithm
viterbiPath :: [RiemannMatrix]  -- ^ Sequence of matrices to analyze
            -> TensionTable     -- ^ Tension/transition weights
            -> HarmonicPath     -- ^ Optimal path through matrices
viterbiPath matrices tensions = HarmonicPath $
  let
    -- Initialize tracking matrices
    numMatrices = length matrices
    rm0 = head matrices

    -- Matrix to store best weights
    weights :: Vector (Vector Double)
    weights = V.generate (fromIntegral $ rowCount rm0) $ \r ->
      V.generate 12 $ \c ->
        case getValue rm0 (Row $ fromIntegral r) (Col $ fromIntegral c) of
          Just v -> v
          Nothing -> 0.0

    -- Matrix to store backpointers
    backpointers :: Vector (Vector (Maybe (Row, Col)))
    backpointers = V.replicate (fromIntegral $ rowCount rm0)
                              (V.replicate 12 Nothing)

    -- Forward pass
    (!finalWeights, !finalBackpointers) =
      foldl (forwardStep tensions) (weights, backpointers)
            (zip [1..] $ tail matrices)

    -- Backtrack
    finalPath = backtrack matrices finalWeights finalBackpointers
  in
    finalPath

-- | Forward step of Viterbi algorithm
forwardStep :: TensionTable
            -> (Vector (Vector Double), Vector (Vector (Maybe (Row, Col))))
            -> (Int, RiemannMatrix)
            -> (Vector (Vector Double), Vector (Vector (Maybe (Row, Col))))
forwardStep tensions (!weights, !backpointers) (i, rm) =
  let
    newWeights = V.generate (fromIntegral $ rowCount rm) $ \r ->
      V.generate 12 $ \c -> do
        let currentRow = Row $ fromIntegral r
            currentCol = Col $ fromIntegral c
        case getValue rm currentRow currentCol of
          Nothing -> 0.0
          Just currentVal ->
            -- Find best previous state
            let candidates = do
                  pr <- [0..V.length weights - 1]
                  pc <- [0..11]
                  let prevRow = Row $ fromIntegral pr
                      prevCol = Col $ fromIntegral pc
                      prevWeight = (weights V.! pr) V.! pc
                      transition = transitionWeight tensions
                                   (prevRow, prevCol)
                                   (currentRow, currentCol)
                  pure (prevWeight + transition + currentVal, (prevRow, prevCol))
            in case candidates of
                 [] -> currentVal
                 cs -> fst $ maximumBy (comparing fst) cs

    newBackpointers = V.generate (fromIntegral $ rowCount rm) $ \r ->
      V.generate 12 $ \c -> do
        let currentRow = Row $ fromIntegral r
            currentCol = Col $ fromIntegral c
        case getValue rm currentRow currentCol of
          Nothing -> Nothing
          Just _ ->
            -- Find best previous state
            let candidates = do
                  pr <- [0..V.length weights - 1]
                  pc <- [0..11]
                  let prevRow = Row $ fromIntegral pr
                      prevCol = Col $ fromIntegral pc
                      prevWeight = (weights V.! pr) V.! pc
                      transition = transitionWeight tensions
                                   (prevRow, prevCol)
                                   (currentRow, currentCol)
                  pure (prevWeight + transition, (prevRow, prevCol))
            in case candidates of
                 [] -> Nothing
                 cs -> Just $ snd $ maximumBy (comparing fst) cs
  in
    (newWeights, newBackpointers)

-- | Backtrack to construct optimal path
backtrack :: [RiemannMatrix]
          -> Vector (Vector Double)
          -> Vector (Vector (Maybe (Row, Col)))
          -> [RMPoint]
backtrack matrices weights backpointers =
  let
    -- Find best final state
    (finalRow, finalCol) = maximumBy (comparing score) $ do
      r <- [0..V.length weights - 1]
      c <- [0..11]
      pure (Row $ fromIntegral r, Col $ fromIntegral c)
    where score (Row r, Col c) = (weights V.! fromIntegral r) V.! fromIntegral (unMod c)

    -- Follow backpointers
    buildPath :: Int -> Row -> Col -> [RMPoint]
    buildPath i row col
      | i < 0 = []
      | otherwise =
          let rm = matrices !! i
              point = RMPoint
                { matrixIndex = fromIntegral i
                , rmChord = chord rm
                , row = row
                , col = col
                , value = case getValue rm row col of
                           Just v -> v
                           Nothing -> 0.0
                }
          in case backpointers V.! fromIntegral (unRow row) V.! fromIntegral (unMod col) of
               Nothing -> [point]
               Just (prevRow, prevCol) ->
                 point : buildPath (i-1) prevRow prevCol
  in
    reverse $ buildPath (length matrices - 1) finalRow finalCol

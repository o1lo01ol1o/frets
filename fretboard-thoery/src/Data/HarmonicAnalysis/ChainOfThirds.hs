{-# LANGUAGE TupleSections #-}

module Data.HarmonicAnalysis.ChainOfThirds
  ( findChainsOfThirds,
    chainRepository,
    transposeChain,
    isSubsetOfChain,
    normalizeChain,
  )
where

import Chord (Chord (..), chordToChromatics)
import Data.HarmonicAnalysis.Types
import Data.List (minimumBy)
import Data.Mod
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Modulation (Chromatic (..))

-- | Standard chain of thirds patterns from music theory
-- These represent common chord structures
chainRepository :: [ChainOfThirds]
chainRepository =
  map
    (ChainOfThirds . Set.fromList . map fromIntegral)
    [ -- Major/minor thirds
      [0, 3],
      [0, 4], -- minor third, major third
      -- Triads
      [0, 3, 6], -- diminished
      [0, 3, 7], -- minor
      [0, 4, 7], -- major
      [0, 4, 8], -- augmented
      -- Seventh chords
      [0, 3, 6, 9], -- fully diminished
      [0, 3, 6, 10], -- half diminished
      [0, 3, 7, 10], -- minor seventh
      [0, 4, 7, 10], -- dominant seventh
      [0, 4, 7, 11], -- major seventh
      -- Extended tertian structures
      [0, 3, 7, 10, 14], -- minor ninth
      [0, 4, 7, 10, 14], -- dominant ninth
      [0, 4, 7, 11, 14] -- major ninth
    ]

-- | Find all possible chains of thirds that could represent a chord
findChainsOfThirds :: Chord -> [ChainOfThirds]
findChainsOfThirds chord =
  let pitchClasses = Set.map (fromIntegral . unMod) $ chordToChromatics chord
      normalizedPitches = normalizeChain $ ChainOfThirds pitchClasses

      -- Find all matching chains from repository
      matchingChains = filter (isSubsetOfChain normalizedPitches) chainRepository

      -- Get minimal chains (no proper subset is also a match)
      minimalChains = filter isMinimal matchingChains

      -- Transpose back to original pitch level
      originalPitch = minimum $ Set.toList pitchClasses
      transposedChains = map (transposeChain originalPitch) minimalChains
   in transposedChains
  where
    isMinimal chain =
      not $
        any
          ( \other ->
              chainPitches other `Set.isProperSubsetOf` chainPitches chain
                && other `elem` matchingChains
          )
          matchingChains

-- | Transpose a chain of thirds by a given interval
transposeChain :: Int -> ChainOfThirds -> ChainOfThirds
transposeChain interval (ChainOfThirds pitches) =
  ChainOfThirds $ Set.map (\p -> (p + interval) `mod` 12) pitches

-- | Test if one chain is a subset of another
isSubsetOfChain :: ChainOfThirds -> ChainOfThirds -> Bool
isSubsetOfChain (ChainOfThirds a) (ChainOfThirds b) = a `Set.isSubsetOf` b

-- | Normalize a chain by transposing it to start from 0
normalizeChain :: ChainOfThirds -> ChainOfThirds
normalizeChain (ChainOfThirds pitches) =
  let minPitch = minimum $ Set.toList pitches
      interval = (12 - minPitch) `mod` 12
   in transposeChain interval (ChainOfThirds pitches)

-- Helper function to find all rotations of a chain that preserve its third structure
rotateChain :: ChainOfThirds -> [ChainOfThirds]
rotateChain (ChainOfThirds pitches) =
  let positions = Set.toList pitches
   in map
        ( \p ->
            normalizeChain $
              ChainOfThirds $
                Set.fromList $
                  map (\x -> (x - p) `mod` 12) $
                    Set.toList pitches
        )
        positions

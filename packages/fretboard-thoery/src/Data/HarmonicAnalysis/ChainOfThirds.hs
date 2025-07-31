{-# LANGUAGE DataKinds #-}

module Data.HarmonicAnalysis.ChainOfThirds
  ( findChainsOfThirds,
    chainRepository,
    transposeChain,
    isSubsetOfChain,
    normalizeChain,
  )
where

import Data.HarmonicAnalysis.Types
import Data.Mod (Mod, unMod)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Standard chain of thirds patterns from music theory
-- These represent common chord structures in root position
chainRepository :: [ChainOfThirds]
chainRepository =
  map
    (ChainOfThirds . Set.fromList . map fromIntegral)
    [ -- Intervals
      [0, 3], -- minor third
      [0, 4], -- major third
      -- Triads
      [0, 3, 6], -- diminished
      [0, 3, 7], -- minor
      [0, 4, 7], -- major
      [0, 4, 8], -- augmented
      -- Triad inversions and wide voicings
      [0, 4, 9], -- major triad with fifth up an octave
      [0, 5, 9], -- major triad first inversion wide voicing
      [0, 3, 9], -- minor triad with fifth up an octave
      [0, 5, 8], -- minor triad first inversion wide voicing
      -- Seventh chords
      [0, 3, 6, 9], -- fully diminished seventh
      [0, 3, 6, 10], -- half diminished seventh
      [0, 3, 7, 10], -- minor seventh
      [0, 4, 7, 10], -- dominant seventh
      [0, 4, 7, 11], -- major seventh
      -- Extended chords
      [0, 3, 7, 10, 14], -- minor ninth
      [0, 4, 7, 10, 14], -- dominant ninth
      [0, 4, 7, 11, 14] -- major ninth
    ]

-- | Find all possible chains of thirds that could represent a pitch set
-- Tests each pitch in the set as a potential root to find all matching patterns
findChainsOfThirds :: Set (Mod 12) -> [ChainOfThirds]
findChainsOfThirds pitchSet =
  let -- Test each pitch as a potential root
      pitchList = Set.toList pitchSet
      allMatches = concatMap (findMatchesWithRoot pitchSet) pitchList

      -- Filter to minimal chains (remove chains that are proper subsets of others)
      minimalChains = filter (isMinimalChain allMatches) allMatches
   in minimalChains
  where
    isMinimalChain allChains candidate =
      not $ any (isProperSubsetChain candidate) allChains

    isProperSubsetChain (ChainOfThirds a) (ChainOfThirds b) =
      a `Set.isProperSubsetOf` b

-- | Find all matching chain patterns with a specific root
findMatchesWithRoot :: Set (Mod 12) -> Mod 12 -> [ChainOfThirds]
findMatchesWithRoot pitchSet root =
  let -- Normalize the pitch set so the root becomes 0
      normalizedPitchSet = transposeChainToRoot root (ChainOfThirds pitchSet)

      -- Find chains from repository that are subsets of the normalized pitch set
      matchingChains = filter (isSubsetOfChain normalizedPitchSet) chainRepository

      -- Transpose matches back to original pitch level
      originalTransposition = fromIntegral (unMod root)
      transposedMatches = map (transposeChain originalTransposition) matchingChains
   in transposedMatches

-- | Transpose a chain of thirds by a given interval (in semitones)
transposeChain :: Int -> ChainOfThirds -> ChainOfThirds
transposeChain interval (ChainOfThirds pitches) =
  ChainOfThirds $ Set.map (transposePitch interval) pitches
  where
    transposePitch n pitch =
      fromIntegral ((fromIntegral (unMod pitch) + n) `mod` 12)

-- | Test if one chain is a subset of another
isSubsetOfChain :: ChainOfThirds -> ChainOfThirds -> Bool
isSubsetOfChain (ChainOfThirds subset) (ChainOfThirds superset) =
  subset `Set.isSubsetOf` superset

-- | Normalize a chain by transposing it so its lowest pitch becomes 0
normalizeChain :: ChainOfThirds -> ChainOfThirds
normalizeChain (ChainOfThirds pitches) =
  case Set.toList pitches of
    [] -> ChainOfThirds pitches
    ps ->
      let minPitch = minimum ps
          interval = (12 - fromIntegral (unMod minPitch)) `mod` 12
       in transposeChain interval (ChainOfThirds pitches)

-- | Transpose a chain so that a specific pitch becomes the root (0)
transposeChainToRoot :: Mod 12 -> ChainOfThirds -> ChainOfThirds
transposeChainToRoot rootPitch (ChainOfThirds pitches) =
  let interval = (12 - fromIntegral (unMod rootPitch)) `mod` 12
   in transposeChain interval (ChainOfThirds pitches)

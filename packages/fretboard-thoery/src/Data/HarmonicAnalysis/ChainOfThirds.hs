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
import Data.List (nub)
import Data.Mod (Mod, unMod)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Standard chain of thirds patterns from music theory
-- These represent common chord structures in root position
chainRepository :: [ChainOfThirds]
chainRepository =
  map (ChainOfThirds . Set.fromList . map fromIntegral) rawChains
  where
    rawChains :: [[Int]]
    rawChains =
      [ [0, 3]
      , [0, 4]
      , [0, 3, 6]
      , [0, 3, 7]
      , [0, 4, 7]
      , [0, 4, 8]
      , [0, 3, 6, 9]
      , [0, 3, 6, 10]
      , [0, 3, 7, 10]
      , [0, 3, 7, 11]
      , [0, 4, 7, 10]
      , [0, 4, 7, 11]
      , [0, 4, 8, 11]
      , [0, 3, 6, 9, 1]
      , [0, 3, 6, 10, 1]
      , [0, 3, 6, 10, 2]
      , [0, 3, 7, 10, 1]
      , [0, 3, 7, 10, 2]
      , [0, 3, 7, 11, 2]
      , [0, 4, 7, 10, 1]
      , [0, 4, 7, 10, 2]
      , [0, 4, 7, 11, 2]
      , [0, 4, 7, 11, 3]
      , [0, 4, 8, 11, 2]
      , [0, 4, 8, 11, 3]
      , [0, 3, 6, 9, 1, 4]
      , [0, 3, 6, 9, 1, 5]
      , [0, 3, 6, 10, 1, 4]
      , [0, 3, 6, 10, 1, 5]
      , [0, 3, 6, 10, 2, 5]
      , [0, 3, 7, 10, 1, 4]
      , [0, 3, 7, 10, 1, 5]
      , [0, 3, 7, 10, 2, 5]
      , [0, 3, 7, 10, 2, 6]
      , [0, 3, 7, 11, 2, 5]
      , [0, 3, 7, 11, 2, 6]
      , [0, 4, 7, 10, 1, 5]
      , [0, 4, 7, 10, 2, 5]
      , [0, 4, 7, 10, 2, 6]
      , [0, 4, 7, 11, 2, 5]
      , [0, 4, 7, 11, 2, 6]
      , [0, 4, 7, 11, 3, 6]
      , [0, 4, 8, 11, 2, 5]
      , [0, 4, 8, 11, 2, 6]
      , [0, 4, 8, 11, 3, 6]
      , [0, 4, 8, 11, 3, 7]
      , [0, 3, 6, 9, 1, 4, 7]
      , [0, 3, 6, 9, 1, 4, 8]
      , [0, 3, 6, 9, 1, 5, 8]
      , [0, 3, 6, 10, 1, 4, 7]
      , [0, 3, 6, 10, 1, 4, 8]
      , [0, 3, 6, 10, 1, 5, 8]
      , [0, 3, 6, 10, 1, 5, 9]
      , [0, 3, 6, 10, 2, 5, 8]
      , [0, 3, 6, 10, 2, 5, 9]
      , [0, 3, 7, 10, 1, 4, 8]
      , [0, 3, 7, 10, 1, 5, 8]
      , [0, 3, 7, 10, 1, 5, 9]
      , [0, 3, 7, 10, 2, 5, 8]
      , [0, 3, 7, 10, 2, 5, 9]
      , [0, 3, 7, 10, 2, 6, 9]
      , [0, 3, 7, 11, 2, 5, 8]
      , [0, 3, 7, 11, 2, 5, 9]
      , [0, 3, 7, 11, 2, 6, 9]
      , [0, 3, 7, 11, 2, 6, 10]
      , [0, 4, 7, 10, 1, 5, 8]
      , [0, 4, 7, 10, 1, 5, 9]
      , [0, 4, 7, 10, 2, 5, 8]
      , [0, 4, 7, 10, 2, 5, 9]
      , [0, 4, 7, 10, 2, 6, 9]
      , [0, 4, 7, 11, 2, 5, 8]
      , [0, 4, 7, 11, 2, 5, 9]
      , [0, 4, 7, 11, 2, 6, 9]
      , [0, 4, 7, 11, 2, 6, 10]
      , [0, 4, 7, 11, 3, 6, 9]
      , [0, 4, 7, 11, 3, 6, 10]
      , [0, 4, 8, 11, 2, 5, 9]
      , [0, 4, 8, 11, 2, 6, 9]
      , [0, 4, 8, 11, 2, 6, 10]
      , [0, 4, 8, 11, 3, 6, 9]
      , [0, 4, 8, 11, 3, 6, 10]
      , [0, 4, 8, 11, 3, 7, 10]
      , [0, 3, 6, 9, 1, 4, 7, 10]
      , [0, 3, 6, 9, 1, 4, 7, 11]
      , [0, 3, 6, 9, 1, 4, 8, 11]
      , [0, 3, 6, 9, 1, 5, 8, 11]
      , [0, 3, 6, 10, 1, 4, 7, 11]
      , [0, 3, 6, 10, 1, 4, 8, 11]
      , [0, 3, 6, 10, 1, 5, 8, 11]
      , [0, 3, 6, 10, 2, 5, 8, 11]
      , [0, 3, 6, 10, 2, 5, 9, 11]
      ]

-- | Find all possible chains of thirds that could represent a pitch set
-- Tests each pitch in the set as a potential root to find all matching patterns
findChainsOfThirds :: Set (Mod 12) -> [ChainOfThirds]
findChainsOfThirds pitchSet
  | null roots = []
  | otherwise =
      case equalityMatches of
        [] -> minimalSupersets
        matches -> matches
  where
    roots = Set.toList pitchSet
    chordInts = Set.map (fromIntegral . unMod) pitchSet
    chordSize = Set.size chordInts

    normalizeBy root = Set.map (\p -> (p - root) `mod` 12)

    chainToIntSet (ChainOfThirds s) = Set.map (\m -> fromIntegral (unMod m) `mod` 12) s

    equalityMatches =
      let go [] = []
          go (chain : rest)
            | Set.size (chainToIntSet chain) /= chordSize = go rest
            | otherwise =
                let matches =
                      [ transposeChain rootInt chain
                        | r <- roots
                        , let rootInt = fromIntegral (unMod r)
                        , normalizeBy rootInt chordInts == chainToIntSet chain
                      ]
                 in if null matches then go rest else nub matches
       in go chainRepository

    minimalSupersets =
      let go [] _ acc = acc
          go (chain : rest) currentMin acc =
            let chainSize = Set.size (chainToIntSet chain)
                continue =
                  case currentMin of
                    Just minSize | chainSize > minSize -> False
                    _ -> True
             in if not continue
                  then acc
                  else
                    let matches =
                          [ transposeChain rootInt chain
                            | r <- roots
                            , let rootInt = fromIntegral (unMod r)
                            , normalizeBy rootInt chordInts `Set.isSubsetOf` chainToIntSet chain
                          ]
                        newAcc = if null matches then acc else acc ++ matches
                        newMin = if null matches then currentMin else Just chainSize
                     in go rest newMin newAcc
       in nub $ go chainRepository Nothing []

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

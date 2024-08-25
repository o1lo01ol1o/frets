{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Fretboard (tests) where

import Chord
import Control.Monad (when)
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Finger
import Fretboard
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Modulation
import Test.Tasty
import Test.Tasty.Hedgehog

-- Main test group
tests :: TestTree
tests =
  testGroup
    "Fretboard Tests"
    [ --   testProperty "findFrettings generates valid frettings" prop_findFrettings_valid,
      -- testProperty "calculateDistance is consistent" prop_calculateDistance_consistent,
      -- testProperty "optimizeFrettings generates valid frettings" prop_optimizeFrettings_valid,
      -- testProperty "genFretboard generates valid fretboards" prop_genFretboard_valid,
      testGroup
        "findFrettings granular tests"
        [ -- testProperty "multiple chromatics" prop_findFrettings_multipleChromatics,
          testProperty "findFrettings generates valid Am7 fretting" prop_findFrettings_am7,
          testProperty "findFrettings generates valid Fs9 fretting" prop_findFrettings_fs9
        ]
    ]

-- Generator for Fretboard
genFretboard :: Gen Fretboard
genFretboard = do
  numStrings <- Gen.int (Range.linear 4 8)
  tuning <- Gen.list (Range.singleton numStrings) genChromatic
  return $ Fretboard numStrings tuning

-- Generator for Chromatic
genChromatic :: Gen Chromatic
genChromatic = Gen.element [C, Cs, D, Eb, E, F, Fs, G, Gs, A, Bb, B]

-- Generator for Set Chromatic
genChromaticSet :: Gen (Set Chromatic)
genChromaticSet = do
  shuffled <- Gen.shuffle [minBound .. maxBound]
  size <- Gen.int (Range.linear 2 6)
  return $ Set.fromList $ take size shuffled

-- Generator for Fretting
genFretting :: Fretboard -> Gen Fretting
genFretting fretboard = do
  frets <- Gen.filter isValid $ Gen.set (Range.linear 1 (numStrings fretboard)) $ do
    string <- Gen.int (Range.linear 1 (numStrings fretboard))
    fret <- Gen.int (Range.linear 1 24)
    finger <- Gen.element [Thumb, Index, Middle, Ring, Pinky]
    return (string, Just (finger, fret))
  return $ Fretting fretboard frets
  where
    isValid frets = isValidFretting (Fretting fretboard frets)

-- Helper function to footnote draw a fretting given a condition
footnoteFrettingIf :: (Fretting -> Bool) -> Fretting -> PropertyT IO ()
footnoteFrettingIf condition fretting =
  when (condition fretting) $
    footnote $
      "  " ++ drawFretting fretting

-- Property: findFrettings generates valid frettings
prop_findFrettings_valid :: Property
prop_findFrettings_valid = property $ do
  fretboard <- forAll genFretboard
  chromatics <- forAll genChromaticSet
  let frettings = Set.toList $ Set.map snd $ findFrettings 10 fretboard chromatics
      invalidFrettings = filter (not . isValidFretting) frettings
  if null invalidFrettings
    then assert $ all isValidFretting frettings
    else do
      mapM_ (footnoteFrettingIf (not . isValidFretting)) frettings

-- Property: calculateDistance is consistent
prop_calculateDistance_consistent :: Property
prop_calculateDistance_consistent = property $ do
  fretboard <- forAll genFretboard
  frettings <- forAll $ Gen.list (Range.linear 2 5) (genFretting fretboard)
  let distances = calculateDistance frettings (reverse frettings)
  assert (distances >= 0)

-- Property: optimizeFrettings generates valid frettings
prop_optimizeFrettings_valid :: Property
prop_optimizeFrettings_valid = property $ do
  fretboard <- forAll genFretboard
  chromaticSets <- forAll $ Gen.list (Range.linear 2 5) genChromaticSet
  let optimizedFrettings = optimizeFrettings 10 fretboard chromaticSets
  assert $ all isValidFretting optimizedFrettings

-- Property: genFretboard generates valid fretboards
prop_genFretboard_valid :: Property
prop_genFretboard_valid = property $ do
  fretboard <- forAll genFretboard
  assert $ numStrings fretboard >= 4 && numStrings fretboard <= 8
  assert $ length (tuning fretboard) == numStrings fretboard

-- Property: findFrettings generates valid frettings for multiple chromatics
prop_findFrettings_multipleChromatics :: Property
prop_findFrettings_multipleChromatics = property $ do
  fretboard <- forAll genFretboard
  chromatics <- forAll genChromaticSet
  let frettings = findFrettings 10 fretboard chromatics
  mapM_ (footnoteFrettingIf (not . isValidFretting) . snd) $ Set.toList frettings
  mapM_ (footnoteFrettingIf (\f -> chromaticsFromFretting f /= chromatics) . snd) frettings
  assert $ all (isValidFretting . snd) frettings
  let chromaticsFound = nub $ map (chromaticsFromFretting . snd) $ Set.toList frettings
  when (any (/= chromatics) chromaticsFound) $
    footnote $
      "Chromatics found: " ++ show chromaticsFound ++ unlines (show . snd <$> Set.toList frettings)
  assert $ all (\f -> chromaticsFromFretting (snd f) == chromatics) frettings

-- Am7 chord definition
am7Chord :: Set Chromatic
am7Chord = Set.fromList [A, C, E, G]

fs9Chord :: Set Chromatic
fs9Chord = Set.fromList [Fs, Bb, Cs, E, Gs]

-- Property: findFrettings generates valid Am7 fretting
prop_findFrettings_am7 :: Property
prop_findFrettings_am7 = property $ do
  let standardTuning = knownTuning Standard
      frettings = findFrettings 250 standardTuning am7Chord
      expectedFretting =
        Fretting standardTuning $
          Set.fromList
            [ (1, Nothing),
              (2, Just (Ring, 2)),
              (3, Nothing),
              (4, Just (Index, 1)),
              (5, Nothing)
            ]
  assert $ any (\(_, f) -> frettingDiffersOnlyInFingering f expectedFretting) frettings

prop_findFrettings_fs9 :: Property
prop_findFrettings_fs9 = property $ do
  let standardTuning = knownTuning Standard
      frettings = findFrettings 250 standardTuning fs9Chord
      expectedFretting =
        Fretting standardTuning $
          Set.fromList
            [ (0, Just (Index, 2)),
              (1, Just (Middle, 4)),
              (2, Just (Index, 2)),
              (3, Just (Ring, 3)),
              (4, Just (Index, 2)),
              (5, Just (Pinky, 4))
            ]
  assert $ any (\(_, f) -> frettingDiffersOnlyInFingering f expectedFretting) frettings

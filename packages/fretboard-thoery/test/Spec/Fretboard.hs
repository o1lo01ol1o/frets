module Spec.Fretboard (tests) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Fretboard
  ( Fretboard,
    Fretting,
    KnownGuitarTunings (Standard),
    cMajorChord,
    cProgression,
    findFrettings,
    frettingDistance,
    knownTuning,
    optimizeFrettings,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    (@?=),
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Fretboard"
    [ testCase "frettingDistance is symmetric with zero self-distance" testFrettingDistanceSymmetry,
      testCase "optimizeFrettings picks lowest-difficulty voicing for single chord" testOptimizeSingleChord,
      testCase "optimizeFrettings minimises cumulative voice leading cost" testOptimizeProgression
    ]

-- Helpers --------------------------------------------------------------------

tuning :: Fretboard
tuning = knownTuning Standard

withTwoFrettings :: ((Fretting, Fretting) -> Assertion) -> Assertion
withTwoFrettings action = do
  let candidates = Set.toAscList $ findFrettings 8 tuning cMajorChord
  case candidates of
    ((_, firstFretting) : (_, secondFretting) : _) ->
      action (firstFretting, secondFretting)
    _ ->
      assertFailure "Expected at least two candidate frettings for C major chord"

-- Test cases -----------------------------------------------------------------

testFrettingDistanceSymmetry :: Assertion
testFrettingDistanceSymmetry =
  withTwoFrettings $ \(f1, f2) -> do
    frettingDistance f1 f1 @?= Just 0
    frettingDistance f1 f2 @?= frettingDistance f2 f1

testOptimizeSingleChord :: Assertion
testOptimizeSingleChord = do
  let candidates = Set.toAscList $ findFrettings 8 tuning cMajorChord
  case candidates of
    [] -> assertFailure "Expected at least one candidate fretting"
    ((_, expectedFretting) : _) -> do
      let result = optimizeFrettings 8 tuning [cMajorChord]
      result @?= [expectedFretting]

testOptimizeProgression :: Assertion
testOptimizeProgression = do
  let progression = take 2 cProgression
      candidateSets =
        fmap
          (Set.toAscList . findFrettings 6 tuning)
          progression
  case candidateSets of
    [c1, c2] | not (null c1) && not (null c2) -> do
      let allPaths = sequence [c1, c2]
          scoredPaths =
            [ ( (transitions, baseSum),
                fmap snd path
              )
              | path <- allPaths,
                let frettings = fmap snd path,
                let baseSum = sum (fmap (fromIntegral . fst) path),
                Just transitions <- [totalTransitions frettings]
            ]
      case scoredPaths of
        [] -> assertFailure "No valid fretting combinations found"
        _ ->
          let (_, expectedPath) =
                minimumBy (comparing fst) scoredPaths
              optimized = optimizeFrettings 6 tuning progression
           in optimized @?= expectedPath
    _ -> assertFailure "Expected two candidate sets for test progression"

totalTransitions :: [Fretting] -> Maybe Int
totalTransitions [] = Just 0
totalTransitions [_] = Just 0
totalTransitions (a : b : rest) = do
  dist <- frettingDistance a b
  remaining <- totalTransitions (b : rest)
  pure (fromIntegral dist + remaining)

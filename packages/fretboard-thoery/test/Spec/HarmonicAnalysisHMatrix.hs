{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.HarmonicAnalysisHMatrix (tests) where

import qualified Data.HarmonicAnalysis as Original
import qualified Data.HarmonicAnalysis.HMatrix as HMatrix
import Data.HarmonicAnalysis.Types
import Data.Mod (Mod)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Numeric.LinearAlgebra as HMatrix.LA
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "HarmonicAnalysis HMatrix tests"
    [ apiCompatibilityTests,
      configurationTests,
      optimizationTests,
      propertyTests,
      weightTableTests
    ]

-- | Test API compatibility - HMatrix version should produce identical results
apiCompatibilityTests :: TestTree
apiCompatibilityTests =
  testGroup
    "API compatibility tests"
    [ testCase "analyzeMajorMinorTSD equivalence" $ do
        let progression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2], Set.fromList [0, 4, 7]]
            originalResult = Original.analyzeMajorMinorTSD progression
            hmatrixResult = HMatrix.analyzeMajorMinorTSD progression
        hmatrixResult @?= originalResult,
      testCase "analyzeMajorMinorDiatonic equivalence" $ do
        let progression = [Set.fromList [0, 4, 7], Set.fromList [9, 0, 4], Set.fromList [5, 9, 0]]
            originalResult = Original.analyzeMajorMinorDiatonic progression
            hmatrixResult = HMatrix.analyzeMajorMinorDiatonic progression
        hmatrixResult @?= originalResult,
      testCase "analyzeModalTSD equivalence" $ do
        let progression = [Set.fromList [0, 4, 7], Set.fromList [2, 5, 9]]
            originalResult = Original.analyzeModalTSD progression
            hmatrixResult = HMatrix.analyzeModalTSD progression
        hmatrixResult @?= originalResult,
      testCase "analyzeModalDiatonic equivalence" $ do
        let progression = [Set.fromList [0, 3, 7], Set.fromList [7, 10, 2]]
            originalResult = Original.analyzeModalDiatonic progression
            hmatrixResult = HMatrix.analyzeModalDiatonic progression
        hmatrixResult @?= originalResult,
      testCase "harmonicAnalysis equivalence" $ do
        let config = Original.majorMinorTSDConfig
            progression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]
            originalResult = Original.harmonicAnalysis config progression
            hmatrixResult = HMatrix.harmonicAnalysis config progression
        hmatrixResult @?= originalResult,
      testCase "harmonicAnalysisWithConfig equivalence" $ do
        let config = Original.majorMinorTSDConfig
            progression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]
        -- Test all configuration numbers
        let originalResult0 = Original.harmonicAnalysisWithConfig 0 config progression
            hmatrixResult0 = HMatrix.harmonicAnalysisWithConfig 0 config progression
            originalResult1 = Original.harmonicAnalysisWithConfig 1 config progression
            hmatrixResult1 = HMatrix.harmonicAnalysisWithConfig 1 config progression
            originalResult2 = Original.harmonicAnalysisWithConfig 2 config progression
            hmatrixResult2 = HMatrix.harmonicAnalysisWithConfig 2 config progression
            originalResult3 = Original.harmonicAnalysisWithConfig 3 config progression
            hmatrixResult3 = HMatrix.harmonicAnalysisWithConfig 3 config progression
        hmatrixResult0 @?= originalResult0
        hmatrixResult1 @?= originalResult1
        hmatrixResult2 @?= originalResult2
        hmatrixResult3 @?= originalResult3,
      testCase "harmonicAnalysisWithRuntimeConfig equivalence" $ do
        let configs =
              [ Original.makeMajorMinorTSDConfig,
                Original.makeMajorMinorDiatonicConfig,
                Original.makeModalTSDConfig,
                Original.makeModalDiatonicConfig
              ]
            progression = [Set.fromList [0, 4, 7], Set.fromList [9, 0, 4]]
        let config1 = Original.makeMajorMinorTSDConfig
            config2 = Original.makeMajorMinorDiatonicConfig
            config3 = Original.makeModalTSDConfig
            config4 = Original.makeModalDiatonicConfig
            originalResult1 = Original.harmonicAnalysisWithRuntimeConfig config1 progression
            hmatrixResult1 = HMatrix.harmonicAnalysisWithRuntimeConfig config1 progression
            originalResult2 = Original.harmonicAnalysisWithRuntimeConfig config2 progression
            hmatrixResult2 = HMatrix.harmonicAnalysisWithRuntimeConfig config2 progression
            originalResult3 = Original.harmonicAnalysisWithRuntimeConfig config3 progression
            hmatrixResult3 = HMatrix.harmonicAnalysisWithRuntimeConfig config3 progression
            originalResult4 = Original.harmonicAnalysisWithRuntimeConfig config4 progression
            hmatrixResult4 = HMatrix.harmonicAnalysisWithRuntimeConfig config4 progression
        hmatrixResult1 @?= originalResult1
        hmatrixResult2 @?= originalResult2
        hmatrixResult3 @?= originalResult3
        hmatrixResult4 @?= originalResult4,
      testCase "multi-candidate analysis structure equivalence" $ do
        let progression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]
            originalResult = Original.analyzeMajorMinorTSDMultiCandidate progression
            hmatrixResult = HMatrix.analyzeMajorMinorTSDMultiCandidate progression

        -- Results should have same structure
        length (candidatePaths hmatrixResult) @?= length (candidatePaths originalResult)
        maxWeight hmatrixResult @?= maxWeight originalResult
        hasTies hmatrixResult @?= hasTies originalResult,
      testCase "empty progression handling" $ do
        let emptyProgression = []

        HMatrix.analyzeMajorMinorTSD emptyProgression @?= Original.analyzeMajorMinorTSD emptyProgression
        HMatrix.analyzeMajorMinorDiatonic emptyProgression @?= Original.analyzeMajorMinorDiatonic emptyProgression
        HMatrix.analyzeModalTSD emptyProgression @?= Original.analyzeModalTSD emptyProgression
        HMatrix.analyzeModalDiatonic emptyProgression @?= Original.analyzeModalDiatonic emptyProgression
    ]

-- | Test the type-safe configuration system
configurationTests :: TestTree
configurationTests =
  testGroup
    "Configuration system tests"
    [ testCase "makeHarmonicConfig type safety" $ do
        let progression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]

        -- Test all configuration types produce valid results
        let config0 = HMatrix.makeHarmonicConfig HMatrix.MajorMinorTSD
            config1 = HMatrix.makeHarmonicConfig HMatrix.MajorMinorDiatonic
            config2 = HMatrix.makeHarmonicConfig HMatrix.ModalTSD
            config3 = HMatrix.makeHarmonicConfig HMatrix.ModalDiatonic

        -- Should not crash and should produce valid paths
        let result0 = HMatrix.harmonicAnalysis config0 progression
            result1 = HMatrix.harmonicAnalysis config1 progression
            result2 = HMatrix.harmonicAnalysis config2 progression
            result3 = HMatrix.harmonicAnalysis config3 progression

        -- All results should be valid (non-empty paths for non-empty progression)
        length (getPath result0) @?= length progression
        length (getPath result1) @?= length progression
        length (getPath result2) @?= length progression
        length (getPath result3) @?= length progression,
      testCase "configuration equivalence with original" $ do
        let progression = [Set.fromList [0, 4, 7], Set.fromList [9, 0, 4]]

        -- HMatrix type-safe configs should match original configs
        HMatrix.harmonicAnalysis (HMatrix.makeHarmonicConfig HMatrix.MajorMinorTSD) progression
          @?= Original.harmonicAnalysis Original.majorMinorTSDConfig progression

        HMatrix.harmonicAnalysis (HMatrix.makeHarmonicConfig HMatrix.MajorMinorDiatonic) progression
          @?= Original.harmonicAnalysis Original.majorMinorDiatonicConfig progression

        HMatrix.harmonicAnalysis (HMatrix.makeHarmonicConfig HMatrix.ModalTSD) progression
          @?= Original.harmonicAnalysis Original.modalTSDConfig progression

        HMatrix.harmonicAnalysis (HMatrix.makeHarmonicConfig HMatrix.ModalDiatonic) progression
          @?= Original.harmonicAnalysis Original.modalDiatonicConfig progression
    ]

-- | Test HMatrix-specific optimizations
optimizationTests :: TestTree
optimizationTests =
  testGroup
    "HMatrix optimization tests"
    [ testCase "optimizedTensionComputation produces finite results" $ do
        let testPaths =
              [ HarmonicPath [], -- empty path
                HarmonicPath [RMPoint 0 (Row 0) (Col 0) 1.0], -- single point
                HarmonicPath [RMPoint 0 (Row 0) (Col 0) 1.0, RMPoint 1 (Row 1) (Col 2) 0.8], -- two points
                HarmonicPath [RMPoint 0 (Row 0) (Col 0) 1.0, RMPoint 1 (Row 1) (Col 5) 0.8, RMPoint 2 (Row 2) (Col 7) 0.6] -- three points
              ]
            tensionTable = Original.defaultTensionTable

        let result1 = HMatrix.optimizedTensionComputation tensionTable (testPaths !! 0)
            result2 = HMatrix.optimizedTensionComputation tensionTable (testPaths !! 1)
            result3 = HMatrix.optimizedTensionComputation tensionTable (testPaths !! 2)
            result4 = HMatrix.optimizedTensionComputation tensionTable (testPaths !! 3)
        assertBool "Tension should be finite for empty path" (isFinite result1)
        assertBool "Tension should be non-negative for empty path" (result1 >= 0)
        assertBool "Tension should be finite for single point path" (isFinite result2)
        assertBool "Tension should be non-negative for single point path" (result2 >= 0)
        assertBool "Tension should be finite for two point path" (isFinite result3)
        assertBool "Tension should be non-negative for two point path" (result3 >= 0)
        assertBool "Tension should be finite for three point path" (isFinite result4)
        assertBool "Tension should be non-negative for three point path" (result4 >= 0),
      testCase "vectorizedWeightComputation produces correct dimensions" $ do
        let weights = HMatrix.majorMinorTSDWeights
            testProgressions =
              [ [],
                [Set.fromList [0, 4, 7]],
                [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]],
                [Set.fromList [0, 4, 7], Set.fromList [9, 0, 4], Set.fromList [5, 9, 0], Set.fromList [7, 11, 2]]
              ]

        let result1 = HMatrix.vectorizedWeightComputation weights (testProgressions !! 0)
            expectedLength1 = length (testProgressions !! 0)
            actualLength1 = V.length (V.fromList $ HMatrix.LA.toList result1)
            result2 = HMatrix.vectorizedWeightComputation weights (testProgressions !! 1)
            expectedLength2 = length (testProgressions !! 1)
            actualLength2 = V.length (V.fromList $ HMatrix.LA.toList result2)
            result3 = HMatrix.vectorizedWeightComputation weights (testProgressions !! 2)
            expectedLength3 = length (testProgressions !! 2)
            actualLength3 = V.length (V.fromList $ HMatrix.LA.toList result3)
            result4 = HMatrix.vectorizedWeightComputation weights (testProgressions !! 3)
            expectedLength4 = length (testProgressions !! 3)
            actualLength4 = V.length (V.fromList $ HMatrix.LA.toList result4)
        actualLength1 @?= expectedLength1
        actualLength2 @?= expectedLength2
        actualLength3 @?= expectedLength3
        actualLength4 @?= expectedLength4,
      testCase "batchHarmonicAnalysis produces same results as individual analysis" $ do
        let config = HMatrix.makeMajorMinorTSDConfig
            progressions =
              [ [Set.fromList [0, 4, 7]],
                [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]],
                [Set.fromList [9, 0, 4], Set.fromList [5, 9, 0]]
              ]

        let batchResults = HMatrix.batchHarmonicAnalysis config progressions
            individualResults = map (HMatrix.harmonicAnalysisWithRuntimeConfig config) progressions

        batchResults @?= individualResults,
      testCase "matrixCorrelationAnalysis produces valid correlation matrix" $ do
        let progressions =
              [ [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]],
                [Set.fromList [0, 4, 7], Set.fromList [5, 9, 0]],
                [Set.fromList [9, 0, 4], Set.fromList [7, 11, 2]]
              ]

        let corrMatrix = HMatrix.matrixCorrelationAnalysis progressions
            (rows, cols) = HMatrix.LA.size corrMatrix

        -- Should be square matrix with same dimensions as number of progressions
        rows @?= length progressions
        cols @?= rows

        -- All elements should be finite
        let allElements = HMatrix.LA.toList $ HMatrix.LA.flatten corrMatrix
            allFinite = all isFinite allElements
        assertBool "All correlation elements should be finite" allFinite
    ]

-- | Property-based tests with random inputs
propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ testProperty "harmonic analysis functions are equivalent" $ H.property $ do
        progression <- H.forAll genProgression

        -- All analysis functions should produce identical results
        HMatrix.analyzeMajorMinorTSD progression H.=== Original.analyzeMajorMinorTSD progression
        HMatrix.analyzeMajorMinorDiatonic progression H.=== Original.analyzeMajorMinorDiatonic progression
        HMatrix.analyzeModalTSD progression H.=== Original.analyzeModalTSD progression
        HMatrix.analyzeModalDiatonic progression H.=== Original.analyzeModalDiatonic progression,
      testProperty "harmonicAnalysisWithConfig equivalence" $ H.property $ do
        progression <- H.forAll genProgression
        configNum <- H.forAll $ Gen.int (Range.linear 0 3)

        let config = Original.majorMinorTSDConfig -- Use fixed config
            originalResult = Original.harmonicAnalysisWithConfig configNum config progression
            hmatrixResult = HMatrix.harmonicAnalysisWithConfig configNum config progression

        originalResult H.=== hmatrixResult,
      testProperty "harmonicAnalysisWithRuntimeConfig equivalence" $ H.property $ do
        progression <- H.forAll genProgression
        config <- H.forAll genRuntimeConfig

        let originalResult = Original.harmonicAnalysisWithRuntimeConfig config progression
            hmatrixResult = HMatrix.harmonicAnalysisWithRuntimeConfig config progression

        originalResult H.=== hmatrixResult,
      testProperty "multi-candidate analysis structural equivalence" $ H.property $ do
        progression <- H.forAll genProgression

        let originalResult = Original.analyzeMajorMinorTSDMultiCandidate progression
            hmatrixResult = HMatrix.analyzeMajorMinorTSDMultiCandidate progression

        -- Structural properties should match
        length (candidatePaths hmatrixResult) H.=== length (candidatePaths originalResult)
        maxWeight hmatrixResult H.=== maxWeight originalResult
        hasTies hmatrixResult H.=== hasTies originalResult,
      testProperty "optimizedTensionComputation is always finite and non-negative" $ H.property $ do
        path <- H.forAll genHarmonicPath
        let tensionTable = Original.defaultTensionTable
            result = HMatrix.optimizedTensionComputation tensionTable path

        H.assert (isFinite result)
        H.assert (result >= 0),
      testProperty "vectorizedWeightComputation produces correct dimensions" $ H.property $ do
        progression <- H.forAll genProgression
        weights <- H.forAll genWeights

        let result = HMatrix.vectorizedWeightComputation weights progression
            expectedLength = length progression
            actualLength = V.length (V.fromList $ HMatrix.LA.toList result)

        actualLength H.=== expectedLength
    ]

-- | Test weight table consistency
weightTableTests :: TestTree
weightTableTests =
  testGroup
    "Weight table tests"
    [ testCase "majorMinorTSDWeights consistency" $ do
        let originalWeights = V.toList Original.majorMinorTSDWeights
            hmatrixWeights = HMatrix.LA.toList HMatrix.majorMinorTSDWeights
        originalWeights @?= hmatrixWeights,
      testCase "majorMinorDiatonicWeights consistency" $ do
        let originalWeights = V.toList Original.majorMinorDiatonicWeights
            hmatrixWeights = HMatrix.LA.toList HMatrix.majorMinorDiatonicWeights
        originalWeights @?= hmatrixWeights,
      testCase "modalTSDWeights consistency" $ do
        let originalWeights = V.toList Original.modalTSDWeights
            hmatrixWeights = HMatrix.LA.toList HMatrix.modalTSDWeights
        originalWeights @?= hmatrixWeights,
      testCase "modalDiatonicWeights consistency" $ do
        let originalWeights = V.toList Original.modalDiatonicWeights
            hmatrixWeights = HMatrix.LA.toList HMatrix.modalDiatonicWeights
        originalWeights @?= hmatrixWeights,
      testCase "all weight tables have correct dimensions" $ do
        -- MajorMinorTSD: 2 modes × 3 functions × 12 tonalities = 72
        V.length (V.fromList $ HMatrix.LA.toList HMatrix.majorMinorTSDWeights) @?= 72

        -- MajorMinorDiatonic: 2 modes × 7 functions × 12 tonalities = 168
        V.length (V.fromList $ HMatrix.LA.toList HMatrix.majorMinorDiatonicWeights) @?= 168

        -- ModalTSD: 7 modes × 3 functions × 12 tonalities = 252
        V.length (V.fromList $ HMatrix.LA.toList HMatrix.modalTSDWeights) @?= 252

        -- ModalDiatonic: 7 modes × 7 functions × 12 tonalities = 588
        V.length (V.fromList $ HMatrix.LA.toList HMatrix.modalDiatonicWeights) @?= 588
    ]

-- Generators for property tests

genProgression :: H.Gen [Set.Set (Mod 12)]
genProgression = do
  len <- Gen.int (Range.linear 0 8)
  Gen.list (Range.singleton len) genChord

genChord :: H.Gen (Set.Set (Mod 12))
genChord = do
  pitches <- Gen.list (Range.linear 0 12) genPitch
  pure $ Set.fromList pitches

genPitch :: H.Gen (Mod 12)
genPitch = fromIntegral <$> Gen.int (Range.linear 0 11)

-- Removed genAnalysisConfig since HarmonicAnalysisConfig doesn't have Show instance

genRuntimeConfig :: H.Gen RuntimeConfig
genRuntimeConfig =
  Gen.choice
    [ pure Original.makeMajorMinorTSDConfig,
      pure Original.makeMajorMinorDiatonicConfig,
      pure Original.makeModalTSDConfig,
      pure Original.makeModalDiatonicConfig
    ]

genHarmonicPath :: H.Gen HarmonicPath
genHarmonicPath = do
  len <- Gen.int (Range.linear 0 10)
  points <- Gen.list (Range.singleton len) genRMPoint
  pure $ HarmonicPath points

genRMPoint :: H.Gen RMPoint
genRMPoint = do
  idx <- Gen.int (Range.linear 0 20)
  r <- Gen.int (Range.linear 0 20)
  c <- Gen.int (Range.linear 0 11)
  val <- Gen.double (Range.linearFrac 0.0 1.0)
  pure $ RMPoint idx (Row r) (Col (fromIntegral c)) val

genWeights :: H.Gen (HMatrix.LA.Vector Double)
genWeights = do
  len <- Gen.int (Range.linear 12 588) -- Range covers all possible weight table sizes
  weights <- Gen.list (Range.singleton len) (Gen.double (Range.linearFrac 0.0 1.0))
  pure $ HMatrix.LA.fromList weights

-- Helper functions

isFinite :: Double -> Bool
isFinite x = not (isInfinite x || isNaN x)

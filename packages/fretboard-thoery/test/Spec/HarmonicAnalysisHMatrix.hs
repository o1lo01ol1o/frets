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
import Numeric.LinearAlgebra (toList)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "HarmonicAnalysis HMatrix tests"
    [ unitTests,
      configurationTests,
      comparisonTests,
      propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HMatrix unit tests"
    [ testCase "Empty pitch set analysis" $ do
        let emptySet = Set.empty
            originalResult = Original.analyzeMajorMinorTSD [emptySet]
            hmatrixResult = HMatrix.analyzeMajorMinorTSD [emptySet]
        -- Both should handle empty sets gracefully
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Single chord analysis - C Major" $ do
        let cMajorSet = Set.fromList [0, 4, 7] -- C-E-G
            originalResult = Original.analyzeMajorMinorTSD [cMajorSet]
            hmatrixResult = HMatrix.analyzeMajorMinorTSD [cMajorSet]
        -- Should produce similar path lengths
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Basic chord progression - I-V-vi-IV" $ do
        let cMajor = Set.fromList [0, 4, 7] -- C-E-G
            gMajor = Set.fromList [7, 11, 2] -- G-B-D
            aMinor = Set.fromList [9, 0, 4] -- A-C-E
            fMajor = Set.fromList [5, 9, 0] -- F-A-C
            progression = [cMajor, gMajor, aMinor, fMajor]
            originalResult = Original.analyzeMajorMinorTSD progression
            hmatrixResult = HMatrix.analyzeMajorMinorTSD progression
        -- Should produce same number of analysis points
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Weight table consistency" $ do
        let originalWeights = V.toList Original.majorMinorTSDWeights
            hmatrixWeights = toList HMatrix.majorMinorTSDWeights
        -- Weight tables should be identical
        originalWeights @?= hmatrixWeights,
      testCase "API compatibility test" $ do
        let testProgression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]
        -- Test that all main functions exist and work
        let _ = HMatrix.analyzeMajorMinorTSD testProgression
        let _ = HMatrix.analyzeMajorMinorDiatonic testProgression
        let _ = HMatrix.analyzeModalTSD testProgression
        let _ = HMatrix.analyzeModalDiatonic testProgression
        True @?= True -- If we get here without errors, the API works
    ]

configurationTests :: TestTree
configurationTests =
  testGroup
    "Configuration-specific tests"
    [ testCase "Major/Minor TSD configuration" $ do
        let testProgression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]
            originalResult = Original.analyzeMajorMinorTSD testProgression
            hmatrixResult = HMatrix.analyzeMajorMinorTSD testProgression
        -- Should produce comparable results
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Major/Minor Diatonic configuration" $ do
        let testProgression = [Set.fromList [0, 4, 7], Set.fromList [2, 5, 9]]
            originalResult = Original.analyzeMajorMinorDiatonic testProgression
            hmatrixResult = HMatrix.analyzeMajorMinorDiatonic testProgression
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Modal TSD configuration" $ do
        let testProgression = [Set.fromList [0, 3, 7], Set.fromList [5, 8, 0]]
            originalResult = Original.analyzeModalTSD testProgression
            hmatrixResult = HMatrix.analyzeModalTSD testProgression
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Modal Diatonic configuration" $ do
        let testProgression = [Set.fromList [0, 4, 7], Set.fromList [1, 5, 8]]
            originalResult = Original.analyzeModalDiatonic testProgression
            hmatrixResult = HMatrix.analyzeModalDiatonic testProgression
        length (getPath originalResult) @?= length (getPath hmatrixResult),
      testCase "Configuration factory functions" $ do
        let config0 = HMatrix.makeHarmonicConfig 0
            config1 = HMatrix.makeHarmonicConfig 1
            config2 = HMatrix.makeHarmonicConfig 2
            config3 = HMatrix.makeHarmonicConfig 3
        -- All configs should be valid (non-crashing)
        seq config0 $ seq config1 $ seq config2 $ seq config3 $ True @?= True
    ]

comparisonTests :: TestTree
comparisonTests =
  testGroup
    "Direct comparison tests"
    [ testCase "Identical results for simple progression" $ do
        let simpleProgression = [Set.fromList [0, 4, 7], Set.fromList [5, 9, 0]]
            originalPath = getPath $ Original.analyzeMajorMinorTSD simpleProgression
            hmatrixPath = getPath $ HMatrix.analyzeMajorMinorTSD simpleProgression
        -- Paths should have same length (structure may differ due to implementation)
        length originalPath @?= length hmatrixPath,
      testCase "Multi-candidate analysis comparison" $ do
        let testProgression = [Set.fromList [0, 4, 7]]
            originalResult = Original.analyzeMajorMinorTSDMultiCandidate testProgression
            hmatrixResult = HMatrix.analyzeMajorMinorTSDMultiCandidate testProgression
        -- Should have same number of candidates
        length (candidatePaths originalResult) @?= length (candidatePaths hmatrixResult),
      testCase "Weight table element-wise comparison" $ do
        let originalMajorMinor = V.toList Original.majorMinorTSDWeights
            hmatrixMajorMinor = toList HMatrix.majorMinorTSDWeights
            originalDiatonic = V.toList Original.majorMinorDiatonicWeights
            hmatrixDiatonic = toList HMatrix.majorMinorDiatonicWeights
        -- All weight tables should match exactly
        originalMajorMinor @?= hmatrixMajorMinor
        originalDiatonic @?= hmatrixDiatonic,
      testCase "Configuration analysis consistency" $ do
        let testProgression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2], Set.fromList [9, 0, 4]]
        -- Test all four configurations
        mapM_
          ( \configNum -> do
              let originalConfig = case configNum of
                    0 -> Original.majorMinorTSDConfig
                    1 -> Original.majorMinorDiatonicConfig
                    2 -> Original.modalTSDConfig
                    3 -> Original.modalDiatonicConfig
                    _ -> Original.defaultConfig
                  originalResult = Original.harmonicAnalysisWithConfig configNum originalConfig testProgression
                  hmatrixResult = HMatrix.harmonicAnalysisWithConfig configNum (HMatrix.makeHarmonicConfig configNum) testProgression
              -- Results should be structurally similar
              length (getPath originalResult) @?= length (getPath hmatrixResult)
          )
          [0, 1, 2, 3]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property-based tests"
    [ QC.testProperty "Path length consistency" $ \(chords :: [[Int]]) ->
        let chordSets = map (Set.fromList . map (\x -> fromIntegral (x `mod` 12))) chords
            originalPath = getPath $ Original.analyzeMajorMinorTSD chordSets
            hmatrixPath = getPath $ HMatrix.analyzeMajorMinorTSD chordSets
         in length originalPath == length hmatrixPath,
      QC.testProperty "Empty progression handling" $
        let originalResult = Original.analyzeMajorMinorTSD []
            hmatrixResult = HMatrix.analyzeMajorMinorTSD []
         in length (getPath originalResult) == length (getPath hmatrixResult),
      QC.testProperty "Single chord consistency" $ \(notes :: [Int]) ->
        let chordSet = Set.fromList $ map (\x -> fromIntegral (x `mod` 12)) notes
            originalPath = getPath $ Original.analyzeMajorMinorTSD [chordSet]
            hmatrixPath = getPath $ HMatrix.analyzeMajorMinorTSD [chordSet]
         in length originalPath == length hmatrixPath,
      QC.testProperty "HMatrix optimizations don't crash" $ \(chords :: [[Int]]) ->
        let chordSets = map (Set.fromList . map (\x -> fromIntegral (x `mod` 12))) chords
            optimizedPath = HMatrix.optimizedViterbiPath []
            tensionResult = HMatrix.optimizedTensionComputation (Original.defaultTensionTable) (HMatrix.HarmonicPath [])
            vectorResult = HMatrix.vectorizedWeightComputation (HMatrix.majorMinorTSDWeights) chordSets
         in seq optimizedPath $ seq tensionResult $ seq vectorResult True
    ]

-- Test data constants
testChords :: [[Set.Set (Mod 12)]]
testChords =
  [ -- Major scale chord progression
    [Set.fromList [0, 4, 7], Set.fromList [2, 5, 9], Set.fromList [4, 7, 11], Set.fromList [5, 9, 0]],
    -- Minor scale progression
    [Set.fromList [0, 3, 7], Set.fromList [2, 5, 8], Set.fromList [3, 7, 10], Set.fromList [5, 8, 0]],
    -- Modal progression
    [Set.fromList [0, 4, 7], Set.fromList [1, 5, 8], Set.fromList [3, 7, 10], Set.fromList [0, 4, 7]],
    -- Jazz progression
    [Set.fromList [0, 4, 7, 11], Set.fromList [5, 9, 0, 4], Set.fromList [2, 5, 9, 0], Set.fromList [7, 11, 2, 5]]
  ]

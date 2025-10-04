{-# LANGUAGE DataKinds #-}

module Spec.HarmonicAnalysisHMatrix (tests) where

import qualified Data.HarmonicAnalysis as API
import Data.HarmonicAnalysis (AnalysisPreset (..))
import qualified Data.HarmonicAnalysis.HMatrix as HMatrix
import Data.HarmonicAnalysis.Types
import Control.Monad (forM_)
import Data.Mod (Mod)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Numeric.LinearAlgebra as LA
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "HarmonicAnalysis HMatrix tests"
    [ apiCompatibilityTests
    , windowedCompatibilityTests
    , optimizationTests
    , propertyTests
    , helperTests
    ]

presets :: [AnalysisPreset]
presets = [MajorMinorTSD, MajorMinorDiatonic, ModalTSD, ModalDiatonic]

sampleProgression :: [Set.Set (Mod 12)]
sampleProgression = [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2], Set.fromList [0, 5, 9]]

apiCompatibilityTests :: TestTree
apiCompatibilityTests =
  testGroup
    "API compatibility tests"
    [ testCase "analyze matches baseline backend" $
        forM_ presets $ \preset ->
          HMatrix.analyze preset sampleProgression @?= API.analyze preset sampleProgression
    , testCase "analyzeAnnotated matches baseline backend" $
        forM_ presets $ \preset ->
          HMatrix.analyzeAnnotated preset sampleProgression @?= API.analyzeAnnotated preset sampleProgression
    ]

windowedCompatibilityTests :: TestTree
windowedCompatibilityTests =
  testGroup
    "Windowed compatibility"
    [ testCase "windowed analysis matches baseline backend" $ do
        let windowCfg = HMatrix.defaultWindowedConfig
        forM_ presets $ \preset ->
          HMatrix.analyzeWindowed preset windowCfg sampleProgression
            @?= API.analyzeWindowed preset windowCfg sampleProgression
    , testCase "annotatePath matches" $
        forM_ presets $ \preset ->
          let path = API.analyze preset sampleProgression
           in HMatrix.annotatePath preset sampleProgression path
                @?= API.annotatePath preset sampleProgression path
    ]

optimizationTests :: TestTree
optimizationTests =
  testGroup
    "HMatrix optimisation helpers"
    [ testCase "optimised tension is finite" $ do
        let tensionTable = HMatrix.tensionsFor MajorMinorTSD
            paths =
              [ HarmonicPath []
              , HarmonicPath [RMPoint 0 (Row 0) (Col 0) 1.0]
              , HarmonicPath [RMPoint 0 (Row 0) (Col 0) 1.0, RMPoint 1 (Row 1) (Col 2) 0.8]
              ]
        forM_ paths $ \p -> do
          let value = HMatrix.optimizedTensionComputation tensionTable p
          assertBool "tension should be finite" (isFinite value)
          assertBool "tension should be non-negative" (value >= 0)
    , testCase "vectorised weight computation respects progression length" $ do
        let weights = HMatrix.weightsFor MajorMinorTSD
            options =
              [ []
              , [Set.fromList [0, 4, 7]]
              , [Set.fromList [0, 4, 7], Set.fromList [7, 11, 2]]
              ]
        forM_ options $ \prog -> do
          let result = HMatrix.vectorizedWeightComputation weights prog
              actual = length (LA.toList result)
          actual @?= length prog
    , testCase "batch analysis matches individual calls" $ do
        let progressions =
              [ [Set.fromList [0, 4, 7]]
              , [Set.fromList [7, 11, 2], Set.fromList [0, 5, 9]]
              ]
            preset = MajorMinorTSD
        HMatrix.batchAnalyze preset progressions
          @?= fmap (HMatrix.analyze preset) progressions
    , testCase "matrix correlation returns identity" $ do
        let progressions =
              [ [Set.fromList [0, 4, 7]]
              , [Set.fromList [7, 11, 2]]
              ]
            result = HMatrix.matrixCorrelationAnalysis progressions
        LA.rows result @?= length progressions
        LA.cols result @?= length progressions
        LA.toList (LA.flatten result) @?= [1, 0, 0, 1]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ testProperty "HMatrix analyse matches baseline" $ H.property $ do
        preset <- H.forAll genPreset
        progression <- H.forAll genProgression
        HMatrix.analyze preset progression H.=== API.analyze preset progression
    , testProperty "vectorised weights length matches progression" $ H.property $ do
        preset <- H.forAll genPreset
        progression <- H.forAll genProgression
        let weights = HMatrix.weightsFor preset
            result = HMatrix.vectorizedWeightComputation weights progression
        LA.size result H.=== length progression
    ]

helperTests :: TestTree
helperTests =
  testGroup
    "Helper correctness"
    [ testCase "weight vector dimensions match preset expectations" $
        forM_ presets $ \preset -> do
          let cfg = HMatrix.windowedConfig preset
              expected = configNumModes cfg * configNumFunctions cfg * configNumTonalities cfg
          V.length (configWeightTable cfg) @?= expected
    , testCase "tension table dimensions match preset expectations" $
        forM_ presets $ \preset -> do
          let table = HMatrix.tensionsFor preset
              modes = length (modalTension table)
              functions = length (functionalTension table)
          modes @?= configNumModes (HMatrix.windowedConfig preset)
          functions @?= configNumFunctions (HMatrix.windowedConfig preset)
    ]

-- Generators -----------------------------------------------------------------

genPreset :: H.Gen AnalysisPreset
genPreset = Gen.element presets

genProgression :: H.Gen [Set.Set (Mod 12)]
genProgression = do
  len <- Gen.int (Range.linear 0 6)
  Gen.list (Range.singleton len) genChord

genChord :: H.Gen (Set.Set (Mod 12))
genChord = Set.fromList <$> Gen.list (Range.linear 0 6) genPitch

genPitch :: H.Gen (Mod 12)
genPitch = fromIntegral <$> Gen.int (Range.linear 0 11)

-- Utilities ------------------------------------------------------------------

isFinite :: Double -> Bool
isFinite x = not (isInfinite x || isNaN x)

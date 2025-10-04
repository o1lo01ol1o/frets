{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.HarmonicAnalysis (tests) where

import Control.Monad (filterM, zipWithM_)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.HarmonicAnalysis
import Data.HarmonicAnalysis.Types
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Minimal representation of a chord extracted from the Mozart example.
newtype ExtractedChord = ExtractedChord
  { pitches :: [Int]
  }
  deriving (Show, Generic)

instance FromJSON ExtractedChord where
  parseJSON =
    withObject "ExtractedChord" $ \obj ->
      ExtractedChord <$> obj .: "pitches"

loadExtractedChords :: FilePath -> IO [ExtractedChord]
loadExtractedChords path = do
  bytes <- BL.readFile path
  case eitherDecode bytes of
    Left err -> fail ("Unable to decode extracted chord progression: " <> err)
    Right chords -> pure chords

pitchSet :: [Int] -> Set.Set (Mod 12)
pitchSet = Set.fromList . fmap fromIntegral

mozartProgressionPath :: IO AnnotatedHarmonicPath
mozartProgressionPath = do
  chordsPath <- discoverChordFile
  chords <- loadExtractedChords chordsPath
  let pick n = pitchSet . pitches $ chords !! n
      progression = [pick 1, pick 1, pick 2, pick 3]
      windowConfig = Windowed.defaultWindowedConfig
      windowedPath = analyzeWindowed MajorMinorTSD windowConfig progression
   in pure (annotatePath MajorMinorTSD progression windowedPath)

discoverChordFile :: IO FilePath
discoverChordFile = do
  let candidates =
        [ "docs" </> "extracted_chords.json",
          ".." </> "docs" </> "extracted_chords.json",
          ".." </> ".." </> "docs" </> "extracted_chords.json"
        ]
  existing <- filterM doesFileExist candidates
  case existing of
    (path : _) -> pure path
    [] ->
      fail
        "Unable to locate docs/extracted_chords.json from the current working directory."

mozartExpectedSequence ::
  [(Mode, Function, Int, Maybe String)]
mozartExpectedSequence =
  [ (Ionian, Tonic, 9, Just "I"),
    (Ionian, Tonic, 9, Just "I"),
    (Ionian, Dominant, 9, Just "V"),
    (Ionian, Tonic, 9, Just "I")
  ]

tests :: TestTree
tests =
  testGroup
    "HarmonicAnalysis"
    [ mozartWindowedAnalysisTest
    ]

mozartWindowedAnalysisTest :: TestTree
mozartWindowedAnalysisTest =
  testCase "Windowed analysis reproduces Mozart K.331 path" $ do
    AnnotatedHarmonicPath steps <- mozartProgressionPath

    let expectedLength = length mozartExpectedSequence
    assertEqual "windowed path length" expectedLength (length steps)

    zipWithM_ compareStep steps mozartExpectedSequence

    -- Ensure matrix indices advance monotonically and cover every chord
    let indices = fmap (matrixIndex . stepPoint) steps
    assertEqual "matrix indices" [0 .. expectedLength - 1] indices

    -- Ensure we preserved the original pitch-class sets in each annotation
    let pitchSets = fmap stepPitchClasses steps
    assertBool "pitch-class sets should not be empty" (all (not . Set.null) pitchSets)

compareStep :: HarmonicStep -> (Mode, Function, Int, Maybe String) -> IO ()
compareStep step (expectedMode, expectedFunction, expectedTonality, expectedRoman) = do
  let harmony = stepHarmony step
      point = stepPoint step
      actualTonality = fromIntegral (unMod (unCol (col point)))

  assertEqual "mode" expectedMode (annotationMode harmony)
  assertEqual "function" expectedFunction (annotationFunction harmony)
  assertEqual
    "degree"
    (functionToDegree expectedFunction)
    (annotationDegree harmony)
  assertEqual "roman numeral" expectedRoman (annotationRomanNumeral harmony)
  assertEqual "tonality column" expectedTonality actualTonality

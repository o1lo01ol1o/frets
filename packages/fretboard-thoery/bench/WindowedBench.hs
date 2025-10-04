{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.HarmonicAnalysis
import Data.HarmonicAnalysis.Types (AnnotatedHarmonicPath (..))
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

newtype ExtractedChord = ExtractedChord
  { pitches :: [Int]
  }
  deriving (Show, Generic)

instance FromJSON ExtractedChord where
  parseJSON =
    withObject "ExtractedChord" $ \obj ->
      ExtractedChord <$> obj .: "pitches"

main :: IO ()
main = do
  progression <- loadMozartWindow
  defaultMain
    [ bgroup
        "windowed-analysis"
        [ bench "analyzeWindowed" $
            nf (analyzeWindowed MajorMinorTSD Windowed.defaultWindowedConfig) progression,
          bench "annotatedWindowed" $
            nf annotatedWindowed progression
        ]
    ]

annotatedWindowed :: [Set.Set (Mod 12)] -> AnnotatedHarmonicPath
annotatedWindowed chords =
  let windowConfig = Windowed.defaultWindowedConfig
      path = analyzeWindowed MajorMinorTSD windowConfig chords
   in annotatePath MajorMinorTSD chords path

loadMozartWindow :: IO [Set.Set (Mod 12)]
loadMozartWindow = do
  path <- locateExtractionFile
  bytes <- BL.readFile path
  case eitherDecode bytes of
    Left err -> fail ("Unable to decode " <> path <> ": " <> err)
    Right chords ->
      let pick i = Set.fromList . fmap fromIntegral . pitches $ chords !! i
       in pure [pick 1, pick 1, pick 2, pick 3]

locateExtractionFile :: IO FilePath
locateExtractionFile = do
  let candidates =
        [ "docs" </> "extracted_chords.json",
          ".." </> "docs" </> "extracted_chords.json",
          ".." </> ".." </> "docs" </> "extracted_chords.json"
        ]
  existing <- filterM doesFileExist candidates
  case existing of
    (p : _) -> pure p
    [] -> fail "Unable to locate docs/extracted_chords.json for benchmark"

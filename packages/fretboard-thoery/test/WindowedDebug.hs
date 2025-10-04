{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.HarmonicAnalysis
import Data.HarmonicAnalysis.Types
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod)
import qualified Data.Set as Set

newtype ExtractedChord = ExtractedChord { pitches :: [Int] }

instance Data.Aeson.FromJSON ExtractedChord where
  parseJSON = Data.Aeson.withObject "ExtractedChord" $ \obj -> ExtractedChord <$> obj Data.Aeson..: "pitches"

main :: IO ()
main = do
  bytes <- BL.readFile "docs/extracted_chords.json"
  case eitherDecode bytes of
    Left err -> fail err
    Right chords -> do
      let sets = map (Set.fromList . map fromIntegral . pitches) chords :: [Set.Set (Mod 12)]
          prog = take 5 $ drop 1 sets
          counts = map Set.size prog
          config = Windowed.defaultWindowedConfig
      print counts
      print $ length prog
      let path = analyzeWindowed MajorMinorTSD config prog
      print path

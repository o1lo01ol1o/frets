{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.HarmonicAnalysis
import Data.HarmonicAnalysis.Types
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.Mod (Mod)
import qualified Data.Set as Set
import Numeric (showFFloat)

newtype ExtractedChord = ExtractedChord { pitches :: [Int] }

instance Aeson.FromJSON ExtractedChord where
  parseJSON = Aeson.withObject "ExtractedChord" $ \obj -> ExtractedChord <$> obj Aeson..: "pitches"

formatDouble :: Double -> String
formatDouble x = showFFloat (Just 4) x ""

main :: IO () = do
  bytes <- BL.readFile "docs/extracted_chords.json"
  case Aeson.eitherDecode bytes of
    Left err -> fail err
    Right chords -> do
      let pcSets = map (Set.fromList . map fromIntegral . pitches) chords :: [Set.Set (Mod 12)]
          progression = take 4 $ drop 1 pcSets
          config = Windowed.defaultWindowedConfig
          path = analyzeWindowed MajorMinorTSD config progression
          AnnotatedHarmonicPath annotated = annotatePath MajorMinorTSD progression path

      putStrLn "Progression (cardinalities):"
      print (map Set.size progression)

      putStrLn "Windowed path steps:"
      mapM_ printStep annotated

printStep :: HarmonicStep -> IO ()
printStep step = do
  let harmony = stepHarmony step
      point = stepPoint step
      mode' = annotationMode harmony
      function' = annotationFunction harmony
      tonality = fromIntegral (unMod (unCol (col point)))
  putStrLn $ concat
    [ "matrixIndex=", show (matrixIndex point)
    , ", mode=", show mode'
    , ", function=", show function'
    , ", tonality=", show tonality
    ]

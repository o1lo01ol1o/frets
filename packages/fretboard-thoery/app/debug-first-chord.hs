{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (sortOn, foldl')
import qualified Data.Set as Set
import Numeric (showFFloat)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Data.HarmonicAnalysis (AnalysisPreset (..), windowedConfig)
import Data.HarmonicAnalysis.ChainOfThirds (findChainsOfThirds)
import Data.HarmonicAnalysis.RiemannMatrix
import Data.HarmonicAnalysis.Types
import Data.Mod (Mod, unMod)

formatDouble :: Double -> String
formatDouble x = showFFloat (Just 4) x ""

main :: IO ()
main = do
  let config = windowedConfig MajorMinorTSD
      chord :: Set.Set (Mod 12)
      chord = Set.fromList (map fromIntegral [2, 6, 11])
      chains = findChainsOfThirds chord
      matrix = compute3rdChain config chains chord
      rows = rowCount matrix
      cols = colCount matrix
      numFunctions = configNumFunctions config
      entries =
        [ ( r
          , c
          , maybe 0 id (getValue matrix (Row r) (Col (fromIntegral c)))
          )
        | r <- [0 .. rows - 1]
        , c <- [0 .. cols - 1]
        ]
      topEntries = take 12 $ reverse $ sortOn (\(_, _, v) -> v) entries

  putStrLn $ "chains used: " ++ show (length chains)
  putStrLn $ "rows: " ++ show rows ++ ", cols: " ++ show cols
  putStrLn "Top matrix entries (row, col, value, modeIdx, functionIdx):"
  mapM_ (printEntry numFunctions) topEntries

  putStrLn "\n--- Aggregated first four pitch sets ---"
  aggregateFirstFour config

printEntry :: Int -> (Int, Int, Double) -> IO ()
printEntry numFunctions (r, c, v) = do
  let modeIdx = r `div` numFunctions
      funcIdx = r `mod` numFunctions
  putStrLn $ concat
    [ "row="
    , show r
    , " (modeIdx="
    , show modeIdx
    , ", functionIdx="
    , show funcIdx
    , "), col="
    , show c
    , ", value="
    , formatDouble v
    ]

-- Aggregate the first four pitch-class sets and display the combined matrix
aggregateFirstFour :: RuntimeConfig -> IO ()
aggregateFirstFour config = do
  bytes <- BL.readFile "docs/extracted_chords.json"
  case Aeson.eitherDecode bytes of
    Left err -> putStrLn $ "Unable to decode docs/extracted_chords.json: " ++ err
    Right vec -> do
          let vec' :: V.Vector ExtractedChord
              vec' = vec
              pick i = Set.fromList . fmap fromIntegral . pitches $ vec' V.! i
              pcSets = V.fromList [pick 1, pick 1, pick 2, pick 3]
          matrices = fmap (\notes -> compute3rdChain config (findChainsOfThirds notes) notes) pcSets
          aggregated = foldl' addMatrices (zeroMatrix config) (V.toList matrices)
          rows = rowCount aggregated
          cols = colCount aggregated
          numFunctions = configNumFunctions config
          entries =
            [ ( r
              , c
              , maybe 0 id (getValue aggregated (Row r) (Col (fromIntegral c)))
              )
            | r <- [0 .. rows - 1]
            , c <- [0 .. cols - 1]
            ]
          topEntries = take 12 $ reverse $ sortOn (\(_, _, v) -> v) entries

      putStrLn $ "Pitch-class sets: " ++ show (map (map (fromIntegral . unMod) . Set.toList) (V.toList pcSets))
      putStrLn $ "Aggregated matrix dimensions: " ++ show rows ++ " x " ++ show cols
      mapM_ (printEntry numFunctions) topEntries

data ExtractedChord = ExtractedChord
  { pitches :: [Int]
  }

instance Aeson.FromJSON ExtractedChord where
  parseJSON = Aeson.withObject "ExtractedChord" $ \obj ->
    ExtractedChord <$> obj Aeson..: "pitches"

zeroMatrix :: RuntimeConfig -> RiemannMatrix
zeroMatrix cfg =
  RiemannMatrix
    { rowCount = configNumModes cfg * configNumFunctions cfg
    , colCount = configNumTonalities cfg
    , matrix = V.replicate (configNumModes cfg * configNumFunctions cfg)
        (V.replicate (configNumTonalities cfg) (Just 0))
    }

addMatrices :: RiemannMatrix -> RiemannMatrix -> RiemannMatrix
addMatrices acc mat =
  let rows = rowCount acc
      cols = colCount acc
      updates =
        [ ( (Row r, Col (fromIntegral c))
          , Just $ maybe 0 id (getValue acc (Row r) (Col (fromIntegral c)))
            + maybe 0 id (getValue mat (Row r) (Col (fromIntegral c)))
          )
        | r <- [0 .. rows - 1]
        , c <- [0 .. cols - 1]
        ]
   in updateMatrix acc updates

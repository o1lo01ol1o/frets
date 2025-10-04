{-# LANGUAGE DataKinds #-}

module Main (main) where

import Data.List (sortOn)
import qualified Data.Set as Set
import Numeric (showFFloat)

import Data.HarmonicAnalysis
import Data.HarmonicAnalysis.ChainOfThirds (findChainsOfThirds)
import Data.HarmonicAnalysis.RiemannMatrix
import Data.HarmonicAnalysis.Types
import Data.Mod (Mod)

formatDouble :: Double -> String
formatDouble x = showFFloat (Just 4) x ""

main :: IO ()
main = do
  let config = makeMajorMinorTSDConfig
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
      topEntries = take 20 $ reverse $ sortOn (\(_, _, v) -> v) entries

  putStrLn $ "chains used: " ++ show (length chains)
  putStrLn $ "rows: " ++ show rows ++ ", cols: " ++ show cols
  putStrLn "Top matrix entries (row, col, value, modeIdx, functionIdx):"
  mapM_ (printEntry numFunctions) topEntries

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

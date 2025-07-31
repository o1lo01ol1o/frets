module Data.HarmonicAnalysis.Display
  ( displayHarmonicPath,
    displayHarmonicAnalysisResult,
    displayWeights,
  )
where

import Data.HarmonicAnalysis.Types
import Data.Mod (unMod)
import Text.Printf (printf)

-- | Display harmonic analysis results
displayHarmonicPath :: HarmonicPath -> String
displayHarmonicPath (HarmonicPath points) =
  unlines $
    "Harmonic Analysis Path:"
      : "Index | Mode | Function | Tonality | Weight"
      : "------|------|----------|----------|-------"
      : map displayPoint points
  where
    displayPoint point =
      let (mode, function) = rowToModeFunction (unRow (row point))
          tonality = unMod $ unCol (col point)
       in unwords
            [ padRight 5 $ show (matrixIndex point),
              "|",
              padRight 4 $ show mode,
              "|",
              padRight 8 $ show function,
              "|",
              padRight 8 $ show tonality,
              "|",
              printf "%.3f" (value point)
            ]

    padRight n s = take n (s ++ repeat ' ')

-- | Display multi-candidate harmonic analysis results
displayHarmonicAnalysisResult :: HarmonicAnalysisResult -> String
displayHarmonicAnalysisResult (HarmonicAnalysisResult paths maxWeight hasTies) =
  unlines $
    [ "Harmonic Analysis Result:",
      "Max Weight: " ++ printf "%.3f" maxWeight,
      "Number of Candidates: " ++ show (length paths),
      "Has Ties: " ++ show hasTies,
      ""
    ]
      ++ if hasTies
        then
          [ "Multiple equally weighted candidates found:",
            ""
          ]
            ++ concatMap displayCandidate (zip [1 ..] paths)
        else
          [ "Single optimal path:",
            ""
          ]
            ++ case paths of
              [] -> ["No paths found."]
              (path : _) -> lines (displayHarmonicPath path)
  where
    displayCandidate (i, path) =
      [ "Candidate " ++ show i ++ ":",
        ""
      ]
        ++ lines (displayHarmonicPath path)
        ++ [""]

-- | Display harmonic weights
displayWeights :: HarmonicPath -> String
displayWeights (HarmonicPath points) =
  unlines $
    "Harmonic Weights Summary:"
      : ("Total points: " ++ show (length points))
      : ("Average weight: " ++ printf "%.3f" avgWeight)
      : ("Max weight: " ++ printf "%.3f" maxWeight)
      : ("Min weight: " ++ printf "%.3f" minWeight)
      : ""
      : "Individual weights:"
      : zipWith
        (\i p -> "Point " ++ show i ++ ": " ++ printf "%.3f" (value p))
        [0 ..]
        points
  where
    weights = map value points
    avgWeight = if null weights then 0 else sum weights / fromIntegral (length weights)
    maxWeight = if null weights then 0 else maximum weights
    minWeight = if null weights then 0 else minimum weights

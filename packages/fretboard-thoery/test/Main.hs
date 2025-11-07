module Main (main) where

import qualified Spec.Fretboard as FretboardSpec
import qualified Spec.HarmonicAnalysis as HarmonicAnalysis
import qualified Spec.HarmonicAnalysisHMatrix as HarmonicAnalysisHMatrix
import qualified Spec.Playback as PlaybackSpec
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "fretboard-thoery tests"
    [ FretboardSpec.tests,
      HarmonicAnalysis.tests,
      HarmonicAnalysisHMatrix.tests,
      PlaybackSpec.tests
    ]

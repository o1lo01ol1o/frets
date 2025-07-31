module Main (main) where

-- Import test modules

import qualified Debug.PathFindingDebug as PathFindingDebug
import qualified Debug.ProbabilityDebug as ProbabilityDebug
import qualified Debug.ViterbiDebug as ViterbiDebug
import qualified Spec.Fretboard as Fretboard
import qualified Spec.HarmonicAnalysis as HarmonicAnalysis
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "fretboard-thoery tests"
    [ unitTests,
      propertyTests,
      Fretboard.tests,
      HarmonicAnalysis.tests,
      PathFindingDebug.tests,
      ViterbiDebug.tests,
      ProbabilityDebug.tests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Basic test" $ do
        let result = 2 + 2
        result @?= 4
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ QC.testProperty "Addition is commutative" $ \x y ->
        (x :: Int) + y == y + x
    ]

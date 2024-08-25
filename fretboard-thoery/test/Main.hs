module Main where

import qualified Spec.Fretboard
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ Spec.Fretboard.tests
      ]

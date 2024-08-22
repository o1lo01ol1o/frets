module Main where

import Chord
import Control.Monad (replicateM)
import Criterion.Main
import Fretboard

main = go

go :: IO ()
go =
  defaultMain
    [ bgroup
        "optimized fretting"
        [ bench "optimizedFrettings: cProgression" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ optimizeFrettings 25 fretboard cProgression
        ],
      bgroup
        "chromatics from fretting"
        [ bench "chromaticsFromFretting: cMajorChord" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ fmap chromaticsFromFretting cMajor7Frettings'
        ],
      bgroup
        "fretting"
        [ bench "allfrettings: cMajorChord" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ findFrettings 100 (knownTuning Standard) cMajorChord,
          bench "allfrettings: cMajor7Chord" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ findFrettings 100 (knownTuning Standard) cMajor7Chord,
          bench "allfrettings: cMajor7Add9Chord" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ findFrettings 100 (knownTuning Standard) cMajor7Add9Chord,
          bench "allfrettings: cMajor7Add9Sharp11Chord" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ findFrettings 100 (knownTuning Standard) cMajor7Add9Sharp11Chord,
          bench "allfrettings: cMajor7Add9Sharp11Sharp13Chord" $ nfIO $ do
            let fretboard = knownTuning Standard
            return $ findFrettings 100 (knownTuning Standard) cMajor7Add9Sharp11Sharp13Chord
        ]
    ]

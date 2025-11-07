module Spec.Playback (tests) where

import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import Data.HarmonicAnalysis.Playback
  ( LoopMeter (..),
    PlaybackError (..),
    PlaybackOptions (..),
    PlaybackRequest (..),
    QuantizationGrid (..),
    RecordingEvent,
    RenderedChordSpan (..),
    RenderedPlayback (..),
    renderPlayback,
  )
import Data.Ratio ((%))
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Playback"
    [ testCase "rejects empty recording" $
        renderPlayback (PlaybackRequest [] defaultOptions)
          @?= Left PlaybackErrorEmpty,
      testCase "requires start event" $
        renderPlayback (PlaybackRequest missingStartEvents defaultOptions)
          @?= Left PlaybackErrorMissingStart,
      testCase "produces timeline without quantisation" $
        case renderPlayback (PlaybackRequest simpleEvents defaultOptions) of
          Left err -> assertFailure $ "Unexpected error: " <> show err
          Right playback -> assertSimplePlayback playback,
      testCase "applies sixteenth-note quantisation" $
        case
            renderPlayback
              ( PlaybackRequest
                  quantizedEvents
                  defaultOptions {playbackQuantization = Just QuantizeSixteenth}
              )
          of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right playback -> assertQuantizedPlayback playback,
      testCase "applies quarter-note quantisation" $
        case
            renderPlayback
              ( PlaybackRequest
                  quarterEvents
                  defaultOptions {playbackQuantization = Just QuantizeQuarter}
              )
          of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right playback -> assertQuarterPlayback playback,
      testCase "pads loop to full measures" $
        case
            renderPlayback
              ( PlaybackRequest
                  simpleEvents
                  defaultOptions
                    { playbackLoopMeter = Just LoopMeter {meterBeats = 4, meterBeatUnit = 4}
                    }
              )
          of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right playback -> rpTotalSeconds playback @?= 2
    ]

assertSimplePlayback :: RenderedPlayback -> Assertion
assertSimplePlayback playback = do
  assertBool "Expected non-empty MIDI payload" (not (null (rpMidiBase64 playback)))
  rpTotalSeconds playback @?= 1 % 2
  rpEvents playback @?= [RenderedChordSpan (T.pack "c1") 0 (1 % 2) [60, 64, 67]]

assertQuantizedPlayback :: RenderedPlayback -> Assertion
assertQuantizedPlayback playback = do
  rpTotalSeconds playback @?= 1 % 2
  rpEvents playback
    @?= [RenderedChordSpan (T.pack "c2") (1 % 4) (1 % 4) [60, 64, 67]]

assertQuarterPlayback :: RenderedPlayback -> Assertion
assertQuarterPlayback playback = do
  rpTotalSeconds playback @?= 101 % 100
  rpEvents playback
    @?= [RenderedChordSpan (T.pack "c3") 0 (1 % 1) [60, 64, 67]]

defaultOptions :: PlaybackOptions
defaultOptions =
  PlaybackOptions
    { playbackQuantization = Nothing,
      playbackLoopMeter = Nothing,
      playbackTempoBpm = Nothing
    }

simpleEvents :: [RecordingEvent]
simpleEvents =
  decodeEvents $
    B.pack
      "[\
      \ {\"type\":\"start\",\"at\":0},\
      \ {\"type\":\"chord-on\",\"at\":0,\"id\":\"c1\",\"notes\":[60,64,67]},\
      \ {\"type\":\"chord-off\",\"at\":500,\"id\":\"c1\"},\
      \ {\"type\":\"stop\",\"at\":500}\
      \]"

missingStartEvents :: [RecordingEvent]
missingStartEvents =
  decodeEvents $
    B.pack
      "[\
      \ {\"type\":\"chord-on\",\"at\":0,\"id\":\"c1\",\"notes\":[60,64,67]},\
      \ {\"type\":\"chord-off\",\"at\":500,\"id\":\"c1\"},\
      \ {\"type\":\"stop\",\"at\":500}\
      \]"

quantizedEvents :: [RecordingEvent]
quantizedEvents =
  decodeEvents $
    B.pack
      "[\
      \ {\"type\":\"start\",\"at\":0},\
      \ {\"type\":\"chord-on\",\"at\":200,\"id\":\"c2\",\"notes\":[60,64,67]},\
      \ {\"type\":\"chord-off\",\"at\":450,\"id\":\"c2\"},\
      \ {\"type\":\"stop\",\"at\":450}\
      \]"

quarterEvents :: [RecordingEvent]
quarterEvents =
  decodeEvents $
    B.pack
      "[\
      \ {\"type\":\"start\",\"at\":0},\
      \ {\"type\":\"chord-on\",\"at\":10,\"id\":\"c3\",\"notes\":[60,64,67]},\
      \ {\"type\":\"chord-off\",\"at\":1010,\"id\":\"c3\"},\
      \ {\"type\":\"stop\",\"at\":1010}\
      \]"

decodeEvents :: B.ByteString -> [RecordingEvent]
decodeEvents bytes =
  case eitherDecodeStrict bytes of
    Left err -> error $ "Failed to decode events: " <> err
    Right events -> events

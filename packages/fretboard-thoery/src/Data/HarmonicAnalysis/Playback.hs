{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.HarmonicAnalysis.Playback
  ( PlaybackRequest (..),
    PlaybackOptions (..),
    QuantizationGrid (..),
    LoopMeter (..),
    RecordingEvent (..),
    ChordEventKind (..),
    RenderedPlayback (..),
    RenderedChordSpan (..),
    PlaybackError (..),
    renderPlayback,
    playbackErrorMessage,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, when)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object, String),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.!=),
    (.=),
  )
import Data.Aeson.Types (Object, Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.EventList.Relative.TimeBody as EventList
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Numeric.NonNegative.Wrapper as NonNeg
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as MidiEvent
import qualified Sound.MIDI.File.Event.Meta as MidiMeta
import qualified Sound.MIDI.File.Save as MidiSave
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

--------------------------------------------------------------------------------
-- Top-level types

data QuantizationGrid
  = QuantizeQuarter
  | QuantizeEighth
  | QuantizeSixteenth
  | QuantizeTriplet
  | QuantizeSextuplet
  deriving (Eq, Ord, Show, Generic, NFData)

instance FromJSON QuantizationGrid where
  parseJSON = withText "QuantizationGrid" $ \case
    "quarter" -> pure QuantizeQuarter
    "eighth" -> pure QuantizeEighth
    "sixteenth" -> pure QuantizeSixteenth
    "triplet" -> pure QuantizeTriplet
    "sextuplet" -> pure QuantizeSextuplet
    other ->
      fail $
        "Unknown quantization grid: "
          <> T.unpack other

instance ToJSON QuantizationGrid where
  toJSON = \case
    QuantizeQuarter -> String "quarter"
    QuantizeEighth -> String "eighth"
    QuantizeSixteenth -> String "sixteenth"
    QuantizeTriplet -> String "triplet"
    QuantizeSextuplet -> String "sextuplet"

data LoopMeter = LoopMeter
  { meterBeats :: !Int,
    meterBeatUnit :: !Int
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance FromJSON LoopMeter where
  parseJSON = withObject "LoopMeter" $ \obj -> do
    beats <- obj .: "beats"
    beatUnit <- obj .: "beatUnit"
    when (beats <= 0) $
      fail "Loop meter beats must be positive"
    when (beatUnit `notElem` [1, 2, 4, 8, 16, 32]) $
      fail "Loop meter beatUnit must be a power-of-two note length"
    pure LoopMeter {meterBeats = beats, meterBeatUnit = beatUnit}

instance ToJSON LoopMeter where
  toJSON LoopMeter {meterBeats, meterBeatUnit} =
    object
      [ "beats" .= meterBeats,
        "beatUnit" .= meterBeatUnit
      ]

data PlaybackOptions = PlaybackOptions
  { playbackQuantization :: !(Maybe QuantizationGrid),
    playbackLoopMeter :: !(Maybe LoopMeter),
    playbackTempoBpm :: !(Maybe Scientific)
  }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON PlaybackOptions where
  parseJSON = withObject "PlaybackOptions" $ \obj -> do
    playbackQuantization <- obj .:? "quantization"
    playbackLoopMeter <- obj .:? "loopMeter"
    playbackTempoBpm <- obj .:? "tempoBpm"
    pure PlaybackOptions {..}

instance ToJSON PlaybackOptions where
  toJSON PlaybackOptions {playbackQuantization, playbackLoopMeter, playbackTempoBpm} =
    object
      [ "quantization" .= playbackQuantization,
        "loopMeter" .= playbackLoopMeter,
        "tempoBpm" .= playbackTempoBpm
      ]

newtype ChordId = ChordId {unChordId :: Text}
  deriving (Eq, Ord, Show, Generic, NFData)

newtype MidiNote = MidiNote {unMidiNote :: Word8}
  deriving (Eq, Ord, Show, Generic, NFData)

data ChordEventKind
  = ChordBegan
  | ChordEnded
  deriving (Eq, Ord, Show, Generic, NFData)

data RecordingEvent
  = RecordingStarted !Rational
  | RecordingStopped !Rational
  | ChordEvent
      { chordEventKind :: !ChordEventKind,
        chordEventTime :: !Rational,
        chordEventId :: !ChordId,
        chordEventNotes :: !(Maybe (NonEmpty MidiNote))
      }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON RecordingEvent where
  parseJSON = withObject "RecordingEvent" $ \obj -> do
    eventType <- obj .: "type"
    case (eventType :: Text) of
      "start" -> RecordingStarted <$> (millisecondsField obj >>= ensureNonNegative "start event time")
      "stop" -> RecordingStopped <$> (millisecondsField obj >>= ensureNonNegative "stop event time")
      "chord-on" -> do
        chordEventTime <- millisecondsField obj >>= ensureNonNegative "chord-on time"
        chordEventId <- ChordId <$> obj .: "id"
        notesRaw <- obj .: "notes"
        chordEventNotes <- parseMidiNotes notesRaw
        pure
          ChordEvent
            { chordEventKind = ChordBegan,
              chordEventTime,
              chordEventId,
              chordEventNotes = Just chordEventNotes
            }
      "chord-off" -> do
        chordEventTime <- millisecondsField obj >>= ensureNonNegative "chord-off time"
        chordEventId <- ChordId <$> obj .: "id"
        pure
          ChordEvent
            { chordEventKind = ChordEnded,
              chordEventTime,
              chordEventId,
              chordEventNotes = Nothing
            }
      other ->
        fail $ "Unknown recording event type: " <> T.unpack other
    where
      millisecondsField :: Object -> Parser Rational
      millisecondsField o = do
        value <- o .: "at"
        pure (scientificMillisToSeconds value)

      ensureNonNegative :: String -> Rational -> Parser Rational
      ensureNonNegative description value =
        if value < 0
          then fail $ description <> " must be non-negative"
          else pure value

      parseMidiNotes :: [Int] -> Parser (NonEmpty MidiNote)
      parseMidiNotes rawNotes = do
        when (null rawNotes) $
          fail "Chord-on event must include at least one MIDI note"
        mapped <-
          traverse
            ( \n ->
                if n < 0 || n > 127
                  then fail $ "Invalid MIDI note (expected 0-127): " <> show n
                  else pure (MidiNote (fromIntegral n))
            )
            rawNotes
        let (firstNote : rest) = mapped
        pure (firstNote :| rest)

instance ToJSON RecordingEvent where
  toJSON = \case
    RecordingStarted time ->
      object
        [ "type" .= String "start",
          "at" .= secondsToMilliseconds time
        ]
    RecordingStopped time ->
      object
        [ "type" .= String "stop",
          "at" .= secondsToMilliseconds time
        ]
    ChordEvent {chordEventKind = ChordBegan, chordEventTime, chordEventId, chordEventNotes} ->
      object
        [ "type" .= String "chord-on",
          "at" .= secondsToMilliseconds chordEventTime,
          "id" .= unChordId chordEventId,
          "notes"
            .= maybe
              ([] :: [Int])
              (fmap (fromIntegral . unMidiNote) . NE.toList)
              chordEventNotes
        ]
    ChordEvent {chordEventKind = ChordEnded, chordEventTime, chordEventId} ->
      object
        [ "type" .= String "chord-off",
          "at" .= secondsToMilliseconds chordEventTime,
          "id" .= unChordId chordEventId
        ]

data PlaybackRequest = PlaybackRequest
  { playbackEvents :: ![RecordingEvent],
    playbackOptions :: !PlaybackOptions
  }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON PlaybackRequest where
  parseJSON = withObject "PlaybackRequest" $ \obj -> do
    playbackEvents <- obj .:? "events" .!= []
    playbackOptions <- obj .:? "options" .!= PlaybackOptions Nothing Nothing Nothing
    pure PlaybackRequest {..}

instance ToJSON PlaybackRequest where
  toJSON PlaybackRequest {playbackEvents, playbackOptions} =
    object
      [ "events" .= playbackEvents,
        "options" .= playbackOptions
      ]

data RenderedChordSpan = RenderedChordSpan
  { rcsId :: !Text,
    rcsOnsetSeconds :: !Rational,
    rcsDurationSeconds :: !Rational,
    rcsMidiNotes :: ![Int]
  }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON RenderedChordSpan where
  toJSON RenderedChordSpan {..} =
    object
      [ "id" .= rcsId,
        "onsetSeconds" .= secondsToScientific rcsOnsetSeconds,
        "durationSeconds" .= secondsToScientific rcsDurationSeconds,
        "midiNotes" .= rcsMidiNotes
      ]

data RenderedPlayback = RenderedPlayback
  { rpMidiBase64 :: !Text,
    rpEvents :: ![RenderedChordSpan],
    rpTotalSeconds :: !Rational
  }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON RenderedPlayback where
  toJSON RenderedPlayback {..} =
    object
      [ "midiBase64" .= rpMidiBase64,
        "totalSeconds" .= secondsToScientific rpTotalSeconds,
        "events" .= rpEvents
      ]

data PlaybackError
  = PlaybackErrorEmpty
  | PlaybackErrorMissingStart
  | PlaybackErrorMissingStop
  | PlaybackErrorUnmatchedChord !ChordId
  | PlaybackErrorMismatchedChordNotes !ChordId
  | PlaybackErrorNegativeDuration !ChordId
  | PlaybackErrorInternal !Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Public API

renderPlayback :: PlaybackRequest -> Either PlaybackError RenderedPlayback
renderPlayback PlaybackRequest {playbackEvents, playbackOptions}
  | null playbackEvents = Left PlaybackErrorEmpty
  | otherwise = do
      timeline <- buildTimeline playbackEvents
      let quantized =
            case playbackQuantization playbackOptions of
              Nothing -> timeline
              Just grid -> quantizeTimeline grid (tempoFromOptions playbackOptions) timeline
          padded = applyLoopPadding (tempoFromOptions playbackOptions) (playbackLoopMeter playbackOptions) quantized
      midiBytes <- timelineToMidi (tempoFromOptions playbackOptions) padded
      let encoded = T.decodeUtf8 (Base64.encode midiBytes)
          renderedSpans =
            fmap
              ( \ChordSpan {..} ->
                  RenderedChordSpan
                    { rcsId = unChordId spanId,
                      rcsOnsetSeconds = spanOnset,
                      rcsDurationSeconds = spanDuration,
                      rcsMidiNotes = fmap (fromIntegral . unMidiNote) (NE.toList spanNotes)
                    }
              )
              (timelineSpans padded)
      pure
        RenderedPlayback
          { rpMidiBase64 = encoded,
            rpEvents = renderedSpans,
            rpTotalSeconds = timelineTotal padded
          }

--------------------------------------------------------------------------------
-- Timeline construction

data ActiveChord = ActiveChord
  { activeStart :: !Rational,
    activeNotes :: !(NonEmpty MidiNote)
  }

data ChordSpan = ChordSpan
  { spanId :: !ChordId,
    spanOnset :: !Rational,
    spanDuration :: !Rational,
    spanNotes :: !(NonEmpty MidiNote)
  }
  deriving (Eq, Show)

data Timeline = Timeline
  { timelineSpans :: ![ChordSpan],
    timelineTotal :: !Rational
  }

buildTimeline :: [RecordingEvent] -> Either PlaybackError Timeline
buildTimeline events = do
  let startTimes = [t | RecordingStarted t <- events]
      stopTimes = [t | RecordingStopped t <- events]
      chordEvents =
        sortOn chordEventTime
          [ evt
            | evt@ChordEvent {} <- events
          ]
  origin <-
    case startTimes of
      [] -> Left PlaybackErrorMissingStart
      (firstStart : _) -> pure firstStart
  finalStop <-
    case reverse stopTimes of
      [] -> Left PlaybackErrorMissingStop
      (lastStop : _) -> pure lastStop
  when (finalStop < origin) $
    Left (PlaybackErrorInternal "Stop time occurs before start time")
  let initial =
        ( Map.empty,
          [],
          origin,
          origin
        )
      (activeFinal, spans, _originRef, lastSeen) =
        foldl'
          ( \(active, accSpans, seenOrigin, seenTime) event ->
              case chordEventKind event of
                ChordBegan ->
                  case chordEventNotes event of
                    Nothing ->
                      (active, accSpans, seenOrigin, max seenTime (chordEventTime event))
                    Just notes ->
                      ( Map.insert (chordEventId event) (ActiveChord (chordEventTime event) notes) active,
                        accSpans,
                        seenOrigin,
                        max seenTime (chordEventTime event)
                      )
                ChordEnded ->
                  case Map.lookup (chordEventId event) active of
                    Nothing ->
                      (active, accSpans, seenOrigin, max seenTime (chordEventTime event))
                    Just ActiveChord {activeStart, activeNotes} ->
                      let duration = chordEventTime event - activeStart
                          newSpan =
                            ChordSpan
                              { spanId = chordEventId event,
                                spanOnset = activeStart - origin,
                                spanDuration = duration,
                                spanNotes = activeNotes
                              }
                       in ( Map.delete (chordEventId event) active,
                            newSpan : accSpans,
                            seenOrigin,
                            max seenTime (chordEventTime event)
                          )
          )
          initial
          chordEvents
  case Map.toList activeFinal of
    [] -> pure ()
    ((danglingId, _) : _) -> Left (PlaybackErrorUnmatchedChord danglingId)
  spansPositive <-
    traverse
      ( \spanItem ->
          if spanDuration spanItem <= 0
            then Left (PlaybackErrorNegativeDuration (spanId spanItem))
            else pure spanItem
      )
      spans
  let allSpans = reverse spansPositive
      totalDuration = max 0 (finalStop - origin)
  pure Timeline {timelineSpans = allSpans, timelineTotal = totalDuration}

--------------------------------------------------------------------------------
-- Quantisation and padding

tempoFromOptions :: PlaybackOptions -> Rational
tempoFromOptions PlaybackOptions {playbackTempoBpm} =
  maybe 120 (toRational . toRealFloat) playbackTempoBpm

quantizeTimeline :: QuantizationGrid -> Rational -> Timeline -> Timeline
quantizeTimeline grid tempo Timeline {timelineSpans, timelineTotal} =
  Timeline
    { timelineSpans = quantizedSpans,
      timelineTotal = max timelineTotal (maximumEnd quantizedSpans)
    }
  where
    subdivisions =
      case grid of
        QuantizeQuarter -> 1
        QuantizeEighth -> 2
        QuantizeTriplet -> 3
        QuantizeSixteenth -> 4
        QuantizeSextuplet -> 6
    toBeats seconds = seconds * tempo / 60
    fromBeats beats = beats * 60 / tempo
    minimumStepBeats = 1 / fromIntegral subdivisions

    quantizeValue beats =
      fromIntegral (round (beats * fromIntegral subdivisions))
        / fromIntegral subdivisions

    quantizeSpan spanItem =
      let onsetBeats = toBeats (spanOnset spanItem)
          endBeats = toBeats (spanOnset spanItem + spanDuration spanItem)
          onsetQuant = quantizeValue onsetBeats
          endQuant =
            max
              (onsetQuant + minimumStepBeats)
              (quantizeValue endBeats)
          durationSeconds = fromBeats (endQuant - onsetQuant)
       in spanItem
            { spanOnset = fromBeats onsetQuant,
              spanDuration = durationSeconds
            }

    quantizedSpans = sortOn spanOnset (fmap quantizeSpan timelineSpans)

applyLoopPadding :: Rational -> Maybe LoopMeter -> Timeline -> Timeline
applyLoopPadding tempo maybeMeter timeline@Timeline {timelineTotal} =
  case maybeMeter of
    Nothing -> timeline
    Just LoopMeter {meterBeats, meterBeatUnit} ->
      let beatsPerBar = fromIntegral meterBeats * (4 / fromIntegral meterBeatUnit)
          totalBeats = timelineTotal * tempo / 60
          measuresRequired =
            ceiling (totalBeats / beatsPerBar :: Rational)
          paddedBeats =
            max beatsPerBar (fromIntegral (max 1 measuresRequired) * beatsPerBar)
          paddedSeconds = paddedBeats * 60 / tempo
       in timeline {timelineTotal = paddedSeconds}

maximumEnd :: [ChordSpan] -> Rational
maximumEnd = foldl' (\acc spanItem -> max acc (spanOnset spanItem + spanDuration spanItem)) 0

--------------------------------------------------------------------------------
-- MIDI rendering

timelineToMidi :: Rational -> Timeline -> Either PlaybackError BS.ByteString
timelineToMidi tempo Timeline {timelineSpans, timelineTotal} = do
  let ticksPerQuarter :: Integer
      ticksPerQuarter = 480
      mpqn :: Integer
      mpqn =
        let microsPerMinute = 60 * 1000000
            micros = round (toRational microsPerMinute / tempo)
         in max 1 micros
      channel = ChannelMsg.toChannel 0
      velocityOn = VoiceMsg.toVelocity 96
      velocityOff = VoiceMsg.toVelocity 0
      mpqnTempo :: MidiMeta.Tempo
      mpqnTempo =
        NonNeg.fromNumberClip (max 1 (fromIntegral mpqn :: Int))
      toTicks :: Rational -> Integer
      toTicks seconds =
        round (seconds * tempo / 60 * fromIntegral (ticksPerQuarter :: Integer))
      noteEvents ::
        [(Integer, MidiEvent.T)]
      noteEvents =
        concatMap
          ( \ChordSpan {spanOnset, spanDuration, spanNotes} ->
              let startTicks = toTicks spanOnset
                  endTicks = toTicks (spanOnset + spanDuration)
                  safeEnd = max (startTicks + 1) endTicks
               in concatMap
                    ( \note ->
                        [ ( startTicks,
                            MidiEvent.MIDIEvent
                              ( ChannelMsg.Cons
                                  channel
                                  (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch (fromIntegral (unMidiNote note))) velocityOn))
                              )
                          ),
                          ( safeEnd,
                            MidiEvent.MIDIEvent
                              ( ChannelMsg.Cons
                                  channel
                                  (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch (fromIntegral (unMidiNote note))) velocityOff))
                              )
                          )
                        ]
                    )
                    (NE.toList spanNotes)
          )
          timelineSpans
      absoluteEvents =
        sortOn fst $
          [ (0, MidiEvent.MetaEvent (MidiMeta.TrackName "Tonnetz Recording")),
            (0, MidiEvent.MetaEvent (MidiMeta.SetTempo mpqnTempo))
          ]
            <> noteEvents
            <> [ (toTicks timelineTotal, MidiEvent.MetaEvent MidiMeta.EndOfTrack)]
      relEvents = absoluteToRelative absoluteEvents
      track = relEvents
      timeDivision =
        MidiFile.Ticks (NonNeg.fromNumberClip (fromIntegral ticksPerQuarter :: Int))
      midiFile =
        MidiFile.Cons
          MidiFile.Parallel
          timeDivision
          [track]
      midiStrict = BL.toStrict (MidiSave.toByteString midiFile)
  pure midiStrict

absoluteToRelative ::
  [(Integer, MidiEvent.T)] ->
  EventList.T MidiMeta.ElapsedTime MidiEvent.T
absoluteToRelative events =
  let sorted = sortOn fst events
      go [] _acc _prev = []
      go ((timeStamp, body) : rest) acc prevTime =
        let delta = max 0 (timeStamp - prevTime)
            deltaElapsed = MidiMeta.toElapsedTime delta
         in go rest ((deltaElapsed, body) : acc) timeStamp
   in EventList.fromPairList (reverse (go sorted [] 0))

--------------------------------------------------------------------------------
-- Utility helpers

scientificMillisToSeconds :: Scientific -> Rational
scientificMillisToSeconds value =
  toRational (toRealFloat value :: Double) / 1000

secondsToMilliseconds :: Rational -> Scientific
secondsToMilliseconds value =
  realToFrac (fromRational value * 1000 :: Double)

secondsToScientific :: Rational -> Scientific
secondsToScientific value =
  realToFrac (fromRational value :: Double)

playbackErrorMessage :: PlaybackError -> Text
playbackErrorMessage = \case
  PlaybackErrorEmpty ->
    "Recording contained no events to render."
  PlaybackErrorMissingStart ->
    "Recording did not include a start event."
  PlaybackErrorMissingStop ->
    "Recording did not include a stop event."
  PlaybackErrorUnmatchedChord cid ->
    "Chord \""
      <> unChordId cid
      <> "\" never received a corresponding chord-off event."
  PlaybackErrorMismatchedChordNotes cid ->
    "Chord \""
      <> unChordId cid
      <> "\" had inconsistent note data."
  PlaybackErrorNegativeDuration cid ->
    "Chord \""
      <> unChordId cid
      <> "\" produced a non-positive duration."
  PlaybackErrorInternal msg ->
    "Internal playback error: " <> msg

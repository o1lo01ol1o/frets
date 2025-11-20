{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Chord (transposeChromatic)
import Chord.Names (ChordName (..), chordNameFromPitchClasses)
import Control.DeepSeq (force)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Number, String, Object),
    object,
    withObject,
    (.:),
    (.:?),
    (.!=),
    (.=),
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isSpace)
import Data.Foldable (toList)
import Data.List (maximumBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Mod (Mod, unMod)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Ord (comparing)
import Finger (Finger)
import GHC.Generics (Generic)
import Fretboard
  ( Fretboard (..),
    Fretting (..),
    chromaticsFromFretting,
    frettingDistance,
    occurrencesForPitchClasses,
    optimizeFrettings,
    FretboardNoteOccurrence (..),
    scoreDifficulty,
  )
import Modulation
  ( Chromatic (..),
    Degree,
    HeptatonicScale (HeptatonicScale),
    LocalInterpretation (toLocalInterpretation),
    cAeolian,
    cDorian,
    cIonian,
    cLocrian,
    cLydian,
    cMixolydian,
    cPhrygian,
    transposeScale,
    transposition,
  )
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.Cors
  ( cors,
    corsRequestHeaders,
    simpleCorsResourcePolicy
  )
import Servant
  ( Handler,
    (:<|>) (..),
    Get,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    ServerError (..),
    err400,
    err500,
    serve,
    throwError,
    type (:>),
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.HarmonicAnalysis
  ( AnalysisPreset (..),
    analyzeAnnotated,
  )
import Data.HarmonicAnalysis.Playback
  ( PlaybackError,
    PlaybackRequest,
    RenderedPlayback,
    playbackErrorMessage,
    renderPlayback,
  )
import Data.HarmonicAnalysis.Types
  ( AnnotatedHarmonicPath (..),
    Col (..),
    Function (..),
    FunctionalHarmonyAnnotation (..),
    HarmonicStep (..),
    Mode (..),
    RMPoint (..),
  )
import Data.Tonnetz.AmmannBeekner
  ( TonnetzComputation (..),
    PolygonSummary (..),
    VertexNote (..),
    computeTiling,
  )
import Data.Tonnetz.Generalized
  ( IntervalInterpretation (..),
    computeGeneralizedTiling,
  )
import Data.Tonnetz.Geometry
  ( TonnetzCoordinate (..),
    TonnetzFace (..),
    TonnetzPoint (..),
    loadAmmannBeeknerGeometry,
  )
import Data.Tonnetz.Structures
  ( TonnetzStructure (..),
    allStructures,
    intervalOptions,
    structureFromText,
    structureToText,
  )

--------------------------------------------------------------------------------
-- Main / server wiring

main :: IO ()
main = do
  port <- maybe 8080 parsePort <$> lookupEnv "PORT"
  geometryResult <- loadAmmannBeeknerGeometry
  geometryFaces <-
    either
      (fail . ("Failed to load Ammann–Beekner geometry: " <>))
      pure
      geometryResult
  let context = ServerContext {scGeometry = geometryFaces}
  putStrLn $ "Starting harmonic-function server on port " <> show port
  runSettings (setPort port defaultSettings) $
    corsMiddleware (serve harmonicAPI (server context))
  where
    parsePort :: String -> Int
    parsePort str =
      case readMaybe str of
        Just p | p > 0 -> p
        _ -> 8080
    corsMiddleware = cors (const $ Just policy)
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = "Content-Type" : corsRequestHeaders simpleCorsResourcePolicy
        }

data ServerContext = ServerContext
  { scGeometry :: ![TonnetzFace]
  }

type HarmonicAPI =
  "analyze"
    :> ReqBody '[JSON] AnalyzeRequest
    :> Post '[JSON] AnalyzeResponse
    :<|> "voice-leading"
      :> ReqBody '[JSON] VoiceLeadingRequest
      :> Post '[JSON] VoiceLeadingResponse
    :<|> "scale-notes"
      :> ReqBody '[JSON] ScaleNotesRequest
      :> Post '[JSON] ScaleNotesResponse
    :<|> "fretboard"
      :> "occurrences"
      :> ReqBody '[JSON] FretboardOccurrencesRequest
      :> Post '[JSON] FretboardOccurrencesResponse
    :<|> "fretboard"
      :> "chord-names"
      :> ReqBody '[JSON] FretboardChordNamesRequest
      :> Post '[JSON] FretboardChordNamesResponse
    :<|> "tonnetz"
      :> TonnetzAPI

type TonnetzAPI =
  "options" :> Get '[JSON] TonnetzOptionsResponse
    :<|> "tiling"
      :> ReqBody '[JSON] TonnetzTilingRequest
      :> Post '[JSON] TonnetzTilingResponse
    :<|> "recording"
      :> "render"
      :> ReqBody '[JSON] PlaybackRequest
      :> Post '[JSON] RenderedPlayback

harmonicAPI :: Proxy HarmonicAPI
harmonicAPI = Proxy

server :: ServerContext -> Server HarmonicAPI
server ctx =
  postAnalyze
    :<|> postVoiceLeading
    :<|> postScaleNotes
    :<|> postFretboardOccurrences
    :<|> postFretboardChordNames
    :<|> tonnetzServer ctx

tonnetzServer :: ServerContext -> Server TonnetzAPI
tonnetzServer ctx =
  getTonnetzOptions
    :<|> postTonnetzTiling ctx
    :<|> postTonnetzRecordingRender

getTonnetzOptions :: Handler TonnetzOptionsResponse
getTonnetzOptions = pure $
  TonnetzOptionsResponse
    { torStructures = fmap buildStructure allStructures
    }
  where
    buildStructure structure =
      let label = structureToText structure
          structureId = T.toLower label
          intervals =
            fmap
              (buildInterval structureId)
              (intervalOptions structure)
       in TonnetzStructureOptionsResponse
            { tsoId = structureId,
              tsoLabel = label,
              tsoIntervals = intervals
            }
    buildInterval structureId ints =
      let stepLabel = T.intercalate "-" (fmap (T.pack . show) ints)
          optionId = T.intercalate ":" [structureId, stepLabel]
       in TonnetzIntervalOptionResponse
            { tioId = optionId,
              tioLabel = stepLabel,
              tioSteps = ints
            }

postTonnetzTiling ::
  ServerContext ->
  TonnetzTilingRequest ->
  Handler TonnetzTilingResponse
postTonnetzTiling ServerContext {..} TonnetzTilingRequest {..} = do
  structure <-
    maybe
      (badRequest "Unrecognised Tonnetz structure")
      pure
      (structureFromText ttrStructure)
  degreeValue <-
    maybe
      (badRequest "Unrecognised degree")
      pure
      (readMaybe (T.unpack (T.strip ttrDegree)))
  let baseMidi = clampMidi (fromMaybe 60 ttrBaseMidi)
  (intervalDisplay, computation) <-
    case structure of
      TonnetzStructureClassical -> do
        intervalTuple <-
          case ttrInterval of
            [a, b, c, d] ->
              pure (toMod7 a, toMod7 b, toMod7 c, toMod7 d)
            _ ->
              badRequest "Interval must contain exactly four integers"
        let comp = computeTiling degreeValue intervalTuple baseMidi scGeometry
        pure (tupleToIntList intervalTuple, comp)
      TonnetzStructureTetrads -> do
        intervalTuple <-
          case ttrInterval of
            [a, b, c, d] ->
              pure (toMod7 a, toMod7 b, toMod7 c, toMod7 d)
            _ ->
              badRequest "Interval must contain exactly four integers"
        let comp = computeTiling degreeValue intervalTuple baseMidi scGeometry
        pure (tupleToIntList intervalTuple, comp)
      TonnetzStructureGeneralizedChromatic -> do
        triple <-
          case ttrInterval of
            [a, b, c] ->
              pure (a, b, c)
            _ ->
              badRequest "Interval must contain exactly three integers"
        let comp =
              computeGeneralizedTiling
                IntervalInterpretationChromatic
                degreeValue
                triple
                baseMidi
        pure (tripleToList triple, comp)
      TonnetzStructureGeneralizedModal -> do
        triple <-
          case ttrInterval of
            [a, b, c] ->
              pure (a, b, c)
            _ ->
              badRequest "Interval must contain exactly three integers"
        let comp =
              computeGeneralizedTiling
                IntervalInterpretationModal
                degreeValue
                triple
                baseMidi
        pure (tripleToList triple, comp)
  let vertexResponses = fmap toVertexResponse (Map.elems (tcVertexNotes computation))
      polygonResponses = fmap toPolygonResponse (tcPolygons computation)
  pure
    TonnetzTilingResponse
      { ttvStructure = structureToText structure,
        ttvInterval = intervalDisplay,
        ttvDegree = T.pack (show degreeValue),
        ttvBaseMidi = baseMidi,
        ttvVertices = vertexResponses,
        ttvPolygons = polygonResponses
      }
  where
    toVertexResponse VertexNote {..} =
      TonnetzVertexResponse
        { tvrCoordinate = coordinateToList vnCoordinate,
          tvrMidi = vnMidi,
          tvrPitchClass = vnPitchClass,
          tvrPitchClassName = vnPitchClassName,
          tvrNoteName = vnNoteName,
          tvrOctave = vnOctave
        }
    toPolygonResponse PolygonSummary {..} =
      TonnetzPolygonResponse
        { tprFaceVertices = fmap pointToList psFacePoints,
          tprVertexCoordinates = fmap coordinateToList psVertexCoords,
          tprPitchClasses = psPitchClasses,
          tprMidiNotes = psMidiNotes,
          tprChord = buildChord psChordPrimary psChordAliases
        }

postTonnetzRecordingRender ::
  PlaybackRequest ->
  Handler RenderedPlayback
postTonnetzRecordingRender request =
  either (badRequest . playbackErrorMessage) pure (renderPlayback request)

buildChord :: Maybe Text -> [Text] -> Maybe TonnetzChordLabel
buildChord Nothing _ = Nothing
buildChord (Just name) aliases =
  Just
    TonnetzChordLabel
      { tclName = name,
        tclAliases = aliases
      }

tupleToIntList :: (Mod 7, Mod 7, Mod 7, Mod 7) -> [Int]
tupleToIntList (a, b, c, d) = fmap (fromIntegral . unMod) [a, b, c, d]

tripleToList :: (Int, Int, Int) -> [Int]
tripleToList (a, b, c) = [a, b, c]

coordinateToList :: TonnetzCoordinate -> [Int]
coordinateToList TonnetzCoordinate {..} = [tcA, tcB, tcC, tcD]

pointToList :: TonnetzPoint -> [Double]
pointToList TonnetzPoint {..} = [tpX, tpY]

toMod7 :: Int -> Mod 7
toMod7 = fromIntegral

clampMidi :: Int -> Int
clampMidi n = max 0 (min 127 n)

--------------------------------------------------------------------------------
-- Request / response payloads

data AnalyzeRequest = AnalyzeRequest
  { requestPreset :: !(Maybe AnalysisPreset),
    requestProgression :: ![[PitchClassSpec]]
  }
  deriving (Show, Generic)

instance FromJSON AnalyzeRequest where
  parseJSON = withObject "AnalyzeRequest" $ \obj ->
    AnalyzeRequest
      <$> obj .:? "preset"
      <*> obj .: "progression"

instance FromJSON AnalysisPreset where
  parseJSON = \case
    String txt ->
      case presetFromText txt of
        Just preset -> pure preset
        Nothing ->
          fail $
            "Unknown preset \""
              <> T.unpack txt
              <> "\". Expected one of: "
              <> T.unpack (T.intercalate ", " presetNames)
    _ -> fail "Preset must be a string"

data PitchClassSpec
  = PitchClassByChromatic !Text
  | PitchClassByNumber !Int
  deriving (Show, Eq, Ord)

instance FromJSON PitchClassSpec where
  parseJSON = \case
    String txt -> pure $ PitchClassByChromatic txt
    Number n ->
      case floatingOrInteger n of
        Right (value :: Integer) -> pure $ PitchClassByNumber (fromIntegral value)
        Left (_ :: Double) -> fail "Pitch-class numbers must be integers"
    Object obj -> do
      pitchName <- obj .: "pitch"
      pure $ PitchClassByChromatic pitchName
    _ -> fail "Pitch-class entries must be strings like \"C#\" or integers"

data AnalyzeResponse = AnalyzeResponse
  { responsePreset :: !Text,
    responseSteps :: ![HarmonicStepResponse]
  }
  deriving (Show, Generic)

instance ToJSON AnalyzeResponse where
  toJSON AnalyzeResponse {..} =
    object
      [ "preset" .= responsePreset,
        "steps" .= responseSteps
      ]

data HarmonicStepResponse = HarmonicStepResponse
  { hsIndex :: !Int,
    hsTonality :: !Int,
    hsKey :: !Text,
    hsWeight :: !Double,
    hsPitchClasses :: ![Int],
    hsPitchClassNames :: ![Text],
    hsScalePitchClasses :: ![Int],
    hsScalePitchClassNames :: ![Text],
    hsScaleDeviationPitchClasses :: ![Int],
    hsScaleDeviationPitchClassNames :: ![Text],
    hsMode :: !Text,
    hsFunction :: !Text,
    hsDegree :: !Text,
    hsRomanNumeral :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance ToJSON HarmonicStepResponse where
  toJSON HarmonicStepResponse {..} =
    object
      [ "index" .= hsIndex,
        "tonality" .= hsTonality,
        "key" .= hsKey,
        "weight" .= hsWeight,
        "pitchClasses" .= hsPitchClasses,
        "pitchClassNames" .= hsPitchClassNames,
        "scalePitchClasses" .= hsScalePitchClasses,
        "scalePitchClassNames" .= hsScalePitchClassNames,
        "scaleDeviationPitchClasses" .= hsScaleDeviationPitchClasses,
        "scaleDeviationPitchClassNames" .= hsScaleDeviationPitchClassNames,
        "mode" .= hsMode,
        "function" .= hsFunction,
        "degree" .= hsDegree,
        "romanNumeral" .= hsRomanNumeral
      ]

data TuningNoteSpec
  = TuningNoteSpecText !Text !(Maybe Int)
  | TuningNoteSpecNumber !Int !(Maybe Int)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TuningNoteSpec where
  parseJSON = \case
    String txt -> pure $ TuningNoteSpecText txt Nothing
    Number n ->
      case floatingOrInteger n of
        Right (value :: Integer) -> pure $ TuningNoteSpecNumber (fromIntegral value) Nothing
        Left (_ :: Double) -> fail "Tuning numeric values must be integers"
    Object obj -> do
      let fetch key = do
            candidate <- obj .:? key
            pure $
              case fmap T.strip candidate of
                Just txt | not (T.null txt) -> Just txt
                _ -> Nothing
          firstNonEmpty [] = pure Nothing
          firstNonEmpty (parser : rest) = do
            result <- parser
            case result of
              Just value -> pure (Just value)
              Nothing -> firstNonEmpty rest
      nameTxt <-
        firstNonEmpty
          [ fetch "name",
            fetch "pitchClass",
            fetch "note",
            fetch "pitch"
          ]
      case nameTxt of
        Nothing -> fail "Tuning entries must specify a pitch class name"
        Just baseName -> do
          octaveVal <- obj .:? "octave"
          pure $ TuningNoteSpecText baseName octaveVal
    _ -> fail "Tuning entries must be strings, numbers, or objects"

data VoiceLeadingRequest = VoiceLeadingRequest
  { vlTuning :: ![TuningNoteSpec],
    vlProgression :: ![[PitchClassSpec]],
    vlMaxCandidates :: !(Maybe Int)
  }
  deriving (Show, Generic)

instance FromJSON VoiceLeadingRequest where
  parseJSON = withObject "VoiceLeadingRequest" $ \obj ->
    VoiceLeadingRequest
      <$> obj .: "tuning"
      <*> obj .: "progression"
      <*> obj .:? "maxCandidates"

data VoiceLeadingResponse = VoiceLeadingResponse
  { vlTotalCost :: !Int,
    vlSteps :: ![VoiceLeadingStepResponse]
  }
  deriving (Show, Generic)

instance ToJSON VoiceLeadingResponse where
  toJSON VoiceLeadingResponse {..} =
    object
      [ "totalCost" .= vlTotalCost,
        "steps" .= vlSteps
      ]

data VoiceLeadingStepResponse = VoiceLeadingStepResponse
  { vlsIndex :: !Int,
    vlsDifficulty :: !Int,
    vlsTransitionCost :: !Int,
    vlsPitchClasses :: ![Int],
    vlsPitchClassNames :: ![Text],
    vlsPositions :: ![StringPositionResponse]
  }
  deriving (Show, Generic)

instance ToJSON VoiceLeadingStepResponse where
  toJSON VoiceLeadingStepResponse {..} =
    object
      [ "index" .= vlsIndex,
        "difficulty" .= vlsDifficulty,
        "transitionCost" .= vlsTransitionCost,
        "pitchClasses" .= vlsPitchClasses,
        "pitchClassNames" .= vlsPitchClassNames,
        "positions" .= vlsPositions
      ]

data StringPositionResponse = StringPositionResponse
  { sprString :: !Int,
    sprFret :: !Int,
    sprFinger :: !(Maybe Text),
    sprPitchClass :: !Text,
    sprPitchClassNumber :: !Int,
    sprOctave :: !(Maybe Int),
    sprNoteName :: !Text
  }
  deriving (Show, Generic)

instance ToJSON StringPositionResponse where
  toJSON StringPositionResponse {..} =
    object
      [ "string" .= sprString,
        "fret" .= sprFret,
        "finger" .= sprFinger,
        "pitchClass" .= sprPitchClass,
        "pitchClassNumber" .= sprPitchClassNumber,
        "octave" .= sprOctave,
        "noteName" .= sprNoteName
      ]

data ScaleNotesRequest = ScaleNotesRequest
  { snrMode :: !Text,
    snrTonic :: !PitchClassSpec
  }
  deriving (Show, Generic)

instance FromJSON ScaleNotesRequest where
  parseJSON = withObject "ScaleNotesRequest" $ \obj ->
    ScaleNotesRequest
      <$> obj .: "mode"
      <*> obj .: "tonic"

data ScaleNotesResponse = ScaleNotesResponse
  { snsPitchClasses :: ![Int],
    snsPitchClassNames :: ![Text],
    snsDiffPitchClasses :: ![Int],
    snsDiffPitchClassNames :: ![Text]
  }
  deriving (Show, Generic)

instance ToJSON ScaleNotesResponse where
  toJSON ScaleNotesResponse {..} =
    object
      [ "pitchClasses" .= snsPitchClasses,
        "pitchClassNames" .= snsPitchClassNames,
        "diffPitchClasses" .= snsDiffPitchClasses,
        "diffPitchClassNames" .= snsDiffPitchClassNames
      ]

data FretboardNoteSetSpec = FretboardNoteSetSpec
  { fnsId :: !Text,
    fnsPitchClasses :: ![PitchClassSpec],
    fnsLabel :: !(Maybe Text),
    fnsMode :: !(Maybe Mode),
    fnsTonic :: !(Maybe PitchClassSpec)
  }
  deriving (Show, Generic)

instance FromJSON FretboardNoteSetSpec where
  parseJSON = withObject "FretboardNoteSetSpec" $ \obj -> do
    noteSetId <- obj .:? "id" .!= ""
    pitchClasses <- obj .: "pitchClasses"
    label <- obj .:? "label"
    modeText <- obj .:? "mode"
    modeValue <-
      case modeText of
        Nothing -> pure Nothing
        Just txt ->
          case modeFromText txt of
            Just m -> pure (Just m)
            Nothing ->
              fail $
                "Unrecognised mode \""
                  <> T.unpack txt
                  <> "\""
    tonicSpec <- obj .:? "tonic"
    let setId = if T.null (T.strip noteSetId) then "set" else T.strip noteSetId
    pure $ FretboardNoteSetSpec setId pitchClasses label modeValue tonicSpec

data FretboardOccurrencesRequest = FretboardOccurrencesRequest
  { forTuning :: ![TuningNoteSpec],
    forMaxFrets :: !(Maybe Int),
    forNoteSets :: ![FretboardNoteSetSpec]
  }
  deriving (Show, Generic)

instance FromJSON FretboardOccurrencesRequest where
  parseJSON = withObject "FretboardOccurrencesRequest" $ \obj ->
    FretboardOccurrencesRequest
      <$> obj .: "tuning"
      <*> obj .:? "maxFrets"
      <*> obj .: "noteSets"

data FretboardOccurrenceResponse = FretboardOccurrenceResponse
  { foString :: !Int,
    foFret :: !Int,
    foPitchClass :: !Int,
    foPitchClassName :: !Text,
    foOctave :: !(Maybe Int),
    foNoteName :: !Text
  }
  deriving (Show, Generic)

instance ToJSON FretboardOccurrenceResponse where
  toJSON FretboardOccurrenceResponse {..} =
    object
      [ "string" .= foString,
        "fret" .= foFret,
        "pitchClass" .= foPitchClass,
        "pitchClassName" .= foPitchClassName,
        "octave" .= foOctave,
        "noteName" .= foNoteName
      ]

data FretboardOccurrenceSetResponse = FretboardOccurrenceSetResponse
  { fosId :: !Text,
    fosLabel :: !(Maybe Text),
    fosPitchClasses :: ![Int],
    fosPitchClassNames :: ![Text],
    fosOccurrences :: ![FretboardOccurrenceResponse]
  }
  deriving (Show, Generic)

instance ToJSON FretboardOccurrenceSetResponse where
  toJSON FretboardOccurrenceSetResponse {..} =
    object
      [ "id" .= fosId,
        "label" .= fosLabel,
        "pitchClasses" .= fosPitchClasses,
        "pitchClassNames" .= fosPitchClassNames,
        "occurrences" .= fosOccurrences
      ]

data FretboardOccurrencesResponse = FretboardOccurrencesResponse
  { forResponseNoteSets :: ![FretboardOccurrenceSetResponse]
  }
  deriving (Show, Generic)

instance ToJSON FretboardOccurrencesResponse where
  toJSON FretboardOccurrencesResponse {..} =
    object
      [ "noteSets" .= forResponseNoteSets
      ]

data FretboardChordNamesRequest = FretboardChordNamesRequest
  { fcnrNoteSets :: ![FretboardNoteSetSpec]
  }
  deriving (Show, Generic)

instance FromJSON FretboardChordNamesRequest where
  parseJSON = withObject "FretboardChordNamesRequest" $ \obj ->
    FretboardChordNamesRequest
      <$> obj .: "noteSets"

data FretboardChordNameEntry = FretboardChordNameEntry
  { fcnId :: !Text,
    fcnPitchClasses :: ![Int],
    fcnName :: !(Maybe Text),
    fcnAliases :: ![Text]
  }
  deriving (Show, Generic)

instance ToJSON FretboardChordNameEntry where
  toJSON FretboardChordNameEntry {..} =
    object
      [ "id" .= fcnId,
        "pitchClasses" .= fcnPitchClasses,
        "name" .= fcnName,
        "aliases" .= fcnAliases
      ]

data FretboardChordNamesResponse = FretboardChordNamesResponse
  { fcnrChordNames :: ![FretboardChordNameEntry]
  }
  deriving (Show, Generic)

instance ToJSON FretboardChordNamesResponse where
  toJSON FretboardChordNamesResponse {..} =
    object
      [ "chordNames" .= fcnrChordNames
      ]

data TonnetzIntervalOptionResponse = TonnetzIntervalOptionResponse
  { tioId :: !Text,
    tioLabel :: !Text,
    tioSteps :: ![Int]
  }
  deriving (Show, Generic)

instance ToJSON TonnetzIntervalOptionResponse where
  toJSON TonnetzIntervalOptionResponse {..} =
    object
      [ "id" .= tioId,
        "label" .= tioLabel,
        "steps" .= tioSteps
      ]

data TonnetzStructureOptionsResponse = TonnetzStructureOptionsResponse
  { tsoId :: !Text,
    tsoLabel :: !Text,
    tsoIntervals :: ![TonnetzIntervalOptionResponse]
  }
  deriving (Show, Generic)

instance ToJSON TonnetzStructureOptionsResponse where
  toJSON TonnetzStructureOptionsResponse {..} =
    object
      [ "id" .= tsoId,
        "label" .= tsoLabel,
        "intervals" .= tsoIntervals
      ]

data TonnetzOptionsResponse = TonnetzOptionsResponse
  { torStructures :: ![TonnetzStructureOptionsResponse]
  }
  deriving (Show, Generic)

instance ToJSON TonnetzOptionsResponse where
  toJSON TonnetzOptionsResponse {..} =
    object
      [ "structures" .= torStructures
      ]

data TonnetzTilingRequest = TonnetzTilingRequest
  { ttrStructure :: !Text,
    ttrInterval :: ![Int],
    ttrDegree :: !Text,
    ttrBaseMidi :: !(Maybe Int)
  }
  deriving (Show, Generic)

instance FromJSON TonnetzTilingRequest where
  parseJSON = withObject "TonnetzTilingRequest" $ \obj ->
    TonnetzTilingRequest
      <$> obj .: "structure"
      <*> obj .:? "interval" .!= []
      <*> obj .: "degree"
      <*> obj .:? "baseMidi"

data TonnetzChordLabel = TonnetzChordLabel
  { tclName :: !Text,
    tclAliases :: ![Text]
  }
  deriving (Show, Generic)

instance ToJSON TonnetzChordLabel where
  toJSON TonnetzChordLabel {..} =
    object
      [ "name" .= tclName,
        "aliases" .= tclAliases
      ]

data TonnetzVertexResponse = TonnetzVertexResponse
  { tvrCoordinate :: ![Int],
    tvrMidi :: !Int,
    tvrPitchClass :: !Int,
    tvrPitchClassName :: !Text,
    tvrNoteName :: !Text,
    tvrOctave :: !Int
  }
  deriving (Show, Generic)

instance ToJSON TonnetzVertexResponse where
  toJSON TonnetzVertexResponse {..} =
    object
      [ "coordinate" .= tvrCoordinate,
        "midi" .= tvrMidi,
        "pitchClass" .= tvrPitchClass,
        "pitchClassName" .= tvrPitchClassName,
        "noteName" .= tvrNoteName,
        "octave" .= tvrOctave
      ]

data TonnetzPolygonResponse = TonnetzPolygonResponse
  { tprFaceVertices :: ![[Double]],
    tprVertexCoordinates :: ![[Int]],
    tprPitchClasses :: ![Int],
    tprMidiNotes :: ![Int],
    tprChord :: !(Maybe TonnetzChordLabel)
  }
  deriving (Show, Generic)

instance ToJSON TonnetzPolygonResponse where
  toJSON TonnetzPolygonResponse {..} =
    object
      [ "faceVertices" .= tprFaceVertices,
        "vertexCoordinates" .= tprVertexCoordinates,
        "pitchClasses" .= tprPitchClasses,
        "midiNotes" .= tprMidiNotes,
        "chord" .= tprChord
      ]

data TonnetzTilingResponse = TonnetzTilingResponse
  { ttvStructure :: !Text,
    ttvInterval :: ![Int],
    ttvDegree :: !Text,
    ttvBaseMidi :: !Int,
    ttvVertices :: ![TonnetzVertexResponse],
    ttvPolygons :: ![TonnetzPolygonResponse]
  }
  deriving (Show, Generic)

instance ToJSON TonnetzTilingResponse where
  toJSON TonnetzTilingResponse {..} =
    object
      [ "structure" .= ttvStructure,
        "interval" .= ttvInterval,
        "degree" .= ttvDegree,
        "baseMidi" .= ttvBaseMidi,
        "vertices" .= ttvVertices,
        "polygons" .= ttvPolygons
      ]

defaultMaxVoiceCandidates :: Int
defaultMaxVoiceCandidates = 12

maxVoiceCandidatesCap :: Int
maxVoiceCandidatesCap = 50

clampVoiceCandidates :: Int -> Int
clampVoiceCandidates n = max 1 (min maxVoiceCandidatesCap n)

--------------------------------------------------------------------------------
-- Endpoint implementation

postAnalyze :: AnalyzeRequest -> Handler AnalyzeResponse
postAnalyze AnalyzeRequest {..} = do
  when (null requestProgression) $
    badRequest "Progression must contain at least one pitch-class set"
  progression <-
    either (badRequest . T.pack) pure $
      traverse parsePitchClassSet requestProgression
  let preset = maybe ModalDiatonic id requestPreset
  resultOrErr <-
    liftIO $
      try @SomeException $
        evaluate . force $
          analyzeAnnotated preset progression
  case resultOrErr of
    Left err ->
      throwJSONError err500 (T.pack (displayException err))
    Right annotated ->
      pure $ buildResponse preset annotated

postVoiceLeading :: VoiceLeadingRequest -> Handler VoiceLeadingResponse
postVoiceLeading VoiceLeadingRequest {..} = do
  when (null vlTuning) $
    badRequest "Tuning must contain at least one pitch-class"
  when (null vlProgression) $
    badRequest "Progression must contain at least one pitch-class set"
  tuningInfo <-
    either (badRequest . T.pack) pure $
      traverse tuningNoteSpecToChromatic vlTuning
  progressionMods <-
    either (badRequest . T.pack) pure $
      traverse parsePitchClassSet vlProgression
  let tuningChromatics = fmap fst tuningInfo
      tuningOctaves = fmap snd tuningInfo
  let fretboard =
        Fretboard
          { numStrings = length tuningChromatics,
            tuning = tuningChromatics
          }
      chordSets =
        fmap modsToChromatics progressionMods
      candidateCount =
        clampVoiceCandidates $
          fromMaybe defaultMaxVoiceCandidates vlMaxCandidates
      frettings = optimizeFrettings candidateCount fretboard chordSets
  when (null frettings) $
    badRequest "No valid frettings found for the supplied tuning and chords"
  pure $ buildVoiceLeadingResponse fretboard tuningOctaves frettings

postScaleNotes :: ScaleNotesRequest -> Handler ScaleNotesResponse
postScaleNotes ScaleNotesRequest {..} = do
  mode <-
    maybe (badRequest "Unrecognised mode") pure $
      modeFromText snrMode
  tonicChromatic <-
    either (badRequest . T.pack) pure $
      pitchClassSpecToChromatic snrTonic
  let tonicMod = toLocalInterpretation tonicChromatic :: Mod 12
      scaleClasses = modePitchClasses mode tonicMod Set.empty
      scaleNames = spellScaleDegrees tonicChromatic scaleClasses
      ionianClasses = modePitchClasses Ionian tonicMod Set.empty
      diffPairs =
        [ (tone, name)
          | ((tone, name), ionianTone) <- zip (zip scaleClasses scaleNames) ionianClasses,
            tone /= ionianTone
        ]
      diffClasses = fmap fst diffPairs
      diffNames = fmap snd diffPairs
      ints = fmap (fromIntegral . unMod) scaleClasses
      names = scaleNames
      diffInts = fmap (fromIntegral . unMod) diffClasses
  pure
    ScaleNotesResponse
      { snsPitchClasses = ints,
        snsPitchClassNames = names,
        snsDiffPitchClasses = diffInts,
        snsDiffPitchClassNames = diffNames
      }

postFretboardOccurrences :: FretboardOccurrencesRequest -> Handler FretboardOccurrencesResponse
postFretboardOccurrences FretboardOccurrencesRequest {..} = do
  when (null forTuning) $
    badRequest "Tuning must contain at least one string"
  when (null forNoteSets) $
    badRequest "noteSets must contain at least one entry"
  tuningInfo <-
    either (badRequest . T.pack) pure $
      traverse tuningNoteSpecToChromatic forTuning
  let (tuningChromatics, tuningOctaves) = unzip tuningInfo
      fretboard = Fretboard (length tuningChromatics) tuningChromatics
      maxFrets = clampFrets (fromMaybe defaultFretboardFrets forMaxFrets)
  occurrenceSets <-
    traverse (buildNoteSet fretboard tuningOctaves maxFrets) forNoteSets
  pure $
    FretboardOccurrencesResponse
      { forResponseNoteSets = occurrenceSets
      }
  where
    defaultFretboardFrets = 12
    clampFrets n
      | n < 0 = 0
      | otherwise = n

    buildNoteSet :: Fretboard -> [Maybe Int] -> Int -> FretboardNoteSetSpec -> Handler FretboardOccurrenceSetResponse
    buildNoteSet fretboard tuningOctaves maxFrets FretboardNoteSetSpec {..} = do
      pitchChromatics <-
        either (badRequest . T.pack) pure $
          traverse pitchClassSpecToChromatic fnsPitchClasses
      tonicChromatic <-
        case fnsTonic of
          Nothing -> pure Nothing
          Just tonicSpec ->
            fmap Just $
              either (badRequest . T.pack) pure $
                pitchClassSpecToChromatic tonicSpec
      let tonicMod = fmap (toLocalInterpretation :: Chromatic -> Mod 12) tonicChromatic
          spelledMapping =
            case (tonicChromatic, tonicMod, fnsMode) of
              (Just tonicChrom, Just tonicPc, Just mode) ->
                let degrees = modePitchClasses mode tonicPc Set.empty
                    names = spellScaleDegrees tonicChrom degrees
                 in Just $
                      Map.fromList $
                        zip
                          (fmap (fromIntegral . unMod) degrees)
                          names
              _ -> Nothing
      let spelledMap = fromMaybe Map.empty spelledMapping
          uniqueChromatics = Set.toList (Set.fromList pitchChromatics)
          chromaticSet = Set.fromList uniqueChromatics
          occurrences =
            occurrencesForPitchClasses fretboard maxFrets tuningOctaves chromaticSet
          sortedPitchClasses = sortOn unMod $ fmap toLocalInterpretation uniqueChromatics
          pitchClassInts = fmap (fromIntegral . unMod) sortedPitchClasses
          pitchClassNames =
            fmap
              ( \pc ->
                  let key = fromIntegral (unMod pc)
                   in Map.findWithDefault (pitchClassName pc) key spelledMap
              )
              sortedPitchClasses
          occurrenceResponses = fmap (occurrenceToResponse spelledMap) occurrences
      pure
        FretboardOccurrenceSetResponse
          { fosId = fnsId,
            fosLabel = fnsLabel,
            fosPitchClasses = pitchClassInts,
            fosPitchClassNames = pitchClassNames,
            fosOccurrences = occurrenceResponses
          }

    occurrenceToResponse ::
      Map Int Text ->
      FretboardNoteOccurrence ->
      FretboardOccurrenceResponse
    occurrenceToResponse spelledMap' FretboardNoteOccurrence {..} =
      let pitchClassNameText =
            let pcInt = fromIntegral (unMod fnoPitchClass)
                defaultName = pitchClassName (toLocalInterpretation fnoChromatic :: Mod 12)
             in Map.findWithDefault defaultName pcInt spelledMap'
          pitchClassInt = fromIntegral (unMod fnoPitchClass)
          noteLabel =
            maybe
              pitchClassNameText
              (\oct -> pitchClassNameText <> T.pack (show oct))
              fnoOctave
       in FretboardOccurrenceResponse
            { foString = fnoString,
              foFret = fnoFret,
              foPitchClass = pitchClassInt,
              foPitchClassName = pitchClassNameText,
              foOctave = fnoOctave,
              foNoteName = noteLabel
            }

postFretboardChordNames :: FretboardChordNamesRequest -> Handler FretboardChordNamesResponse
postFretboardChordNames FretboardChordNamesRequest {..} = do
  when (null fcnrNoteSets) $
    badRequest "noteSets must contain at least one entry"
  chordEntries <- traverse resolveName fcnrNoteSets
  pure
    FretboardChordNamesResponse
      { fcnrChordNames = chordEntries
      }
  where
    resolveName :: FretboardNoteSetSpec -> Handler FretboardChordNameEntry
    resolveName FretboardNoteSetSpec {..} = do
      pitchChromatics <-
        either (badRequest . T.pack) pure $
          traverse pitchClassSpecToChromatic fnsPitchClasses
      let uniqueChromatics = Set.toList (Set.fromList pitchChromatics)
          sortedPitchClasses =
            sortOn
              unMod
              (fmap (toLocalInterpretation :: Chromatic -> Mod 12) uniqueChromatics)
          pitchClassInts = fmap (fromIntegral . unMod) sortedPitchClasses
          nameInfo = chordNameFromPitchClasses pitchClassInts
          (nameText, aliasTexts) =
            case nameInfo of
              Just (ChordName primary aliases) ->
                (Just (T.pack primary), fmap T.pack aliases)
              Nothing -> (Nothing, [])
      pure
        FretboardChordNameEntry
          { fcnId = fnsId,
            fcnPitchClasses = pitchClassInts,
            fcnName = nameText,
            fcnAliases = aliasTexts
          }

parsePitchClassSet :: [PitchClassSpec] -> Either String (Set (Mod 12))
parsePitchClassSet specs = do
  mods <- traverse pitchClassSpecToMod specs
  pure (Set.fromList mods)

pitchClassSpecToMod :: PitchClassSpec -> Either String (Mod 12)
pitchClassSpecToMod = \case
  PitchClassByChromatic name ->
    case parseNoteWithOctave name of
      Right (chrom, _) -> Right (toLocalInterpretation chrom)
      Left _ ->
        case chromaticFromText name of
          Just chrom -> Right (toLocalInterpretation chrom)
          Nothing ->
            Left $
              "Unrecognised pitch-class name \""
                <> T.unpack name
                <> "\". Expected one of: "
                <> T.unpack (T.intercalate ", " (Map.keys chromaticNameMap))
  PitchClassByNumber n ->
    Right (fromIntegral (n `mod` 12))

pitchClassSpecToChromatic :: PitchClassSpec -> Either String Chromatic
pitchClassSpecToChromatic =
  fmap (toLocalInterpretation :: Mod 12 -> Chromatic) . pitchClassSpecToMod

modsToChromatics :: Set (Mod 12) -> Set Chromatic
modsToChromatics =
  Set.map (toLocalInterpretation :: Mod 12 -> Chromatic)

--------------------------------------------------------------------------------
-- Response building

buildResponse :: AnalysisPreset -> AnnotatedHarmonicPath -> AnalyzeResponse
buildResponse preset (AnnotatedHarmonicPath steps) =
  AnalyzeResponse
    { responsePreset = presetToText preset,
      responseSteps = zipWith stepToResponse [0 ..] steps
    }
  where
    stepToResponse :: Int -> HarmonicStep -> HarmonicStepResponse
    stepToResponse idx HarmonicStep {..} =
      let RMPoint {..} = stepPoint
          pitchList = sortOn unMod (Set.toList stepPitchClasses)
          pitchInts = fmap (fromIntegral . unMod) pitchList
          pitchNames = fmap pitchClassName pitchList
          (scaleRoot, _) =
            modeScaleWithRoot annotationMode annotationKeyCenter stepPitchClasses
          tonicChromatic = toLocalInterpretation scaleRoot :: Chromatic
          scalePitchClasses =
            modePitchClasses annotationMode scaleRoot Set.empty
          scalePitchNames = spellScaleDegrees tonicChromatic scalePitchClasses
          scalePitchInts = fmap (fromIntegral . unMod) scalePitchClasses
          ionianPitchClasses = modePitchClasses Ionian scaleRoot Set.empty
          deviationPairs =
            [ (tone, name)
              | ((tone, name), ionianTone) <-
                  zip (zip scalePitchClasses scalePitchNames) ionianPitchClasses,
                tone /= ionianTone
            ]
          scaleDeviationClasses = fmap fst deviationPairs
          scaleDeviationInts = fmap (fromIntegral . unMod) scaleDeviationClasses
          scaleDeviationNames = fmap snd deviationPairs
          FunctionalHarmonyAnnotation {..} = stepHarmony
     in HarmonicStepResponse
          { hsIndex = idx,
            hsTonality = fromIntegral (unMod scaleRoot),
            hsKey = keyDescription annotationMode scaleRoot,
            hsWeight = value,
            hsPitchClasses = pitchInts,
            hsPitchClassNames = pitchNames,
            hsScalePitchClasses = scalePitchInts,
            hsScalePitchClassNames = scalePitchNames,
            hsScaleDeviationPitchClasses = scaleDeviationInts,
            hsScaleDeviationPitchClassNames = scaleDeviationNames,
            hsMode = modeToText annotationMode,
            hsFunction = functionToText annotationFunction,
            hsDegree = degreeToText annotationDegree,
            hsRomanNumeral = fmap T.pack annotationRomanNumeral
          }

buildVoiceLeadingResponse :: Fretboard -> [Maybe Int] -> [Fretting] -> VoiceLeadingResponse
buildVoiceLeadingResponse fretboard baseOctaves frettings =
  VoiceLeadingResponse
    { vlTotalCost = totalDifficulty + totalTransitions,
      vlSteps = steps
    }
  where
    stepTriples =
      zip3
        [0 ..]
        frettings
        (Nothing : fmap Just frettings)
    steps = fmap buildStep stepTriples
    totalDifficulty = sum (fmap vlsDifficulty steps)
    totalTransitions = sum (fmap vlsTransitionCost steps)

    buildStep :: (Int, Fretting, Maybe Fretting) -> VoiceLeadingStepResponse
    buildStep (idx, current, prev) =
      VoiceLeadingStepResponse
        { vlsIndex = idx,
          vlsDifficulty = scoreDifficulty current,
          vlsTransitionCost =
            maybe
              0
              (\prevFretting -> maybe 0 fromIntegral (frettingDistance prevFretting current))
              prev,
          vlsPitchClasses = pitchClasses,
          vlsPitchClassNames = pitchNames,
          vlsPositions = stringPositions
        }
      where
        pitchMods =
          sortOn unMod
            . Set.toList
            . Set.map (toLocalInterpretation :: Chromatic -> Mod 12)
            $ chromaticsFromFretting current
        pitchClasses = fmap (fromIntegral . unMod) pitchMods
        pitchNames = fmap pitchClassName pitchMods
        stringPositions =
          fmap positionToResponse
            (Set.toAscList (frets current))

        positionToResponse :: (Int, Maybe (Finger, Int)) -> StringPositionResponse
        positionToResponse (stringIndex, mbFingerFret) =
          let openPitch = tuning fretboard !! stringIndex
              openPitchClass = toLocalInterpretation openPitch :: Mod 12
              baseOctave = lookupBaseOctave stringIndex
              (fingerTxt, fretNumber, pitch) =
                case mbFingerFret of
                  Nothing -> (Nothing, 0, openPitch)
                  Just (finger, fretVal) ->
                    ( Just (T.pack (show finger)),
                      fretVal,
                      transposeChromatic openPitch (transposition (fromIntegral fretVal))
                    )
              pitchMod = toLocalInterpretation pitch :: Mod 12
              pitchClassNumber = fromIntegral (unMod pitchMod)
              pitchLabel = pitchClassName pitchMod
              octaveValue = computeOctave baseOctave openPitchClass fretNumber
              noteLabel =
                maybe
                  pitchLabel
                  (\oct -> pitchLabel <> T.pack (show oct))
                  octaveValue
           in StringPositionResponse
                { sprString = stringIndex,
                  sprFret = fretNumber,
                  sprFinger = fingerTxt,
                  sprPitchClass = pitchLabel,
                  sprPitchClassNumber = pitchClassNumber,
                  sprOctave = octaveValue,
                  sprNoteName = noteLabel
                }

    lookupBaseOctave :: Int -> Maybe Int
    lookupBaseOctave idx =
      if idx < length baseOctaves then baseOctaves !! idx else Nothing

    computeOctave :: Maybe Int -> Mod 12 -> Int -> Maybe Int
    computeOctave Nothing _ _ = Nothing
    computeOctave (Just baseOct) openPitchClass fretNumber =
      let openPcInt = fromIntegral (unMod openPitchClass)
          baseMidi = (baseOct + 1) * 12 + openPcInt
          midiValue = baseMidi + fretNumber
       in Just (midiValue `div` 12 - 1)

--------------------------------------------------------------------------------
-- Helpers

badRequest :: Text -> Handler a
badRequest = throwJSONError err400

throwJSONError :: ServerError -> Text -> Handler a
throwJSONError baseErr msg =
  throwError
    baseErr
      { errBody = encodeUtf8Json msg,
        errHeaders = ("Content-Type", "application/json") : errHeaders baseErr
      }

encodeUtf8Json :: Text -> BL.ByteString
encodeUtf8Json txt =
  Aeson.encode (object ["error" .= txt])

presetToText :: AnalysisPreset -> Text
presetToText = \case
  MajorMinorTSD -> "MajorMinorTSD"
  MajorMinorDiatonic -> "MajorMinorDiatonic"
  ModalTSD -> "ModalTSD"
  ModalDiatonic -> "ModalDiatonic"

presetFromText :: Text -> Maybe AnalysisPreset
presetFromText txt =
  Map.lookup (normalizeName txt) presetNameMap

presetNames :: [Text]
presetNames = fmap presetToText [minBound .. maxBound]

presetNameMap :: Map Text AnalysisPreset
presetNameMap =
  Map.fromList
    [ (normalizeName "MajorMinorTSD", MajorMinorTSD),
      (normalizeName "MajorMinorDiatonic", MajorMinorDiatonic),
      (normalizeName "ModalTSD", ModalTSD),
      (normalizeName "ModalDiatonic", ModalDiatonic)
    ]

chromaticFromText :: Text -> Maybe Chromatic
chromaticFromText txt =
  Map.lookup (normalizeName txt) chromaticNameMap

chromaticNameMap :: Map Text Chromatic
chromaticNameMap =
  Map.fromList
    [ ("C", C),
      ("B#", C),
      ("C#", Cs),
      ("CS", Cs),
      ("DB", Cs),
      ("D", D),
      ("D#", Eb),
      ("DS", Eb),
      ("EB", Eb),
      ("E", E),
      ("FB", E),
      ("E#", F),
      ("F", F),
      ("F#", Fs),
      ("FS", Fs),
      ("GB", Fs),
      ("G", G),
      ("G#", Gs),
      ("GS", Gs),
      ("AB", Gs),
      ("A", A),
      ("A#", Bb),
      ("AS", Bb),
      ("BB", Bb),
      ("B", B),
      ("CB", B)
    ]

parseNoteWithOctave :: Text -> Either String (Chromatic, Maybe Int)
parseNoteWithOctave rawTxt =
  let trimmed = T.strip rawTxt
      (namePart, octavePart) = T.span (\c -> not (isDigit c) && c /= '-') trimmed
      noteName = T.strip namePart
      octaveText = T.strip octavePart
   in case chromaticFromText noteName of
        Nothing ->
          Left $
            "Unrecognised pitch-class name \""
              <> T.unpack noteName
              <> "\""
        Just chrom ->
          if T.null octaveText
            then Right (chrom, Nothing)
            else case readMaybe (T.unpack octaveText) of
              Just octaveVal -> Right (chrom, Just octaveVal)
              Nothing ->
                Left $
                  "Invalid octave value in \""
                    <> T.unpack rawTxt
                    <> "\""

tuningNoteSpecToChromatic :: TuningNoteSpec -> Either String (Chromatic, Maybe Int)
tuningNoteSpecToChromatic = \case
  TuningNoteSpecText nameMbOctave maybeOctave -> do
    (chrom, detectedOctave) <- parseNoteWithOctave nameMbOctave
    pure (chrom, maybeOctave <|> detectedOctave)
  TuningNoteSpecNumber val maybeOctave ->
    let idx = (val `mod` 12 + 12) `mod` 12
        chrom = toEnum idx :: Chromatic
     in pure (chrom, maybeOctave)
pitchClassName :: Mod 12 -> Text
pitchClassName n =
  T.pack . show $ (toLocalInterpretation n :: Chromatic)

modeToText :: Mode -> Text
modeToText = \case
  Ionian -> "Ionian"
  Dorian -> "Dorian"
  Phrygian -> "Phrygian"
  Lydian -> "Lydian"
  Mixolydian -> "Mixolydian"
  Aeolian -> "Aeolian"
  Locrian -> "Locrian"

functionToText :: Function -> Text
functionToText = \case
  Tonic -> "Tonic"
  Supertonic -> "Supertonic"
  Mediant -> "Mediant"
  Subdominant -> "Subdominant"
  Dominant -> "Dominant"
  Submediant -> "Submediant"
  LeadingTone -> "LeadingTone"

degreeToText :: Degree -> Text
degreeToText = T.pack . show

keyDescription :: Mode -> Mod 12 -> Text
keyDescription mode tonic =
  let tonicName = pitchClassName tonic
   in case mode of
        Ionian -> tonicName <> " major"
        Aeolian -> tonicName <> " minor"
        _ -> tonicName <> " " <> modeToText mode

modeFromText :: Text -> Maybe Mode
modeFromText txt = Map.lookup (normalizeName txt) modeNameMap

modeScale :: Mode -> HeptatonicScale (Mod 12)
modeScale = \case
  Ionian -> cIonian
  Dorian -> cDorian
  Phrygian -> cPhrygian
  Lydian -> cLydian
  Mixolydian -> cMixolydian
  Aeolian -> cAeolian
  Locrian -> cLocrian

modePitchClasses :: Mode -> Mod 12 -> Set (Mod 12) -> [Mod 12]
modePitchClasses mode defaultTonic pitchHints =
  snd (modeScaleWithRoot mode defaultTonic pitchHints)

modeScaleWithRoot :: Mode -> Mod 12 -> Set (Mod 12) -> (Mod 12, [Mod 12])
modeScaleWithRoot mode tonic _ =
  let rooted = transposeScale (transposition shift) (modeScale mode)
   in (tonic, toList rooted)
  where
    shift = tonic - baseRoot
    HeptatonicScale baseRoot _ _ _ _ _ _ = modeScale mode

data NoteLetter
  = LetterC
  | LetterD
  | LetterE
  | LetterF
  | LetterG
  | LetterA
  | LetterB
  deriving (Eq, Ord, Show)

nextLetter :: NoteLetter -> NoteLetter
nextLetter = \case
  LetterC -> LetterD
  LetterD -> LetterE
  LetterE -> LetterF
  LetterF -> LetterG
  LetterG -> LetterA
  LetterA -> LetterB
  LetterB -> LetterC

chromaticToLetter :: Chromatic -> NoteLetter
chromaticToLetter = \case
  C -> LetterC
  Cs -> LetterC
  D -> LetterD
  Eb -> LetterE
  E -> LetterE
  F -> LetterF
  Fs -> LetterF
  G -> LetterG
  Gs -> LetterG
  A -> LetterA
  Bb -> LetterB
  B -> LetterB

letterText :: NoteLetter -> Text
letterText = \case
  LetterC -> "C"
  LetterD -> "D"
  LetterE -> "E"
  LetterF -> "F"
  LetterG -> "G"
  LetterA -> "A"
  LetterB -> "B"

letterBasePitch :: NoteLetter -> Int
letterBasePitch = \case
  LetterC -> 0
  LetterD -> 2
  LetterE -> 4
  LetterF -> 5
  LetterG -> 7
  LetterA -> 9
  LetterB -> 11

accidentalSuffix :: Int -> Text
accidentalSuffix diff
  | diff == 0 = ""
  | diff <= 6 = T.replicate diff (T.singleton 's')
  | otherwise = T.replicate (12 - diff) (T.singleton 'b')

spellScaleDegrees :: Chromatic -> [Mod 12] -> [Text]
spellScaleDegrees tonic degrees =
  zipWith spell letters degrees
  where
    letters = take (length degrees) (iterate nextLetter (chromaticToLetter tonic))
    spell letter tone =
      let target = fromIntegral (unMod tone)
          base = letterBasePitch letter
          diff = (target - base) `mod` 12
       in letterText letter <> accidentalSuffix diff

modeNameMap :: Map Text Mode
modeNameMap =
  Map.fromList
    [ (normalizeName "Ionian", Ionian),
      (normalizeName "Major", Ionian),
      (normalizeName "Dorian", Dorian),
      (normalizeName "Phrygian", Phrygian),
      (normalizeName "Lydian", Lydian),
      (normalizeName "Mixolydian", Mixolydian),
      (normalizeName "Aeolian", Aeolian),
      (normalizeName "Minor", Aeolian),
      (normalizeName "Locrian", Locrian)
    ]

normalizeName :: Text -> Text
normalizeName = T.toUpper . T.filter (not . isSpace)

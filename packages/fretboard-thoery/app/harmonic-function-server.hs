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
import Control.DeepSeq (force)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Number, String),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Mod (Mod, unMod)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Finger (Finger)
import GHC.Generics (Generic)
import Fretboard
  ( Fretboard (..),
    Fretting (..),
    chromaticsFromFretting,
    frettingDistance,
    optimizeFrettings,
    scoreDifficulty,
  )
import Modulation
  ( Chromatic (..),
    Degree,
    LocalInterpretation (toLocalInterpretation),
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
import Data.HarmonicAnalysis.Types
  ( AnnotatedHarmonicPath (..),
    Col (..),
    Function (..),
    FunctionalHarmonyAnnotation (..),
    HarmonicStep (..),
    Mode (..),
    RMPoint (..),
  )

--------------------------------------------------------------------------------
-- Main / server wiring

main :: IO ()
main = do
  port <- maybe 8080 parsePort <$> lookupEnv "PORT"
  putStrLn $ "Starting harmonic-function server on port " <> show port
  runSettings (setPort port defaultSettings) $
    corsMiddleware (serve harmonicAPI server)
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

type HarmonicAPI =
  "analyze"
    :> ReqBody '[JSON] AnalyzeRequest
    :> Post '[JSON] AnalyzeResponse
    :<|> "voice-leading"
      :> ReqBody '[JSON] VoiceLeadingRequest
      :> Post '[JSON] VoiceLeadingResponse

harmonicAPI :: Proxy HarmonicAPI
harmonicAPI = Proxy

server :: Server HarmonicAPI
server = postAnalyze :<|> postVoiceLeading

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
        "mode" .= hsMode,
        "function" .= hsFunction,
        "degree" .= hsDegree,
        "romanNumeral" .= hsRomanNumeral
      ]

data VoiceLeadingRequest = VoiceLeadingRequest
  { vlTuning :: ![PitchClassSpec],
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
    sprPitchClassNumber :: !Int
  }
  deriving (Show, Generic)

instance ToJSON StringPositionResponse where
  toJSON StringPositionResponse {..} =
    object
      [ "string" .= sprString,
        "fret" .= sprFret,
        "finger" .= sprFinger,
        "pitchClass" .= sprPitchClass,
        "pitchClassNumber" .= sprPitchClassNumber
      ]

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
  tuningChromatics <-
    either (badRequest . T.pack) pure $
      traverse pitchClassSpecToChromatic vlTuning
  progressionMods <-
    either (badRequest . T.pack) pure $
      traverse parsePitchClassSet vlProgression
  let fretboard =
        Fretboard
          { numStrings = length tuningChromatics,
            tuning = tuningChromatics
          }
      chordSets =
        fmap modsToChromatics progressionMods
      candidateCount =
        clampCandidates $
          fromMaybe defaultMaxCandidates vlMaxCandidates
      frettings = optimizeFrettings candidateCount fretboard chordSets
  when (null frettings) $
    badRequest "No valid frettings found for the supplied tuning and chords"
  pure $ buildVoiceLeadingResponse fretboard frettings
  where
    defaultMaxCandidates = 12
    maxCandidatesCap = 50
    clampCandidates n = max 1 (min maxCandidatesCap n)

parsePitchClassSet :: [PitchClassSpec] -> Either String (Set (Mod 12))
parsePitchClassSet specs = do
  mods <- traverse pitchClassSpecToMod specs
  pure (Set.fromList mods)

pitchClassSpecToMod :: PitchClassSpec -> Either String (Mod 12)
pitchClassSpecToMod = \case
  PitchClassByChromatic name ->
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
          FunctionalHarmonyAnnotation {..} = stepHarmony
       in HarmonicStepResponse
            { hsIndex = idx,
              hsTonality = fromIntegral (unMod (unCol col)),
              hsKey = keyDescription annotationMode annotationKeyCenter,
              hsWeight = value,
              hsPitchClasses = pitchInts,
              hsPitchClassNames = pitchNames,
              hsMode = modeToText annotationMode,
              hsFunction = functionToText annotationFunction,
              hsDegree = degreeToText annotationDegree,
              hsRomanNumeral = fmap T.pack annotationRomanNumeral
            }

buildVoiceLeadingResponse :: Fretboard -> [Fretting] -> VoiceLeadingResponse
buildVoiceLeadingResponse fretboard frettings =
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
              (fingerTxt, fretNumber, pitch) =
                case mbFingerFret of
                  Nothing -> (Nothing, 0, openPitch)
                  Just (finger, fret) ->
                    ( Just (T.pack (show finger)),
                      fret,
                      transposeChromatic openPitch (transposition (fromIntegral fret))
                    )
              pitchMod = toLocalInterpretation pitch :: Mod 12
           in StringPositionResponse
                { sprString = stringIndex,
                  sprFret = fretNumber,
                  sprFinger = fingerTxt,
                  sprPitchClass = pitchClassName pitchMod,
                  sprPitchClassNumber = fromIntegral (unMod pitchMod)
                }

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

normalizeName :: Text -> Text
normalizeName = T.toUpper . T.filter (not . isSpace)

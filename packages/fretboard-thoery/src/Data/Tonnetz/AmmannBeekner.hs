{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Tonnetz.AmmannBeekner
  ( VertexNote (..),
    PolygonSummary (..),
    TonnetzComputation (..),
    computeTiling,
  )
where

import Chord.Names (ChordName (..), chordNameFromPitchClasses)
import Control.Lens ((^.))
import Data.Functor.Rix.Rep (rix)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tonnetz.Geometry
import Modulation (Chromatic, Degree, HeptatonicScale, LocalInterpretation (toLocalInterpretation), cModes)

data VertexNote = VertexNote
  { vnCoordinate :: !TonnetzCoordinate,
    vnMidi :: !Int,
    vnPitchClass :: !Int,
    vnPitchClassName :: !Text,
    vnNoteName :: !Text,
    vnOctave :: !Int
  }
  deriving (Eq, Show)

data PolygonSummary = PolygonSummary
  { psFacePoints :: ![TonnetzPoint],
    psVertexCoords :: ![TonnetzCoordinate],
    psPitchClasses :: ![Int],
    psMidiNotes :: ![Int],
    psChordPrimary :: !(Maybe Text),
    psChordAliases :: ![Text]
  }
  deriving (Eq, Show)

data TonnetzComputation = TonnetzComputation
  { tcVertexNotes :: !(Map TonnetzCoordinate VertexNote),
    tcPolygons :: ![PolygonSummary]
  }
  deriving (Eq, Show)

computeTiling ::
  Degree ->
  (Mod 7, Mod 7, Mod 7, Mod 7) ->
  Int ->
  [TonnetzFace] ->
  TonnetzComputation
computeTiling degree intervals baseMidi faces =
  TonnetzComputation vertexMap polygonSummaries
  where
    vertexMap =
      Map.fromList
        [ (coord, buildVertex coord)
          | coord <- Set.toList allCoords
        ]
    polygonSummaries = fmap buildPolygon faces
    buildPolygon TonnetzFace {..} =
      let notes = fmap lookupNote tfVertexCoords
          pitchClasses = fmap vnPitchClass notes
          midiNotes = fmap vnMidi notes
          chordInformation = chordNameFromPitchClasses pitchClasses
          chordPrimaryText = fmap (T.pack . chordPrimary) chordInformation
          chordAliasTexts = maybe [] (fmap T.pack . chordAliases) chordInformation
       in PolygonSummary
            { psFacePoints = tfFacePoints,
              psVertexCoords = tfVertexCoords,
              psPitchClasses = pitchClasses,
              psMidiNotes = midiNotes,
              psChordPrimary = chordPrimaryText,
              psChordAliases = chordAliasTexts
            }
    lookupNote coord =
      maybe (buildVertex coord) id (Map.lookup coord vertexMap)
    buildVertex coord =
      let offsets = coordinateVector coord
          semitoneDelta = sum (zipWith (*) axisSteps offsets)
          midi = baseMidi + semitoneDelta
          pitchClass = normalisePitchClass midi
          pitchName = pitchClassName pitchClass
          octave = midi `div` 12 - 1
          noteLabel = pitchName <> T.pack (show octave)
       in VertexNote
            { vnCoordinate = coord,
              vnMidi = midi,
              vnPitchClass = pitchClass,
              vnPitchClassName = pitchName,
              vnNoteName = noteLabel,
              vnOctave = octave
            }
    axisSteps =
      fmap (semitoneSteps degree) (tupleToList intervals)
    allCoords =
      Set.fromList (concatMap tfVertexCoords faces)

tupleToList :: (a, a, a, a) -> [a]
tupleToList (a, b, c, d) = [a, b, c, d]

coordinateVector :: TonnetzCoordinate -> [Int]
coordinateVector TonnetzCoordinate {..} = [tcA, tcB, tcC, tcD]

normalisePitchClass :: Int -> Int
normalisePitchClass n =
  let r = n `mod` 12
   in if r < 0 then r + 12 else r

pitchClassName :: Int -> Text
pitchClassName n =
  let chromatic = toLocalInterpretation (fromIntegral n :: Mod 12) :: Chromatic
   in T.pack (show chromatic)

semitoneSteps :: Degree -> Mod 7 -> Int
semitoneSteps degree interval =
  fromIntegral . unMod $
    modalDegree degree ^. rix (fromIntegral $ unMod interval)

modalDegree :: Degree -> HeptatonicScale (Mod 12)
modalDegree degree =
  case drop (fromEnum degree) cModes of
    (mode : _) -> mode
    [] -> error "modalDegree: unexpected degree index"

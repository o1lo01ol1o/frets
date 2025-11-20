{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Tonnetz.Generalized
  ( IntervalInterpretation (..),
    computeGeneralizedTiling,
  )
where

import Chord.Names (ChordName (..), chordNameFromPitchClasses)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Mod (Mod, unMod)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tonnetz.AmmannBeekner
  ( PolygonSummary (..),
    TonnetzComputation (..),
    VertexNote (..),
  )
import Data.Tonnetz.Geometry
  ( TonnetzCoordinate (..),
    TonnetzPoint (..),
  )
import Modulation
  ( Chromatic,
    Degree,
    LocalInterpretation (toLocalInterpretation),
    cModes,
  )

data IntervalInterpretation
  = IntervalInterpretationChromatic
  | IntervalInterpretationModal
  deriving (Eq, Show)

computeGeneralizedTiling ::
  IntervalInterpretation ->
  Degree ->
  (Int, Int, Int) ->
  Int ->
  TonnetzComputation
computeGeneralizedTiling interpretation degree (stepX, _stepDiag, stepY) baseMidi =
  TonnetzComputation vertexMap polygonSummaries
  where
    AxisDeltas {axisDeltaX, axisDeltaY} =
      resolveAxis interpretation degree stepX stepY
    faces = generalizedFaces gridRadius
    vertexCoordsSet = collectVertices faces
    vertexMap =
      Map.fromList
        [ (toCoordinate coord, buildVertex coord)
          | coord <- Set.toList vertexCoordsSet
        ]
    polygonSummaries = fmap buildPolygon faces

    buildPolygon coords =
      let notes = fmap lookupNote coords
          pitchClasses = fmap vnPitchClass notes
          midiNotes = fmap vnMidi notes
          chordInfo = chordNameFromPitchClasses pitchClasses
          chordPrimaryText = fmap (T.pack . chordPrimary) chordInfo
          chordAliasTexts = maybe [] (fmap T.pack . chordAliases) chordInfo
       in PolygonSummary
            { psFacePoints = fmap toPoint coords,
              psVertexCoords = fmap toCoordinate coords,
              psPitchClasses = pitchClasses,
              psMidiNotes = midiNotes,
              psChordPrimary = chordPrimaryText,
              psChordAliases = chordAliasTexts
            }

    lookupNote coord =
      Map.findWithDefault (buildVertex coord) (toCoordinate coord) vertexMap

    buildVertex coord@(x, y) =
      let midi = baseMidi + x * axisDeltaX + y * axisDeltaY
          pitchClass = normalisePitchClass midi
          pitchName = pitchClassName pitchClass
          octave = midi `div` 12 - 1
          noteLabel = pitchName <> T.pack (show octave)
       in VertexNote
            { vnCoordinate = toCoordinate coord,
              vnMidi = midi,
              vnPitchClass = pitchClass,
              vnPitchClassName = pitchName,
              vnNoteName = noteLabel,
              vnOctave = octave
            }

gridRadius :: Int
gridRadius = 7

type LatticeCoord = (Int, Int)

generalizedFaces :: Int -> [[LatticeCoord]]
generalizedFaces radius =
  lower
    ++ upper
  where
    range = [-radius .. radius - 1]
    rangeY = [-radius .. radius - 1]
    lower =
      [ [(x, y), (x + 1, y), (x, y + 1)]
        | x <- range,
          y <- rangeY
      ]
    upper =
      [ [(x + 1, y), (x + 1, y + 1), (x, y + 1)]
        | x <- range,
          y <- rangeY
      ]

collectVertices :: [[LatticeCoord]] -> Set LatticeCoord
collectVertices = Set.fromList . concat

data AxisDeltas = AxisDeltas
  { axisDeltaX :: !Int,
    axisDeltaY :: !Int
  }

resolveAxis ::
  IntervalInterpretation ->
  Degree ->
  Int ->
  Int ->
  AxisDeltas
resolveAxis interpretation degree xSteps ySteps =
  case interpretation of
    IntervalInterpretationChromatic ->
      AxisDeltas
        { axisDeltaX = negate xSteps,
          axisDeltaY = ySteps - 12
        }
    IntervalInterpretationModal ->
      let horizontal = diatonicStepsToSemitones degree xSteps
          vertical = diatonicStepsToSemitones degree ySteps
       in AxisDeltas
            { axisDeltaX = negate horizontal,
              axisDeltaY = vertical - 12
            }

diatonicStepsToSemitones :: Degree -> Int -> Int
diatonicStepsToSemitones degree steps =
  baseOffset + octaveShifts * 12
  where
    offsets = degreeOffsets degree
    (quotient, remainder) = steps `divMod` 7
    octaveShifts = quotient
    baseOffset = offsets !! remainder

degreeOffsets :: Degree -> [Int]
degreeOffsets degree =
  fmap (fromIntegral . unMod) . toList $ mode
  where
    mode =
      case drop (fromEnum degree) cModes of
        (m : _) -> m
        [] -> head cModes

toCoordinate :: LatticeCoord -> TonnetzCoordinate
toCoordinate (x, y) =
  TonnetzCoordinate
    { tcA = x,
      tcB = y,
      tcC = -(x + y),
      tcD = 0
    }

toPoint :: LatticeCoord -> TonnetzPoint
toPoint (x, y) =
  TonnetzPoint
    { tpX = fromIntegral x + 0.5 * fromIntegral y,
      tpY = fromIntegral y * verticalSpacing
    }

verticalSpacing :: Double
verticalSpacing = sqrt 3 / 2

normalisePitchClass :: Int -> Int
normalisePitchClass n =
  let r = n `mod` 12
   in if r < 0 then r + 12 else r

pitchClassName :: Int -> Text
pitchClassName n =
  let chromatic = toLocalInterpretation (fromIntegral n :: Mod 12) :: Chromatic
   in T.pack (show chromatic)

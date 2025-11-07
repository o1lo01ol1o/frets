{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Fretboard
  ( -- * API
    Fretboard (..),
    Fretting (..),
    frettingDistance,
    findFrettings,
    scoreDifficulty,
    drawFretting,
    knownTuning,
    KnownGuitarTunings (..),
    optimizeFrettings,
    chromaticsFromFretting,
    occurrencesForPitchClasses,
    FretboardNoteOccurrence (..),
    drawFrettingWithPossibleChords,
    VoiceScale,
    drawScale,
    fretScale,

    -- * Tests
    cMajor7Chord,
    cMajorChord,
    cMajor7Add9Chord,
    cMajor7Add9Sharp11Chord,
    cMajor7Add9Sharp11Sharp13Chord,
    cMajor7Frettings,
    cMajor7Frettings',
    cProgression,
    isValidFretting,
    frettingDiffersOnlyInFingering,
  )
where

import Chord (chromaticsToChords, showChordName, transposeChromatic)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Biapplicative (Bifunctor (..))
import qualified Data.Bifunctor
import Data.Foldable (Foldable (..), minimumBy)
import Data.Function (on)
import Data.Functor.Rep (Representable (Rep), index)
import Data.List (elemIndex, groupBy, nub, permutations, sort, sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Mod (Mod, unMod)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Finger (Finger (..))
import Finger.TH ()
import GHC.Generics (Generic)
import GHC.OldList (sortOn)
import GHC.Word (Word8)
import Modulation
  ( Chromatic (A, B, Bb, C, Cs, D, E, Eb, F, Fs, G, Gs),
    IsScale,
    LocalInterpretation (toLocalInterpretation),
    VoiceScale (drawScale),
    cDorian,
    cIonian,
    cMixolydian,
    transpose,
    transposeFunctor,
    transposition,
  )

-- | Represents a fretboard with a given number of strings and tuning
data Fretboard = Fretboard
  { numStrings :: Int,
    tuning :: [Chromatic]
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

-- | Represents a fretting of a set of chromatics on a fretboard
data Fretting = Fretting
  { -- | The fretboard on which the fretting is performed
    fretboard :: Fretboard,
    -- | An associated list of fret positions for each string
    -- Each tuple represents (string, Maybe (fret, finger)
    -- where 'string' is the index of the string (0-based)
    -- 'fret' is the fret number (0 represents an open string)
    -- and 'finger' is the finger used to fret the note. An expected
    -- invariant is that fret > 0
    frets :: Set (Int, Maybe (Finger, Int))
  }
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

lowestFrettedNote :: Fretting -> Maybe Chromatic
lowestFrettedNote (Fretting fretboard frets)
  | Set.null frets = Nothing
  | otherwise =
      let frettedStrings =
            [ (s, fromIntegral f)
              | (s, Just (_, f)) <- Set.toList frets
            ]
          chromatics =
            map
              ( \(s, f) ->
                  transposeChromatic (tuning fretboard !! s) (transposition f)
              )
              frettedStrings
       in Just $ minimum chromatics

data NotePosition = NotePosition
  { npString :: !Int,
    npFinger :: !(Maybe Finger),
    npFret :: !Int
  }
  deriving (Eq, Show)

data PathCost = PathCost
  { pcTransitions :: !Int,
    pcBase :: !Int
  }
  deriving (Eq, Show, Ord)

data PathEntry = PathEntry
  { peCost :: !(Maybe PathCost),
    pePrev :: !(Maybe Int),
    peFretting :: !Fretting
  }

data FretboardNoteOccurrence = FretboardNoteOccurrence
  { fnoString :: !Int,
    fnoFret :: !Int,
    fnoChromatic :: !Chromatic,
    fnoPitchClass :: !(Mod 12),
    fnoOctave :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

notePosition :: (Int, Maybe (Finger, Int)) -> NotePosition
notePosition (s, mbFingerFret) =
  case mbFingerFret of
    Nothing -> NotePosition s Nothing 0
    Just (finger, fret) -> NotePosition s (Just finger) fret

noteDistance :: NotePosition -> NotePosition -> Int
noteDistance (NotePosition s1 f1 fret1) (NotePosition s2 f2 fret2) =
  abs (s1 - s2)
    + abs (fret1 - fret2)
    + fingerPenalty
    + openPenalty
  where
    fingerPenalty =
      case (f1, f2) of
        (Just finger1, Just finger2) ->
          abs (fromEnum finger1 - fromEnum finger2)
        _ -> 0
    openPenalty =
      case (f1, f2) of
        (Nothing, Just _) -> 1
        (Just _, Nothing) -> 1
        _ -> 0

frettingDistance :: Fretting -> Fretting -> Maybe Word8
frettingDistance (Fretting fb1 frets1) (Fretting fb2 frets2)
  | fb1 /= fb2 = Nothing
  | otherwise =
      let positions1 = fmap notePosition (Set.toList frets1)
          positions2 = fmap notePosition (Set.toList frets2)
          len1 = length positions1
          len2 = length positions2
          matchCost shorter longer =
            [ sum (zipWith noteDistance shorter perm)
              | combo <- combinations (length shorter) longer,
                perm <- permutations combo
            ]
          costOptions
            | len1 == 0 || len2 == 0 = [0]
            | len1 <= len2 = matchCost positions1 positions2
            | otherwise = matchCost positions2 positions1
       in case costOptions of
            [] -> Nothing
            opts ->
              let totalCost = minimum opts
               in if totalCost > fromIntegral (maxBound :: Word8)
            then Nothing
            else Just (fromIntegral totalCost)

-- | Checks if a fretting is valid
isValidFretting :: Fretting -> Bool
isValidFretting (Fretting fretboard frets) =
  let openStrings = [s | (s, Nothing) <- Set.toList frets]
      frettedStrings = [(s, f) | (s, Just (f, _)) <- Set.toList frets]
      minNonZeroFret =
        safeMinimum
          (filter (/= 0) (mapMaybe (fmap snd . snd) $ Set.toList frets))
      fretSpan =
        let maxFret = safeMaximum (mapMaybe (fmap snd . snd) $ Set.toList frets)
            minFret = safeMinimum (mapMaybe (fmap snd . snd) $ Set.toList frets)
         in maxFret - minFret
      validThumb =
        let hasThumb = any (\(_, mbf) -> fmap fst mbf == Just Thumb) frets
            thumbOnTopString =
              all
                ( \(s, mbf) ->
                    fmap fst mbf /= Just Thumb || s == 0
                )
                frets
            thumbNotOnOpenString =
              all
                ( \(s, mbf) ->
                    fmap fst mbf /= Just Thumb || s `notElem` openStrings
                )
                frets
         in not hasThumb || hasThumb && thumbOnTopString && thumbNotOnOpenString
      noFretsOnOpenString =
        all
          ( \s ->
              s
                `notElem` map fst frettedStrings
          )
          openStrings
      frettedStrings' = [(s, f, finger) | (s, Just (finger, f)) <- Set.toList frets]
      isMonotonicFingering =
        let frettedStrings =
              [ (s, f, finger)
                | (s, Just (finger, f)) <- Set.toList frets
              ]
            sortedFrettedStrings =
              sortBy (\(_, f1, _) (_, f2, _) -> compare f1 f2) frettedStrings
            fingers = map (\(_, _, finger) -> finger) sortedFrettedStrings
         in fingers == sort fingers
      hasOneNotePerstring =
        length (nub $ map (\(s, _, _) -> s) frettedStrings')
          == length frettedStrings'
      hasNoMoreThanOneFretPositionPerFinger =
        -- group all the fret positions by finger
        -- if a finger (except Index) has more than one fret position, then the fretting is invalid
        let thd (_, _, a) = a
            fretPositionsByFinger =
              groupBy (\(_, _, f1) (_, _, f2) -> f1 == f2) $
                sortBy (\(_, _, f1) (_, _, f2) -> compare f1 f2) frettedStrings'
         in all (\fretPositions -> thd (head fretPositions) == Index || length fretPositions <= 1) fretPositionsByFinger
      validFretSpan
        | minNonZeroFret > 5 = fretSpan <= 6
        | otherwise = fretSpan <= 6
   in validFretSpan
        && hasOneNotePerstring
        && validThumb
        && noFretsOnOpenString
        && hasNoMoreThanOneFretPositionPerFinger
        && isMonotonicFingering

safeMaximum :: (Num a, Ord a) => [a] -> a
safeMaximum [] = 0
safeMaximum x = maximum x

safeMinimum :: (Num a, Ord a) => [a] -> a
safeMinimum [] = 0
safeMinimum x = minimum x

-- Represents a vertex in the graph
type Vertex = (Int, Maybe (Finger, Int))

-- | Finds the top-k valid frettings for a given set of chromatics on a fretboard
findFrettings :: Int -> Fretboard -> Set Chromatic -> Set (Word8, Fretting)
findFrettings k fretboard chromatics =
  let graph = constructGraph allVertices
      theseChromatics = filter (\v -> chromatic v `Set.member` chromatics) allVertices
      initialMap = Map.empty
      dfs ::
        Vertex ->
        Fretting ->
        Word8 ->
        Map.Map (Set (Int, Maybe Int)) (Word8, Fretting) ->
        Map.Map (Set (Int, Maybe Int)) (Word8, Fretting)
      dfs vertex fretting dist frettingMap
        | Set.size (frets fretting) == Set.size chromatics =
            let key = Set.map (second (fmap snd)) (frets fretting)
                value = (dist, fretting)
             in case Map.lookup key frettingMap of
                  Nothing -> Map.insert key value frettingMap
                  Just (existingDist, _)
                    | dist < existingDist -> Map.insert key value frettingMap
                    | otherwise -> frettingMap
        | otherwise =
            let validNeighbors =
                  filter
                    ( \v ->
                        notAlreadyFretted v
                          && thisIsValidFretting v
                          && notSameVertex v
                    )
                    theseChromatics
                newDist v = dist + graph V.! vertexIndex vertex V.! vertexIndex v
                frettingMap' = foldl' (\acc v -> dfs v (addVertexToFretting v) (newDist v) acc) frettingMap validNeighbors
                notSameVertex v = vertexIndex v /= vertexIndex vertex
                thisIsValidFretting v = isValidFretting (addToFretting fretting v)
                notAlreadyFretted v = chromatic v `Set.notMember` frettedChromatics fretting
                addVertexToFretting = addToFretting fretting
             in frettingMap'

      finalMap =
        foldl'
          ( \acc v ->
              dfs
                v
                (Fretting fretboard (Set.singleton v))
                0
                acc
          )
          initialMap
          theseChromatics
   in Set.fromList $ take k $ sortOn fst $ Map.elems finalMap
  where
    -- all possible frettings of the given chromatics with all possible fingers
    allVertices =
      [ (s, Just (f, fromIntegral $ unMod i))
        | (s, i, c) <- allTranspositions,
          f <- [minBound .. maxBound]
      ]
        ++ [ (s, Nothing)
             | s <- [0 .. numStrings fretboard - 1],
               tuning fretboard !! s `elem` chromatics
           ]

    allTranspositions =
      [ (s, i, c)
        | s <- [0 .. numStrings fretboard - 1],
          i <- [minBound .. maxBound],
          let c = transposeChromatic (tuning fretboard !! s) (transposition i),
          Set.member c chromatics
      ]
    -- \| Constructs the graph with weighted edges based on fret distances and finger positions
    constructGraph :: [Vertex] -> V.Vector (V.Vector Word8)
    constructGraph vertices = V.generate n $ \i ->
      V.generate n $ \j ->
        let (s1, mbf1) = vertices !! i
            (s2, mbf2) = vertices !! j
            weight = case (mbf1, mbf2) of
              (Nothing, Nothing) -> 0
              (Just (f1, p1), Just (f2, p2)) ->
                let fingerDist = abs (fromEnum f1 - fromEnum f2)
                    fretDist = abs (p1 - p2)
                 in fromIntegral (fingerDist + fretDist)
              _ -> maxBound
         in weight
      where
        n = length vertices
    -- \| Adds a vertex to a fretting
    addToFretting :: Fretting -> Vertex -> Fretting
    addToFretting (Fretting fb frets) v = Fretting fb (Set.insert v frets)

    -- \| Retrieves the chromatic corresponding to a vertex
    chromatic :: Vertex -> Chromatic
    chromatic (s, Just (_, fret)) =
      transposeChromatic
        (tuning fretboard !! s)
        (transposition (fromIntegral fret))
    chromatic (s, Nothing) = tuning fretboard !! s

    -- \| Retrieves the index of a vertex in the graph
    vertexIndex :: Vertex -> Int
    vertexIndex v = fromJust $ elemIndex v allVertices

    -- \| Retrieves the fretted chromatics in a fretting
    frettedChromatics :: Fretting -> Set Chromatic
    frettedChromatics (Fretting _ frets) = Set.map chromatic frets

-- | Define a new type to represent string states
data StringState = Open | Fretted | Unplayed deriving (Eq, Show, Ord, Bounded, Enum)

frettingDiffersOnlyInFingering :: Fretting -> Fretting -> Bool
frettingDiffersOnlyInFingering (Fretting _ frets1) (Fretting _ frets2) =
  let frets1' = Set.map (second (fmap snd)) frets1
      frets2' = Set.map (second (fmap snd)) frets2
   in Set.difference frets1' frets2' == Set.empty

-- | Scores the difficulty of a fretting based on a set of rules
scoreDifficulty :: Fretting -> Int
scoreDifficulty (Fretting fretboard frets) =
  baseScore + mixedTypesPenalty + alternatingPenalty + unplayedFrettedPenalty
  where
    -- \| Extract fret positions from the fretting
    fretPositions = mapMaybe snd $ Set.toList frets
    -- \| Extract fingers used in the fretting
    fingers = map fst fretPositions
    -- \| Extract fret numbers used in the fretting
    fretNumbers = map snd fretPositions
    -- \| Calculate the span between the lowest and highest fretted positions
    fretSpan = safeMaximum fretNumbers - safeMinimum fretNumbers
    -- \| Determine if the chord is barred (more than 3 notes fretted with the index finger)
    barredChord = length (filter (== Index) fingers) > 3
    -- \| Count the number of fretted notes
    numFrettedNotes = length $ Set.filter (isJust . snd) frets
    -- \| Check if the chord is completely open (all frets are 0)
    isOpenChord = all ((== 0) . snd) fretPositions
    -- \| Check if only a single note is fretted
    isSingleFrettedNote = numFrettedNotes == 1

    -- \| Calculate the base difficulty score
    baseScore
      | isOpenChord = 0
      | isSingleFrettedNote = 2
      | otherwise = fretSpan + bareChordScore

    -- \| Penalty for non-barred chords
    bareChordScore = if barredChord then 0 else 4
    -- \| Additional score for chords with many fretted notes
    frettedCountScore = if numFrettedNotes > 4 then 3 else 1

    -- \| Count the number of open strings
    numOpenStrings = length $ Set.filter ((== Just 0) . fmap snd . snd) frets
    -- \| Count the number of unplayed strings
    numUnplayedStrings = numStrings fretboard - Set.size frets

    -- \| Coefficients for penalties
    alternatingCoeff = 3

    -- \| Calculate penalty for mixed types of string usage
    mixedTypesPenalty
      | numFrettedNotes > 0 && numOpenStrings > 0 && numUnplayedStrings > 0 = 5 + frettedCountScore
      | numFrettedNotes > 0 && numOpenStrings > 0 = 3 + frettedCountScore
      | numFrettedNotes > 0 && numUnplayedStrings > 0 = 2 + frettedCountScore
      | otherwise = 0

    -- \| Calculate penalty for alternating between open, fretted, and unplayed strings
    alternatingPenalty =
      let ssTail = [1 .. numStrings fretboard - 1]
          stringStates = fmap getStringState $ 0 :| ssTail
          transitions = zipWith (/=) (NE.toList stringStates) (NE.tail stringStates)
          transitionCount = length (filter id transitions)
       in transitionCount * alternatingCoeff

    -- \| Calculate penalty for unplayed strings and fretted strings
    unplayedFrettedPenalty =
      if numUnplayedStrings > 0 && numFrettedNotes > 0
        then numUnplayedStrings * alternatingCoeff + numFrettedNotes
        else 0

    -- \| Helper function to get the state of a string
    getStringState :: Int -> StringState
    getStringState s
      | Set.member (s, Nothing) frets = Open
      | Set.member s (Set.map fst frets) = Fretted
      | otherwise = Unplayed

-- | Helper function to generate all combinations of a given length from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

-- | Helper function to generate all subsequences of a list
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) = [x : sub | sub <- subsequences xs] ++ subsequences xs

-- Example usage
standardTuning :: Fretboard
standardTuning = Fretboard 6 [E, A, D, G, B, E]

dropDTuning :: Fretboard
dropDTuning = Fretboard 6 [D, A, D, G, B, E]

doubleHarmonicMajorTuning :: Fretboard
doubleHarmonicMajorTuning = Fretboard 6 [Bb, A, D, G, Bb, D]

dropCsTuning :: Fretboard
dropCsTuning = Fretboard 6 [Cs, A, D, G, B, E]

dropCTuning :: Fretboard
dropCTuning = Fretboard 6 [C, G, C, F, A, D]

dropBTuning :: Fretboard
dropBTuning = Fretboard 6 [B, Fs, B, E, Gs, Cs]

dropATuning :: Fretboard
dropATuning = Fretboard 6 [A, E, A, D, Fs, B]

dadgadTuning :: Fretboard
dadgadTuning = Fretboard 6 [D, A, D, G, A, D]

halfStepDownTuning :: Fretboard
halfStepDownTuning = Fretboard 6 [Eb, Gs, Cs, Fs, Bb, Eb]

fullStepDownTuning :: Fretboard
fullStepDownTuning = Fretboard 6 [D, G, C, F, A, D]

halfStepUpTuning :: Fretboard
halfStepUpTuning = Fretboard 6 [F, Bb, Eb, Gs, C, F]

openCTuning :: Fretboard
openCTuning = Fretboard 6 [C, G, C, G, C, E]

openDTuning :: Fretboard
openDTuning = Fretboard 6 [D, A, D, Fs, A, D]

openETuning :: Fretboard
openETuning = Fretboard 6 [E, B, E, Gs, B, E]

openFTuning :: Fretboard
openFTuning = Fretboard 6 [F, A, C, F, C, F]

openGTuning :: Fretboard
openGTuning = Fretboard 6 [D, G, D, G, B, D]

openATuning :: Fretboard
openATuning = Fretboard 6 [E, A, E, A, Cs, E]

data KnownGuitarTunings
  = Standard
  | DropD
  | DoubleHarmonicMajor
  | DropCSharp
  | DropC
  | DropB
  | DropA
  | Dadgad
  | HalfStepDown
  | FullStepDown
  | HalfStepUp
  | OpenC
  | OpenD
  | OpenE
  | OpenF
  | OpenG
  | OpenA
  deriving stock (Eq, Show, Enum, Bounded, Read, Ord)

knownTuning :: KnownGuitarTunings -> Fretboard
knownTuning Standard = standardTuning
knownTuning DropD = dropDTuning
knownTuning DoubleHarmonicMajor = doubleHarmonicMajorTuning
knownTuning DropCSharp = dropCsTuning
knownTuning DropC = dropCTuning
knownTuning DropB = dropBTuning
knownTuning DropA = dropATuning
knownTuning Dadgad = dadgadTuning
knownTuning HalfStepDown = halfStepDownTuning
knownTuning FullStepDown = fullStepDownTuning
knownTuning HalfStepUp = halfStepUpTuning
knownTuning OpenC = openCTuning
knownTuning OpenD = openDTuning
knownTuning OpenE = openETuning
knownTuning OpenF = openFTuning
knownTuning OpenG = openGTuning
knownTuning OpenA = openATuning

cMajor7Chord :: Set Chromatic
cMajor7Chord = Set.fromList [C, E, G, B]

cMajorChord :: Set Chromatic
cMajorChord = Set.fromList [C, E, G]

cMajor7Add9Chord :: Set Chromatic
cMajor7Add9Chord = Set.fromList [C, E, G, B, D]

cMajor7Add9Sharp11Chord :: Set Chromatic
cMajor7Add9Sharp11Chord = Set.fromList [C, E, G, B, D, Fs]

cMajor7Add9Sharp11Sharp13Chord :: Set Chromatic
cMajor7Add9Sharp11Sharp13Chord = Set.fromList [C, E, G, B, D, Fs, Bb]

cMajor7Frettings :: Set Fretting
cMajor7Frettings = Set.map snd $ findFrettings 100 standardTuning cMajor7Chord

cMajor7Frettings' :: [Fretting]
cMajor7Frettings' = Set.toList cMajor7Frettings

drawFretting :: Fretting -> String
drawFretting (Fretting fretboard frets) =
  let maxFret = maximum $ 0 : [f | (_, Just (_, f)) <- Set.toList frets]
      fretNumbers = [1 .. maxFret - 1]
      fretLines =
        map
          (drawFretLine frets maxFret)
          [numStrings fretboard - 1, numStrings fretboard - 2 .. 0]
      fretNumberLine =
        "    " ++ concatMap padFretNumber fretNumbers
      tuningLine =
        zipWith
          ( \s c -> (if Set.member (s, Nothing) frets then show c else " ")
          )
          [numStrings fretboard - 1, numStrings fretboard - 2 .. 0]
          (reverse $ tuning fretboard)
   in unlines $
        zipWith (<>) tuningLine fretLines
          ++ [fretNumberLine]
  where
    padFretNumber n
      | length (show n) == 1 = " " ++ show n ++ "  "
      | otherwise = " " ++ show n ++ " "

drawFretLine :: Set (Int, Maybe (Finger, Int)) -> Int -> Int -> String
drawFretLine frets maxFret stringIndex =
  "|" ++ concatMap (drawFretSymbol frets stringIndex) [0 .. maxFret]

drawFretSymbol :: Set (Int, Maybe (Finger, Int)) -> Int -> Int -> String
drawFretSymbol frets stringIndex fretIndex =
  case Set.lookupLE (stringIndex, Just (maxBound, fretIndex)) frets of
    Just (s, Just (finger, f))
      | s == stringIndex && f == fretIndex ->
          "-" ++ fingerSymbol finger ++ "-|"
    _ -> "---|"

fingerSymbol :: Finger -> String
fingerSymbol Thumb = "T"
fingerSymbol Index = "1"
fingerSymbol Middle = "2"
fingerSymbol Ring = "3"
fingerSymbol Pinky = "4"

-- | Optimizes frettings for a given list of sets of chromatics (chords) in a progression
optimizeFrettings :: Int -> Fretboard -> [Set Chromatic] -> [Fretting]
optimizeFrettings k tuning chromaticSets =
  case candidateVectors of
    [] -> []
    firstVec : restVecs
      | any V.null (firstVec : restVecs) -> []
      | otherwise ->
          let initialLayer = V.map toInitialEntry firstVec
              layers = scanl buildNext initialLayer restVecs
              finalLayer = last layers
           in case bestFinalIndex finalLayer of
                Nothing -> []
                Just bestIdx ->
                  let frettingsRev = reconstructPath (reverse layers) bestIdx
                   in reverse frettingsRev
  where
    candidateVectors =
      parMap
        rdeepseq
        ( V.fromList
            . Set.toAscList
            . findFrettings k tuning
        )
        chromaticSets

    toInitialEntry :: (Word8, Fretting) -> PathEntry
    toInitialEntry (score, fretting) =
      PathEntry
        { peCost =
            Just
              PathCost
                { pcTransitions = 0,
                  pcBase = fromIntegral score
                },
          pePrev = Nothing,
          peFretting = fretting
        }

    buildNext :: V.Vector PathEntry -> V.Vector (Word8, Fretting) -> V.Vector PathEntry
    buildNext prevVec currentVec =
      V.imap
        ( \idx (score, currentFretting) ->
            let baseCost = fromIntegral score
                options =
                  [ ( PathCost
                        { pcTransitions = pcTransitions prevCost + dist,
                          pcBase = pcBase prevCost + baseCost
                        },
                      prevIdx
                    )
                    | (prevIdx, prevEntry) <- zip [0 ..] (V.toList prevVec),
                      Just prevCost <- [peCost prevEntry],
                      let prevFretting = peFretting prevEntry,
                      Just distWord <- [frettingDistance prevFretting currentFretting],
                      let dist = fromIntegral distWord
                  ]
             in case options of
                  [] ->
                    PathEntry
                      { peCost = Nothing,
                        pePrev = Nothing,
                        peFretting = currentFretting
                      }
                  _ ->
                    let (bestTransition, bestPrevIdx) =
                          minimumBy (comparing fst) options
                        bestCost =
                          bestTransition
                            { pcBase = pcBase bestTransition + baseCost
                            }
                     in PathEntry
                          { peCost = Just bestCost,
                            pePrev = Just bestPrevIdx,
                            peFretting = currentFretting
                          }
        )
        currentVec

    bestFinalIndex :: V.Vector PathEntry -> Maybe Int
    bestFinalIndex vec =
      fmap fst $
        V.ifoldl'
          ( \acc idx entry ->
              case peCost entry of
                Nothing -> acc
                Just entryCost ->
                  case acc of
                    Nothing -> Just (idx, entryCost)
                    Just (_, bestCost)
                      | entryCost < bestCost -> Just (idx, entryCost)
                      | otherwise -> acc
          )
          Nothing
          vec

    reconstructPath :: [V.Vector PathEntry] -> Int -> [Fretting]
    reconstructPath [] _ = []
    reconstructPath (vec : rest) idx =
      let entry = vec V.! idx
       in peFretting entry
            : case (pePrev entry, rest) of
                (Just prevIdx, prevVec : prevRest) ->
                  reconstructPath (prevVec : prevRest) prevIdx
                _ -> []

occurrencesForPitchClasses ::
  Fretboard ->
  Int ->
  [Maybe Int] ->
  Set Chromatic ->
  [FretboardNoteOccurrence]
occurrencesForPitchClasses fretboard maxFrets baseOctaves chromatics =
  concat
    [ stringOccurrences stringIndex openChromatic baseOctave
      | (stringIndex, openChromatic, baseOctave) <- zip3 [0 ..] (tuning fretboard) octavesWithFallback
    ]
  where
    octavesWithFallback = take (numStrings fretboard) (baseOctaves ++ repeat Nothing)

    stringOccurrences :: Int -> Chromatic -> Maybe Int -> [FretboardNoteOccurrence]
    stringOccurrences stringIndex openChromatic baseOctave =
      [ FretboardNoteOccurrence
          { fnoString = stringIndex,
            fnoFret = fret,
            fnoChromatic = noteChromatic,
            fnoPitchClass = notePitchClass,
            fnoOctave = computeOctave baseOctave openPitchClass fret
          }
        | fret <- [0 .. maxFrets],
          let noteChromatic = transposeChromatic openChromatic (transposition (fromIntegral fret)),
          Set.member noteChromatic chromatics,
          let notePitchClass = toLocalInterpretation noteChromatic
      ]
      where
        openPitchClass = toLocalInterpretation openChromatic :: Mod 12

    computeOctave :: Maybe Int -> Mod 12 -> Int -> Maybe Int
    computeOctave Nothing _ _ = Nothing
    computeOctave (Just baseOctave) openPitchClass fret =
      let openPcInt = fromIntegral (unMod openPitchClass)
          total = openPcInt + fret
          octaveOffset = total `div` 12
       in Just (baseOctave + octaveOffset)

cProgression :: [Set Chromatic]
cProgression =
  [ cMajorChord,
    go 4 cMajor7Chord,
    go 6 cMajor7Add9Chord,
    cMajor7Chord,
    go 8 cMajor7Add9Chord,
    cMajor7Add9Sharp11Chord
  ]
  where
    go :: Mod 12 -> Set Chromatic -> Set Chromatic
    go n =
      Set.fromList
        . fmap toLocalInterpretation
        . transposeFunctor (transposition n)
        . fmap toLocalInterpretation
        . Set.toList

chromaticsFromFretting :: Fretting -> Set Chromatic
chromaticsFromFretting (Fretting fretboard frets) =
  Set.fromList $
    fmap
      ( \(s, mbf) ->
          maybe
            (tuning fretboard !! s)
            ( ( \t ->
                  transposeChromatic
                    (tuning fretboard !! s)
                    (transposition $ fromIntegral t)
              )
                . snd
            )
            mbf
      )
      (Set.toList frets)

drawFrettingWithPossibleChords :: Fretting -> String
drawFrettingWithPossibleChords fretting =
  "Possible Chords:\n"
    <> unlines
      ( fmap showChordName
          . Set.toList
          . chromaticsToChords
          $ chromaticsFromFretting fretting
      )
    <> "\n"
    <> "Fretting:\n"
    <> drawFretting fretting

-- Test drawFrettingWithPossibleChords function on cProgression
testdrawFrettingWithPossibleChords :: IO ()
testdrawFrettingWithPossibleChords = do
  let frettings = optimizeFrettings 25 standardTuning cProgression
  mapM_ (putStrLn . drawFrettingWithPossibleChords) frettings

fretScale ::
  (IsScale f, Enum (Rep f), Ord (Rep f)) =>
  Fretboard ->
  f Chromatic ->
  Int ->
  Set ((Int, Int), (Rep f, Chromatic))
fretScale fretboard scale maxFret =
  Set.fromList
    [ ((string, fret), (degree, note))
      | string <- [0 .. numStrings fretboard - 1],
        let openNote = tuning fretboard !! string,
        fret <- [0 .. maxFret],
        let note =
              transposeChromatic openNote (transposition (fromIntegral fret)),
        degree <- [minBound .. maxBound],
        index scale degree == note
    ]

instance (IsScale f, Ord (Rep f), Enum (Rep f), Show (Rep f)) => VoiceScale f Fretboard (Mod 12) String where
  drawScale scale fretboard =
    let maxFret = 14
        scaleFretting = fretScale @f fretboard (fmap toLocalInterpretation scale) maxFret
        maxFretNumber = maximum $ 0 : [fret | ((_, fret), _) <- Set.toList scaleFretting]
        fretNumbers = [1 .. maxFretNumber]
        fretLines =
          map
            (drawFretLine scaleFretting maxFretNumber)
            [numStrings fretboard - 1, numStrings fretboard - 2 .. 0]
        fretNumberLine =
          "    "
            ++ concatMap
              ( \f ->
                  show f
                    ++ replicate
                      (4 - length (show f))
                      ' '
              )
              fretNumbers
        tuningLine =
          zipWith
            ( \s c ->
                case Set.lookupLE ((s, 0), (maxBound, c)) scaleFretting of
                  Just ((_, 0), (d, _)) -> show (repTo1Index d) ++ "|"
                  _ -> "X|"
            )
            [numStrings fretboard - 1, numStrings fretboard - 2 .. 0]
            (reverse $ tuning fretboard)
     in unlines $
          zipWith (<>) tuningLine fretLines
            ++ [fretNumberLine]
    where
      drawFretLine :: (IsScale f, Show (Rep f)) => Set ((Int, Int), (Rep f, Chromatic)) -> Int -> Int -> String
      drawFretLine scaleFretting maxFret stringIndex =
        let fretPositions = [1 .. maxFret]
            fretSymbols = map (drawFretSymbol scaleFretting stringIndex) fretPositions
         in '|' : concat fretSymbols

      drawFretSymbol :: (IsScale f, Show (Rep f)) => Set ((Int, Int), (Rep f, Chromatic)) -> Int -> Int -> String
      drawFretSymbol scaleFretting stringIndex fretIndex =
        case Set.lookupLE ((stringIndex, fretIndex), (maxBound, maxBound)) scaleFretting of
          Just ((s, f), (d, _))
            | s == stringIndex && f == fretIndex ->
                "-" ++ show (repTo1Index d) ++ "-|"
          _ -> "---|"

repTo1Index :: (Enum a, Bounded a) => a -> Int
repTo1Index x = fromEnum x + 1

-- Test fretboards for visualization
testStandardFretboard :: Fretboard
testStandardFretboard = standardTuning

testDropDFretboard :: Fretboard
testDropDFretboard = dropDTuning

testOpenDFretboard :: Fretboard
testOpenDFretboard = openDTuning

-- Test drawing functions
drawTestStandardFretboard :: String
drawTestStandardFretboard = drawScale cIonian testStandardFretboard

drawTestDropDFretboard :: String
drawTestDropDFretboard = drawScale cMixolydian testDropDFretboard

drawTestDadgadFretboard :: String
drawTestDadgadFretboard = drawScale cMixolydian dadgadTuning

drawTestOpenDFretboard :: String
drawTestOpenDFretboard = drawScale cDorian testOpenDFretboard

-- Function to display all test fretboards
displayAllTestFretboards :: IO ()
displayAllTestFretboards = do
  putStrLn "Standard Tuning (C Ionian):"
  mapM_ putStrLn $ lines drawTestStandardFretboard
  putStrLn "Drop D Tuning (C Mixolydian):"
  mapM_ putStrLn $ lines drawTestDropDFretboard
  putStrLn "Open D Tuning (C Dorian):"
  mapM_ putStrLn $ lines drawTestOpenDFretboard
  putStrLn "DADGAD Tuning (C Mixolydian):"
  mapM_ putStrLn $ lines drawTestDadgadFretboard

-- Function to display some chord frettings
displayChordFrettings :: IO ()
displayChordFrettings = do
  putStrLn "C Major Chord Frettings:"
  mapM_ (putStrLn . drawFrettingWithPossibleChords) $ take 3 $ fmap snd $ sortBy (compare `on` fst) $ Set.toList $ findFrettings 25 standardTuning cMajorChord
  putStrLn "\nC Major 7 Chord Frettings:"
  mapM_ (putStrLn . drawFrettingWithPossibleChords) $ take 3 $ fmap snd $ sortBy (compare `on` fst) $ Set.toList $ findFrettings 25 standardTuning cMajor7Chord
  putStrLn "\nC Major 7 Add 9 Chord Frettings:"
  mapM_ (putStrLn . drawFrettingWithPossibleChords) $ take 3 $ fmap snd $ sortBy (compare `on` fst) $ Set.toList $ findFrettings 25 standardTuning cMajor7Add9Chord

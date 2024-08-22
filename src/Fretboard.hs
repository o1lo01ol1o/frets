{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
    drawFrettingWithChord,

    -- * Tests
    cMajor7Chord,
    cMajorChord,
    cMajor7Add9Chord,
    cMajor7Add9Sharp11Chord,
    cMajor7Add9Sharp11Sharp13Chord,
    cMajor7Frettings,
    cMajor7Frettings',
    cProgression,
  )
where

import Chord (chromaticsToChords, showChordName, transposeChromatic)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Foldable (Foldable (..), minimumBy)
import Data.Functor.Rep (Representable (Rep), index)
import Data.List (elemIndex, groupBy, nub, sort, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Mod (Mod, unMod)
import Data.Ord (comparing)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, generate, (!), (//))
import qualified Data.Vector as V
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Finger (Finger (..))
import Finger.TH ()
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import GHC.Word (Word8)
import Modulation
  ( Chromatic (A, B, Bb, C, Cs, D, E, Eb, F, Fs, G, Gs),
    IsScale,
    LocalInterpretation (toLocalInterpretation),
    VoiceScale (drawScale),
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

-- A utility function to create an infinite distance representation
inf :: Word8
inf = maxBound :: Word8

-- Initialize the distance vector from the adjacency matrix,
-- setting self-distances to 0 and non-existing edges to infinity
initDist :: S.Vector (U.Vector Word8) -> S.Vector (U.Vector Word8)
initDist =
  S.imap
    ( \i row ->
        U.imap
          ( \j val ->
              if i == j then 0 else if val == 0 then inf else val
          )
          row
    )

-- The core of the Floyd-Warshall algorithm, updating distances
floydWarshallCore :: S.Vector (U.Vector Word8) -> Int -> S.Vector (U.Vector Word8)
floydWarshallCore dist k = S.imap updateRow dist
  where
    updateRow i = U.imap (updateDist i)
    updateDist i j oldDist = min oldDist (dist S.! i U.! k + dist S.! k U.! j)

-- Iteratively apply the core logic over all vertices
floydWarshall :: S.Vector (U.Vector Word8) -> S.Vector (U.Vector Word8)
floydWarshall adjMatrix =
  foldl
    floydWarshallCore
    (initDist adjMatrix)
    [0 .. S.length adjMatrix - 1]

frettingDistance :: Fretting -> Fretting -> Maybe Word8
frettingDistance (Fretting fb1 frets1) (Fretting fb2 frets2)
  | fb1 /= fb2 = Nothing -- Fretboards are different, return Nothing
  | otherwise = Just $ sumMinDistances shortestPaths
  where
    toPostion = V.fromList . mapMaybe (\(s, mbf) -> (s,) <$> mbf) . Set.toList
    !playedPositions1 = toPostion frets1
    !playedPositions2 = toPostion frets2

    n1 = V.length playedPositions1
    n2 = V.length playedPositions2

    initDist = S.generate n1 $ \i ->
      U.generate n2 $ \j ->
        let !(s1, (_, f1)) = playedPositions1 ! i
            !(s2, (_, f2)) = playedPositions2 ! j
            !f_ = if f2 <= f1 then f1 - f2 else f2 - f1
            !s_ = if s2 <= s1 then s1 - s2 else s2 - s1
         in fromIntegral (f_ + s_)

    shortestPaths = floydWarshall initDist
    -- Sum the minimum distances for each position in the first fretting
    sumMinDistances :: S.Vector (U.Vector Word8) -> Word8
    sumMinDistances = S.sum . S.map U.minimum

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
         in not hasThumb || (hasThumb && thumbOnTopString && thumbNotOnOpenString)
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
        -- if a finger has more than one fret position, then the fretting is invalid
        let fretPositionsByFinger =
              groupBy (\(_, _, f1) (_, _, f2) -> f1 == f2) $
                sortBy (\(_, _, f1) (_, _, f2) -> compare f1 f2) frettedStrings'
         in all (\fretPositions -> length fretPositions <= 1) fretPositionsByFinger
      validFretSpan
        | minNonZeroFret > 5 = fretSpan <= 6
        | otherwise = fretSpan <= 4
   in validFretSpan
        && hasOneNotePerstring
        && validThumb
        && (not . null) frettedStrings
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
findFrettings :: Int -> Fretboard -> Set Chromatic -> Set Fretting
findFrettings k fretboard chromatics =
  let graph = constructGraph allVertices
      startVertices = filter (\v -> chromatic v `Set.member` chromatics) allVertices
      pq = PQ.empty -- Priority queue to store the top-k frettings
      dfs ::
        Vertex ->
        Fretting ->
        Word8 ->
        PQ.MinPQueue Word8 Fretting ->
        PQ.MinPQueue Word8 Fretting
      dfs vertex fretting dist pq
        | Set.size (frets fretting) == Set.size chromatics =
            let pq' = PQ.insert dist fretting pq
             in if PQ.size pq' > k
                  then snd $ PQ.deleteFindMin pq'
                  else pq'
        | otherwise =
            let neighbors =
                  filter (\v -> chromatic v `Set.notMember` frettedChromatics fretting) $
                    filter (\v -> isValidFretting (addToFretting fretting v)) $
                      filter (\v -> vertexIndex v /= vertexIndex vertex) allVertices
                newFretting v = addToFretting fretting v
                newDist v = dist + graph V.! vertexIndex vertex V.! vertexIndex v
                pq' = foldl' (\acc v -> dfs v (newFretting v) (newDist v) acc) pq neighbors
             in if PQ.size pq' >= k && dist >= fst (PQ.findMin pq')
                  then snd $ PQ.deleteFindMin pq'
                  else pq'

      topKFrettings =
        foldl'
          ( \acc v ->
              dfs
                v
                (Fretting fretboard (Set.singleton v))
                0
                acc
          )
          PQ.empty
          startVertices
   in Set.fromList $ map snd $ PQ.toList topKFrettings
  where
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
          i /= 0,
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

-- | Scores the difficulty of a fretting based on a set of rules
scoreDifficulty :: Fretting -> Int
scoreDifficulty (Fretting fretboard frets) =
  let fretPositions = mapMaybe snd $ Set.toList frets
      fingers = map snd fretPositions
      fretNumbers = map snd fretPositions
      fretSpan = safeMaximum fretNumbers - safeMinimum fretNumbers
      barredChord = any (\f -> length (filter (== f) fingers) > 1) fingers
      numNotes = length frets
   in fretSpan
        + (if barredChord then 0 else 2)
        + (if numNotes > 4 then 3 else 0)

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
cMajor7Frettings = findFrettings 100 standardTuning cMajor7Chord

cMajor7Frettings' :: [Fretting]
cMajor7Frettings' = Set.toList cMajor7Frettings

drawFretting :: Fretting -> String
drawFretting (Fretting fretboard frets) =
  let maxFret = maximum $ 0 : [f | (_, Just (_, f)) <- Set.toList frets]
      fretNumbers = [1 .. maxFret]
      fretLines =
        map
          (drawFretLine frets maxFret)
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
              if Set.member (s, Nothing) frets then head (show c) : "|" else "X|"
          )
          [numStrings fretboard - 1, numStrings fretboard - 2 .. 0]
          (reverse $ tuning fretboard)
   in unlines $
        zipWith (<>) tuningLine fretLines
          ++ [fretNumberLine]

drawFretLine :: Set (Int, Maybe (Finger, Int)) -> Int -> Int -> String
drawFretLine frets maxFret stringIndex =
  let fretPositions = [1 .. maxFret]
      fretSymbols = map (drawFretSymbol frets stringIndex) fretPositions
   in '|' : concat fretSymbols

drawFretSymbol :: Set (Int, Maybe (Finger, Int)) -> Int -> Int -> String
drawFretSymbol frets stringIndex fretIndex
  | Set.member (stringIndex, Just (Thumb, fretIndex)) frets = "-T-|"
  | Set.member (stringIndex, Just (Index, fretIndex)) frets = "-I-|"
  | Set.member (stringIndex, Just (Middle, fretIndex)) frets = "-M-|"
  | Set.member (stringIndex, Just (Ring, fretIndex)) frets = "-R-|"
  | Set.member (stringIndex, Just (Pinky, fretIndex)) frets = "-P-|"
  | otherwise = "---|"

-- | Optimizes frettings for a given list of sets of chromatics (chords) in a progression
optimizeFrettings :: Int -> Fretboard -> [Set Chromatic] -> [Fretting]
optimizeFrettings k tuning chromaticSets =
  let frettingSets = map (findFrettings k tuning) chromaticSets
      allCombinations = sequence (map Set.toList frettingSets)
   in if null allCombinations
        then []
        else
          let distances = parMap rdeepseq (calculateDistance allCombinations) [1 .. length allCombinations - 1]
              minDistanceCombination = minimumBy (comparing fst) (zip distances allCombinations)
              optimalFrettings = snd minDistanceCombination
           in optimalFrettings

-- | Calculates the total distance between frettings in a combination
calculateDistance :: [[Fretting]] -> Int -> Int
calculateDistance allCombinations index =
  let currentFretting = allCombinations !! index
      previousFretting = allCombinations !! (index - 1)
   in sum $ zipWith frettingDistance' currentFretting previousFretting

-- | Calculates the distance between two frettings
frettingDistance' :: Fretting -> Fretting -> Int
frettingDistance' (Fretting _ frets1) (Fretting _ frets2) =
  let positions1 = Set.map (\(s, mbf) -> (s, fromJust mbf)) $ Set.filter ((/= Nothing) . snd) frets1
      positions2 = Set.map (\(s, mbf) -> (s, fromJust mbf)) $ Set.filter ((/= Nothing) . snd) frets2
      distance = sum $ zipWith (\(_, (_, f1)) (_, (_, f2)) -> abs (f1 - f2)) (Set.toList positions1) (Set.toList positions2)
   in distance

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
    mapMaybe
      ( \(s, mbf) ->
          fmap
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

drawFrettingWithChord :: Fretting -> String
drawFrettingWithChord fretting =
  unlines
    ( fmap showChordName
        . Set.toList
        . chromaticsToChords
        $ chromaticsFromFretting fretting
    )
    <> "\n"
    <> drawFretting fretting

fretScale :: (IsScale f) => Fretboard -> f Chromatic -> Int -> Set ((Int, Int), (Rep f, Chromatic))
fretScale fretboard scale maxFret =
  Set.fromList
    [ ((string, fret), (degree, note))
      | string <- [0 .. numStrings fretboard - 1],
        fret <- [0 .. maxFret],
        let openNote = tuning fretboard !! string,
        let note = transposeChromatic openNote (transposition (fromIntegral fret)),
        degree <- [minBound .. maxBound],
        index scale degree == toLocalInterpretation note
    ]

instance (IsScale f) => VoiceScale f Fretboard (Mod 12) where
  drawScale scale fretboard =
    let maxFret = 18
        scaleFretting = fretScale @f fretboard (fmap toLocalInterpretation scale) maxFret
        maxFretNumber = maximum $ 0 : [fret | ((_, fret), _) <- Set.toList scaleFretting]
        fretNumbers = [0 .. maxFretNumber]
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
                if Set.member ((s, 0), (0, c)) scaleFretting
                  then head (show c) : "|"
                  else "X|"
            )
            [numStrings fretboard - 1, numStrings fretboard - 2 .. 0]
            (reverse $ tuning fretboard)
     in unlines $
          zipWith (<>) tuningLine fretLines
            ++ [fretNumberLine]
    where
      drawFretLine :: (IsScale f) => Set ((Int, Int), (Rep f, Chromatic)) -> Int -> Int -> String
      drawFretLine scaleFretting maxFret stringIndex =
        let fretPositions = [0 .. maxFret]
            fretSymbols = map (drawFretSymbol scaleFretting stringIndex) fretPositions
         in '|' : concat fretSymbols

      drawFretSymbol :: (IsScale f) => Set ((Int, Int), (Rep f, Chromatic)) -> Int -> Int -> String
      drawFretSymbol scaleFretting stringIndex fretIndex
        | Just (degree, note) <- Set.lookupLE ((stringIndex, fretIndex), (maxBound, maxBound)) scaleFretting =
            let degreeStr = show degree
                noteStr = (show $ snd note)
             in "-" ++ degreeStr ++ noteStr ++ "|"
        | otherwise = "----|"

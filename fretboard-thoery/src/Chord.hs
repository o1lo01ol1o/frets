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
{-# LANGUAGE UndecidableInstances #-}

module Chord where

import Algebra.Lattice
import Control.Lens hiding (index)
import Control.Lens.TH
import Control.Monad (guard)
import Control.Parallel.Strategies (NFData, parMap, rseq)
import Data.Distributive (Distributive (collect))
import Data.Distributive.Generic (genericCollect)
import qualified Data.Foldable as Foldable
import Data.Functor.Rep
import Data.Functor.Rix.Rep (rix)
import Data.List
import Data.Mod
import qualified Data.Mod as Modular
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic, Generic1)
import Modulation

data ChordQuality
  = Maj
  | Min
  | Aug
  | Dim
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordSeventh
  = Maj7
  | Min7
  | Dom7
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordSixth
  = Maj6
  | Min6
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordAlteration9
  = Sharp9
  | Flat9
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordAlteration11
  = Sharp11
  | Flat11
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordAlteration13
  = Sharp13
  | Flat13
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordAddition2_9
  = Add2
  | Add9
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordAddition4_11
  = Add4
  | Add11
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data ChordAddition6_13
  = Add6
  | Add13
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving anyclass (NFData)

data Chord = Chord
  { root :: Chromatic,
    bass :: Maybe Chromatic,
    quality :: Maybe ChordQuality,
    seventh :: Maybe ChordSeventh,
    sixth :: Maybe ChordSixth,
    alteration9 :: Maybe ChordAlteration9,
    alteration11 :: Maybe ChordAlteration11,
    alteration13 :: Maybe ChordAlteration13,
    addition2_9 :: Maybe ChordAddition2_9,
    addition4_11 :: Maybe ChordAddition4_11,
    addition6_13 :: Maybe ChordAddition6_13
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

showChordName :: Chord -> String
showChordName chord =
  let root' = show $ root chord
      quality' = case quality chord of
        Just Maj -> ""
        Just Min -> "m"
        Just Aug -> "aug"
        Just Dim -> "dim"
        Nothing -> ""
      seventh' = case seventh chord of
        Just Maj7 -> "maj7"
        Just Min7 -> "m7"
        Just Dom7 -> "7"
        Nothing -> ""
      sixth' = case sixth chord of
        Just Maj6 -> "6"
        Just Min6 -> "m6"
        Nothing -> ""
      alteration9' = case alteration9 chord of
        Just Sharp9 -> "#9"
        Just Flat9 -> "b9"
        Nothing -> ""
      alteration11' = case alteration11 chord of
        Just Sharp11 -> "#11"
        Just Flat11 -> "b11"
        Nothing -> ""
      alteration13' = case alteration13 chord of
        Just Sharp13 -> "#13"
        Just Flat13 -> "b13"
        Nothing -> ""
      addition2_9' = case addition2_9 chord of
        Just Add2 -> "add2"
        Just Add9 -> "add9"
        Nothing -> ""
      addition4_11' = case addition4_11 chord of
        Just Add4 -> "add4"
        Just Add11 -> "add11"
        Nothing -> ""
      addition6_13' = case addition6_13 chord of
        Just Add6 -> "add6"
        Just Add13 -> "add13"
        Nothing -> ""
   in root'
        ++ quality'
        ++ seventh'
        ++ sixth'
        ++ alteration9'
        ++ alteration11'
        ++ alteration13'
        ++ addition2_9'
        ++ addition4_11'
        ++ addition6_13'

-- Function to transpose a chord by a given interval
transposeChord :: Chord -> Mod 12 -> Chord
transposeChord chord interval =
  chord
    { root =
        transposeChromatic
          (root chord)
          (transposition interval)
    }

-- Helper function to find candidate chords for a given root and optional scale
candidateChordsForRoot ::
  Chromatic ->
  Maybe (HeptatonicScale Chromatic) ->
  [(Chord, Maybe Degree)]
candidateChordsForRoot root' maybeScale =
  let qualities = Nothing : (Just <$> [Maj, Min, Aug, Dim])
      sevenths = [Nothing, Just Maj7, Just Min7, Just Dom7]
      sixths = [Nothing, Just Maj6, Just Min6]
      alterations9 = [Nothing, Just Sharp9, Just Flat9]
      alterations11 = [Nothing, Just Sharp11, Just Flat11]
      alterations13 = [Nothing, Just Sharp13, Just Flat13]
      additions2_9 = [Nothing, Just Add2, Just Add9]
      additions4_11 = [Nothing, Just Add4, Just Add11]
      additions6_13 = [Nothing, Just Add6, Just Add13]
      chords =
        [ Chord root' Nothing q s sx a9 a11 a13 ad2_9 ad4_11 ad6_13
          | q <- qualities,
            s <- sevenths,
            sx <- sixths,
            a9 <- alterations9,
            a11 <- alterations11,
            a13 <- alterations13,
            ad2_9 <- additions2_9,
            ad4_11 <- additions4_11,
            ad6_13 <- additions6_13
        ]
      chordsWithDegrees = case maybeScale of
        Just scale -> map (\c -> (c, findScaleDegree scale (root c))) chords
        Nothing -> map (\c -> (c, Nothing)) chords
   in chordsWithDegrees

-- Function to compute the set of Chromatics that constitute a chord
chordToChromatics :: Chord -> Set Chromatic
chordToChromatics chord =
  Set.fromList $
    root chord
      : bassNote
      ++ qualityNotes
      ++ seventhNotes
      ++ sixthNotes
      ++ alteration9Notes
      ++ alteration11Notes
      ++ alteration13Notes
      ++ addition2_9Notes
      ++ addition4_11Notes
      ++ addition6_13Notes
  where
    bassNote = maybe [] pure (bass chord)
    qualityNotes = case quality chord of
      Just Maj ->
        [ transposeChromatic (root chord) (AffineSymmetry 4 1),
          transposeChromatic (root chord) (AffineSymmetry 7 1)
        ]
      Just Min ->
        [ transposeChromatic (root chord) (AffineSymmetry 3 1),
          transposeChromatic (root chord) (AffineSymmetry 7 1)
        ]
      Just Aug ->
        [ transposeChromatic (root chord) (AffineSymmetry 4 1),
          transposeChromatic (root chord) (AffineSymmetry 8 1)
        ]
      Just Dim ->
        [ transposeChromatic (root chord) (AffineSymmetry 3 1),
          transposeChromatic (root chord) (AffineSymmetry 6 1)
        ]
      Nothing -> []
    seventhNotes = case seventh chord of
      Just Maj7 -> [transposeChromatic (root chord) (AffineSymmetry 11 1)]
      Just Min7 -> [transposeChromatic (root chord) (AffineSymmetry 10 1)]
      Just Dom7 -> [transposeChromatic (root chord) (AffineSymmetry 10 1)]
      Nothing -> []
    sixthNotes = case sixth chord of
      Just Maj6 -> [transposeChromatic (root chord) (AffineSymmetry 9 1)]
      Just Min6 -> [transposeChromatic (root chord) (AffineSymmetry 8 1)]
      Nothing -> []
    alteration9Notes = case alteration9 chord of
      Just Sharp9 -> [transposeChromatic (root chord) (AffineSymmetry 15 1)]
      Just Flat9 -> [transposeChromatic (root chord) (AffineSymmetry 13 1)]
      Nothing -> []
    alteration11Notes = case alteration11 chord of
      Just Sharp11 -> [transposeChromatic (root chord) (AffineSymmetry 18 1)]
      Just Flat11 -> [transposeChromatic (root chord) (AffineSymmetry 16 1)]
      Nothing -> []
    alteration13Notes = case alteration13 chord of
      Just Sharp13 -> [transposeChromatic (root chord) (AffineSymmetry 21 1)]
      Just Flat13 -> [transposeChromatic (root chord) (AffineSymmetry 20 1)]
      Nothing -> []
    addition2_9Notes = case addition2_9 chord of
      Just Add2 -> [transposeChromatic (root chord) (AffineSymmetry 2 1)]
      Just Add9 -> [transposeChromatic (root chord) (AffineSymmetry 14 1)]
      Nothing -> []
    addition4_11Notes = case addition4_11 chord of
      Just Add4 -> [transposeChromatic (root chord) (AffineSymmetry 5 1)]
      Just Add11 -> [transposeChromatic (root chord) (AffineSymmetry 17 1)]
      Nothing -> []
    addition6_13Notes = case addition6_13 chord of
      Just Add6 -> [transposeChromatic (root chord) (AffineSymmetry 9 1)]
      Just Add13 -> [transposeChromatic (root chord) (AffineSymmetry 21 1)]
      Nothing -> []

-- Function to compute the chord formed by the triad at a given scale degree with optional extensions and alterations
chordAtScaleDegree ::
  HeptatonicScale Chromatic ->
  Degree ->
  Maybe ChordSeventh ->
  Maybe ChordSixth ->
  Maybe ChordAlteration9 ->
  Maybe ChordAlteration11 ->
  Maybe ChordAlteration13 ->
  Maybe ChordAddition2_9 ->
  Maybe ChordAddition4_11 ->
  Maybe ChordAddition6_13 ->
  Chord
chordAtScaleDegree scale degree seventh sixth alt9 alt11 alt13 add2_9 add4_11 add6_13 =
  let root' = scale ^. rix (toEnum $ fromEnum degree)
      third = scale ^. rix (toEnum $ 2 + fromEnum degree)
      fifth = scale ^. rix (toEnum $ 4 + fromEnum degree)
      quality' = getChordQuality root' third fifth
   in Chord root' Nothing quality' seventh sixth alt9 alt11 alt13 add2_9 add4_11 add6_13

getChordQuality :: Chromatic -> Chromatic -> Chromatic -> Maybe ChordQuality
getChordQuality root third fifth =
  let thirdInterval = (fromEnum third - fromEnum root) `mod` 12
      fifthInterval = (fromEnum fifth - fromEnum root) `mod` 12
   in case (thirdInterval, fifthInterval) of
        (4, 7) -> Just Maj
        (3, 7) -> Just Min
        (4, 8) -> Just Aug
        (3, 6) -> Just Dim
        _ -> Nothing

-- Helper function to find the scale degree of a given root in a scale
findScaleDegree :: HeptatonicScale Chromatic -> Chromatic -> Maybe Degree
findScaleDegree scale root =
  let degrees = [I, II, III, IV, V, VI, VII]
      scaleNotes = Foldable.toList scale
      maybeIndex = elemIndex root scaleNotes
   in fmap (degrees !!) maybeIndex

-- Helper function to transpose a Chromatic by a given interval
transposeChromatic :: Chromatic -> AffineSymmetry (Mod 12) -> Chromatic
transposeChromatic c i =
  toLocalInterpretation . runIdentity $
    transposeFunctor i (Identity $ toLocalInterpretation c)

chromaticsToChords :: Set Chromatic -> Set Chord
chromaticsToChords chromatics = Set.fromList $ concat $ parMap rseq findChordsForChromatic (Set.toList chromatics)
  where
    findChordsForChromatic :: Chromatic -> [Chord]
    findChordsForChromatic root =
      let candidateChords = map fst $ candidateChordsForRoot root Nothing
          validChords = filter (\c -> chordToChromatics c `Set.isSubsetOf` chromatics) candidateChords
       in validChords

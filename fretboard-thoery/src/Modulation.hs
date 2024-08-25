{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Modulation where

import Algebra.Lattice
import Control.Lens
import Control.Lens.TH
import Control.Monad (guard)
import Control.Parallel.Strategies (NFData)
import Data.Distributive (Distributive (collect))
import Data.Distributive.Generic (genericCollect)
import Data.Foldable (toList)
import Data.Functor.Rep
import Data.Functor.Rix.Rep (rix)
import Data.List
import Data.Mod
import qualified Data.Mod as Modular
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic, Generic1)

-- | An enumeration of chromatic notes (without regard to enharmonics)
data Chromatic
  = C
  | Cs
  | D
  | Eb
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | Bb
  | B
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

--   deriving anyclass (Universe)

-- instance Arbitrary Diatonic where
--   arbitrary = elements universe

-- | Class modeling local compositions as interpretations of some value n
-- as `a`.  Using this, one could interpret values of `Z / 12`  as, eg, values in the Chromatic scale, Cmaj, Cminor, etc.
class (Ord a, Eq a) => LocalInterpretation n a where
  toLocalInterpretation :: n -> a

-- >>> Modular.unMod @Int @12 (toLocalInterpretation B)
-- >>> Modular.unMod @Int @12 (toLocalInterpretation C)
-- 11
-- 0

instance LocalInterpretation (Mod 12) Chromatic where
  toLocalInterpretation = toEnum . fromIntegral . unMod

instance LocalInterpretation Chromatic (Mod 12) where
  toLocalInterpretation = fromIntegral . fromEnum

data AffineSymmetry n = AffineSymmetry
  { _transpose :: n,
    _scale :: n
  }

makeLenses ''AffineSymmetry

transposeFunctor :: (Functor f, Num b) => AffineSymmetry b -> f b -> f b
transposeFunctor (AffineSymmetry t n) = fmap (\s' -> t + (s' * n))

transposeSet :: (Ord b, Num b) => AffineSymmetry b -> Set b -> Set b
transposeSet (AffineSymmetry t n) = Set.map (\s' -> t + (s' * n))

transposition :: (Num b) => b -> AffineSymmetry b
transposition t = AffineSymmetry t 1

class (Bounded (Rep f), Representable f) => IsScale f where
  succMode :: f a -> f a
  predMode :: f a -> f a
  maxDegree :: Rep f
  maxDegree = maxBound @(Rep f)
  transposeScale :: (Num a) => AffineSymmetry a -> f a -> f a
  transposeScale = transposeFunctor
  transposeScaleSemiTones :: (Num a) => a -> f a -> f a
  transposeScaleSemiTones i = transposeFunctor (transposition i)

class (IsScale f, LocalInterpretation a Chromatic) => VoiceScale f i a s where
  drawScale :: f a -> i -> s

-- | An seven note scale
data HeptatonicScale a = HeptatonicScale a a a a a a a
  deriving stock
    ( Show,
      Eq,
      Ord,
      Functor,
      Traversable,
      Foldable,
      Generic,
      Generic1
    )

instance Distributive HeptatonicScale where collect = genericCollect

instance Representable HeptatonicScale where
  type Rep HeptatonicScale = (Mod 7)
  tabulate f = fmap f (HeptatonicScale 0 1 2 3 4 5 6)
  index (HeptatonicScale a b c d e f g) i =
    case i of
      0 -> a
      1 -> b
      2 -> c
      3 -> d
      4 -> e
      5 -> f
      6 -> g
      _ -> error "impossible: index: HeptatonicScale index not in (Mod 7)"

instance IsScale HeptatonicScale where
  succMode (HeptatonicScale a b c d e f g) = HeptatonicScale b c d e f g a

  predMode (HeptatonicScale a b c d e f g) = HeptatonicScale g a b c d e f

cIonian :: HeptatonicScale (Mod 12)
cIonian = fmap toLocalInterpretation (HeptatonicScale C D E F G A B)

cDorian :: HeptatonicScale (Mod 12)
cDorian = succMode cIonian

cPhrygian :: HeptatonicScale (Mod 12)
cPhrygian = succMode cDorian

cLydian :: HeptatonicScale (Mod 12)
cLydian = succMode cPhrygian

cMixolydian :: HeptatonicScale (Mod 12)
cMixolydian = succMode cLydian

cAeolian :: HeptatonicScale (Mod 12)
cAeolian = succMode cMixolydian

cLocrian :: HeptatonicScale (Mod 12)
cLocrian = succMode cAeolian

cModes :: [HeptatonicScale (Mod 12)]
cModes =
  [ cIonian,
    cDorian,
    cPhrygian,
    cLydian,
    cMixolydian,
    cAeolian,
    cLocrian
  ]

cHarmonicMinor :: HeptatonicScale (Mod 12)
cHarmonicMinor = fmap toLocalInterpretation (HeptatonicScale C D Eb F G Gs B)

cHarmonicMajor :: HeptatonicScale (Mod 12)
cHarmonicMajor = fmap toLocalInterpretation (HeptatonicScale C D E F G Gs B)

cDoubleHarmonic :: HeptatonicScale (Mod 12)
cDoubleHarmonic = fmap toLocalInterpretation (HeptatonicScale C Cs E F G Gs B)

cMelodicMinor :: HeptatonicScale (Mod 12)
cMelodicMinor = fmap toLocalInterpretation (HeptatonicScale C D Eb F G A B)

data PentatonicScale a = PentatonicScale a a a a a
  deriving stock
    ( Show,
      Eq,
      Ord,
      Functor,
      Traversable,
      Foldable,
      Generic,
      Generic1
    )

data KnownHeptatonicScale
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  | HarmonicMinor
  | HarmonicMajor
  | DoubleHarmonic
  | MelodicMinor
  deriving stock (Show, Eq, Ord, Bounded, Enum, Read)

knownHeptatonicScale :: KnownHeptatonicScale -> HeptatonicScale (Mod 12)
knownHeptatonicScale Ionian = cIonian
knownHeptatonicScale Dorian = cDorian
knownHeptatonicScale Phrygian = cPhrygian
knownHeptatonicScale Lydian = cLydian
knownHeptatonicScale Mixolydian = cMixolydian
knownHeptatonicScale Aeolian = cAeolian
knownHeptatonicScale Locrian = cLocrian
knownHeptatonicScale HarmonicMinor = cHarmonicMinor
knownHeptatonicScale HarmonicMajor = cHarmonicMajor
knownHeptatonicScale DoubleHarmonic = cDoubleHarmonic
knownHeptatonicScale MelodicMinor = cMelodicMinor

instance Distributive PentatonicScale where collect = genericCollect

instance Representable PentatonicScale where
  type Rep PentatonicScale = (Mod 5)
  tabulate f = fmap f (PentatonicScale 0 1 2 3 4)
  index (PentatonicScale a b c d e) i =
    case i of
      0 -> a
      1 -> b
      2 -> c
      3 -> d
      4 -> e
      _ -> error "impossible: index: PentatonicScale index not in (Mod 5)"

instance IsScale PentatonicScale where
  succMode (PentatonicScale a b c d e) = PentatonicScale b c d e a

  predMode (PentatonicScale a b c d e) = PentatonicScale e a b c d

cMinorPentatonic :: PentatonicScale (Mod 12)
cMinorPentatonic = fmap toLocalInterpretation (PentatonicScale C Eb F G Bb)

data HexatonicScale a = HexatonicScale a a a a a a
  deriving stock
    ( Show,
      Eq,
      Ord,
      Functor,
      Traversable,
      Foldable,
      Generic,
      Generic1
    )

instance Distributive HexatonicScale where collect = genericCollect

instance Representable HexatonicScale where
  type Rep HexatonicScale = (Mod 5)
  tabulate f = fmap f (HexatonicScale 0 1 2 3 4 5)
  index (HexatonicScale a b c d e f) i =
    case i of
      0 -> a
      1 -> b
      2 -> c
      3 -> d
      4 -> e
      5 -> f
      _ -> error "impossible: index: HeptatonicScale index not in (Mod 5)"

instance IsScale HexatonicScale where
  succMode (HexatonicScale a b c d e f) = HexatonicScale b c d e f a

  predMode (HexatonicScale a b c d e f) = HexatonicScale f a b c d e

cMajorBlues :: HexatonicScale (Mod 12)
cMajorBlues = fmap toLocalInterpretation (HexatonicScale C D Eb E G A)

-- Minor Blues Scale
cMinorBlues :: HexatonicScale (Mod 12)
cMinorBlues = fmap toLocalInterpretation (HexatonicScale C Eb F Fs G Bb)

cAugmented :: HexatonicScale (Mod 12)
cAugmented = fmap toLocalInterpretation (HexatonicScale C E Gs D Fs A)

data OctatonicScale a = OctatonicScale a a a a a a a a
  deriving stock
    ( Show,
      Eq,
      Ord,
      Functor,
      Traversable,
      Foldable,
      Generic,
      Generic1
    )

instance Distributive OctatonicScale where collect = genericCollect

instance Representable OctatonicScale where
  type Rep OctatonicScale = (Mod 8)
  tabulate f = fmap f (OctatonicScale 0 1 2 3 4 5 6 7)
  index (OctatonicScale a b c d e f g h) i =
    case i of
      0 -> a
      1 -> b
      2 -> c
      3 -> d
      4 -> e
      5 -> f
      6 -> g
      7 -> h
      _ -> error "impossible: index: HeptatonicScale index not in (Mod 8)"

instance IsScale OctatonicScale where
  succMode (OctatonicScale a b c d e f g h) = OctatonicScale b c d e f g h a

  predMode (OctatonicScale a b c d e f g h) = OctatonicScale h a b c d e f g

cDiminished :: OctatonicScale (Mod 12)
cDiminished = fmap toLocalInterpretation (OctatonicScale C D Eb F Fs Gs A B)

-- | We're going to consider scales to be drawn from cycles of semitones (Mod 12)
-- They'll consist of 7 notes
type Scale = HeptatonicScale (Mod 12)

-- | A set of three notes.  They are, as far as I know, only defined in Mod 12
-- but we can let them be defined for any `n`.
data Triad n = Triad n n n
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

tMod12 :: [AffineSymmetry (Mod 12)]
tMod12 = do
  t <- Set.toList $ Set.fromList [0 .. 12]
  AffineSymmetry t <$> [-1, 1]

data Accidental a = Natural a | Sharp a | Flat a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Read, Generic1)
  deriving anyclass (NFData)

-- The triadic interpretation is stacked thirds on each member of the scale.
type Triads = [(Mod 7, Triad (Mod 12))]

data Tetrad d = Tetrad (Triad d) d
  deriving stock (Ord, Eq, Show, Generic, Functor, Foldable, Traversable)

type Tetrads = [(Mod 7, Tetrad (Mod 12))]

data Degree = I | II | III | IV | V | VI | VII
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum)

data CadentialDegree n degreeType = CadentialDegree
  { _cadentialScale :: HeptatonicScale (Mod n),
    _chord :: degreeType (Mod n),
    _degree :: Degree
  }

deriving instance (Show (degreeType (Mod n))) => Show (CadentialDegree n degreeType)

deriving instance (Eq (degreeType (Mod n))) => Eq (CadentialDegree n degreeType)

deriving instance (Ord (degreeType (Mod n))) => Ord (CadentialDegree n degreeType)

mapDegreeType ::
  (degreeType1 (Mod n) -> degreeType2 (Mod n)) ->
  CadentialDegree n degreeType1 ->
  CadentialDegree n degreeType2
mapDegreeType f (CadentialDegree s ch d) = CadentialDegree s (f ch) d

makeLenses ''CadentialDegree

toDegree :: Mod 7 -> Degree
toDegree = toEnum . fromIntegral . unMod

diaToChromatics :: (Foldable s) => Dia s -> [Chromatic]
diaToChromatics (Dia dia) =
  fmap toLocalInterpretation
    . concatMap (toList . _chord)
    $ Set.toList dia

-- | The set of triads for a given scale.
-- >>> triadSet cIonian
-- Dia {unDia = fromList [CadentialDegree {_cadentialScale = HeptatonicScale 0 2
-- 4 5 7 9 11, _chord = Triad 0 4 7, _degree = I},CadentialDegree
-- {_cadentialScale = HeptatonicScale 0 2 4 5 7 9 11, _chord = Triad 2 5 9,
-- _degree = II},CadentialDegree {_cadentialScale = HeptatonicScale 0 2 4 5 7 9
-- 11, _chord = Triad 4 7 11, _degree = III},CadentialDegree {_cadentialScale =
-- HeptatonicScale 0 2 4 5 7 9 11, _chord = Triad 5 9 0, _degree =
-- IV},CadentialDegree {_cadentialScale = HeptatonicScale 0 2 4 5 7 9 11, _chord
-- = Triad 7 11 2, _degree = V},CadentialDegree {_cadentialScale =
-- HeptatonicScale 0 2 4 5 7 9 11, _chord = Triad 9 0 4, _degree =
-- VI},CadentialDegree {_cadentialScale = HeptatonicScale 0 2 4 5 7 9 11, _chord
-- = Triad 11 2 5, _degree = VII}]}
triadSet :: Scale -> Dia Triad
triadSet s = Dia
  . Set.fromList
  . fmap makeDegree
  . nubBy (\a b -> snd a == snd b)
  $ do
    (i :: Mod 7) <- [0 .. 7]
    let j = i + 2
        k = j + 2
    pure (toDegree i, Triad (s ^. rix i) (s ^. rix j) (s ^. rix k))
  where
    makeDegree (i, t) = CadentialDegree s t i

triadAtDegree' :: Degree -> Scale -> Dia Triad
triadAtDegree' d s = Dia . Set.fromList $ do
  let i = fromIntegral $ fromEnum d
      j = i + 2
      k = j + 2
  pure (CadentialDegree s (Triad (s ^. rix i) (s ^. rix j) (s ^. rix k)) d)

triadAtDegree :: Degree -> Scale -> Set Chromatic
triadAtDegree d = Set.fromList . diaToChromatics . triadAtDegree' d

-- | The set of tetrads for a given scale.
-- >>> tetradSet cIonian
-- Dia {unDia = fromList [CadentialDegree {_cadentialScale = HeptatonicScale 0 2
-- 4 5 7 9 11, _chord = Tetrad (Triad 0 4 7) 11, _degree = I},CadentialDegree
-- {_cadentialScale = HeptatonicScale 0 2 4 5 7 9 11, _chord = Tetrad (Triad 2 5
-- 9) 0, _degree = II},CadentialDegree {_cadentialScale = HeptatonicScale 0 2 4
-- 5 7 9 11, _chord = Tetrad (Triad 4 7 11) 2, _degree = III},CadentialDegree
-- {_cadentialScale = HeptatonicScale 0 2 4 5 7 9 11, _chord = Tetrad (Triad 5 9
-- 0) 4, _degree = IV},CadentialDegree {_cadentialScale = HeptatonicScale 0 2 4
-- 5 7 9 11, _chord = Tetrad (Triad 7 11 2) 5, _degree = V},CadentialDegree
-- {_cadentialScale = HeptatonicScale 0 2 4 5 7 9 11, _chord = Tetrad (Triad 9 0
-- 4) 7, _degree = VI},CadentialDegree {_cadentialScale = HeptatonicScale 0 2 4
-- 5 7 9 11, _chord = Tetrad (Triad 11 2 5) 9, _degree = VII}]}
tetradSet :: Scale -> Dia Tetrad
tetradSet s = Dia
  . Set.fromList
  . fmap makeDegree
  . nubBy (\a b -> snd a == snd b)
  $ do
    (i :: Mod 7) <- [0 .. 7]
    let j = i + 2
        k = j + 2
        l = k + 2
    pure
      ( toDegree i,
        Tetrad
          ( Triad
              (s ^. rix i)
              (s ^. rix j)
              (s ^. rix k)
          )
          $ s ^. rix l
      )
  where
    makeDegree (i, t) = CadentialDegree s t i

tetradAtDegree :: Degree -> Scale -> Dia Tetrad
tetradAtDegree d s = Dia . Set.fromList $ do
  let i = fromIntegral $ fromEnum d
      j = i + 2
      k = j + 2
      l = k + 2
  pure
    ( CadentialDegree
        s
        (Tetrad (Triad (s ^. rix i) (s ^. rix j) (s ^. rix k)) (s ^. rix l))
        d
    )

-- >>> length $ powerSetOfSetsOfAtMostSize 4 . tetradSet $ eIonian
-- 98
powerSetOfSetsOfAtMostSize ::
  (Ord (s (Mod 12))) =>
  Int ->
  Dia s ->
  Set (Set (s (Mod 12)))
powerSetOfSetsOfAtMostSize n (Dia dia) =
  Set.filter (\s -> Set.size s <= n && Set.size s > 0)
    . Set.powerSet
    $ Set.map _chord dia

-- | All "diatonic" sets of degree chords under T12 transpositions
newtype DiaT12 s = DiaT12 {unDiaT12 :: Set (CadentialDegree 12 s)}

deriving instance (Show (s (Mod 12))) => Show (DiaT12 s)

-- | All "diatonic" sets of degree chords
newtype Dia s = Dia {unDia :: Set (CadentialDegree 12 s)}

deriving newtype instance (Ord (s (Mod 12))) => Semigroup (Dia s)

deriving newtype instance (Ord (s (Mod 12))) => Monoid (Dia s)

deriving instance (Show (s (Mod 12))) => Show (Dia s)

-- | A minimal set of cadences
newtype MinimalCadence s = MinimalCadence {unMinimalCadence :: Set (CadentialDegree 12 s)}

deriving newtype instance (Ord (s (Mod 12))) => Semigroup (MinimalCadence s)

deriving newtype instance (Ord (s (Mod 12))) => Monoid (MinimalCadence s)

deriving instance (Show (s (Mod 12))) => Show (MinimalCadence s)

-- |
-- >>> Set.map (Set.map _degree) $ minimalCadences cIonian triadSet
-- >>> Set.map (Set.map _degree) $ minimalCadences fIonian tetradSet
-- fromList [fromList [II,III],fromList [II,V],fromList [III,IV],fromList [IV,V],fromList [VII]]
-- fromList [fromList [I,II],fromList [I,IV],fromList [II,III],fromList [III,IV],fromList [V],fromList [VII]]
minimalCadences ::
  forall s.
  (Ord (s (Mod 12)), Show (s (Mod 12))) =>
  Scale ->
  (Scale -> Dia s) ->
  Set (Set (CadentialDegree 12 s))
minimalCadences scale f =
  Set.fromList $ do
    i <- Set.toList minimallyCadencial
    guard . all (`Set.member` sChords) $ Set.toList i
    -- only cadential candidates whose cords are in the provided diatonic chords
    pure $
      Set.fromList
        ( Set.toAscList s
            ^.. folded
              . filtered ((`Set.member` i) . _chord)
        ) -- those diatonic CadentialDegree that share cords with the (passing) candidate
  where
    Dia s = f scale
    sChords = Set.map _chord s
    minimallyCadencial = Set.fromList $ do
      c <- cadencial' -- all cadences whose proper subsets are no in cadencial
      let c' :: [Set (s (Mod 12))] =
            filter (`Set.isProperSubsetOf` c)
              . Set.toList
              $ Set.powerSet c
          notInCadencial c'' = Set.member c'' cadencial
      guard (not $ any notInCadencial c')
      pure c
      where
        cadencial = Set.fromList . fmap fst $ mod12Universe' scale f
        cadencial' = Set.toList cadencial

mod12Universe' = undefined

data ModulationSpec a = ModulationSpec
  { -- | The target
    target :: !(Set a),
    -- | The modulation
    modulator :: a -> a,
    -- | A minimal "cadential" set of target
    mu :: !(Set a),
    -- | All group automorphisms of a
    symmetry :: [a -> a]
  }

maybeQuantum :: (Ord a) => ModulationSpec a -> Set a -> Maybe (Set a)
maybeQuantum (ModulationSpec t mod' mu' sym) q
  | or (fmap isSymmetry sym) = Nothing
  | otherwise = Just q''
  where
    q' = Set.map mod' q
    q'' = q' \/ Set.map mod' mu'
    m = t /\ q''
    isSymmetry f = Set.map f m == m

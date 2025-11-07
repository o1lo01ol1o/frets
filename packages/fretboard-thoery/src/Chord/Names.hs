{-# LANGUAGE DerivingStrategies #-}

module Chord.Names
  ( ChordName (..)
  , chordName
  , chordNameFromPitchClasses
  )
where

import Chord (Chord, chordToChromatics)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set

-- | Canonical label and alternate spellings for a chord class.
data ChordName = ChordName
  { chordPrimary :: String
  , chordAliases :: [String]
  }
  deriving stock (Eq, Show)

newtype PrimeForm = PrimeForm {unPrimeForm :: [Int]}
  deriving stock (Eq, Ord, Show)

-- | Attempt to identify a chord using the manually transcribed music21 tables.
chordName :: Chord -> Maybe ChordName
chordName chord =
  let pcs = chordPitchClasses chord
   in resolve pcs

-- | Resolve a chord name directly from a list of pitch-class integers.
chordNameFromPitchClasses :: [Int] -> Maybe ChordName
chordNameFromPitchClasses =
  resolve . List.sort . List.nub . fmap mod12

resolve :: [Int] -> Maybe ChordName
resolve pcs =
  let candidates = primeFormsFor pcs
   in listToMaybe $ mapMaybe (`Map.lookup` music21ChordTable) candidates

chordPitchClasses :: Chord -> [Int]
chordPitchClasses =
  map fromEnum . Set.toAscList . chordToChromatics

primeFormsFor :: [Int] -> [PrimeForm]
primeFormsFor [] = []
primeFormsFor pcs =
  let normalised = map normalise (rotations sorted)
      inversions = map invert normalised
   in List.nub $ map PrimeForm (normalised ++ inversions)
 where
  sorted = List.sort (map mod12 pcs)

rotations :: [a] -> [[a]]
rotations [] = []
rotations xs = take (length xs) (iterate rotateOnce xs)
 where
  rotateOnce [] = []
  rotateOnce (y : ys) = ys ++ [y]

normalise :: [Int] -> [Int]
normalise [] = []
normalise (x : xs) =
  let shift = x
   in map (mod12 . subtract shift) (x : xs)

invert :: [Int] -> [Int]
invert xs =
  let inverted = reverse (map (mod12 . (12 -)) xs)
   in normalise inverted

mod12 :: Int -> Int
mod12 n =
  let r = n `mod` 12
   in if r < 0 then r + 12 else r

music21ChordTable :: Map PrimeForm ChordName
music21ChordTable = Map.fromList music21ChordEntries

music21ChordEntries :: [(PrimeForm, ChordName)]
music21ChordEntries =
  [
  entry [0] "unison" ["monad", "singleton"],
  entry [0, 1] "interval class 1" ["minor second", "m2", "half step", "semitone"],
  entry [0, 2] "interval class 2" ["major second", "M2", "whole step", "whole tone"],
  entry [0, 3] "interval class 3" ["minor third", "m3"],
  entry [0, 4] "interval class 4" ["major third", "M3"],
  entry [0, 5] "interval class 5" ["perfect fourth", "P4"],
  entry [0, 6] "tritone" ["diminished fifth", "augmented fourth"],
  entry [0, 1, 2] "chromatic trimirror" [],
  entry [0, 2, 3] "minor trichord" [],
  entry [0, 1, 3] "phrygian trichord" [],
  entry [0, 3, 4] "major-minor trichord" [],
  entry [0, 1, 4] "major-minor trichord" [],
  entry [0, 4, 5] "incomplete major-seventh chord" [],
  entry [0, 1, 5] "incomplete major-seventh chord" [],
  entry [0, 5, 6] "tritone-fourth" [],
  entry [0, 1, 6] "tritone-fourth" [],
  entry [0, 2, 4] "whole-tone trichord" [],
  entry [0, 3, 5] "incomplete dominant-seventh chord" [],
  entry [0, 2, 5] "incomplete minor-seventh chord" [],
  entry [0, 4, 6] "incomplete half-diminished seventh chord" [],
  entry [0, 2, 6] "incomplete dominant-seventh chord" ["Italian augmented sixth chord"],
  entry [0, 2, 7] "quartal trichord" [],
  entry [0, 3, 6] "diminished triad" [],
  entry [0, 4, 7] "major triad" [],
  entry [0, 3, 7] "minor triad" [],
  entry [0, 4, 8] "augmented triad" ["equal 3-part octave division"],
  entry [0, 1, 2, 3] "chromatic tetramirror" ["BACH"],
  entry [0, 2, 3, 4] "major-second tetracluster" [],
  entry [0, 1, 2, 4] "major-second tetracluster" [],
  entry [0, 1, 3, 4] "alternating tetramirror" [],
  entry [0, 3, 4, 5] "minor third tetracluster" [],
  entry [0, 1, 2, 5] "minor third tetracluster" [],
  entry [0, 4, 5, 6] "major third tetracluster" [],
  entry [0, 1, 2, 6] "major third tetracluster" [],
  entry [0, 1, 2, 7] "perfect fourth tetramirror" [],
  entry [0, 1, 4, 5] "Arabian tetramirror" [],
  entry [0, 1, 5, 6] "double-fourth tetramirror" [],
  entry [0, 1, 6, 7] "double tritone tetramirror" [],
  entry [0, 2, 3, 5] "minor tetramirror" [],
  entry [0, 2, 4, 5] "lydian tetrachord" ["major tetrachord"],
  entry [0, 1, 3, 5] "phrygian tetrachord" [],
  entry [0, 3, 4, 6] "major-third diminished tetrachord" [],
  entry [0, 2, 3, 6] "harmonic minor tetrachord" [],
  entry [0, 3, 5, 6] "perfect-fourth diminished tetrachord" [],
  entry [0, 1, 3, 6] "minor-second diminished tetrachord" [],
  entry [0, 4, 5, 7] "perfect-fourth major tetrachord" [],
  entry [0, 2, 3, 7] "major-second minor tetrachord" [],
  entry [0, 2, 5, 6] "all-interval tetrachord" [],
  entry [0, 1, 4, 6] "all-interval tetrachord" [],
  entry [0, 2, 6, 7] "tritone quartal tetrachord" [],
  entry [0, 1, 5, 7] "minor-second quartal tetrachord" [],
  entry [0, 3, 4, 7] "major-minor tetramirror" [],
  entry [0, 3, 6, 7] "minor-diminished tetrachord" [],
  entry [0, 1, 4, 7] "major-diminished tetrachord" [],
  entry [0, 3, 4, 8] "augmented major tetrachord" [],
  entry [0, 1, 4, 8] "minor-augmented tetrachord" [],
  entry [0, 1, 5, 8] "major seventh chord" [],
  entry [0, 2, 4, 6] "whole-tone tetramirror" [],
  entry [0, 3, 5, 7] "perfect-fourth minor tetrachord" [],
  entry [0, 2, 4, 7] "major-second major tetrachord" [],
  entry [0, 2, 5, 7] "quartal tetramirror" [],
  entry [0, 2, 4, 8] "augmented seventh chord" [],
  entry [0, 2, 6, 8] "Messiaen's truncated mode 6" ["French augmented sixth chord"],
  entry [0, 3, 5, 8] "minor seventh chord" [],
  entry [0, 3, 6, 8] "dominant seventh chord" ["major minor seventh chord", "German augmented sixth chord", "Swiss augmented sixth chord"],
  entry [0, 2, 5, 8] "half-diminished seventh chord" [],
  entry [0, 3, 6, 9] "diminished seventh chord" ["equal 4-part octave division"],
  entry [0, 4, 6, 7] "all-interval tetrachord" [],
  entry [0, 1, 3, 7] "all-interval tetrachord" [],
  entry [0, 1, 2, 3, 4] "chromatic pentamirror" [],
  entry [0, 2, 3, 4, 5] "major-second pentacluster" [],
  entry [0, 1, 2, 3, 5] "major-second pentacluster" [],
  entry [0, 1, 3, 4, 5] "Spanish pentacluster" [],
  entry [0, 1, 2, 4, 5] "minor-second major pentachord" [],
  entry [0, 3, 4, 5, 6] "minor-third pentacluster" [],
  entry [0, 1, 2, 3, 6] "blues pentacluster" [],
  entry [0, 4, 5, 6, 7] "major-third pentacluster" [],
  entry [0, 1, 2, 3, 7] "major-third pentacluster" [],
  entry [0, 1, 4, 5, 6] "Asian pentacluster" [],
  entry [0, 1, 2, 5, 6] "Asian pentacluster" ["quasi raga Megharanji"],
  entry [0, 1, 5, 6, 7] "double pentacluster" [],
  entry [0, 1, 2, 6, 7] "double pentacluster" ["quasi raga Nabhomani"],
  entry [0, 2, 3, 4, 6] "tritone-symmetric pentamirror" [],
  entry [0, 2, 4, 5, 6] "tritone-contracting pentachord" [],
  entry [0, 1, 2, 4, 6] "tritone-expanding pentachord" [],
  entry [0, 2, 3, 5, 6] "alternating pentachord" [],
  entry [0, 1, 3, 4, 6] "alternating pentachord" [],
  entry [0, 3, 4, 5, 7] "center-cluster pentachord" [],
  entry [0, 2, 3, 4, 7] "center-cluster pentachord" [],
  entry [0, 1, 3, 5, 6] "locrian pentachord" [],
  entry [0, 2, 3, 4, 8] "augmented pentacluster" [],
  entry [0, 1, 2, 4, 8] "augmented pentacluster" [],
  entry [0, 2, 5, 6, 7] "double-seconds triple-fourth pentachord" [],
  entry [0, 1, 2, 5, 7] "double-seconds triple-fourth pentachord" [],
  entry [0, 1, 2, 6, 8] "asymmetric pentamirror" [],
  entry [0, 3, 4, 6, 7] "major-minor diminished pentachord" [],
  entry [0, 1, 3, 4, 7] "major-minor-diminished pentachord" [],
  entry [0, 1, 3, 4, 8] "minor-major ninth chord" [],
  entry [0, 2, 3, 6, 7] "Roma (Gypsy) pentachord" [],
  entry [0, 1, 4, 5, 7] "Roma (Gypsy) pentachord" [],
  entry [0, 1, 4, 6, 7] "Balinese pentachord" [],
  entry [0, 1, 3, 6, 7] "Javanese pentachord" [],
  entry [0, 1, 5, 7, 8] "Hirajoshi pentatonic" ["Iwato", "Sakura", "quasi raga Saveri"],
  entry [0, 1, 3, 7, 8] "Balinese Pelog pentatonic" ["quasi raga Bhupala", "quasi raga Bibhas"],
  entry [0, 3, 4, 7, 8] "Lebanese pentachord" ["augmented-minor chord"],
  entry [0, 1, 4, 5, 8] "major-augmented ninth chord" ["Syrian pentatonic", "quasi raga Megharanji"],
  entry [0, 1, 4, 7, 8] "Persian pentamirror" ["quasi raga Ramkali"],
  entry [0, 2, 4, 5, 7] "major pentachord" [],
  entry [0, 2, 3, 5, 7] "dorian pentachord" ["minor pentachord"],
  entry [0, 2, 4, 6, 7] "lydian pentachord" [],
  entry [0, 1, 3, 5, 7] "phrygian pentachord" [],
  entry [0, 3, 5, 6, 8] "minor-diminished ninth chord" [],
  entry [0, 2, 3, 5, 8] "diminished-major ninth chord" [],
  entry [0, 3, 4, 6, 8] "augmented-diminished ninth chord" [],
  entry [0, 2, 4, 5, 8] "diminished-augmented ninth chord" [],
  entry [0, 3, 5, 7, 8] "minor-ninth chord" [],
  entry [0, 1, 3, 5, 8] "major-ninth chord" [],
  entry [0, 2, 5, 6, 8] "Javanese pentatonic" ["augmented-sixth pentachord"],
  entry [0, 2, 3, 6, 8] "augmented-sixth pentachord" [],
  entry [0, 2, 5, 7, 8] "Kumoi pentachord" [],
  entry [0, 1, 3, 6, 8] "Kumoi pentachord" [],
  entry [0, 2, 4, 7, 8] "enigmatic pentachord" ["altered pentatonic"],
  entry [0, 1, 4, 6, 8] "enigmatic pentachord" [],
  entry [0, 2, 3, 6, 9] "flat-ninth pentachord" ["quasi raga Ranjaniraga"],
  entry [0, 1, 3, 6, 9] "diminished minor-ninth chord" [],
  entry [0, 1, 4, 7, 9] "Neapolitan pentachord" [],
  entry [0, 1, 4, 6, 9] "Neapolitan pentachord" [],
  entry [0, 2, 4, 6, 8] "whole-tone pentachord" [],
  entry [0, 2, 4, 6, 9] "dominant-ninth" ["major-minor", "Prometheus pentamirror", "dominant pentatonic"],
  entry [0, 2, 4, 7, 9] "major pentatonic" ["black-key scale", "blues pentatonic", "slendro", "quartal pentamirror"],
  entry [0, 3, 5, 6, 7] "minor-seventh pentacluster" [],
  entry [0, 1, 2, 4, 7] "major-seventh pentacluster" [],
  entry [0, 3, 4, 5, 8] "center-cluster pentamirror" [],
  entry [0, 3, 6, 7, 8] "diminished pentacluster" [],
  entry [0, 1, 2, 5, 8] "diminished pentacluster" [],
  entry [0, 1, 2, 3, 4, 5] "A all combinatorial (P6, I11, RI5, RI11)" ["chromatic hexamirror", "first-order all-combinatorial"],
  entry [0, 2, 3, 4, 5, 6] "combinatorial I (I1)" [],
  entry [0, 1, 2, 3, 4, 6] "combinatorial I (I11)" [],
  entry [0, 1, 2, 4, 5, 6] "combinatorial RI (RI6)" [],
  entry [0, 1, 4, 5, 6, 7] "combinatorial I (I3)" [],
  entry [0, 1, 2, 3, 6, 7] "combinatorial I (I11)" [],
  entry [0, 1, 2, 5, 6, 7] "double cluster hexamirror" [],
  entry [0, 1, 2, 6, 7, 8] "B all combinatorial (P3, P9, I5, R6, R12, R8)" ["Messiaen's mode 5", "second-order all combinatorial"],
  entry [0, 2, 3, 4, 5, 7] "D all combinatorial (P6, I1, RI7)" [],
  entry [0, 2, 4, 5, 6, 7] "combinatorial I (I3)" [],
  entry [0, 1, 2, 3, 5, 7] "combinatorial I (I11)" [],
  entry [0, 1, 3, 4, 6, 7] "alternating hexamirror" ["combinatorial I (I7)"],
  entry [0, 3, 4, 5, 7, 8] "combinatorial P (P6)" [],
  entry [0, 1, 3, 4, 5, 8] "combinatorial P (P6)" [],
  entry [0, 3, 4, 6, 7, 8] "combinatorial I (I5)" [],
  entry [0, 1, 2, 4, 5, 8] "combinatorial I (I11)" [],
  entry [0, 2, 3, 4, 7, 8] "combinatorial I (I1)" ["quasi raga Megha"],
  entry [0, 1, 4, 5, 6, 8] "combinatorial I (I3)" [],
  entry [0, 1, 4, 6, 7, 8] "all tri-chord hexachord (inverted form)" [],
  entry [0, 1, 2, 4, 7, 8] "all tri-chord hexachord" [],
  entry [0, 1, 3, 6, 7, 8] "combinatorial I (I5)" [],
  entry [0, 1, 2, 5, 7, 8] "combinatorial I (I11)" [],
  entry [0, 1, 4, 5, 8, 9] "E all combinatorial (P2, P6, P10, I3, I7, R4, R8, RI1, RI5, RI9)" ["Messiaen's truncated mode 3", "Genus tertium", "third-order all combinatorial"],
  entry [0, 2, 4, 5, 6, 8] "combinatorial I (I3)" [],
  entry [0, 2, 3, 4, 6, 8] "combinatorial I (I1)" [],
  entry [0, 2, 4, 6, 7, 8] "combinatorial I (I5)" [],
  entry [0, 1, 2, 4, 6, 8] "combinatorial I (I11)" [],
  entry [0, 2, 3, 5, 6, 8] "combinatorial RI (RI8)" ["super-locrian hexamirror"],
  entry [0, 2, 4, 5, 7, 8] "melodic-minor hexachord" [],
  entry [0, 2, 3, 5, 7, 8] "minor hexachord" [],
  entry [0, 1, 3, 5, 6, 8] "locrian hexachord" [],
  entry [0, 1, 3, 5, 7, 8] "phrygian hexamirror" ["combinatorial RI (RI8)"],
  entry [0, 2, 3, 5, 6, 9] "combinatorial I (I1)" ["pyramid hexachord"],
  entry [0, 1, 3, 4, 6, 9] "combinatorial I (I11)" [],
  entry [0, 1, 3, 5, 6, 9] "double-phrygian hexachord" ["combinatorial RI (RI6)"],
  entry [0, 1, 3, 6, 8, 9] "combinatorial RI (RI9)" [],
  entry [0, 2, 3, 6, 8, 9] "Stravinsky's Petrushka-chord" ["Messiaen's truncated mode 2", "major-bitonal hexachord", "combinatorial R (R6)", "combinatorial I (I1, I7)"],
  entry [0, 1, 3, 6, 7, 9] "Messiaen's truncated mode 2" ["minor-bitonal hexachord", "combinatorial R (R6)", "combinatorial I (I1, I7)"],
  entry [0, 1, 4, 6, 8, 9] "combinatorial I (I11)" [],
  entry [0, 1, 3, 5, 8, 9] "combinatorial I (I7)" [],
  entry [0, 2, 4, 5, 7, 9] "Guidonian hexachord" ["C all combinatorial (P6, I3, RI9)", "major hexamirror", "quartal hexamirror", "first-order all combinatorial"],
  entry [0, 2, 4, 6, 7, 9] "dominant-eleventh" ["lydian hexachord", "combinatorial I (I1)"],
  entry [0, 2, 3, 5, 7, 9] "dorian hexachord" ["combinatorial I (I6)"],
  entry [0, 2, 4, 6, 8, 9] "augmented-eleventh" ["harmonic hexachord", "combinatorial I (I7)"],
  entry [0, 1, 3, 5, 7, 9] "Scriabin's Mystic-chord" ["Prometheus hexachord", "combinatorial I (I11)"],
  entry [0, 2, 4, 6, 8, 10] "whole tone scale" ["6 equal part division", "F all-combinatorial (P1, P3, P5, P7, P9, P11, I1, I3, I5, I7, I9, I11, R2, R4, R6, R8, R10, RI2, RI4, RI6, RI8, RI10)", "Messiaen's mode 1", "sixth-order all combinatorial"],
  entry [0, 1, 2, 3, 4, 8] "combinatorial RI (RI4)" [],
  entry [0, 1, 2, 3, 7, 8] "combinatorial RI (RI3)" [],
  entry [0, 1, 2, 3, 6, 9] "combinatorial RI (RI3)" [],
  entry [0, 2, 3, 6, 7, 8] "complement of all-tri-chord hexachord (inverted form)" [],
  entry [0, 1, 2, 5, 6, 8] "complement of all tri-chord hexachord" [],
  entry [0, 1, 2, 5, 8, 9] "quasi raga Bauli" [],
  entry [0, 1, 2, 5, 6, 9] "Schoenberg Anagram hexachord" [],
  entry [0, 2, 3, 4, 6, 9] "combinatorial RI (RI6)" [],
  entry [0, 2, 3, 4, 7, 9] "blues scale" [],
  entry [0, 1, 2, 5, 7, 9] "combinatorial RI (RI2)" [],
  entry [0, 1, 3, 4, 7, 9] "combinatorial RI (RI4)" ["Prometheus Neapolitan mode"],
  entry [0, 1, 4, 6, 7, 9] "combinatorial RI (RI1)" [],
  entry [0, 1, 2, 3, 4, 5, 6] "chromatic heptamirror" [],
  entry [0, 1, 2, 3, 5, 6, 9] "Debussy's heptatonic" [],
  entry [0, 1, 2, 5, 7, 8, 9] "Greek chromatic" ["chromatic mixolydian", "chromatic dorian", "quasi raga Pantuvarali", "mela Kanakangi"],
  entry [0, 1, 2, 4, 7, 8, 9] "chromatic phrygian inverse" [],
  entry [0, 1, 3, 4, 5, 8, 9] "Roma (Gypsy) hepatonic" [],
  entry [0, 1, 2, 5, 6, 8, 9] "double harmonic scale" ["major Roma (Gypsy)", "Hungarian minor", "double harmonic scale", "quasi raga Mayamdavagaula"],
  entry [0, 2, 4, 5, 6, 7, 9] "tritone major heptachord" [],
  entry [0, 2, 4, 6, 7, 8, 9] "mystic heptachord" ["Enigmatic heptatonic"],
  entry [0, 2, 4, 5, 7, 8, 9] "modified blues" [],
  entry [0, 1, 2, 4, 6, 8, 9] "Neapolitan-minor mode" [],
  entry [0, 2, 3, 5, 6, 8, 9] "diminished scale" ["alternating heptachord"],
  entry [0, 1, 3, 4, 6, 7, 9] "alternating heptachord" ["Hungarian major mode"],
  entry [0, 1, 3, 5, 6, 8, 9] "harmonic major scale (inverted)" ["harmonic minor collection (inverted)", "mela Cakravana", "quasi raga Ahir Bhairav"],
  entry [0, 1, 3, 4, 6, 8, 9] "harmonic minor scale" ["harmonic minor collection", "Spanish Roma (Gypsy)", "mela Kiravani"],
  entry [0, 1, 2, 4, 6, 8, 10] "Neapolitan-major mode" ["leading-whole-tone mode"],
  entry [0, 1, 3, 4, 6, 8, 10] "melodic minor ascending scale" ["jazz minor", "augmented thirteenth heptamirror", "harmonic/super-locrian"],
  entry [0, 1, 3, 5, 6, 8, 10] "major scale" ["major diatonic heptachord", "natural minor scale", "dominant thirteenth", "locrian", "phrygian", "major inverse"],
  entry [0, 1, 2, 3, 4, 5, 6, 7] "chromatic octamirror" [],
  entry [0, 1, 2, 3, 6, 7, 8, 9] "Messiaen's mode 4" [],
  entry [0, 2, 4, 5, 6, 7, 8, 9] "blues octatonic" [],
  entry [0, 1, 2, 3, 4, 6, 7, 9] "blues octatonic" [],
  entry [0, 1, 2, 4, 6, 7, 8, 9] "enigmatic octachord" [],
  entry [0, 1, 2, 3, 5, 7, 9, 10] "Spanish octatonic scale" [],
  entry [0, 1, 2, 3, 5, 7, 8, 10] "Greek" ["quartal octachord", "diatonic octad"],
  entry [0, 1, 2, 4, 6, 7, 8, 10] "Messiaen's mode 6" [],
  entry [0, 1, 2, 4, 5, 7, 9, 10] "Spanish phrygian" ["blues"],
  entry [0, 1, 3, 4, 6, 7, 9, 10] "octatonic scale" ["Messiaen's mode 2", "alternating octatonic scale", "diminished scale"],
  entry [0, 1, 2, 3, 4, 5, 6, 7, 8] "chromatic nonamirror" [],
  entry [0, 1, 2, 3, 4, 5, 7, 8, 10] "nonatonic blues" [],
  entry [0, 1, 2, 3, 5, 6, 8, 9, 10] "diminishing nonachord" [],
  entry [0, 1, 2, 4, 5, 6, 8, 9, 10] "Messiaen's mode 3" ["Tsjerepnin"],
  entry [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] "chromatic decamirror" [],
  entry [0, 1, 2, 3, 4, 5, 7, 8, 9, 10] "major-minor mixed" [],
  entry [0, 1, 2, 3, 4, 6, 7, 8, 9, 10] "Messiaen's mode 7" [],
  entry [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] "chromatic undecamirror" [],
  entry [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] "aggregate" ["dodecachord", "twelve-tone chromatic", "chromatic scale", "dodecamirror"]
  ]

entry :: [Int] -> String -> [String] -> (PrimeForm, ChordName)
entry form primary aliases =
  (PrimeForm form, ChordName primary aliases)

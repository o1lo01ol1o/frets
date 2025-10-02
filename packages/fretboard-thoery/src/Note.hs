{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Note
  ( Note (..),
    noteNumber,
    noteEnd
  )
where

import Control.DeepSeq (NFData)
import Data.Mod (Mod, unMod)
import GHC.Generics (Generic)

-- | A concrete pitch with temporal placement.
-- Pitch classes live in @Mod 12@ so transposition
-- and interval arithmetic remain modular by construction.
data Note = Note
  { notePitchClass :: Mod 12,
    noteOctave :: !Int,
    noteOnset :: !Rational,
    noteDuration :: !Rational
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Convert a note to an absolute chromatic index (MIDI-style numbering).
-- The octave is treated as a multiple of 12 semitones with the modular pitch
-- class added on top.
noteNumber :: Note -> Int
noteNumber (Note pc octave _ _) =
  let pitchClass = fromIntegral (unMod pc)
   in octave * 12 + pitchClass

-- | Compute the ending onset for the note.
noteEnd :: Note -> Rational
noteEnd (Note _ _ onset duration) = onset + duration

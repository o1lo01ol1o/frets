module Main where

import qualified Fretboard
import Modulation
import Options.Applicative

main :: IO ()
main = undefined

-- | The CLI options.  We take a Fretboard.KnownTuning, KnownHeptatonicScale,
-- an integer mod 12 and a list of Accidental Degrees as options.
data Options = Options
  { tuning :: Fretboard.KnownGuitarTunings,
    scale :: KnownHeptatonicScale,
    mod12 :: Int,
    accidentals :: [Degree]
  }

-- | The parser for the CLI options.
optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      auto
      ( long "tuning"
          <> short 't'
          <> value Fretboard.Standard
          <> metavar "TUNING"
          <> help "The tuning of the guitar"
      )
    <*> option
      auto
      ( long "scale"
          <> short 's'
          <> value Ionian
          <> metavar "SCALE"
          <> help "The scale to use (root at c)"
      )
    <*> option
      auto
      ( long "mod"
          <> short 'm'
          <> value 0
          <> metavar "MOD"
          <> help "The mod 12 to offset the scale by"
      )
    <*> many
      ( option
          auto
          ( long "Scale degrees and their accidentals"
              <> short 'a'
              <> value I
              <> metavar "ACCIDENTAL"
              <> help "The accidental to use"
          )
      )

-- compute the optimal fretting for the given scale and tuning
-- and draw it to the console
drawFretboard :: Options -> IO ()
drawFretboard (Options t s m a) = do
  let fretboard = Fretboard.fretboard t
  let scale = modulate m s
  let frets = Fretboard.fretNotes fretboard scale
  let frets' = map (map (show . Fretboard.noteName)) frets
  mapM_ putStrLn frets'

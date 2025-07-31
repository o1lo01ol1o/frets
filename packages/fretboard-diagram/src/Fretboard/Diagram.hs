{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fretboard.Diagram (FretboardConfig (..), main) where

import Chord ()
import Data.Colour.Names
import Data.List (scanl')
import Data.Mod
import qualified Data.Set as Set
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (scale)
import Diagrams.TwoD.Offset
import qualified Finger
import Fretboard hiding (drawScale)
import Fretboard.Color (interpolateViridis)
import Modulation hiding (drawScale)

-- | Configuration for the fretboard diagram
data FretboardConfig = FretboardConfig
  { fbWidth :: Double,
    fbHeight :: Double,
    fbFontSize :: Double,
    fbNumFrets :: Int,
    fbTuning :: Fretboard,
    fbOctaveDenominator :: Int,
    fbStringThicknesses :: [Double]
  }

-- Helper function to calculate fret positions
calcFretPositions :: FretboardConfig -> [Double]
calcFretPositions config = fmap (/ last propotions) propotions
  where
    propotions =
      (1 -)
        <$> scanl'
          ( \acc _ ->
              acc
                / (2 ** (1 / fromIntegral (fbOctaveDenominator config)))
          )
          1
          [0 .. fbNumFrets config - 1]

stringSpacing :: FretboardConfig -> Double
stringSpacing config =
  fbHeight config
    / ( fromIntegral
          (length $ fbStringThicknesses config)
          - 1
      )

fretX :: FretboardConfig -> Int -> Double
fretX config fret = (calcFretPositions config !! fret) * fbWidth config

-- Helper function to draw a single fret
drawFret :: Double -> Double -> Double -> Diagram B
drawFret height' xPos width' =
  p
    # stroke
    # lw none
    <> e # strokePath # lw none # fillTexture radialGradient
  where
    p = fromVertices . map p2 $ [(xPos, -height' / 2), (xPos, height' / 2)]
    e = expandTrail' opts (width' * 6) p
    opts =
      with
        & expandJoin .~ LineJoinRound
        & expandCap .~ LineCapRound
    radialGradient =
      mkRadialGradient
        (mkStops [(white, 0, 1), (black, 1, 1)])
        ((-(width' / 2)) ^& (0.001))
        width'
        (0 ^& (-height' / 2))
        (width' * 3)
        GradPad

-- Draw a basic fretboard
drawFretboard :: FretboardConfig -> Diagram B
drawFretboard config =
  drawFretLabels config
    <> (strings # translateY (fbHeight config / 2))
    <> (frets' # translateX (fbWidth config / 2))
    <> ( rect (fbWidth config) (fbHeight config)
           # lw thin
       )
  where
    strings =
      vcat'
        (with & sep .~ stringSpacing config)
        ( zipWith
            ( \thickness i ->
                hrule (fbWidth config)
                  # lw (normalized thickness `atLeast` output 0.5)
                  # named ("string" ++ show i)
            )
            (fbStringThicknesses config)
            [0 :: Int ..]
        )
        # named "strings"
    frets' =
      mconcat
        [ drawFret (fbHeight config) (fretX config fret - fbWidth config) 0.3
          | fret <- [0 .. fbNumFrets config]
        ]
        # named "frets"

    drawFretLabels :: FretboardConfig -> Diagram B
    drawFretLabels config' =
      mconcat
        [ text (show fret)
            # fontSize (local (fbFontSize config' / 4))
            # moveTo
              ( p2
                  ( labelX fret,
                    -( fbHeight config'
                         / 2
                         + fbFontSize config' / 3
                     )
                  )
              )
            # translateX (-(fbWidth config' / 2))
          | fret <- [1 .. fbNumFrets config']
        ]
      where
        fretPositions = calcFretPositions config'
        labelX fret =
          let prevFret = fretPositions !! (fret - 1)
              currentFret = fretPositions !! fret
           in (prevFret + currentFret) / 2 * fbWidth config'

-- Draw a chord on the fretboard
drawChord :: FretboardConfig -> Fretting -> Diagram B
drawChord config (Fretting _ frets') =
  ( mconcat (drawFret' <$> Set.toList frets')
      # translateY (-(fbHeight config / 2))
      # translateX (-(fbWidth config / 2))
  )
    `atop` drawFretboard config
  where
    drawFret' (string, Just (finger, fret)) =
      drawNote (string, fret) (show finger)
    drawFret' (string, Nothing) =
      text "O"
        # fontSize (local (fbFontSize config))
        # fc black
        # moveTo
          (p2 (fretX config 0, fromIntegral string * stringSpacing config))

    -- Draw a note on the fretboard
    drawNote :: (Int, Int) -> String -> Diagram B
    drawNote (string, fret) label =
      ( text label
          # fontSize (local (fbFontSize config / 6))
          # fc white
          <> circle 10 # fc red # lw none
      )
        # moveTo (p2 (fretX config fret, fromIntegral string * stringSpacing config))

degreeColor :: Int -> Int -> Colour Double
degreeColor degree' numDegrees =
  let (r, g, b) = interpolateViridis (fromIntegral degree' / fromIntegral numDegrees)
   in sRGB r g b

scaleNoteText :: FretboardConfig -> Int -> String -> Diagram B
scaleNoteText config degree' note =
  text (note <> " (" <> show (degree' + 1) <> ")")
    # fontSize (local (fbFontSize config / 6))
    # fc white

drawScale :: FretboardConfig -> HeptatonicScale (Mod 12) -> Diagram B
drawScale config heptatonicScale =
  mconcat (map drawScaleNote scaleNotes) <> drawFretboard config
  where
    scaleNotes =
      Set.toList $
        fretScale
          (fbTuning config)
          (fmap toLocalInterpretation heptatonicScale)
          (fbOctaveDenominator config)
    numDegrees = length heptatonicScale

    drawScaleNote ((string, fret), (degree', note)) =
      ( scaleNoteText config (fromIntegral $ unMod degree') (show note)
          <> circle 10 # fc (degreeColor (fromIntegral $ unMod degree') numDegrees) # lw none
      )
        # moveTo (p2 (fretX config fret, fromIntegral string * stringSpacing config))
        # translateX (-(fbWidth config / 2))
        # translateY (-(fbHeight config / 2))

-- Helper function to render the diagram as SVG
renderFretboardSVG :: Diagram B -> FilePath -> IO ()
renderFretboardSVG diagram filename =
  renderSVG filename (mkWidth 800) diagram

-- Example usage functions
exampleChord :: IO ()
exampleChord = do
  let config =
        FretboardConfig
          { fbWidth = 1400,
            fbHeight = 300,
            fbFontSize = 48,
            fbNumFrets = 14,
            fbTuning = knownTuning Standard,
            fbOctaveDenominator = 12,
            fbStringThicknesses = (/ 1000) <$> [1 .. 6] -- Thickest to thinnest
          }
      chord' =
        Fretting
          (knownTuning Standard)
          ( Set.fromList
              [ (0, Just (Finger.Index, 1)),
                (1, Just (Finger.Middle, 2)),
                (2, Just (Finger.Ring, 3)),
                (3, Nothing),
                (4, Nothing),
                (5, Just (Finger.Pinky, 1))
              ]
          )
  renderFretboardSVG (drawChord config chord') "example_chord.svg"

exampleScale :: IO ()
exampleScale = do
  let config =
        FretboardConfig
          { fbWidth = 1400,
            fbHeight = 300,
            fbFontSize = 48,
            fbNumFrets = 14,
            fbTuning = knownTuning Standard,
            fbOctaveDenominator = 12,
            fbStringThicknesses = (/ 1000) <$> [1 .. 6] -- Thickest to thinnest
          }
      scale' = fmap (+ (-4)) cDorian
  renderFretboardSVG (drawScale config scale') "example_scale.svg"

-- Main function to generate example diagrams
main :: IO ()
main = do
  exampleChord
  exampleScale

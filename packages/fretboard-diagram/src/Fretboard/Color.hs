{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Fretboard.Color
  ( interpolateViridis,
    interpolateInferno,
    interpolateMagma,
    interpolatePlasma,
  )
where

import Data.Function ((&))
import qualified Data.Vector as V
import Graphics.Color.Illuminant.Wikipedia as W
import Graphics.Color.Model (Color)
import Graphics.Color.Space
  ( ColorModel (fromComponents, toComponents),
    ColorSpace (..),
    Linearity (..),
  )
import Graphics.Color.Space.CIE1931.XYZ
import Graphics.Color.Space.DIN99
import Graphics.Color.Space.RGB.Derived.SRGB

pointToRGB :: (Double, Double, Double) -> (Double, Double, Double)
pointToRGB (x, y, z) =
  let din99Color = ColorDIN99 x y z :: Color (DIN99 'W.D65) Double
      xyzColor = toColorXYZ din99Color :: Color (XYZ 'W.D65) Double
      rgbColor = fromColorXYZ xyzColor :: Color (SRGB 'W.D65 'NonLinear) Double
   in toComponents rgbColor

type Point3D = (Double, Double, Double)

type Spline = V.Vector (Double, Point3D, Point3D, Point3D, Point3D)

-- Calculate the coefficients for the cubic spline
calculateSplineCoefficients :: V.Vector Point3D -> Spline
calculateSplineCoefficients points
  | V.length points < 2 = error "At least two points are required for interpolation"
  | otherwise = V.generate (V.length points - 1) calculateSegment
  where
    n = V.length points - 1

    calculateSegment i =
      let p0 = points V.! i
          p1 = points V.! (i + 1)
          t = fromIntegral i
          m0 = if i == 0 then (0, 0, 0) else tangent (points V.! (i - 1)) p0 p1
          m1 = if i == n - 1 then (0, 0, 0) else tangent p0 p1 (points V.! (i + 2))
       in (t, p0, p1, m0, m1)

    tangent p0 p1 p2 =
      let (x0, y0, z0) = p0
          (x1, y1, z1) = p1
          (x2, y2, z2) = p2
       in ((x2 - x0) / 2, (y2 - y0) / 2, (z2 - z0) / 2)

-- Interpolate a point on the spline
interpolate :: Spline -> Double -> Point3D
interpolate spline t
  | t < 0 = V.head spline & \(_, p, _, _, _) -> p
  | t >= fromIntegral (V.length spline) = V.last spline & \(_, _, p, _, _) -> p
  | otherwise =
      let i = floor t
          (t0, p0, p1, m0, m1) = spline V.! i
          h = t - t0
          h2 = h * h
          h3 = h2 * h
       in hermite p0 p1 m0 m1 h h2 h3

-- Hermite interpolation
hermite :: Point3D -> Point3D -> Point3D -> Point3D -> Double -> Double -> Double -> Point3D
hermite (x0, y0, z0) (x1, y1, z1) (mx0, my0, mz0) (mx1, my1, mz1) h h2 h3 =
  let f1 = 2 * h3 - 3 * h2 + 1
      f2 = -2 * h3 + 3 * h2
      f3 = h3 - 2 * h2 + h
      f4 = h3 - h2
   in ( f1 * x0 + f2 * x1 + f3 * mx0 + f4 * mx1,
        f1 * y0 + f2 * y1 + f3 * my0 + f4 * my1,
        f1 * z0 + f2 * z1 + f3 * mz0 + f4 * mz1
      )

-- Convert RGB to DIN99
rgbToDIN99 :: (Double, Double, Double) -> Point3D
rgbToDIN99 (r, g, b) =
  let srgbColor = fromComponents (r, g, b) :: Color (SRGB 'W.D65 'NonLinear) Double
      xyzColor = toColorXYZ srgbColor :: Color (XYZ 'W.D65) Double
      din99Color = fromColorXYZ xyzColor :: Color (DIN99 'W.D65) Double
   in toComponents din99Color

-- Interpolate color for a given t
interpolateViridis :: Double -> (Double, Double, Double)
interpolateViridis t =
  let din99Point = interpolate din99Spline (t * fromIntegral (V.length din99Spline))
   in pointToRGB din99Point
  where
    -- Create the DIN99 spline
    din99Spline :: Spline
    din99Spline = calculateSplineCoefficients (V.fromList (map rgbToDIN99 viridisPalette))
    -- Define the RGB palette
    viridisPalette :: [(Double, Double, Double)]
    viridisPalette =
      [ (253 / 255, 231 / 255, 37 / 255),
        (181 / 255, 222 / 255, 43 / 255),
        (110 / 255, 206 / 255, 88 / 255),
        (53 / 255, 183 / 255, 121 / 255),
        (31 / 255, 158 / 255, 137 / 255),
        (38 / 255, 130 / 255, 142 / 255),
        (49 / 255, 104 / 255, 142 / 255),
        (62 / 255, 73 / 255, 137 / 255),
        (72 / 255, 40 / 255, 120 / 255),
        (68 / 255, 1 / 255, 84 / 255)
      ]

interpolateInferno :: Double -> (Double, Double, Double)
interpolateInferno t =
  let din99Point = interpolate din99Spline (t * fromIntegral (V.length din99Spline))
   in pointToRGB din99Point
  where
    din99Spline = calculateSplineCoefficients (V.fromList (map rgbToDIN99 infernoPalette))
    infernoPalette =
      [ (252 / 255, 255 / 255, 164 / 255),
        (247 / 255, 209 / 255, 61 / 255),
        (251 / 255, 155 / 255, 6 / 255),
        (237 / 255, 105 / 255, 37 / 255),
        (207 / 255, 68 / 255, 70 / 255),
        (165 / 255, 44 / 255, 96 / 255),
        (120 / 255, 28 / 255, 109 / 255),
        (74 / 255, 12 / 255, 107 / 255),
        (27 / 255, 12 / 255, 65 / 255),
        (0 / 255, 0 / 255, 4 / 255)
      ]

interpolateMagma :: Double -> (Double, Double, Double)
interpolateMagma t =
  let din99Point = interpolate din99Spline (t * fromIntegral (V.length din99Spline))
   in pointToRGB din99Point
  where
    din99Spline = calculateSplineCoefficients (V.fromList (map rgbToDIN99 magmaPalette))
    magmaPalette =
      [ (252 / 255, 253 / 255, 191 / 255),
        (254 / 255, 202 / 255, 141 / 255),
        (253 / 255, 150 / 255, 104 / 255),
        (241 / 255, 96 / 255, 93 / 255),
        (205 / 255, 64 / 255, 113 / 255),
        (158 / 255, 47 / 255, 127 / 255),
        (114 / 255, 31 / 255, 129 / 255),
        (68 / 255, 15 / 255, 118 / 255),
        (24 / 255, 15 / 255, 61 / 255),
        (0 / 255, 0 / 255, 4 / 255)
      ]

interpolatePlasma :: Double -> (Double, Double, Double)
interpolatePlasma t =
  let din99Point = interpolate din99Spline (t * fromIntegral (V.length din99Spline))
   in pointToRGB din99Point
  where
    din99Spline = calculateSplineCoefficients (V.fromList (map rgbToDIN99 plasmaPalette))
    plasmaPalette =
      [ (240 / 255, 249 / 255, 33 / 255),
        (253 / 255, 202 / 255, 38 / 255),
        (251 / 255, 159 / 255, 58 / 255),
        (237 / 255, 121 / 255, 83 / 255),
        (216 / 255, 87 / 255, 107 / 255),
        (189 / 255, 55 / 255, 134 / 255),
        (156 / 255, 23 / 255, 158 / 255),
        (114 / 255, 1 / 255, 168 / 255),
        (70 / 255, 3 / 255, 159 / 255),
        (13 / 255, 8 / 255, 135 / 255)
      ]

{-# LANGUAGE OverloadedStrings #-}

module Data.Tonnetz.Geometry
  ( TonnetzPoint (..),
    TonnetzCoordinate (..),
    TonnetzFace (..),
    loadAmmannBeeknerGeometry,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecodeStrict',
    withArray,
    withObject,
    (.:),
  )
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Paths_fretboard_thoery (getDataFileName)

data TonnetzPoint = TonnetzPoint
  { tpX :: !Double,
    tpY :: !Double
  }
  deriving (Eq, Ord, Show)

data TonnetzCoordinate = TonnetzCoordinate
  { tcA :: !Int,
    tcB :: !Int,
    tcC :: !Int,
    tcD :: !Int
  }
  deriving (Eq, Ord, Show)

data TonnetzFace = TonnetzFace
  { tfFacePoints :: ![TonnetzPoint],
    tfVertexCoords :: ![TonnetzCoordinate]
  }
  deriving (Eq, Ord, Show)

instance FromJSON TonnetzPoint where
  parseJSON = withArray "TonnetzPoint" $ \arr ->
    case V.toList arr of
      [xVal, yVal] -> TonnetzPoint <$> parseJSON xVal <*> parseJSON yVal
      _ -> fail "TonnetzPoint expects exactly two numeric entries"

instance FromJSON TonnetzCoordinate where
  parseJSON = withArray "TonnetzCoordinate" $ \arr ->
    case V.toList arr of
      [aVal, bVal, cVal, dVal] ->
        TonnetzCoordinate
          <$> parseJSON aVal
          <*> parseJSON bVal
          <*> parseJSON cVal
          <*> parseJSON dVal
      _ -> fail "TonnetzCoordinate expects exactly four integral entries"

instance FromJSON TonnetzFace where
  parseJSON = withObject "TonnetzFace" $ \obj ->
    TonnetzFace
      <$> obj .: "faceVerts"
      <*> obj .: "vertexCoords"

loadAmmannBeeknerGeometry :: IO (Either String [TonnetzFace])
loadAmmannBeeknerGeometry = do
  path <- getDataFileName "data/tonnetz/ammann-beekner-geometry.json"
  contents <- BS.readFile path
  pure (eitherDecodeStrict' contents)

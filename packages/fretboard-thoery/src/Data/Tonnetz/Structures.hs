{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Tonnetz.Structures
  ( TonnetzStructure (..),
    allStructures,
    structureToText,
    structureFromText,
    intervalOptions,
  )
where

import Control.Monad (guard)
import Data.Mod (Mod, unMod)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sort)

data TonnetzStructure
  = TonnetzStructureClassical
  | TonnetzStructureTetrads
  deriving (Eq, Ord, Enum, Bounded, Show)

allStructures :: [TonnetzStructure]
allStructures = [minBound .. maxBound]

structureToText :: TonnetzStructure -> Text
structureToText TonnetzStructureClassical = "Classical"
structureToText TonnetzStructureTetrads = "Tetrads"

structureFromText :: Text -> Maybe TonnetzStructure
structureFromText txt =
  case T.toLower (T.strip txt) of
    "classical" -> Just TonnetzStructureClassical
    "tetrads" -> Just TonnetzStructureTetrads
    _ -> Nothing

intervalOptions ::
  TonnetzStructure ->
  [(Mod 7, Mod 7, Mod 7, Mod 7)]
intervalOptions TonnetzStructureClassical = classicalOptions
intervalOptions TonnetzStructureTetrads = tetradOptions

classicalOptions :: [(Mod 7, Mod 7, Mod 7, Mod 7)]
classicalOptions =
  collectUnique $ do
    a <- diatonicIntervals
    b <- diatonicIntervals
    c <- diatonicIntervals
    s <- diatonicIntervals
    let option = (a, b, c, s)
    guard (not (allEqual option))
    pure option
tetradOptions :: [(Mod 7, Mod 7, Mod 7, Mod 7)]
tetradOptions =
  collectUnique $ do
    a <- diatonicIntervals
    b <- diatonicIntervals
    c <- diatonicIntervals
    d <- diatonicIntervals
    let option = (a, b, c, d)
        signature = Set.fromList (fmap toInt [a, b, c, d])
    guard (signature == target)
    guard (not (allEqual option))
    pure option
  where
    target = Set.fromList [0, 2, 4, 6]

diatonicIntervals :: [Mod 7]
diatonicIntervals = [minBound .. maxBound]

toInt :: Mod 7 -> Int
toInt = fromIntegral . unMod

collectUnique :: [(Mod 7, Mod 7, Mod 7, Mod 7)] -> [(Mod 7, Mod 7, Mod 7, Mod 7)]
collectUnique =
  Map.elems
    . Map.fromListWith (\existing _ -> existing)
    . fmap (\opt -> (normalize opt, opt))
  where
    normalize (a, b, c, d) = sort (fmap toInt [a, b, c, d])

allEqual ::
  (Eq a) =>
  (a, a, a, a) ->
  Bool
allEqual (a, b, c, d) = a == b && b == c && c == d

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
import Data.List (nub, sort)

data TonnetzStructure
  = TonnetzStructureClassical
  | TonnetzStructureTetrads
  | TonnetzStructureGeneralizedChromatic
  | TonnetzStructureGeneralizedModal
  deriving (Eq, Ord, Enum, Bounded, Show)

allStructures :: [TonnetzStructure]
allStructures = [minBound .. maxBound]

structureToText :: TonnetzStructure -> Text
structureToText TonnetzStructureClassical = "Classical"
structureToText TonnetzStructureTetrads = "Tetrads"
structureToText TonnetzStructureGeneralizedChromatic = "Generalized (Chromatic)"
structureToText TonnetzStructureGeneralizedModal = "Generalized (Modal)"

structureFromText :: Text -> Maybe TonnetzStructure
structureFromText txt =
  case T.toLower (T.strip txt) of
    "classical" -> Just TonnetzStructureClassical
    "tetrads" -> Just TonnetzStructureTetrads
    "generalized (chromatic)" -> Just TonnetzStructureGeneralizedChromatic
    "generalized (modal)" -> Just TonnetzStructureGeneralizedModal
    _ -> Nothing

intervalOptions ::
  TonnetzStructure ->
  [[Int]]
intervalOptions TonnetzStructureClassical = fmap tupleToIntList classicalOptions
intervalOptions TonnetzStructureTetrads = fmap tupleToIntList tetradOptions
intervalOptions TonnetzStructureGeneralizedChromatic = generalizedChromaticOptions
intervalOptions TonnetzStructureGeneralizedModal = generalizedModalOptions

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

tupleToIntList :: (Mod 7, Mod 7, Mod 7, Mod 7) -> [Int]
tupleToIntList (a, b, c, d) = fmap toInt [a, b, c, d]

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

generalizedChromaticOptions :: [[Int]]
generalizedChromaticOptions =
  [ [1, 1, 10],
    [1, 2, 9],
    [1, 3, 8],
    [1, 4, 7],
    [1, 5, 6],
    [2, 2, 8],
    [2, 3, 7],
    [2, 4, 6],
    [2, 5, 5],
    [3, 4, 5],
    [3, 3, 6],
    [4, 4, 4]
  ]

generalizedModalOptions :: [[Int]]
generalizedModalOptions =
  nub (fmap (fmap (`mod` 7)) generalizedChromaticOptions)

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

module Finger where

import Chord
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Data.List (elemIndex, groupBy, nub, sort, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Mod (Mod, unMod)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Vector, generate, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.Fingerprint (Fingerprint (Fingerprint))
import GHC.Generics (Generic)
import Modulation

-- | Represents a finger on the fretting hand
data Finger = Thumb | Index | Middle | Ring | Pinky
  deriving stock (Eq, Show, Ord, Enum, Generic, Bounded)
  deriving anyclass (NFData)

instance Storable Finger where
  sizeOf _ = sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    intVal <- peek (castPtr ptr :: Ptr Int)
    return (toEnum intVal)
  poke ptr x = poke (castPtr ptr :: Ptr Int) (fromEnum x)

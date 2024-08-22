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

module Finger.TH where

import Chord
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Data.List (elemIndex, groupBy, nub, sort, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Mod (Mod, unMod)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import Finger
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import GHC.Word (Word8)

derivingUnbox
  "Finger"
  [t|Finger -> Word8|]
  [|((fromIntegral . fromEnum) :: Finger -> Word8)|]
  [|((toEnum . fromIntegral) :: Word8 -> Finger)|]

-- define a Storable instance for (Finger.TH.Vector Word8)
instance Storable (U.Vector Word8) where
  sizeOf _ = sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    intVal <- peek (castPtr ptr :: Ptr Int)
    return (V.fromList [fromIntegral intVal])
  poke ptr x = poke (castPtr ptr :: Ptr Int) (fromIntegral (V.head x))

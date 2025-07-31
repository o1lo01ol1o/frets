{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

module Data.Functor.Rix.Rep (rix) where

import Data.Functor.Rep (Representable (..))
import Prelude

-- | @Lens@ into 'Representable'.
--
-- @
-- 'rix' :: ('Representable' f, Eq ('Rep' f)) => 'Rep' f -> Lens' (f a) a
-- @
rix :: (Functor g, Representable f, Eq (Rep f)) => Rep f -> (a -> g a) -> f a -> g (f a)
rix i agb fa = setter <$> agb (index fa i)
  where
    setter b = tabulate $ \i' ->
      if i == i'
        then b
        else index fa i'
{-# INLINE rix #-}

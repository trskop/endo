-- |
-- Module:       $HEADER$
-- Description:  Utilities for Endo data type.
-- Copyright:    (c) 2013-2015 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Utilities for 'Endo' data type from "Data.Monoid" module.
module Data.Monoid.Endo
    (
    -- * Endo
      E
    , Endo(..)
    , runEndo
    , mapEndo
    , mapEndo2
    , liftEndo

    -- ** Lens
    , endo
    )
    where

import Data.Monoid (Endo(..))

import Data.Function.Between ((~@~), (<~@~))


-- | Type synonym for endomorphsm; it can be used simplify type signatures.
type E a = a -> a

-- | Transform function wrapped in 'Endo'.
mapEndo :: (E a -> E b) -> Endo a -> Endo b
mapEndo = Endo ~@~ appEndo
{-# INLINE mapEndo #-}

-- | Variation of 'mapEndo' for functions with arity two.
mapEndo2 :: (E a -> E b -> E c) -> Endo a -> Endo b -> Endo c
mapEndo2 = mapEndo ~@~ appEndo
{-# INLINE mapEndo2 #-}

-- | Apply 'fmap' to function wrapped in 'Endo'. It's a short hand for
-- @'mapEndo' 'fmap'@.
liftEndo :: Functor f => Endo a -> Endo (f a)
liftEndo (Endo f) = Endo (fmap f)
{-# INLINE liftEndo #-}

-- | Flipped version of 'appEndo'.
runEndo :: a -> Endo a -> a
runEndo x (Endo f) = f x
{-# INLINE runEndo #-}

-- | Lens for 'Endo'. In terms of /lens/ package it would have type:
--
-- > endo :: Lens (Endo a) (Endo b) (E a) (E b)
--
-- For details see <http://hackage.haskell.org/package/lens lens package>.
endo :: Functor f => (E a -> f (E b)) -> Endo a -> f (Endo b)
endo = Endo <~@~ appEndo
{-# INLINE endo #-}

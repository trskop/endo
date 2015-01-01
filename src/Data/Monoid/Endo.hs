{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:       $HEADER$
-- Description:  Utilities for Endo data type.
-- Copyright:    (c) 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, FlexibleContexts)
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
    , flipLiftEndo

    -- ** Lens
    , endo

    -- * Monoid
    , Monoid(..)
#if MIN_VERSION_base(4,5,0)
    , (<>)
#endif
    )
    where

import Data.Monoid
    ( Endo(..)
    , Monoid(..)
#if MIN_VERSION_base(4,5,0)
    , (<>)
#endif
    )

import Data.Function.Between (between)
import Data.Functor.FlipT (FlipT, flipmap)
import Data.Functor.Utils (iso)


-- | Type synonym for endomorphsm; it can be used simplify type signatures.
type E a = a -> a

-- | Transform function wrapped in 'Endo'.
mapEndo :: (E a -> E b) -> Endo a -> Endo b
mapEndo = Endo `between` appEndo
{-# INLINE mapEndo #-}

-- | Variation of 'mapEndo' for functions with arity two.
mapEndo2 :: (E a -> E b -> E c) -> Endo a -> Endo b -> Endo c
mapEndo2 = mapEndo `between` appEndo
{-# INLINE mapEndo2 #-}

-- | Apply 'fmap' to function wrapped in 'Endo'. It's a short hand for
-- @'mapEndo' 'fmap'@.
liftEndo :: Functor f => Endo a -> Endo (f a)
liftEndo (Endo f) = Endo (fmap f)
{-# INLINE liftEndo #-}

-- | Apply 'flipmap' to function wrapped in 'Endo'. It's a short hand for
-- @'mapEndo' 'flipmap'@.
flipLiftEndo :: Functor (FlipT f a) => Endo b -> Endo (f b a)
flipLiftEndo (Endo f) = Endo (flipmap f)
{-# INLINE flipLiftEndo #-}

-- | Flipped version of 'appEndo'.
runEndo :: a -> Endo a -> a
runEndo x (Endo f) = f x
{-# INLINE runEndo #-}

-- | Lens for 'Endo'. In terms of /lens/ package it would have type:
--
-- > endo :: Lens (Endo a) (Endo b) (E a) (E b)
endo :: Functor f => (E a -> f (E b)) -> Endo a -> f (Endo b)
endo = iso Endo appEndo
{-# INLINE endo #-}

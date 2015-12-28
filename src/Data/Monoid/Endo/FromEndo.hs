{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Convert endomorphism in to a value.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Convert endomorphism in to a value.
module Data.Monoid.Endo.FromEndo
    (
    -- * Convert Endo to a Value
      FromEndo(..)
    , fromEndoWith
    , fromDualEndoWith
    , fromEndoTo
    , fromDualEndoTo
    )
  where

import Control.Monad (Monad)
import Data.Function ((.), id)
import Data.Monoid (Dual(Dual, getDual), Endo(Endo, appEndo), Monoid)

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader as Reader (asks)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import Control.Monad.Trans.RWS.Lazy as Lazy.RWS (modify)
import Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.RWS.Strict as Strict.RWS (modify)
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Lazy as Lazy.State (modify)
import Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.State.Strict as Strict.State (modify)


-- | Type class provides functionality for converting @'Endo' b@ and @'Dual'
-- ('Endo' b)@ in to some type @a@. Type @b@, on which endomorphism operates,
-- is implied by type @a@, but generally aren't the same type. In other words
-- it is dual type class to 'Data.Monoid.Endo.AnEndo'.
class FromEndo a where
    type EndoOperatedOn a

    -- | Convert endomorphism in to a value of type @a@.
    fromEndo :: Endo (EndoOperatedOn a) -> a

    -- | Provided default implementation:
    --
    -- @
    -- 'fromDualEndo' = 'fromEndo' '.' 'getDual'
    -- @
    fromDualEndo :: Dual (Endo (EndoOperatedOn a)) -> a
    fromDualEndo = fromEndo . getDual

#ifdef HAVE_MINIMAL_PRAGMA
    {-# MINIMAL fromEndo #-}
#endif

instance FromEndo (a -> a) where
    type EndoOperatedOn (a -> a) = a

    fromEndo = appEndo
    fromDualEndo = appEndo . getDual

instance FromEndo (Endo a) where
    type EndoOperatedOn (Endo a) = a

    fromEndo = id
    fromDualEndo = getDual

instance FromEndo e => FromEndo (Dual e) where
    type EndoOperatedOn (Dual a) = EndoOperatedOn a

    fromEndo = Dual . fromEndo
    fromDualEndo = Dual . fromDualEndo

-- {{{ Transformers instances for FromEndo ------------------------------------

-- | Retrieve environment modified by endomorphism.
--
-- @
-- 'fromEndo' ('Endo' f) = 'Reader.asks' f
-- @
instance Monad f => FromEndo (ReaderT r f r) where
    type EndoOperatedOn (ReaderT r f r) = r

    fromEndo (Endo f) = Reader.asks f

-- | Modify state.
--
-- @
-- 'fromEndo' ('Endo' f) = 'Lazy.RWS.modify' f
-- @
instance (Monoid w, Monad f) => FromEndo (Lazy.RWST r w s f ()) where
    type EndoOperatedOn (Lazy.RWST r w s f ()) = s

    fromEndo (Endo f) = Lazy.RWS.modify f

-- | Modify state.
--
-- @
-- 'fromEndo' ('Endo' f) = 'Strict.RWS.modify' f
-- @
instance (Monoid w, Monad f) => FromEndo (Strict.RWST r w s f ()) where
    type EndoOperatedOn (Strict.RWST r w s f ()) = s

    fromEndo (Endo f) = Strict.RWS.modify f

-- | Modify state.
--
-- @
-- 'fromEndo' ('Endo' f) = 'Lazy.State.modify' f
-- @
instance Monad f => FromEndo (Lazy.StateT s f ()) where
    type EndoOperatedOn (Lazy.StateT s f ()) = s

    fromEndo (Endo f) = Lazy.State.modify f

-- | Modify state.
--
-- @
-- 'fromEndo' ('Endo' f) = 'Strict.State.modify' f
-- @
instance Monad f => FromEndo (Strict.StateT s f ()) where
    type EndoOperatedOn (Strict.StateT s f ()) = s

    fromEndo (Endo f) = Strict.State.modify f

-- }}} Transformers instances for FromEndo ------------------------------------

-- | In a lot of cases it is necessary to evaluate result of 'fromEndo'.
-- Example:
--
-- >>> fromEndoWith ((`runState` def) :: State Int () -> ((), Int)) (Endo (+10))
-- ((), 10)
--
-- Following property holds:
--
-- @
-- 'fromEndoWith' 'id' = 'fromEndo'
-- @
--
-- See also 'fromDualEndoWith'.
fromEndoWith :: (FromEndo a, EndoOperatedOn a ~ c) => (a -> b) -> Endo c -> b
fromEndoWith = (. fromEndo)

-- | In a lot of cases it is necessary to evaluate result of 'fromDualEndo'.
-- Example:
--
-- >>> fromEndoWith ((`runState` def) :: State Int () -> ((), Int)) (Dual (Endo (+10)))
-- ((), 10)
--
-- Following property holds:
--
-- @
-- 'fromDualEndoWith' 'id' = 'fromDualEndo'
-- @
--
-- See also 'fromEndoWith'.
fromDualEndoWith
    :: (FromEndo a, EndoOperatedOn a ~ c) => (a -> b) -> Dual (Endo c) -> b
fromDualEndoWith = (. fromDualEndo)

fromEndoTo :: FromEndo a => Endo (EndoOperatedOn a) -> proxy a -> a
fromEndoTo e _ = fromEndo e

fromDualEndoTo :: FromEndo a => Dual (Endo (EndoOperatedOn a)) -> proxy a -> a
fromDualEndoTo e _ = fromDualEndo e

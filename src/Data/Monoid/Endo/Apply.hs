{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}  -- GeneralizedNewtypeDeriving failed.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if !MIN_VERSION_base(4,9,0) && MIN_VERSION_transformers(0,5,0)
-- Definitions in Data.Functor.Classes from transformers >=0.5 are compatible
-- with those in base >=4.9, therefore we can enable them.
#define HAVE_FUNCTOR_CLASSES
#endif

-- |
-- Module:       $HEADER$
-- Description:  ApplyEndo provides easier endomorphism evaluation in cases
--               when there is an "obvious" default value.
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, DeriveFoldable, DeriveFunctor, DeriveDataTypeable,
--               DeriveGeneric, DeriveTraversable, FlexibleInstances,
--               NoImplicitPrelude, TypeFamilies
--
-- 'ApplyEndo' provides easier endomorphism evaluation in cases when there is an
-- \"obvious\" default value.
module Data.Monoid.Endo.Apply
    (
    -- * ApplyEndo
      ApplyEndo(..)
    , apply
    , applyF

    -- ** ApplyEndo Mempty
    , Mempty
    , applyMempty
    , applyMempty_
    , joinApplyMempty

    -- ** ApplyEndo Def
    --
    -- $applyEndoDef
    , Def
    , applyDef
    , applyDef_
    , joinApplyDef

    -- ** ApplyEndo Reader
    , Reader
    , applyReader
    , applyReaderWith
    , joinApplyReader

    -- ** ApplyEndo Modify
    , Modify
    , applyModify
    , joinApplyModify

    -- ** ApplyEndo Modify'
    , Modify'
    , applyModify'
    , joinApplyModify'

    )
  where

import Prelude (seq)

import Control.Applicative (Applicative(pure))
import Control.Monad
    ( Monad((>>=))
#ifdef HAVE_APPLICATIVE_MONAD
    , void
#else
    , liftM
#endif
    )
import Data.Foldable (Foldable)
import Data.Function
    ( (.)
    , ($)
#ifndef HAVE_APPLICATIVE_MONAD
    , const
#endif
    )
import Data.Functor (Functor, (<$>))
#ifdef HAVE_FUNCTOR_CLASSES
import Data.Functor.Classes
    ( Eq1
    , Ord1
    , Read1(liftReadsPrec)
    , Show1(liftShowsPrec)
    , readsData
    , readsUnaryWith
    , showsUnaryWith
    )
#endif
import Data.Functor.Identity (Identity(runIdentity))
import Data.Monoid (Endo(Endo, appEndo), Monoid(mempty))
import Data.Traversable (Traversable)
import GHC.Generics (Generic, Generic1)

#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
import Data.Data (Data, Typeable)
#endif

import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.Reader.Class as MonadReader (asks)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as MonadState (state)

import Data.Default.Class (Default(def))

import Data.Monoid.Endo.FromEndo (FromEndo(EndoOperatedOn, fromEndo))


-- | There are cases when it is \"obvious\" what is the default value, which
-- should be modified by the endomorphism. This type is a result of such
-- endomorphism application and it uses phantom type @t@ as distinguishing
-- property, which decides what is the correct \"default value\".
newtype ApplyEndo t f a = ApplyEndo {applyEndo :: f a}
  deriving
    ( Applicative
    , Foldable
    , Functor
    , Generic
    , Generic1
    , Monad
    , Traversable
#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
    , Data
    , Typeable
#endif
#ifdef HAVE_FUNCTOR_CLASSES
    , Eq1
    , Ord1
#endif
    )

#ifdef HAVE_FUNCTOR_CLASSES
instance Read1 f => Read1 (ApplyEndo t f) where
    liftReadsPrec rp rl =
        readsData $ readsUnaryWith (liftReadsPrec rp rl) "ApplyEndo" ApplyEndo

instance Show1 f => Show1 (ApplyEndo t f) where
    liftShowsPrec sp sl d (ApplyEndo x) =
        showsUnaryWith (liftShowsPrec sp sl) "ApplyEndo" d x
#endif
    -- HAVE_FUNCTOR_CLASSES

-- | Apply endomorphism using provided \"default\" value.
apply :: Applicative f => a -> Endo a -> ApplyEndo t f a
apply defaultValue (Endo f) = ApplyEndo . pure $ f defaultValue
{-# INLINE apply #-}

-- | Similar as 'apply', but expects 'Endo' to be wrapped by a 'Functor'.
applyF :: Functor f => a -> f (Endo a) -> ApplyEndo t f a
applyF defaultValue endo = ApplyEndo $ (`appEndo` defaultValue) <$> endo

-- {{{ ApplyEndo Mempty -------------------------------------------------------

-- | Type tag identifying usage of 'mempty' from 'Monoid'.
data Mempty
  deriving
    ( Generic
#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

instance (Applicative f, Monoid a) => FromEndo (ApplyEndo Mempty f a) where
    type EndoOperatedOn (ApplyEndo Mempty f a) = a

    fromEndo = apply mempty

-- | Constrained version of 'applyEndo'. Usage example:
--
-- @
-- applyMempty . fromEndo :: ('Applicative' f, 'Monoid' a) => 'Endo' a -> f a
-- @
applyMempty :: Monoid a => ApplyEndo Mempty f a -> f a
applyMempty = applyEndo
{-# INLINE applyMempty #-}

-- | Same as 'applyMempty', but 'Applicative' functor is specialized to
-- 'Identity' functor and evaluated.
--
-- Examples:
--
-- >>> fromEndoWith applyMempty_ $ foldEndo (+1) [(*10), (+42)] :: Int
-- 421
-- >>> fromEndoWith applyMempty_ $ dualFoldEndo (+1) [(*10), (+42)] :: Int
-- 52
applyMempty_ :: Monoid a => ApplyEndo Mempty Identity a -> a
applyMempty_ = runIdentity . applyMempty
{-# INLINE applyMempty_ #-}

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyMempty' defined as:
--
-- @
-- 'joinApplyMempty' = ('>>=' 'applyMempty')
-- @
joinApplyMempty
    ::  ( Monad m
        , Monoid a
#ifndef HAVE_APPLICATIVE_MONAD
        , Applicative m
#endif
        )
    => m (ApplyEndo Mempty m a) -> m a
joinApplyMempty = (>>= applyMempty)
{-# INLINE joinApplyMempty #-}

-- }}} ApplyEndo Mempty -------------------------------------------------------

-- {{{ ApplyEndo Def ----------------------------------------------------------

-- $applyEndoDef
--
-- Apply endomorphism to a default value 'def' from 'Default'. See also
-- following packages:
--
-- * <https://hackage.haskell.org/package/data-default-extra data-default-extra>
--
-- * <https://hackage.haskell.org/package/data-default data-default>
--
-- Both of those packages provide additional instances to 'Default' type
-- class.

-- | Type tag identifying usage of 'def' from 'Default'.
data Def
  deriving
    ( Generic
#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

instance (Applicative f, Default a) => FromEndo (ApplyEndo Def f a) where
    type EndoOperatedOn (ApplyEndo Def f a) = a

    fromEndo = apply def

-- | Constrained version of 'applyEndo'. Usage example:
--
-- @
-- applyDef . fromEndo :: ('Applicative' f, 'Default' a) => 'Endo' a -> f a
-- @
applyDef :: (Applicative f, Default a) => ApplyEndo Def f a -> f a
applyDef = applyEndo
{-# INLINE applyDef #-}

-- | Same as 'applyDef', but 'Applicative' functor is specialized to 'Identity'
-- functor and evaluated.
--
-- Examples:
--
-- >>> fromEndoWith applyDef_ $ foldEndo (+1) [(*10), (+42)] :: Int
-- 421
-- >>> fromEndoWith applyDef_ $ dualFoldEndo (+1) [(*10), (+42)] :: Int
-- 52
applyDef_ :: Default a => ApplyEndo Def Identity a -> a
applyDef_ = runIdentity . applyDef
{-# INLINE applyDef_ #-}

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyDef' defined as:
--
-- @
-- 'joinApplyDef' = ('>>=' 'applyDef')
-- @
joinApplyDef
    ::  ( Monad m
        , Default a
#ifndef HAVE_APPLICATIVE_MONAD
        , Applicative m
#endif
        )
    => m (ApplyEndo Def m a) -> m a
joinApplyDef = (>>= applyDef)
{-# INLINE joinApplyDef #-}

-- }}} ApplyEndo Def ----------------------------------------------------------

-- {{{ ApplyEndo Reader -------------------------------------------------------

-- | Type tag identifying usage of 'MonadReader.asks' operation in 'FromEndo'
-- instance of 'ApplyEndo'.
data Reader
  deriving
    ( Generic
#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

-- | Evaluates 'ApplyEndo' in terms of 'MonadReader.asks' operation:
--
-- @
-- 'fromEndo' = 'ApplyEndo' . 'MonadReader.asks' . 'appEndo'
-- @
instance MonadReader r m => FromEndo (ApplyEndo Reader m r) where
    type EndoOperatedOn (ApplyEndo Reader m r) = r

    fromEndo = ApplyEndo . MonadReader.asks . appEndo

-- | Evaluates 'ApplyEndo' in terms of 'MonadReader.asks' operation.
--
-- This @(->) r@ is a valid 'MonadReader' instance, therefore, this is a valid
-- use case:
--
-- >>> (applyReader . fromEndo $ foldEndo (*10) (+1)) 0 :: Int
-- 10
applyReader :: MonadReader r m => ApplyEndo Reader m r -> m r
applyReader = applyEndo

-- | Evaluates 'ApplyEndo' in terms of 'MonadReader.asks' operation and then
-- evaluates the resalt using provided function.
--
-- This @(->) r@ is a valid 'MonadReader' instance, therefore, this is a valid
-- use case:
--
-- >>> applyReaderWith ($ 0) . fromEndo $ foldEndo (*10) (+1) :: Int
-- 10
applyReaderWith :: MonadReader r m => (m r -> a) -> ApplyEndo Reader m r -> a
applyReaderWith = (. applyEndo)

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyReader' defined as:
--
-- @
-- 'joinApplyReader' = ('>>=' 'applyReader')
-- @
joinApplyReader :: MonadReader r m => m (ApplyEndo Reader m r) -> m r
joinApplyReader = (>>= applyEndo)

-- }}} ApplyEndo Reader -------------------------------------------------------

-- {{{ ApplyEndo Modify -------------------------------------------------------

-- | Type tag identifying usage of 'MonadState.state' operation in 'FromEndo'
-- instance of 'ApplyEndo'.
data Modify
  deriving
    ( Generic
#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

-- | Evaluates 'ApplyEndo' in terms of 'MonadState.state' operation:
--
-- @
-- 'fromEndo' e = 'ApplyEndo' . 'MonadState.state' '$' \\s ->
--     let s' = 'appEndo' e s in (s', s')
-- @
instance MonadState s m => FromEndo (ApplyEndo Modify m s) where
    type EndoOperatedOn (ApplyEndo Modify m s) = s

    fromEndo e =
        ApplyEndo . MonadState.state $ \s -> let s' = appEndo e s in (s', s')

-- | Evaluates 'ApplyEndo' in terms of 'MonadState.state' operation.
applyModify :: MonadState s m => ApplyEndo Modify m s -> m s
applyModify = applyEndo

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyModify' defined as:
--
-- @
-- 'joinApplyModify' = ('>>=' 'applyModify')
-- @
joinApplyModify :: MonadState s m => m (ApplyEndo Modify m s) -> m s
joinApplyModify = (>>= applyEndo)

-- | Same as 'Modify', but strictness is implied.
data Modify'
  deriving
    ( Generic
#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

-- | Evaluates 'ApplyEndo' in terms of 'MonadState.state' operation:
--
-- @
-- 'fromEndo' ('Endo' f) = 'ApplyEndo' . 'MonadState.state' $ \\s ->
--     let s' = f s in s' \`seq\` (s', s')
-- @
instance MonadState s m => FromEndo (ApplyEndo Modify' m s) where
    type EndoOperatedOn (ApplyEndo Modify' m s) = s

    fromEndo (Endo f) =
        ApplyEndo . MonadState.state $ \s -> let s' = f s in s' `seq` (s', s')

-- | Evaluates 'ApplyEndo' in terms of 'MonadState.state' operation.
applyModify' :: MonadState r m => ApplyEndo Modify' m () -> m ()
applyModify' = void . applyEndo

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyModify'' defined as:
--
-- @
-- 'joinApplyModify'' = ('>>=' 'applyModify'')
-- @
joinApplyModify' :: MonadState r m => m (ApplyEndo Modify' m r) -> m r
joinApplyModify' = (>>= applyEndo)

-- }}} ApplyEndo Modify -------------------------------------------------------

-- {{{ Helper functions (not exported) ----------------------------------------

#ifndef HAVE_APPLICATIVE_MONAD
void :: Monad m => m a -> m ()
void = liftM $ const ()
#endif

-- {{{ Helper functions (not exported) ----------------------------------------

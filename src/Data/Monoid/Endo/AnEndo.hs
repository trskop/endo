{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- Description:  Conversion of values in to endomorphisms.
-- Copyright:    (c) 2014-2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, TypeFamilies
--
-- Conversion of values in to endomorphisms.
module Data.Monoid.Endo.AnEndo
    (
    -- * Conversion Into Endo
    --
    -- | Various types can encode endomorphisms. In example, enum can be viewed
    -- as family of endomorphisms where each sets a specific field of a record
    -- to a specific enum value, i.e. data constructor. Type class 'AnEndo'
    -- provides generic way to convert values in to an endomorphism using
    -- 'anEndo' and 'aDualEndo' functions.
      AnEndo(..)

    -- ** WrappedFoldable
    --
    -- $wrappedFoldable
    , WrappedFoldable(..)

    -- * Utility Functions and Types
    , embedEndoWith
    , embedDualEndoWith
    )
  where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Foldable (Foldable(foldMap))
import Data.Function (($), (.), id)
import Data.Functor (Functor)
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
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid
    ( Dual(Dual, getDual)
    , Endo(Endo)
    , Monoid(mempty, mconcat)
    , (<>)
    )
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

#ifdef HAVE_KIND_POLYMORPHIC_TYPEABLE
import Data.Data (Data, Typeable)
#endif

#ifdef HAVE_PROXY
import Data.Proxy (Proxy(Proxy))
#endif

import Data.Functor.Reverse (Reverse)


-- {{{ AnEndo Type Class ------------------------------------------------------

-- | Class that represents various endomorphism representation. In other words
-- anything that encodes @(a -> a)@ can be instance of this class.
--
-- Here are some important instances with not so obvious definitions.
--
-- @
-- instance 'AnEndo' ('Proxy' a) where
--     type 'EndoOperatesOn' ('Proxy' a) = a
--
--     'anEndo'    _ = 'mempty' -- = Endo 'id'
--     'aDualEndo' _ = 'mempty'
-- @
--
-- It got quite common to use 'Proxy' data type as an explicit way to pass
-- types around. Above instance allows you to restrict type of result of
-- endomorphism folding, to some extent.
--
-- @
-- instance 'AnEndo' a => 'AnEndo' (Maybe a) where
--     type 'EndoOperatesOn' (Maybe a) = 'EndoOperatesOn' a
--
--     'anEndo' Nothing  = 'mempty' -- = Endo 'id'
--     'anEndo' (Just e) = 'anEndo' e
--
--     -- Definition of 'aDualEndo' is analogous.
-- @
--
-- Instance for @Maybe@ lets us conditionally inject endomorphism in to a
-- folding chain.
class AnEndo a where
    -- | Extract type on which endomorphism operates, e.g. for
    -- @('Endo' a)@ it would be @a@.
    type EndoOperatesOn a

    -- | Convert value encoding @(a -> a)@ in to 'Endo'. Default
    -- implementation:
    --
    -- @
    -- 'anEndo' = 'getDual' . 'aDualEndo'
    -- @
    anEndo :: a -> Endo (EndoOperatesOn a)
    anEndo = getDual . aDualEndo

    -- | Dual to 'anEndo'. Default implementation:
    --
    -- @
    -- 'aDualEndo' = 'Dual' . 'anEndo'
    -- @
    aDualEndo :: a -> Dual (Endo (EndoOperatesOn a))
    aDualEndo = Dual . anEndo

#if HAVE_MINIMAL_PRAGMA
    {-# MINIMAL anEndo | aDualEndo #-}
#endif

instance AnEndo (Endo a) where
    type EndoOperatesOn (Endo a) = a
    anEndo = id

instance AnEndo (a -> a) where
    type EndoOperatesOn (a -> a) = a
    anEndo = Endo

instance AnEndo a => AnEndo (Identity a) where
    type EndoOperatesOn (Identity a) = EndoOperatesOn a

    anEndo (Identity e) = anEndo e
    aDualEndo (Identity e) = aDualEndo e

instance AnEndo a => AnEndo (Maybe a) where
    type EndoOperatesOn (Maybe a) = EndoOperatesOn a

    anEndo Nothing  = mempty
    anEndo (Just e) = anEndo e

    aDualEndo Nothing  = mempty
    aDualEndo (Just e) = aDualEndo e

#ifdef HAVE_PROXY
-- | Constructs identity endomorphism for specified phantom type.
instance AnEndo (Proxy a) where
    type EndoOperatesOn (Proxy a) = a

    anEndo    Proxy = mempty
    aDualEndo Proxy = mempty
#endif

-- {{{ Foldable Instances -----------------------------------------------------

-- | Wrapper for 'Foldable' types. Used to provide instances that work for all
-- 'Foldable' types without the need for @OverlappingInstances@ language
-- extension.
newtype WrappedFoldable f a = WrapFoldable {getFoldable :: f a}
  deriving
    ( Applicative
    , Foldable
    , Functor
    , Generic
    , Monad
    , Read
    , Show
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
instance Read1 f => Read1 (WrappedFoldable f) where
    liftReadsPrec rp rl = readsData
        $ readsUnaryWith (liftReadsPrec rp rl) "WrapFoldable" WrapFoldable

instance Show1 f => Show1 (WrappedFoldable f) where
    liftShowsPrec sp sl d (WrapFoldable x) =
        showsUnaryWith (liftShowsPrec sp sl) "WrapFoldable" d x
#endif
    -- HAVE_FUNCTOR_CLASSES

instance (Foldable f, AnEndo a) => AnEndo (WrappedFoldable f a) where
    type EndoOperatesOn (WrappedFoldable f a) = EndoOperatesOn a
    anEndo    (WrapFoldable fa) = foldMap anEndo    fa
    aDualEndo (WrapFoldable fa) = foldMap aDualEndo fa

instance AnEndo a => AnEndo [a] where
    type EndoOperatesOn [a] = EndoOperatesOn a
    anEndo    = anEndo    . WrapFoldable
    aDualEndo = aDualEndo . WrapFoldable

-- {{{ Transformers -----------------------------------------------------------

-- | Fold in reverese order.
instance (Foldable f, AnEndo a) => AnEndo (Reverse f a) where
    type EndoOperatesOn (Reverse f a) = EndoOperatesOn a
    anEndo    = anEndo    . WrapFoldable
    aDualEndo = aDualEndo . WrapFoldable

-- }}} Transformers -----------------------------------------------------------

-- }}} Foldable Instances -----------------------------------------------------

-- {{{ Instances For Tuples ---------------------------------------------------

instance
    ( AnEndo a
    , AnEndo b
    , EndoOperatesOn a ~ EndoOperatesOn b
    ) => AnEndo (a, b)
  where
    type EndoOperatesOn (a, b) = EndoOperatesOn a
    anEndo    (a, b) = anEndo    a <> anEndo    b
    aDualEndo (a, b) = aDualEndo a <> aDualEndo b

instance
    ( AnEndo a
    , AnEndo b
    , AnEndo c
    , EndoOperatesOn a ~ EndoOperatesOn b
    , EndoOperatesOn a ~ EndoOperatesOn c
    ) => AnEndo (a, b, c)
  where
    type EndoOperatesOn (a, b, c) = EndoOperatesOn a
    anEndo    (a, b, c) = anEndo    a <> anEndo    b <> anEndo    c
    aDualEndo (a, b, c) = aDualEndo a <> aDualEndo b <> aDualEndo c

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    ) => AnEndo (a1, a2, a3, a4)
  where
    type EndoOperatesOn (a1, a2, a3, a4) = EndoOperatesOn a1
    anEndo (a1, a2, a3, a4) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        ]

    aDualEndo (a1, a2, a3, a4) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        ]

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , AnEndo a5
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a5
    ) => AnEndo (a1, a2, a3, a4, a5)
  where
    type EndoOperatesOn (a1, a2, a3, a4, a5) = EndoOperatesOn a1

    anEndo (a1, a2, a3, a4, a5) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        , anEndo a5
        ]

    aDualEndo (a1, a2, a3, a4, a5) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        , aDualEndo a5
        ]

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , AnEndo a5
    , AnEndo a6
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a5
    , EndoOperatesOn a1 ~ EndoOperatesOn a6
    ) => AnEndo (a1, a2, a3, a4, a5, a6)
  where
    type EndoOperatesOn (a1, a2, a3, a4, a5, a6) = EndoOperatesOn a1

    anEndo (a1, a2, a3, a4, a5, a6) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        , anEndo a5
        , anEndo a6
        ]

    aDualEndo (a1, a2, a3, a4, a5, a6) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        , aDualEndo a5
        , aDualEndo a6
        ]

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , AnEndo a5
    , AnEndo a6
    , AnEndo a7
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a5
    , EndoOperatesOn a1 ~ EndoOperatesOn a6
    , EndoOperatesOn a1 ~ EndoOperatesOn a7
    ) => AnEndo (a1, a2, a3, a4, a5, a6, a7)
  where
    type EndoOperatesOn (a1, a2, a3, a4, a5, a6, a7) = EndoOperatesOn a1

    anEndo (a1, a2, a3, a4, a5, a6, a7) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        , anEndo a5
        , anEndo a6
        , anEndo a7
        ]

    aDualEndo (a1, a2, a3, a4, a5, a6, a7) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        , aDualEndo a5
        , aDualEndo a6
        , aDualEndo a7
        ]

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , AnEndo a5
    , AnEndo a6
    , AnEndo a7
    , AnEndo a8
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a5
    , EndoOperatesOn a1 ~ EndoOperatesOn a6
    , EndoOperatesOn a1 ~ EndoOperatesOn a7
    , EndoOperatesOn a1 ~ EndoOperatesOn a8
    ) => AnEndo (a1, a2, a3, a4, a5, a6, a7, a8)
  where
    type EndoOperatesOn (a1, a2, a3, a4, a5, a6, a7, a8) = EndoOperatesOn a1

    anEndo (a1, a2, a3, a4, a5, a6, a7, a8) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        , anEndo a5
        , anEndo a6
        , anEndo a7
        , anEndo a8
        ]

    aDualEndo (a1, a2, a3, a4, a5, a6, a7, a8) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        , aDualEndo a5
        , aDualEndo a6
        , aDualEndo a7
        , aDualEndo a8
        ]

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , AnEndo a5
    , AnEndo a6
    , AnEndo a7
    , AnEndo a8
    , AnEndo a9
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a5
    , EndoOperatesOn a1 ~ EndoOperatesOn a6
    , EndoOperatesOn a1 ~ EndoOperatesOn a7
    , EndoOperatesOn a1 ~ EndoOperatesOn a8
    , EndoOperatesOn a1 ~ EndoOperatesOn a9
    ) => AnEndo (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
    type EndoOperatesOn (a1, a2, a3, a4, a5, a6, a7, a8, a9) = EndoOperatesOn a1

    anEndo (a1, a2, a3, a4, a5, a6, a7, a8, a9) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        , anEndo a5
        , anEndo a6
        , anEndo a7
        , anEndo a8
        , anEndo a9
        ]

    aDualEndo (a1, a2, a3, a4, a5, a6, a7, a8, a9) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        , aDualEndo a5
        , aDualEndo a6
        , aDualEndo a7
        , aDualEndo a8
        , aDualEndo a9
        ]

instance
    ( AnEndo a1
    , AnEndo a2
    , AnEndo a3
    , AnEndo a4
    , AnEndo a5
    , AnEndo a6
    , AnEndo a7
    , AnEndo a8
    , AnEndo a9
    , AnEndo a10
    , EndoOperatesOn a1 ~ EndoOperatesOn a2
    , EndoOperatesOn a1 ~ EndoOperatesOn a3
    , EndoOperatesOn a1 ~ EndoOperatesOn a4
    , EndoOperatesOn a1 ~ EndoOperatesOn a5
    , EndoOperatesOn a1 ~ EndoOperatesOn a6
    , EndoOperatesOn a1 ~ EndoOperatesOn a7
    , EndoOperatesOn a1 ~ EndoOperatesOn a8
    , EndoOperatesOn a1 ~ EndoOperatesOn a9
    , EndoOperatesOn a1 ~ EndoOperatesOn a10
    ) => AnEndo (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
    type EndoOperatesOn (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = EndoOperatesOn a1

    anEndo (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = mconcat
        [ anEndo a1
        , anEndo a2
        , anEndo a3
        , anEndo a4
        , anEndo a5
        , anEndo a6
        , anEndo a7
        , anEndo a8
        , anEndo a9
        , anEndo a10
        ]

    aDualEndo (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = mconcat
        [ aDualEndo a1
        , aDualEndo a2
        , aDualEndo a3
        , aDualEndo a4
        , aDualEndo a5
        , aDualEndo a6
        , aDualEndo a7
        , aDualEndo a8
        , aDualEndo a9
        , aDualEndo a10
        ]

-- }}} Instances For Tuples ---------------------------------------------------
-- }}} AnEndo Type Class ------------------------------------------------------

-- {{{ Utility Functions and Types --------------------------------------------

-- | Use 'Endo' (possibly result of 'Data.Endo.Fold.foldEndo') and use it to
-- create value of different type.
--
-- Examples:
--
-- @
-- 'embedEndoWith' 'Control.Monad.Trans.Writer.Lazy.tell'
--     :: (Monad m, 'AnEndo' e, w ~ 'EndoOperatesOn' e)
--     => e
--     -> 'Control.Monad.Trans.Writer.Lazy.WriterT' ('Endo' w) m ()
--
-- 'embedEndoWith' ('Control.Monad.Trans.State.Lazy.modify' . 'Data.Monoid.appEndo')
--     :: (Monad m, 'AnEndo' e, s ~ 'EndoOperatesOn' e)
--     => e
--     -> 'Control.Monad.Trans.State.Lazy.StateT' s m ()
-- @
--
-- See also 'embedDualEndoWith'.
embedEndoWith :: (AnEndo e, EndoOperatesOn e ~ a)
    => (Endo a -> b)
    -- ^ Embedding function.
    -> e -> b
embedEndoWith = (. anEndo)

-- | Dual to 'embedEndoWith', which uses 'aDualEndo' instead of 'anEndo'.
embedDualEndoWith
    :: (AnEndo e, EndoOperatesOn e ~ a)
    => (Dual (Endo a) -> b)
    -- ^ Embedding function.
    -> e -> b
embedDualEndoWith = (. aDualEndo)

-- }}} Utility Functions and Types --------------------------------------------

-- $wrappedFoldable
--
-- Newtype 'WrappedFoldable' allows us to use 'anEndo', 'aDualEndo',
-- 'Data.Endo.Fold.foldEndo', and 'Data.Endo.Fold.dualFoldEndo' for any
-- 'Foldable' instance without the need to create specific instance for that
-- specific 'Foldable' type and reduces. It would be possible to create
-- 'AnEndo' instance for all 'Foldable' types, but that would require
-- @OverlappingInstances@ language extension.
--
-- Usage examples:
--
-- @
-- \\vectorOfEndos -> 'anEndo' ('WrappedFoldable' vectorOfEndos)
--     :: Vector ('Data.Monoid.Endo.E' a) -> 'Endo' a
-- @
--
-- @
-- \\vectorOfEndos -> 'Data.Monoid.Endo.Fold.foldEndo' ('WrappedFoldable' vectorOfEndos)
--     :: 'Data.Monoid.Endo.Fold.FoldEndoArgs' => Vector ('Data.Monoid.Endo.E' a) -> args
-- @
--
-- Note that the @Vector@ is just one of possible 'Foldable' data types that
-- may be used here. Also, @('Data.Monoid.Endo.E' a)@ is just an example of
-- endomorphism representation, any 'AnEndo' instance can be used.

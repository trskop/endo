{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Generic folding for various endomorphism representations.
-- Copyright:    (c) 2014, 2015 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, TypeFamilies
--
-- Generic folding for various endomorphism representations.
module Data.Monoid.Endo.Fold
    (
    -- * Usage Example
    -- $usageExample

    -- * API
      foldEndo
    , dualFoldEndo
    , FoldEndoArgs(..)
    , AnEndo(..)

    -- ** Type Wrappers
    , Foldable(..)
    )
  where

import Control.Applicative (Applicative(pure))
import Data.Data (Data)
import Data.Either (Either(Right))
import qualified Data.Foldable as Foldable (Foldable(foldMap))
import Data.Function ((.), id)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Dual(Dual), Endo(Endo), Monoid(mempty, mconcat), (<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Read (Read)
import Text.Show (Show)

import Control.Monad.Trans.Identity (IdentityT(..))


-- | Fold all variously represented endomorphisms in to one endomorphism.
--
-- Order in which endomorphisms are folded is preserved:
--
-- >>> foldEndo (Endo (1:)) [(2:), (3:)] `appEndo` []
-- [1,2,3]
--
-- For numbers it would look like:
--
-- >>> foldEndo (Endo (+1)) [(+2), (*3)] `appEndo` 1
-- 6
--
-- Above can be seen as:
--
-- >>> (+1) . (+2) . (*3) $ 1
-- 6
foldEndo :: FoldEndoArgs args => args
foldEndo = foldEndoArgs mempty

-- | Same as 'foldEndo', but folds endomorphisms in reverse order.
--
-- Following are the same examples as for 'foldEndo' function. Please, note the
-- differences in results.
--
-- Order in which endomorphisms are folded is reversed:
--
-- >>> dualFoldEndo (Endo (1:)) [(2:), (3:)] `appEndo` []
-- [2,3,1]
--
-- For numbers it would look like:
--
-- >>> dualFoldEndo (Endo (+1)) [(+2), (*3)] `appEndo` 1
-- 12
--
-- Above can be seen as:
--
-- >>> (*3) . (+2) . (+1) $ 1
-- 12
dualFoldEndo :: FoldEndoArgs args => args
dualFoldEndo = dualFoldEndoArgs mempty

-- {{{ FoldEndoArgs Type Class ------------------------------------------------

class FoldEndoArgs a where
    type ResultOperatesOn a
    foldEndoArgs     ::       Endo (ResultOperatesOn a)  -> a
    dualFoldEndoArgs :: Dual (Endo (ResultOperatesOn a)) -> a

instance
    ( AnEndo a
    , FoldEndoArgs r
    , EndoOperatesOn a ~ ResultOperatesOn r
    ) => FoldEndoArgs (a -> r)
  where
    type ResultOperatesOn (a -> r) = ResultOperatesOn r
    foldEndoArgs     e e' = foldEndoArgs     (e <> anEndo    e')
    dualFoldEndoArgs e e' = dualFoldEndoArgs (e <> aDualEndo e')

instance FoldEndoArgs (Endo a) where
    type ResultOperatesOn (Endo a) = a
    foldEndoArgs              = id
    dualFoldEndoArgs (Dual e) = e

instance FoldEndoArgs (Dual (Endo a)) where
    type ResultOperatesOn (Dual (Endo a)) = a
    foldEndoArgs     = Dual
    dualFoldEndoArgs = id

instance (Applicative f, FoldEndoArgs r) => FoldEndoArgs (IdentityT f r) where
    type ResultOperatesOn (IdentityT f r) = ResultOperatesOn r
    foldEndoArgs     = IdentityT . pure . foldEndoArgs
    dualFoldEndoArgs = IdentityT . pure . dualFoldEndoArgs

instance FoldEndoArgs r => FoldEndoArgs (Either e r) where
    type ResultOperatesOn (Either e r) = ResultOperatesOn r
    foldEndoArgs     = Right . foldEndoArgs
    dualFoldEndoArgs = Right . dualFoldEndoArgs

instance FoldEndoArgs r => FoldEndoArgs (Identity r) where
    type ResultOperatesOn (Identity r) = ResultOperatesOn r
    foldEndoArgs     = Identity . foldEndoArgs
    dualFoldEndoArgs = Identity . dualFoldEndoArgs

instance FoldEndoArgs r => FoldEndoArgs (IO r) where
    type ResultOperatesOn (IO r) = ResultOperatesOn r
    foldEndoArgs     = pure . foldEndoArgs
    dualFoldEndoArgs = pure . dualFoldEndoArgs

instance FoldEndoArgs r => FoldEndoArgs (Maybe r) where
    type ResultOperatesOn (Maybe r) = ResultOperatesOn r
    foldEndoArgs     = Just . foldEndoArgs
    dualFoldEndoArgs = Just . dualFoldEndoArgs

-- {{{ FoldEndoArgs Type Class ------------------------------------------------

-- {{{ AnEndo Type Class ------------------------------------------------------

class AnEndo a where
    type EndoOperatesOn a

    anEndo    :: a -> Endo (EndoOperatesOn a)

    aDualEndo :: a -> Dual (Endo (EndoOperatesOn a))
    aDualEndo = Dual . anEndo

instance AnEndo (Endo a) where
    type EndoOperatesOn (Endo a) = a
    anEndo    = id
    aDualEndo = Dual

instance AnEndo (a -> a) where
    type EndoOperatesOn (a -> a) = a
    anEndo    = Endo
    aDualEndo = Dual . Endo

instance AnEndo a => AnEndo (Maybe a) where
    type EndoOperatesOn (Maybe a) = EndoOperatesOn a

    anEndo Nothing  = mempty
    anEndo (Just e) = anEndo e

-- {{{ Foldable Instances -----------------------------------------------------

newtype Foldable f a = Foldable {getFoldable :: f a}
    deriving (Data, Generic, Read, Show, Typeable)

instance (Foldable.Foldable f, AnEndo a) => AnEndo (Foldable f a) where
    type EndoOperatesOn (Foldable f a) = EndoOperatesOn a
    anEndo    (Foldable fa) = Foldable.foldMap anEndo fa
    aDualEndo (Foldable fa) = Foldable.foldMap aDualEndo fa

instance AnEndo a => AnEndo [a] where
    type EndoOperatesOn [a] = EndoOperatesOn a
    anEndo    = anEndo    . Foldable
    aDualEndo = aDualEndo . Foldable

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

-- $usageExample
--
-- TODO

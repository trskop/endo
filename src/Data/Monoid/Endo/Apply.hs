{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Data.Monoid.Endo.Apply
    (
    -- * ApplyEndo
      ApplyEndo(..)
    , apply
    , applyF

    -- ** ApplyEndo Mempty
    , Mempty
    , applyMempty
    , applyMempty'
    , joinApplyMempty

    -- ** ApplyEndo Def
    , Def
    , applyDef
    , applyDef'
    , joinApplyDef

    -- ** ApplyEndo Reader
    , Reader
    , applyReader
    , joinApplyReader
    )
  where

import Control.Applicative (Applicative((<*>), pure))
import Control.Monad (Monad((>>=), return))
import Data.Foldable (Foldable)
import Data.Function ((.), ($))
import Data.Functor (Functor, (<$>))
import Data.Functor.Identity (Identity(runIdentity))
import Data.Monoid (Endo(Endo, appEndo), Monoid(mempty))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

#ifdef KIND_POLYMORPHIC_TYPEABLE
import Data.Data (Data, Typeable)
#endif

import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.Reader.Class as MonadReader (asks)

import Data.Default.Class (Default(def))

import Data.Monoid.Endo.FromEndo (FromEndo(..))


-- | There are cases when it is \"obvious\" what is the default value, which
-- should be modified by the endomorphism. This type is a result of such
-- endomorphism application and it uses phantom type @t@ as distinguishing
-- property, which decides what is the correct \"default value\".
newtype ApplyEndo t f a = ApplyEndo {applyEndo :: f a}
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
#ifdef KIND_POLYMORPHIC_TYPEABLE
    , Data
    , Typeable
#endif
    )

instance Applicative f => Applicative (ApplyEndo t f) where
    pure = ApplyEndo . pure
    ApplyEndo f <*> ApplyEndo x = ApplyEndo (f <*> x)

instance Monad f => Monad (ApplyEndo t f) where
    return = ApplyEndo . return
    ApplyEndo x >>= f = ApplyEndo (x >>= applyEndo . f)

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
#ifdef KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

instance (Applicative f, Monoid a) => FromEndo (ApplyEndo Mempty f a) where
    type EndoOperatedOn (ApplyEndo Mempty f a) = a

    fromEndo = apply mempty

applyMempty :: Monoid a => ApplyEndo Mempty f a -> f a
applyMempty = applyEndo
{-# INLINE applyMempty #-}

-- | Same as 'applyMempty', but 'Applicative' functor is specialized to
-- 'Identity' functor and evaluated.
--
-- Examples:
--
-- >>> fromEndoWith applyMempty' $ foldEndo (+1) [(*10), (+42)] :: Int
-- 421
-- >>> fromEndoWith applyMempty' $ dualFoldEndo (+1) [(*10), (+42)] :: Int
-- 52
applyMempty' :: Monoid a => ApplyEndo Mempty Identity a -> a
applyMempty' = runIdentity . applyMempty
{-# INLINE applyMempty' #-}

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyMempty' defined as:
--
-- @
-- 'joinApplyMempty' = ('>>=' 'applyMempty')
-- @
joinApplyMempty
    ::  ( Monad m
        , Monoid a
#ifndef APPLICATIVE_MONAD
        , Applicative m
#endif
        )
    => m (ApplyEndo Mempty m a) -> m a
joinApplyMempty = (>>= applyMempty)
{-# INLINE joinApplyMempty #-}

-- }}} ApplyEndo Mempty -------------------------------------------------------

-- {{{ ApplyEndo Def ----------------------------------------------------------

-- | Type tag identifying usage of 'def' from 'Default'.
data Def
  deriving
    ( Generic
#ifdef KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

instance (Applicative f, Default a) => FromEndo (ApplyEndo Def f a) where
    type EndoOperatedOn (ApplyEndo Def f a) = a

    fromEndo = apply def

applyDef :: (Applicative f, Default a) => ApplyEndo Def f a -> f a
applyDef = applyEndo
{-# INLINE applyDef #-}

-- | Same as 'applyDef', but 'Applicative' functor is specialized to 'Identity'
-- functor and evaluated.
--
-- Examples:
--
-- >>> fromEndoWith applyDef' $ foldEndo (+1) [(*10), (+42)] :: Int
-- 421
-- >>> fromEndoWith applyDef' $ dualFoldEndo (+1) [(*10), (+42)] :: Int
-- 52
applyDef' :: Default a => ApplyEndo Def Identity a -> a
applyDef' = runIdentity . applyDef
{-# INLINE applyDef' #-}

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyDef' defined as:
--
-- @
-- 'joinApplyDef' = ('>>=' 'applyDef')
-- @
joinApplyDef
    ::  ( Monad m
        , Default a
#ifndef APPLICATIVE_MONAD
        , Applicative m
#endif
        )
    => m (ApplyEndo Def m a) -> m a
joinApplyDef = (>>= applyDef)
{-# INLINE joinApplyDef #-}

-- }}} ApplyEndo Def ----------------------------------------------------------

-- {{{ ApplyEndo Reader -------------------------------------------------------

data Reader
  deriving
    ( Generic
#ifdef KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )

-- | Evaluates 'ApplyEndo' in terms of 'MonadReader.asks' operation:
--
-- @
-- 'fromEndo' = 'ApplyEndo' . 'MonadReader.asks' . 'appEndo'
-- @
instance MonadReader r m => FromEndo (ApplyEndo Reader m r) where
    type EndoOperatedOn (ApplyEndo Reader f r) = r

    fromEndo = ApplyEndo . MonadReader.asks . appEndo

-- | Evaluates 'ApplyEndo' in terms of 'MonadReader.asks' operation.
applyReader :: MonadReader r m => ApplyEndo Reader m r -> m r
applyReader = applyEndo

-- | Evaluates 'ApplyEndo' in a 'Monad' by joining it with the monad it
-- contains. It can be also viewed as a variant of 'applyReader' defined as:
--
-- @
-- 'joinApplyReader' = ('>>=' 'applyReader')
-- @
joinApplyReader :: MonadReader r m => m (ApplyEndo Reader m r) -> m r
joinApplyReader = (>>= applyEndo)

-- }}} ApplyEndo Reader -------------------------------------------------------

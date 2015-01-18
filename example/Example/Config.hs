{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Simple example of Config data type.
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Simple example of 'Config' data type. Definitions from this package are used
-- in other examples.
module Example.Config (Config(..), Verbosity(..))
  where

import Prelude (Bounded, Enum)

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Show (Show)
import Data.Typeable (Typeable)
import System.IO (FilePath)


data Verbosity = Silent | Normal | Verbose | Annoying
  deriving (Bounded, Data, Enum, Eq, Ord, Show, Typeable)

data Config = Config
    { _verbosity :: Verbosity
    , _outputFile :: FilePath
    }
  deriving (Show)

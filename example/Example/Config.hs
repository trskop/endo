{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Simple example of Config data type.
-- Copyright:    (c) 2015 Peter Trsko
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

import Text.Show (Show)
import System.IO (FilePath)


data Verbosity = Silent | Normal | Verbose | Annoying
  deriving (Show)

data Config = Config
    { _verbosity :: Verbosity
    , _outputFile :: FilePath
    }
  deriving (Show)

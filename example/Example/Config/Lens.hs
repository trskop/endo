{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Simple setters for Config data type.
-- Copyright:    (c) 2013-2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Simple setters for 'Config' data type. Definitions from this package are
-- used in other examples.
module Example.Config.Lens
    ( outputFile
    , verbosity
    )
  where

import System.IO (FilePath)

import Control.Lens (Lens')
import Data.Function.Between ((~@@^>))

import Example.Config (Config(..), Verbosity)


verbosity :: Lens' Config Verbosity
verbosity = _verbosity ~@@^> \s b -> s{_verbosity = b}

outputFile :: Lens' Config FilePath
outputFile = _outputFile ~@@^> \s b -> s{_outputFile = b}

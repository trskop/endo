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
module Example.Config.Setters
    ( setOutputFile
    , setVerbosity
    )
  where

import System.IO (FilePath)

import Data.Monoid.Endo (E)

import Example.Config (Config(..), Verbosity)


setVerbosity :: Verbosity -> E Config
setVerbosity b cfg = cfg{_verbosity = b}

setOutputFile :: FilePath -> E Config
setOutputFile b cfg = cfg{_outputFile = b}

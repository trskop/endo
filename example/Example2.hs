{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 2
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Example 2.
module Example2
  where

import Data.Function (($), id)
import Data.Monoid (Endo(appEndo))
import System.IO (FilePath)

import Data.Monoid.Endo (E)
import Data.Monoid.Endo.Fold ((&$), foldEndo)

import Example.Config (Config, Verbosity(Annoying))
import Example.Config.Setters (setOutputFile, setVerbosity)


-- | Trying to set output file to \"\" will result in keeping original value.
setOutputFile2 :: FilePath -> E Config
setOutputFile2 "" = id
setOutputFile2 fp = setOutputFile fp

example2 :: E Config
example2 = appEndo $ foldEndo
    &$ setVerbosity Annoying
    &$ setOutputFile2 "an.out.put"

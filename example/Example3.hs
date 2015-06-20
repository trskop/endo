{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 3
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Example 3.
module Example3
  where

import Control.Monad (guard)
import Data.Bool (not)
import Data.Function (($))
import Data.Functor ((<$))
import Data.List (null)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Endo(appEndo))
import System.IO (FilePath)

import Data.Monoid.Endo (E)
import Data.Monoid.Endo.Fold ((&$), foldEndo)

import Example.Config (Config, Verbosity(Annoying))
import Example.Config.Setters (setOutputFile, setVerbosity)


setOutputFile3 :: FilePath -> Maybe (E Config)
setOutputFile3 "" = Nothing
setOutputFile3 fp = Just $ setOutputFile fp

example3 :: E Config
example3 = appEndo $ foldEndo
    &$ setVerbosity Annoying
    &$ setOutputFile3 "an.out.put"

setOutputFile3':: FilePath -> Maybe (E Config)
setOutputFile3' fp = setOutputFile fp <$ guard (not (null fp))

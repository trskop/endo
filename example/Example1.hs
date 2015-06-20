{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 1
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Example 1.
module Example1
  where

import Data.Function (($))
import Data.Monoid (Endo(appEndo))

import Data.Monoid.Endo (E)
import Data.Monoid.Endo.Fold ((&$), foldEndo)

import Example.Config (Config, Verbosity(Annoying))
import Example.Config.Setters (setOutputFile, setVerbosity)


example1 :: E Config
example1 = appEndo $ foldEndo
    &$ setVerbosity Annoying
    &$ setOutputFile "an.out.put"

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 6
-- Copyright:    (c) 2015 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude, TypeFamilies
--
-- Example 6.
module Example6
  where

import Data.Function ((.), ($))
import Data.Monoid (Endo(Endo, appEndo))
import System.IO (FilePath)

import Control.Lens ((.~), set)
import Data.Monoid.Endo (E)
import Data.Monoid.Endo.Fold (AnEndo(EndoOperatesOn, anEndo), (&$), foldEndo)

import Example.Config (Config, Verbosity(Annoying))
import Example.Config.Lens (outputFile, verbosity)


-- Please ignore warning about orphan instances.
instance AnEndo Verbosity where
    type EndoOperatesOn Verbosity = Config
    anEndo = Endo . set verbosity

newtype OutputFile = OutputFile FilePath

instance AnEndo OutputFile where
    type EndoOperatesOn OutputFile = Config
    anEndo (OutputFile fp) = Endo $ outputFile .~ fp

example6 :: E Config
example6 = Data.Monoid.appEndo $ foldEndo
    &$ Annoying
    &$ OutputFile "an.out.put"

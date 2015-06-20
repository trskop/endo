{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 5
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Example 5.
module Example5
  where

import Data.Function (($))
import Data.Monoid (Endo(appEndo))

import Control.Lens ((.~))
import Data.Monoid.Endo (E)
import Data.Monoid.Endo.Fold ((&$), foldEndo)

import Example.Config (Config, Verbosity(Annoying))
import Example.Config.Lens (outputFile, verbosity)


example5 :: E Config
example5 = appEndo $ foldEndo
    &$ verbosity  .~ Annoying
    &$ outputFile .~ "an.out.put"

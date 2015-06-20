{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 4
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Example 4.
module Example4
  where

import Control.Applicative (Applicative((<*>), pure))
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Monoid (Endo(appEndo))
import Data.String (String)
import System.IO (FilePath)

import Data.Monoid.Endo (E)
import Data.Monoid.Endo.Fold (foldEndo, (<&$>))

import Example.Config (Config, Verbosity(Annoying))
import Example.Config.Setters (setOutputFile, setVerbosity)


setOutputFile4 :: FilePath -> Either String (E Config)
setOutputFile4 "" = Left "Output file: Empty file path."
setOutputFile4 fp = Right $ setOutputFile fp

example4 :: Either String (E Config)
example4 = appEndo <&$> foldEndo
    <*> pure (setVerbosity Annoying)
    <*> setOutputFile4 "an.out.put"

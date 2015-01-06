{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example 7
-- Copyright:    (c) 2015 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Example 2.
module Example7
  where

import Control.Applicative (Applicative((<*>)), (<$>))
import Control.Monad (Monad((>>=)))
import Data.Either (Either(Left, Right))
import Data.Function ((.), ($), id)
import qualified Data.List as List (null, unwords)
import Data.Maybe (Maybe)
import Data.Monoid (Endo, (<>))
import System.IO (IO, print)

import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT))

import Control.Lens ((.~), set)
import Options.Applicative
    ( Parser
    , eitherReader
    , execParser
    , flag
    , fullDesc
    , help
    , info
    , long
    , metavar
    , option
    , optional
    , short
    )

import Data.Monoid.Endo (E, runEndo)
import Data.Monoid.Endo.Fold (foldEndo)

import Example.Config (Config(Config), Verbosity(..))
import Example.Config.Lens (outputFile, verbosity)


options :: Parser Config
options = runIdentityT $ runEndo defaultConfig <$> options'
  where
    options' :: IdentityT Parser (Endo Config)
    options' = foldEndo
        <*> outputOption     -- IdentityT Parser (Maybe (E Config))
        <*> verbosityOption  -- IdentityT Parser (Maybe (E Config))
        <*> annoyingFlag     -- IdentityT Parser (E Config)
        <*> silentFlag       -- IdentityT Parser (E Config)
        <*> verboseFlag      -- IdentityT Parser (E Config)

    defaultConfig :: Config
    defaultConfig = Config Normal ""

-- >>> :main -o an.out.put --annoying
-- Config {_verbosity = Annoying, _outputFile = "an.out.put"}
main :: IO ()
main = execParser (info options fullDesc) >>= print

outputOption :: IdentityT Parser (Maybe (E Config))
outputOption =
    IdentityT . optional . option (set outputFile <$> parseFilePath)
    $ short 'o' <> long "output" <> metavar "FILE"
        <> help "Store output in to a FILE."
  where
    parseFilePath = eitherReader $ \s ->
        if List.null s
            then Left "Option argument can not be empty file path."
            else Right s

verbosityOption :: IdentityT Parser (Maybe (E Config))
verbosityOption =
    IdentityT . optional . option (set verbosity <$> parseVerbosity)
    $ long "verbosity" <> metavar "LEVEL" <> help "Set verbosity to LEVEL."
  where
    parseVerbosity = eitherReader $ \s -> case s of
        "0"        -> Right Silent
        "silent"   -> Right Silent
        "1"        -> Right Normal
        "normal"   -> Right Normal
        "default"  -> Right Normal
        "2"        -> Right Verbose
        "verbose"  -> Right Verbose
        "3"        -> Right Annoying
        "annoying" -> Right Annoying
        _          -> Left $ List.unwords
            [ "Verbosity can be only number from 0 to 3 or one of the"
            , "following:"
            , "silent", "normal", "default", "verbose", "annoying"
            ]

annoyingFlag :: IdentityT Parser (E Config)
annoyingFlag = IdentityT . flag id (verbosity .~ Annoying)
    $ long "annoying" <> help "Set verbosity to maximum."

silentFlag :: IdentityT Parser (E Config)
silentFlag = IdentityT . flag id (verbosity .~ Silent)
    $ short 's' <> long "silent" <> help "Set verbosity to minimum."

verboseFlag :: IdentityT Parser (E Config)
verboseFlag = IdentityT . flag id (verbosity .~ Verbose)
    $ short 'v' <> long "verbose" <> help "Be verbose."

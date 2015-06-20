# Endo

[![Hackage](http://img.shields.io/hackage/v/endo.svg)][Hackage: endo]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/endo.svg)](http://packdeps.haskellers.com/reverse/endo)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/endo.svg)](https://travis-ci.org/trskop/endo)


## Description

Endomorphism utilities.


## Usage Examples

Examples in this section were taken from real live production code, but they
were tamed down a little.


### Basic Idea

Lets define simple application `Config` data type as:

````Haskell
data Verbosity = Silent | Normal | Verbose | Annoying
  deriving (Show)

data Config = Config
    { _verbosity :: Verbosity
    , _outputFile :: FilePath
    }
  deriving (Show)
````

Now lets define setters for `_verbosity` and `_outputFile`:

````Haskell
setVerbosity :: Verbosity -> E Config
setVerbosity b cfg = cfg{_verbosity = b}

setOutputFile :: FilePath -> E Config
setOutputFile b cfg = cfg{_outputFile = b}
````

Note that E is defined in `Data.Monoid.Endo` module and it looks like:

````Haskell
type E a = a -> a
````

Its purpose is to simplify type signatures.

Now lets get to our first example:

````Haskell
example1 :: E Config
example1 = appEndo $ foldEndo
    &$ setVerbosity Annoying
    &$ setOutputFile "an.out.put"
````

Above example shows us that it is possible to modify `Config` as if it was a
monoid, but without actually having to state it as such. In practice it is not
always possible to define it as `Monoid`, or at least as a `Semigroup`.
Endomorphism are monoids under composition, therefore they are what usually
works in situations when the modified data type can not be instantiated as a
monoid.


### Working With Corner Cases

In real applications corner cases arise quite easily, e.g.
`System.IO.FilePath` has one pathological case, and that is \"\". There
is a lot of ways to handle it. Here we will concentrate only few basic
techniques to illustrate versatility of our approach.

````Haskell
-- | Trying to set output file to \"\" will result in keeping original value.
setOutputFile2 :: FilePath -> E Config
setOutputFile2 "" = id
setOutputFile2 fp = setOutputFile fp

example2 :: E Config
example2 = appEndo $ foldEndo
    &$ setVerbosity Annoying
    &$ setOutputFile2 "an.out.put"
````

Same as above, but exploits `instance AnEndo a => AnEndo Maybe a`:

````Haskell
setOutputFile3 :: FilePath -> Maybe (E Config)
setOutputFile3 "" = Nothing
setOutputFile3 fp = Just $ setOutputFile fp

example3 :: E Config
example3 = appEndo $ foldEndo
    &$ setVerbosity Annoying
    &$ setOutputFile3 "an.out.put"
````
Great thing about `Maybe` is the fact that it has `Alternative` and `MonadPlus`
instances. Using `guard` may simplify `setOutputFile3` in to definition like
following:

````Haskell
setOutputFile3':: FilePath -> Maybe (E Config)
setOutputFile3' fp = setOutputFile fp <$ guard (not (null fp))
````

Following example uses common pattern of using `Either` as error reporting
monad. This approach can be easily modified for arbitrary error reporting
monad.

````Haskell
setOutputFile4 :: FilePath -> Either String (E Config)
setOutputFile4 "" = Left "Output file: Empty file path."
setOutputFile4 fp = Right $ setOutputFile fp

example4 :: Either String (E Config)
example4 = appEndo <&$> foldEndo
    <*> pure (setVerbosity Annoying)
    <*> setOutputFile4 "an.out.put"
````

Notice, that above example uses applicative style. Normally when using this
style, for setting record values, one needs to keep in sync order of
constructor arguments and order of operations. Using `foldEndo` (and its
dual `dualFoldEndo`) doesn't have this restriction.

### Lenses

Instead of setter functions one may want to use lenses. In this example we use
types from [lens package][Hackage: lens], but definitions use function from
[between package][Hackage: between]:

````Haskell
verbosity :: Lens' Config Verbosity
verbosity = _verbosity ~@@^> \s b -> s{_verbosity = b}

outputFile :: Lens' Config FilePath
outputFile = _outputFile ~@@^> \s b -> s{_outputFile = b}
````

Now setting values of `Config` would look like:

````Haskell
example5 :: E Config
example5 = appEndo $ foldEndo
    &$ verbosity  .~ Annoying
    &$ outputFile .~ "an.out.put"
````

### Other Usage

Probably one of the most interesting things that can be done with this
module is following:

````Haskell
instance AnEndo Verbosity where
    type EndoOperatesOn Verbosity = Config
    anEndo = Endo . set verbosity

newtype OutputFile = OutputFile FilePath

instance AnEndo OutputFile where
    type EndoOperatesOn OutputFile = Config
    anEndo (OutputFile fp) = Endo $ outputFile .~ fp

example6 :: E Config
example6 = appEndo $ foldEndo
    &$ Annoying
    &$ OutputFile "an.out.put"
````


### Using with optparse-applicative

This is a more complex example that defines parser for
[optparse-applicative][Hackage: optparse-applicative] built on top of some of
the above definitions:

````Haskell
options :: Parser Config
options = runIdentityT $ runEndo defaultConfig <$> options'
  where
    -- All this IdentityT clutter is here to avoid orphan instances.
    options' :: IdentityT Parser (Endo Config)
    options' = foldEndo
        <*> outputOption     -- :: IdentityT Parser (Maybe (E Config))
        <*> verbosityOption  -- :: IdentityT Parser (Maybe (E Config))
        <*> annoyingFlag     -- :: IdentityT Parser (E Config)
        <*> silentFlag       -- :: IdentityT Parser (E Config)
        <*> verboseFlag      -- :: IdentityT Parser (E Config)

    defaultConfig :: Config
    defaultConfig = Config Normal ""

-- >>> :main -o an.out.put --annoying
-- Config {_verbosity = Annoying, _outputFile = "an.out.put"}
main :: IO ()
main = execParser (info options fullDesc) >>= print
````

Parsers for individual options and flags are wrapped in `IdentityT`, because
there is no following instance:

````Haskell
instance FoldEndoArgs r => FoldEndoArgs (Parser r)
````

But there is:

````Haskell
instance (Applicative f, FoldEndoArgs r) => FoldEndoArgs (IdentityT f r)
````

Functions used by the above code example:

````Haskell
outputOption :: IdentityT Parser (Maybe (E Config))
outputOption =
    IdentityT . optional . option (set outputFile <$> parseFilePath)
    $ short 'o' <> long "output" <> metavar "FILE"
        <> help "Store output in to a FILE."
  where
    parseFilePath = eitherReader $ \s ->
        if null s
            then Left "Option argument can not be empty file path."
            else Right s

verbosityOption :: IdentityT Parser (Maybe (E Config))
verbosityOption =
    IdentityT . optional . option (set verbosity <$> parseVerbosity)
    $ long "verbosity" <> metavar "LEVEL" <> help "Set verbosity to LEVEL."
  where
    verbosityToStr = map toLower . Data.showConstr . Data.toConstr
    verbosityIntValues = [(show $ fromEnum v, v) | v <- [Silent .. Annoying]]
    verbosityStrValues =
        ("default", Normal) : [(verbosityToStr v, v) | v <- [Silent .. Annoying]]

    parseVerbosityError = unwords
        [ "Verbosity can be only number from interval"
        , show $ map fromEnum [minBound, maxBound :: Verbosity]
        , "or one of the following:"
        , concat . intersperse ", " $ map fst verbosityStrValues
        ]

    parseVerbosity = eitherReader $ \s ->
        case lookup s $ verbosityIntValues ++ verbosityStrValues of
            Just v  -> Right v
            Nothing -> Left parseVerbosityError

annoyingFlag :: IdentityT Parser (E Config)
annoyingFlag = IdentityT . flag id (verbosity .~ Annoying)
    $ long "annoying" <> help "Set verbosity to maximum."

silentFlag :: IdentityT Parser (E Config)
silentFlag = IdentityT . flag id (verbosity .~ Silent)
    $ short 's' <> long "silent" <> help "Set verbosity to minimum."

verboseFlag :: IdentityT Parser (E Config)
verboseFlag = IdentityT . flag id (verbosity .~ Verbose)
    $ short 'v' <> long "verbose" <> help "Be verbose."
````


## Building Options

* `-fpedantic` (disabled by default)

  Pass additional warning flags to GHC.


## Contributions

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail.



[Hackage: between]:
  http://hackage.haskell.org/package/between
  "between package on Hackage"
[Hackage: endo]:
  http://hackage.haskell.org/package/endo
  "endo package on Hackage"
[Hackage: lens]:
  http://hackage.haskell.org/package/lens
  "lens package on Hackage"
[Hackage: optparse-applicative]:
  http://hackage.haskell.org/package/optparse-applicative
  "optparse-applicative package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

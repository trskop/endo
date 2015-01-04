# Endo

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


## Description

Endomorphism utilities.


## Usage Examples

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
monoid, but without actually having to state it as such. In practice it is
not always possible to define it as `Monoid` or at least a `Semigroup`. What
usually works are endomorphisms, like in this example.

Now, `FilePath` has one pathological case, and that is `""`. There is a lot of
ways to handle it. Here we will concentrate only on few basic techniques to
illustrate versatility of our approach.

````Haskell
-- | Trying to set output file to \"\" will result in keeping original
-- value.
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

Following example uses common pattern of using `Either` as error reporting
monad. This approach can be easily modified for arbitrary error reporting
monad.

````Haskell
setOutputFile4 :: FilePath -> Either String (E Config)
setOutputFile4 "" = Left "Output file: Empty file path."
setOutputFile4 fp = Right $ setOutputFile fp

example4 :: Either String (E Config)
example4 = fmap appEndo $ foldEndo
    <*> pure (setVerbosity Annoying)
    <*> setOutputFile4 "an.out.put"
````

Notice, that above example uses applicative style. Normally when using this
style, for setting record values, one needs to keep in sync order of
constructor arguments and order of operations. Using `foldEndo` (and its
dual `dualFoldEndo`) doesn't have this restriction.

Instead of setter functions one may want to use lenses (in terms of
[lens package][Hackage: Lens]):

````Haskell
verbosity :: Lens' Config Verbosity
verbosity = _verbosity ~@@^> \\s b -> s{_verbosity = b}

outputFile :: Lens' Config FilePath
outputFile = _outputFile ~@@^> \\s b -> s{_outputFile = b}
````

Now setting values of `Config` would look like:

````Haskell
example5 :: E Config
example5 = appEndo $ foldEndo
    &$ verbosity  .~ Annoying
    &$ outputFile .~ "an.out.put"
````

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



[Hackage: Lens]:
  http://hackage.haskell.org/package/lens
  "Lens package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

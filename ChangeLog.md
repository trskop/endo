# ChangeLog / ReleaseNotes


## Version 0.3.0.0

* Introducing type `:->` to simplify type signatures of endomorphism folding
  functions that restrict type of a result. (**new**)
* Type class `AnEndo` moved in to a separate module `Data.Monoid.Endo.AnEndo`.
  Definitions are reexported by `Data.Monoid.Endo.Fold`, therefore providing
  backward compatible API. (**change**)
* Introducing `FromEndo` type class for conversion of endomorphism in to a
  value. It is a dual to `AnEndo` type class. This type class resides in its
  own module `Data.Monoid.Endo.FromEndo`. (**new**)
* Introducing `ApplyEndo` newtype that provides easier endomorphism evaluation
  in cases when there is an "obvious" default value. This type has its own
  module `Data.Monoid.Endo.Apply` that also provides various helper functions
  and type class instances. (**new**)
* Bumped upper bound of [transformers package][transformers] to include 0.5.\*
  versions. (**change**)
* Synchronized API documentation of `Data.Monoid.Endo.Fold` with README.
  (**trivial change**)


## Version 0.2.0.1

* API documentation ehancements and clarifications.
* Explicit implementation of `aDualEndo` for
  `instance AnEndo a => AnEndo (Maybe a)`.
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/endo-0.2.0.1>


## Version 0.2.0.0

* Default implementation for `anEndo` method of 'AnEndo' type class, which is
  now defined as: `anEndo = getDual . aDualEndo`. As a consequence it is now
  possible to define complete instances of `AnEndo` by providing either
  `anEndo` or `aDualEndo`. (**new**, **change**)
* Introducing associated type `Result` to `FoldEndoArgs` type class. This
  allows result of the whole folding to be explicitly stated in a type
  signature. (**new**, **change**)
* Introducing functions `embedEndoWith` and `embedDualEndoWith`. Both can be
  used to simplify application of endomorphisms that are result of folding.
  (**new**)
    - `embedEndoWith :: (AnEndo e, EndoOperatesOn e ~ a) => (Endo a -> b) -> e
      -> b`
    - `embedDualEndoWith :: (AnEndo e, EndoOperatesOn e ~ a) => (Dual (Endo a)
      -> b) -> e -> b`
* Introducing `instance AnEndo (Proxy a)`, which is useful in cases when one
  needs to force constraint `EndoOperatesOn args ~ a` where `a` is the `a` from
  `Proxy a`. This is done by encoding value of `Proxy` in to identity
  endomorphism that operates on specified type `a`. (**new**)
* Introducing `instance (Monoid c, FoldEndoArgs r) => FoldEndoArgs (Const c
  r)`, which is useful in cases when one needs to discard the computation and
  return a constant instead. (**new**)
* Bumping upper bounds on base and between, therefore it now builds on GHC 7.10
  with base 4.8. (**new**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/endo-0.2.0.0>


## Version 0.1.0.2

* Bugfix release.
* Bugfix: Unable to compile with [transformers][] >= 0.4 (again).
* Minor documentation updates.
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/endo-0.1.0.2>


## Version 0.1.0.1

* Bugfix release.
* Bugfix: Unable to compile with [transformers][] >= 0.4.
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/endo-0.1.0.1>


## Version 0.1.0.0

* **This version doesn't work with [transformers][] >= 0.4.**
* First public release.
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/endo-0.1.0.0>



[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[transformers]:
  https://hackage.haskell.org/package/transformers
  "Package transformers on Hackage."

<!--
  vim: filetype=markdown softtabstop=4 shiftwidth=4 expandtab
-->

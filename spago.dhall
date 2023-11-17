{ name = "hydra-auction-offchain"
, dependencies =
  [ "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "errors"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

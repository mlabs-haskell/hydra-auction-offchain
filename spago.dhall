{ name = "hydra-auction-offchain"
, dependencies =
  [ "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "errors"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "prelude"
  , "record"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

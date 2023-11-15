{ name = "hydra-auction-offchain"
, dependencies =
  [ "cardano-transaction-lib"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

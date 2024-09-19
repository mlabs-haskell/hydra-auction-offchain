{ name = "hydra-auction-offchain"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "affjax"
  , "ansi"
  , "argonaut"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "cardano-key-wallet"
  , "cardano-plutus-data-schema"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "codec-argonaut"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "errors"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "formatters"
  , "gen"
  , "http-methods"
  , "httpure"
  , "integers"
  , "js-bigints"
  , "js-timers"
  , "lists"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "now"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "parsing"
  , "partial"
  , "plutus-types"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "quickcheck"
  , "random"
  , "read"
  , "record"
  , "refs"
  , "safely"
  , "spec"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-lists"
  , "uint"
  , "uplc-apply-args"
  , "uri"
  , "uuid"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "app/**/*.purs", "test/**/*.purs" ]
}

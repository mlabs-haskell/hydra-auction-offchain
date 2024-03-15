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
  , "cardano-transaction-lib"
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
  , "http-methods"
  , "httpure"
  , "integers"
  , "js-bigints"
  , "lists"
  , "maybe"
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
  , "ply-ctl"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "quickcheck"
  , "read"
  , "record"
  , "safely"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "uint"
  , "uri"
  , "uuid"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "app/**/*.purs", "test/**/*.purs" ]
}

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20230105/packages.dhall
        sha256:3e9fbc9ba03e9a1fcfd895f65e2d50ee2f5e86c4cd273f3d5c841b655a0e1bda

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "bigints"
          , "bignumber"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/errfrom/purescript-aeson.git"
        , version = "f614a840036aabb059017d03cf365f4199b9547b"
        }
      , bignumber =
        { dependencies =
          [ "console"
          , "effect"
          , "either"
          , "exceptions"
          , "functions"
          , "integers"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo = "https://github.com/errfrom/purescript-bignumber"
        , version = "9b3179ad07428d189e42a7a205aab9c7c4849d4a"
        }
      , properties =
        { dependencies = [ "prelude", "console" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
        , version = "v0.2.0"
        }
      , lattice =
        { dependencies = [ "prelude", "console", "properties" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
        , version = "v0.3.0"
        }
      , mote =
        { dependencies = [ "these", "transformers", "arrays" ]
        , repo = "https://github.com/garyb/purescript-mote"
        , version = "v1.1.0"
        }
      , medea =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "arrays"
          , "bifunctors"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "free"
          , "integers"
          , "lists"
          , "maybe"
          , "mote"
          , "naturals"
          , "newtype"
          , "node-buffer"
          , "node-fs-aff"
          , "node-path"
          , "nonempty"
          , "ordered-collections"
          , "parsing"
          , "partial"
          , "prelude"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-combinators"
          , "safely"
          , "spec"
          , "strings"
          , "these"
          , "transformers"
          , "typelevel"
          , "tuples"
          , "unicode"
          , "unordered-collections"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/errfrom/medea-ps.git"
        , version = "00981e4ce7249808413a6db8d88d849bbe85245a"
        }
      , purescript-toppokki =
        { dependencies =
          [ "prelude"
          , "record"
          , "functions"
          , "node-http"
          , "aff-promise"
          , "node-buffer"
          , "node-fs-aff"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-toppokki"
        , version = "f90f92f0ddf0eecc73705c1675db37918d18cbcb"
        }
      , noble-secp256k1 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "effect"
          , "prelude"
          , "spec"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-noble-secp256k1.git"
        , version = "a3c0f67e9fdb0086016d7aebfad35d09a08b4ecd"
        }
      , cardano-transaction-lib =
        { dependencies =
          [ "aeson"
          , "aff"
          , "aff-promise"
          , "aff-retry"
          , "affjax"
          , "argonaut"
          , "argonaut-codecs"
          , "arraybuffer-types"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "bigints"
          , "bignumber"
          , "checked-exceptions"
          , "console"
          , "control"
          , "crypto"
          , "datetime"
          , "debug"
          , "effect"
          , "either"
          , "encoding"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "formatters"
          , "functions"
          , "gen"
          , "heterogeneous"
          , "http-methods"
          , "identity"
          , "integers"
          , "js-date"
          , "lattice"
          , "lists"
          , "maybe"
          , "medea"
          , "media-types"
          , "monad-logger"
          , "mote"
          , "newtype"
          , "noble-secp256k1"
          , "node-buffer"
          , "node-child-process"
          , "node-fs"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-readline"
          , "node-streams"
          , "nonempty"
          , "now"
          , "numbers"
          , "optparse"
          , "ordered-collections"
          , "orders"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "purescript-toppokki"
          , "quickcheck"
          , "quickcheck-combinators"
          , "quickcheck-laws"
          , "random"
          , "rationals"
          , "record"
          , "refs"
          , "safe-coerce"
          , "spec"
          , "spec-quickcheck"
          , "strings"
          , "stringutils"
          , "tailrec"
          , "these"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "unfoldable"
          , "untagged-union"
          , "variant"
          , "web-html"
          , "web-storage"
          ]
        , repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git"
        , version = "5a560eb9ac1bef94d718b90f5ac2a541515127dd"
        }
      , errors =
        { dependencies =
          [ "control"
          , "effect"
          , "either"
          , "identity"
          , "maybe"
          , "newtype"
          , "prelude"
          , "test-unit"
          , "transformers"
          ]
        , repo = "https://github.com/passy/purescript-errors.git"
        , version = "670485beb1e026f77d52ca58ce10c145d96c11ba"
        }
      , ply-ctl =
        { dependencies =
          [ "effect"
          , "prelude"
          , "cardano-transaction-lib"
          , "bigints"
          , "aeson"
          , "either"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "tuples"
          , "arrays"
          , "uint"
          , "node-process"
          , "integers"
          ]
        , repo = "https://github.com/mlabs-haskell/ply-ctl.git"
        , version = "727b811b0d561cf13d5594b9352a7294e5a20378"
        }
      }

in  (upstream // additions)

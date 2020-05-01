{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-medea"
, dependencies =
  [ "aff"
  , "argonaut"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "generics-rep"
  , "lcg"
  , "leibniz"
  , "mote"
  , "naturals"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "nonempty"
  , "ordered-collections"
  , "parallel"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-combinators"
  , "safely"
  , "spec"
  , "these"
  , "transformers"
  , "tree"
  , "typelevel"
  , "unicode"
  , "unordered-collections"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

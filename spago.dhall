{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-medea"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "const"
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
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

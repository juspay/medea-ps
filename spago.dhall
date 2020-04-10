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
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "generics-rep"
  , "leibniz"
  , "naturals"
  , "node-buffer"
  , "node-fs-aff"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "safely"
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

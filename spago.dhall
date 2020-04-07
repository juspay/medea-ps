{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-medea"
, dependencies =
    [ "argonaut"
    , "console"
    , "control"
    , "effect"
    , "exceptions"
    , "foldable-traversable"
    , "free"
    , "generics-rep"
    , "leibniz"
    , "nonempty"
    , "ordered-collections"
    , "prelude"
    , "psci-support"
    , "transformers"
    , "tree"
    , "typelevel"
    , "unordered-collections"
    , "unsafe-coerce"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

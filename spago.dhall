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
    , "naturals"
    , "nonempty"
    , "ordered-collections"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "safely"
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

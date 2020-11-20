{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "lists"
  , "maybe"
  , "newtype"
  , "profunctor-lenses"
  , "psci-support"
  , "run"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

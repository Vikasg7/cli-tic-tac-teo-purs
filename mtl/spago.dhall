{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = 
   [ "console"
   , "effect"
   , "psci-support"
   , "transformers"
   , "aff"
   , "functions"
   , "lists"
   , "psci-support"
   , "tuples"
   ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web-extension"
, dependencies =
  [ "prelude"
  , "console"
  , "control"  
  , "effect"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web-extension"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "web-dom"
  , "yoga-tree"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

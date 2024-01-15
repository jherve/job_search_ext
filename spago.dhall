{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web-extension"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "nonempty"
  , "partial"
  , "prelude"
  , "strings"
  , "tuples"
  , "web-dom"
  , "yoga-tree"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

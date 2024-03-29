{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web-extension"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "int64"
  , "integers"
  , "js-promise"
  , "js-promise-aff"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-url"
  , "yoga-tree"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

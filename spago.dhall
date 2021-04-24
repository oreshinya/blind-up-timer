{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "blind-up-timer"
, dependencies =
  [ "arrays"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "grain"
  , "grain-router"
  , "integers"
  , "js-timers"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "simple-json"
  , "simple-ulid"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

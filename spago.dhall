{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "blind-up-timer"
, dependencies =
  [ "arrays"
  , "control"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "grain"
  , "grain-router"
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

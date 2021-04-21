module View.Root
  ( root
  ) where

import Prelude

import Grain (GProxy(..), VNode, useValue)
import Grain.Markup as H
import Grain.Router (useRouter)
import State.Route (Route(..))
import View.Root.NotFound (notFound)
import View.Root.StructuresIndex (structuresIndex)
import View.Root.StructuresNew (structuresNew)
import View.Root.StructuresShow (structuresShow)

root :: VNode
root = H.component do
  route <- useValue (GProxy :: _ Route)
  startRouter <- useRouter (GProxy :: _ Route)

  pure $ H.div
    # H.didCreate (const startRouter)
    # H.kids
        [ case route of
            StructuresIndex ->
              H.key "structuresIndex" structuresIndex
            StructuresNew ->
              H.key "structuresNew" structuresNew
            StructuresShow structureId ->
              H.key ("structuresShow" <> show structureId) $ structuresShow structureId
            NotFound ->
              H.key "notFound" notFound
        ]

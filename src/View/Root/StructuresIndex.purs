module View.Root.StructuresIndex
  ( structuresIndex
  ) where

import Prelude

import Data.Newtype (unwrap)
import Entity.Structure (Structure)
import Grain (GProxy(..), VNode, useValue)
import Grain.Markup as H
import Grain.Router (link)
import State.Structures (Structures(..))

structuresIndex :: VNode
structuresIndex = H.component do
  Structures structures <- useValue (GProxy :: _ Structures)

  pure $ H.div # H.kids
    [ link "/structures/new" # H.kids [ H.text "Add a structure" ]
    , H.div # H.kids (structureCard <$> structures)
    ]

structureCard :: Structure -> VNode
structureCard structure =
  H.key (show structure.id) $ H.div # H.kids
    [ H.span # H.kids [ H.text structure.title ]
    , link ("/structures/" <> unwrap structure.id) # H.kids
        [ H.text "Show"
        ]
    ]

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

  pure $ H.div # H.css styles # H.kids
    [ H.header # H.css headerStyles # H.kids
        [ H.h3 # H.kids [ H.text "Structures" ]
        , link "/structures/new" # H.kids [ H.text "Add a structure" ]
        ]
    , H.div # H.kids (structureItem <$> structures)
    ]

structureItem :: Structure -> VNode
structureItem structure =
  H.key (show structure.id) $ link ("/structures/" <> unwrap structure.id)
    # H.css itemStyles
    # H.kids [ H.text structure.title ]

styles :: String
styles =
  """
  .& {
    padding: 16px;
    width: 50%;
    height: 100vh;
    margin: 0 auto;
  }
  """

headerStyles :: String
headerStyles =
  """
  .& {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 32px;
  }
  .& h3 {
    margin-bottom: 0;
  }
  """

itemStyles :: String
itemStyles =
  """
  .& {
    display: block;
    padding: 8px 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    transition: all 0.2s linear;
  }
  .&:hover {
    background: #EEE;
  }
  """

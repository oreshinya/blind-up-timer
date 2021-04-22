module View.Root.NotFound
  ( notFound
  ) where

import Prelude

import Grain (VNode)
import Grain.Markup as H

notFound :: VNode
notFound =
  H.div # H.css styles # H.kids
    [ H.h1 # H.kids [ H.text "404" ]
    ]

styles :: String
styles =
  """
  .& {
    height: 100vh;
    display: flex;
    justify-content: center;
    align-items: center;
  }
  """

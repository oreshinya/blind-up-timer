module View.Root.NotFound
  ( notFound
  ) where

import Prelude

import Grain (VNode)
import Grain.Markup as H

notFound :: VNode
notFound =
  H.h1 # H.kids [ H.text "NotFound" ]

module Entity.SE
  ( SE
  , createSE
  , play
  ) where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLMediaElement as E
import Web.HTML.Window (document)

newtype SE = SE E.HTMLMediaElement

createSE :: String -> Effect SE
createSE url = do
  el <- window >>= document <#> toDocument >>= createElement "audio"
  let audio = unsafePartial $ fromJust $ E.fromElement el
  E.setSrc url audio
  pure $ SE audio

play :: SE -> Effect Unit
play (SE audio) = E.play audio

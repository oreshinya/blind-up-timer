module Util
  ( genId
  , nmap
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Simple.ULID (genULID, toString)
import Simple.ULID.Window (prng)

genId :: forall a. Newtype a String => Effect a
genId =
  genULID prng <#> toString <#> wrap

nmap :: forall t a. Newtype t a => (a -> a) -> t -> t
nmap f a = wrap $ f $ unwrap a

module Util
  ( genId
  , nmap
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Simple.ULID (genULID, toString)

genId :: forall a. Newtype a String => Effect a
genId =
  genULID <#> toString <#> wrap

nmap :: forall t a. Newtype t a => (a -> a) -> t -> t
nmap f a = wrap $ f $ unwrap a

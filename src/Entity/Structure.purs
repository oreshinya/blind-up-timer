module Entity.Structure
  ( StructureId(..)
  , Structure
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Entity.BlindSet (BlindSet)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype StructureId = StructureId String

derive newtype instance showStructureId :: Show StructureId
derive newtype instance eqStructureId :: Eq StructureId
derive newtype instance readForeignStructureId :: ReadForeign StructureId
derive newtype instance writeForeignStructureId :: WriteForeign StructureId
derive instance newtypeStructureId :: Newtype StructureId _

type Structure =
  { id :: StructureId
  , title :: String
  , blindSets :: Array BlindSet
  }

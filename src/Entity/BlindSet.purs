module Entity.BlindSet
  ( BlindSetId(..)
  , BlindSet
  , toExtraBlind
  , extraBlindMinutes
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Util (genId)

newtype BlindSetId = BlindSetId String

derive newtype instance showBlindSetId :: Show BlindSetId
derive newtype instance eqBlindSetId :: Eq BlindSetId
derive newtype instance readForeignBlindSetId :: ReadForeign BlindSetId
derive newtype instance writeForeignBlindSetId :: WriteForeign BlindSetId
derive instance newtypeBlindSetId :: Newtype BlindSetId _

type BlindSet =
  { id :: BlindSetId
  , small :: Int
  , big :: Int
  , minutes :: Int
  }

toExtraBlind :: BlindSet -> Effect BlindSet
toExtraBlind x = do
  id <- genId
  pure
    { id
    , small: x.small * 2
    , big: x.big * 2
    , minutes: extraBlindMinutes
    }

extraBlindMinutes :: Int
extraBlindMinutes = 5

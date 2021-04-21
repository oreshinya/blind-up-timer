module State.Route where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Entity.Structure (StructureId)
import Grain (class GlobalGrain, fromConstructor)
import Grain.Router (class Router, initialRouter)
import Grain.Router.Parser (end, lit, match, str)

data Route
  = StructuresIndex
  | StructuresNew
  | StructuresShow StructureId
  | NotFound

instance routerRoute :: Router Route where
  parse path =
    fromMaybe NotFound $ match path $
      StructuresIndex <$ end
      <|>
      StructuresNew <$ (lit "structures" *> lit "new") <* end
      <|>
      StructuresShow <<< wrap <$> (lit "structures" *> str) <* end

instance globalGrainRoute :: GlobalGrain Route where
  typeRefOf _ = fromConstructor NotFound
  initialState _ = initialRouter

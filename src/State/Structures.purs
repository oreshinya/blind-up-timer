module State.Structures
  ( Structures(..)
  , useStructure
  , useSaveStructure
  ) where

import Prelude

import Data.Array (cons, find)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Entity.Structure (Structure, StructureId)
import Grain (class GlobalGrain, GProxy(..), Render, fromConstructor, useFinder, useUpdater, useValue)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON_, writeJSON)
import Util (nmap)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype Structures = Structures (Array Structure)

derive newtype instance readForeignStructures :: ReadForeign Structures
derive newtype instance writeForeignStructures :: WriteForeign Structures
derive instance newtypeStructures :: Newtype Structures _

instance globalGrainStructures :: GlobalGrain Structures where
  typeRefOf _ = fromConstructor Structures
  initialState _ = do
    m <- fromStorage
    pure $ fromMaybe (Structures []) m

useStructure :: StructureId -> Render (Maybe Structure)
useStructure id = do
  Structures xs <- useValue (GProxy :: _ Structures)
  pure $ find (_.id >>> (_ == id)) xs

useSaveStructure :: Render (Structure -> Effect Unit)
useSaveStructure = do
  find <- useFinder (GProxy :: _ Structures)
  update <- useUpdater (GProxy :: _ Structures)
  pure \x -> do
    update $ nmap $ cons x
    find >>= toStorage

fromStorage :: Effect (Maybe Structures)
fromStorage = do
  val <- window >>= localStorage >>= getItem storeKey
  pure $ join $ readJSON_ <$> val

toStorage :: Structures -> Effect Unit
toStorage x =
  window >>= localStorage >>= setItem storeKey (writeJSON x)

storeKey :: String
storeKey = "structures"

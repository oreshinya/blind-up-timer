module View.Root.StructuresNew
  ( structuresNew
  ) where

import Prelude

import Data.Array (filter, length, snoc)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence)
import Effect (Effect)
import Entity.BlindSet (BlindSetId)
import Entity.Structure (StructureId, Structure)
import Grain (class LocalGrain, LProxy(..), VNode, fromConstructor, useFinder, useUpdater, useValue)
import Grain.Markup as H
import Grain.Router (navigateTo)
import Simple.JSON (readJSON_)
import State.Structures (useSaveStructure)
import Util (genId, nmap)
import Web.Event.Event (currentTarget)
import Web.HTML.HTMLInputElement as I

type BlindSetForm =
  { id :: BlindSetId
  , small :: String
  , big :: String
  , minutes :: String
  }

newtype NewState = NewState
  { id :: StructureId
  , title :: String
  , blindSets :: Array BlindSetForm
  , error :: Maybe String
  }

derive instance newtypeNewState :: Newtype NewState _

instance localGrainNewState :: LocalGrain NewState where
  typeRefOf _ = fromConstructor NewState
  initialState _ = do
    id <- genId
    pure $ NewState
      { id
      , title: ""
      , blindSets: []
      , error: Nothing
      }

initialBlindSet :: Effect BlindSetForm
initialBlindSet = do
  id <- genId
  pure
    { id
    , small: ""
    , big: ""
    , minutes: ""
    }

validateRoughly :: NewState -> Maybe Structure
validateRoughly (NewState s) = do
  if length s.blindSets <= 0
    then Nothing
    else pure unit
  blindSets <- sequence $ decodeBlindSet <$> s.blindSets
  pure { id: s.id, title: s.title, blindSets }
  where
    decodeBlindSet bs = do
      small <- readJSON_ bs.small
      big <- readJSON_ bs.big
      minutes <- readJSON_ bs.minutes
      pure { id: bs.id, small, big, minutes }

structuresNew :: VNode
structuresNew = H.component do
  NewState { title, blindSets, error } <- useValue (LProxy :: _ NewState)
  findState <- useFinder (LProxy :: _ NewState)
  updateState <- useUpdater (LProxy :: _ NewState)
  saveStructure <- useSaveStructure

  let add = do
        form <- initialBlindSet
        updateState $ nmap \s ->
          s { blindSets = snoc s.blindSets form }

      updateBlindSet id f =
        updateState $ nmap \s ->
          s { blindSets = s.blindSets <#> \form ->
                if form.id == id then f form else form
            }

      deleteBlindSet id =
        updateState $ nmap \s ->
          s { blindSets = filter
                (_.id >>> (_ /= id))
                s.blindSets
            }

      updateTitle evt =
        case currentTarget evt >>= I.fromEventTarget of
          Nothing -> pure unit
          Just el -> do
            val <- I.value el
            updateState $ nmap _ { title = val }

      save = do
        s <- findState
        case validateRoughly s of
          Nothing ->
            updateState $ nmap _ { error = Just "Something wrong." }
          Just structure -> do
            saveStructure structure
            updateState $ nmap _ { error = Nothing }
            navigateTo $ "/structures/" <> unwrap structure.id

  pure $ H.div # H.kids
    [ case error of
        Nothing -> H.span
        Just msg -> H.p # H.kids [ H.text msg ]
    , H.button
        # H.onClick (const save)
        # H.kids [ H.text "Save" ]
    , H.input
        # H.value title
        # H.onChange updateTitle
    , H.button
        # H.onClick (const add)
        # H.kids [ H.text "Add SB/BB" ]
    , H.div # H.kids
        (blindSetForm updateBlindSet deleteBlindSet <$> blindSets)
    ]

blindSetForm
  :: (BlindSetId -> (BlindSetForm -> BlindSetForm) -> Effect Unit)
  -> (BlindSetId -> Effect Unit)
  -> BlindSetForm
  -> VNode
blindSetForm onChange onDelete { id, small, big, minutes } =
  H.key (show id) $ H.div # H.kids
    [ H.button
        # H.onClick (const $ onDelete id)
        # H.kids [ H.text "Delete" ]
    , H.input
        # H.value minutes
        # H.placeholder "minutes"
        # H.onChange updateMinutes
    , H.input
        # H.value small
        # H.placeholder "SB"
        # H.onChange updateSmall
    , H.input
        # H.value big
        # H.placeholder "BB"
        # H.onChange updateBig
    ]

  where
    updateMinutes evt =
      case currentTarget evt >>= I.fromEventTarget of
        Nothing -> pure unit
        Just el -> do
          val <- I.value el
          onChange id _ { minutes = val }

    updateSmall evt =
      case currentTarget evt >>= I.fromEventTarget of
        Nothing -> pure unit
        Just el -> do
          val <- I.value el
          onChange id _ { small = val }

    updateBig evt =
      case currentTarget evt >>= I.fromEventTarget of
        Nothing -> pure unit
        Just el -> do
          val <- I.value el
          onChange id _ { big = val }

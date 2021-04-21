module View.Root.StructuresShow
  ( structuresShow
  ) where

import Prelude

import Data.Array (drop, head, last, snoc)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (class Newtype)
import Effect.Exception (throw)
import Effect.Timer (IntervalId, clearInterval, setInterval, setTimeout)
import Entity.BlindSet (BlindSet, toExtraBlind)
import Entity.SE (SE, createSE, play)
import Entity.Structure (StructureId)
import Grain (class LocalGrain, LProxy(..), VNode, fromConstructor, useFinder, useUpdater, useValue)
import Grain.Class.LProxy (initialState)
import Grain.Markup as H
import Grain.Router (redirectTo)
import State.Structures (useStructure)
import Util (nmap)

newtype ShowState = ShowState
  { blindSets :: Array BlindSet
  , currentBlindSet :: Maybe BlindSet
  , currentSeconds :: Int
  , intervalId :: Maybe IntervalId
  , se :: SE
  }

derive instance newtypeShowState :: Newtype ShowState _

instance localGrainShowState :: LocalGrain ShowState where
  typeRefOf _ = fromConstructor ShowState
  initialState _ = do
    se <- createSE "/blind_up.mp3"
    pure $ ShowState
      { blindSets: []
      , currentBlindSet: Nothing
      , currentSeconds: 0
      , intervalId: Nothing
      , se
      }

structuresShow :: StructureId -> VNode
structuresShow structureId = H.component do
  mStructure <- useStructure structureId
  ShowState { currentBlindSet } <- useValue (LProxy :: _ ShowState)
  findState <- useFinder (LProxy :: _ ShowState)
  updateState <- useUpdater (LProxy :: _ ShowState)

  let toNotFound = void $ setTimeout 1 do
        when (isNothing mStructure) $ redirectTo "/not_found"

      proceedNextBlind = do
        ShowState { blindSets } <- findState
        case head blindSets, last blindSets of
          Just fbs, Just lbs -> do
            extra <- toExtraBlind lbs
            updateState $ nmap _
              { blindSets = drop 1 $ snoc blindSets extra
              , currentBlindSet = Just fbs
              , currentSeconds = 0
              }
          _, _ -> throw "Something went wrong."

      startTimer =
        case mStructure of
          Nothing ->  throw "Something went wrong."
          Just structure -> do
            updateState $ nmap _ { blindSets = structure.blindSets }
            proceedNextBlind
            intervalId <- setInterval 1000 do
              ShowState s <- findState
              let nextSeconds = s.currentSeconds + 1
              updateState $ nmap _ { currentSeconds = nextSeconds }

              case s.currentBlindSet of
                Nothing -> throw "Something went wrong."
                Just { minutes } ->
                  when (nextSeconds >= minutes * 60) do
                    proceedNextBlind
                    play s.se
            updateState $ nmap _ { intervalId = Just intervalId}

      stopTimer = do
        ShowState { intervalId } <- findState
        maybe (pure unit) clearInterval intervalId
        state <- initialState (LProxy :: _ ShowState)
        updateState $ const state

  pure case mStructure of
    Nothing ->
      H.span # H.didCreate (const toNotFound)
    Just { title } ->
      H.div # H.kids
        [ H.h1 # H.kids [ H.text title ]
        , case currentBlindSet of
            Nothing ->
              H.button # H.onClick (const startTimer) # H.kids [ H.text "Start" ]
            Just bs ->
              H.div # H.kids
                [ H.div # H.kids [ H.text $ (show bs.small) <> "/" <> (show bs.big) ]
                , H.button # H.onClick (const stopTimer) # H.kids [ H.text "Stop" ]
                ]
        ]

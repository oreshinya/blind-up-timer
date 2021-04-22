module View.Root.StructuresShow
  ( structuresShow
  ) where

import Prelude

import Data.Array (drop, head, last, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Effect.Exception (throw)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Entity.BlindSet (BlindSet, extraBlindMinutes, toExtraBlind)
import Entity.SE (SE, createSE, play)
import Entity.Structure (StructureId)
import Grain (class LocalGrain, LProxy(..), VNode, fromConstructor, useFinder, useUpdater, useValue)
import Grain.Class.LProxy (initialState)
import Grain.Markup as H
import State.Structures (useStructure)
import Util (nmap)
import View.Root.NotFound (notFound)

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

  let proceedNextBlind = do
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
            updateState $ nmap _ { intervalId = Just intervalId }

      stopTimer = do
        ShowState { intervalId } <- findState
        maybe (pure unit) clearInterval intervalId
        state <- initialState (LProxy :: _ ShowState)
        updateState $ const state

  pure case mStructure of
    Nothing -> notFound
    Just { title, blindSets } ->
      H.div # H.css styles # H.kids
        [ H.div # H.css leftStyles # H.kids
            [ H.header # H.css headerStyles # H.kids
                [ H.h3 # H.kids [ H.text title ]
                , case currentBlindSet of
                    Nothing ->
                      H.button
                        # H.onClick (const startTimer)
                        # H.kids [ H.text "Start" ]
                    _ ->
                      H.button
                        # H.className "button-outline"
                        # H.onClick (const stopTimer)
                        # H.kids [ H.text "Stop" ]
                ]
            , H.div # H.css mainStyles # H.kids
                [ case currentBlindSet of
                    Nothing ->
                      H.span
                    Just bs ->
                      H.div # H.css blindStyles # H.kids
                        [ H.text $ (show bs.small) <> "/" <> (show bs.big)
                        ]
                ]
            ]
        , H.div # H.css rightStyles # H.kids
            [ H.table # H.kids
                [ H.thead # H.kids
                    [ H.tr # H.kids
                        [ H.th # H.kids [ H.text "Duration (m)" ]
                        , H.th # H.kids [ H.text "SB" ]
                        , H.th # H.kids [ H.text "BB" ]
                        ]
                    ]
                , H.tbody # H.kids ((blindRow <$> blindSets) <> [ extraBlindRow ])
                ]
            ]
        ]

blindRow :: BlindSet -> VNode
blindRow bs =
  H.key (show bs.id) $ H.tr # H.kids
    [ H.td # H.kids [ H.text $ show bs.minutes ]
    , H.td # H.kids [ H.text $ show bs.small ]
    , H.td # H.kids [ H.text $ show bs.big ]
    ]

extraBlindRow :: VNode
extraBlindRow =
  H.key "extra" $ H.tr # H.kids
    [ H.td # H.kids [ H.text (show extraBlindMinutes) ]
    , H.td # H.kids [ H.text "Double up" ]
    , H.td # H.kids [ H.text "Double up" ]
    ]

styles :: String
styles =
  """
  .& {
    padding: 16px;
    height: 100vh;
    display: flex;
    justify-content: center;
    align-items: center;
  }
  """

leftStyles :: String
leftStyles =
  """
  .& {
    padding-right: 16px;
    width: 70%;
    height: 100%;
    display: flex;
    justify-content: flex-start;
    align-items: flex-start;
    flex-direction: column;
  }
  """

headerStyles :: String
headerStyles =
  """
  .& {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  .& h3 {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    width: 80%;
    margin-bottom: 0;
  }
  .& button {
    margin-bottom: 0;
  }
  """

rightStyles :: String
rightStyles =
  """
  .& {
    padding-left: 16px;
    width: 30%;
    height: 100%;
    border-left: 2px solid #EEE;
  }
  """

mainStyles :: String
mainStyles =
  """
  .& {
    flex-grow: 1;
    width: 100%;
    display: flex;
    justify-content: center;
    align-items: center;
  }
  """

blindStyles :: String
blindStyles =
  """
  .& {
    font-size: 80px;
  }
  """

module App.CurrentVolSelector (State, Action(..), spec, initialState, changeVols) where

import Prelude 

import Data.List (List, (!!), find, toUnfoldable)
import Data.Maybe (Maybe(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Common (unsafeEventSelectedIndex, isJustWith, sortWith)
import ServerTypes (Volunteer(..))

type State = { vols :: List Volunteer
             , currentVol :: Maybe Volunteer
             } 
 
data Action = ChangeCurrentVol (Maybe Volunteer)

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.select [ RP.className "vol-select"
                , RP.onChange $ dispatch <<< respond state <<< unsafeEventSelectedIndex
                ]
             (  [ RD.option [ RP.value "" ]
                            [ RD.text "All volunteers" ]
                ]
             <> map (option dispatch state.currentVol) (toUnfoldable state.vols)
             )
    ]

  respond :: State -> Int -> Action
  respond state i | i > 0 = ChangeCurrentVol $ state.vols !! (i - 1)
  respond _ _ = ChangeCurrentVol Nothing

  findVol :: List Volunteer -> Maybe Int -> Maybe Volunteer
  findVol vols = (=<<) \id -> find (\(Volunteer v) -> v.vId == id) vols

  option :: _ -> Maybe Volunteer -> Volunteer -> ReactElement
  option dispatch currentVolId (Volunteer v) = RD.option [ RP.selected $ isJustWith (\(Volunteer cv) -> cv.vId == v.vId) currentVolId
                                             , RP.value $ show v.vId
                                             ]
                                             [ RD.text v.vName ]

  performAction :: T.PerformAction _ State _ Action
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \state -> state{ currentVol = v }

initialState :: List Volunteer -> Maybe Volunteer -> State
initialState vols currentVol = { vols: sortWith (\(Volunteer v) -> v.vName) vols
                               , currentVol: currentVol
                               }

changeVols :: List Volunteer -> Maybe Volunteer -> State -> State
changeVols vols currentVol state = state { vols = sortWith (\(Volunteer v) -> v.vName) vols
                                         , currentVol = currentVol
                                         }
module App.CurrentVolSelector (State, Action(..), spec, initialState, changeVols) where

import Prelude

import App.Common (unsafeEventSelectedIndex, ifJust)
import App.Data (Volunteer(..), VolId(..), parseVolId)
import Data.Array ((!!), find)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type State = { vols :: Array Volunteer
             , currentVol :: Maybe Volunteer
             } 
 
data Action = ChangeCurrentVol (Maybe Volunteer)
            | DefineNewVol

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
                , RD.option [ RP.value "" ]
                            [ RD.text "New volunteer" ]
                ]
             <> map (option dispatch state.currentVol) state.vols
             )
    ]

  respond :: State -> Int -> Action
  respond _     1 = DefineNewVol
  respond state i | i > 1 = ChangeCurrentVol $ state.vols !! (i - 2)
  respond _ _ = ChangeCurrentVol Nothing

  findVol :: Array Volunteer -> Maybe VolId -> Maybe Volunteer
  findVol vols = (=<<) \id -> find (\v -> v.id == id) vols

  option :: _ -> Maybe Volunteer -> Volunteer -> ReactElement
  option dispatch currentVolId v = RD.option [ RP.selected $ ifJust (\cv -> cv.id == v.id) currentVolId
                                             , RP.value $ show v.id
                                             ]
                                             [ RD.text v.name ]

  performAction :: T.PerformAction _ State _ Action
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \state -> state{ currentVol = v }
  performAction _ _ _ = pure unit 

initialState :: Array Volunteer -> Maybe Volunteer -> State
initialState vols currentVol = { vols: vols
                               , currentVol: currentVol
                               }

changeVols :: Array Volunteer -> Maybe Volunteer -> State -> State
changeVols vols currentVol state = state { vols = vols
                                         , currentVol = currentVol
                                         }

module App.CurrentVolSelector (State, VolState, Action(..), spec, initialState, changeVols) where

import Prelude

import App.Common (unsafeEventValue)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
 
import App.Data (Volunteer(..), VolId(..), parseVolId)

type VolState = { id :: VolId
                , name :: String
                }
 
type State = { vols :: Array VolState
             , currentVolId :: Maybe VolId
             } 
 
data Action = ChangeCurrentVol (Maybe VolId)

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.select [ RP.className "vol-select"
                , RP.onChange \e -> dispatch (ChangeCurrentVol $ parseVolId $ unsafeEventValue e)
                ]
              ( [ RD.option [ RP.value "" ]
                            [ RD.text "All volunteers" ] ]
                <> map (option dispatch state.currentVolId) state.vols
              )
    ]
  
  option :: _ -> Maybe VolId -> VolState -> ReactElement
  option dispatch currentVolId {id, name} = RD.option [ RP.selected (maybe false (_ == id) currentVolId) 
                                                      , RP.value $ show id
                                                      ]
                                                      [ RD.text name ]

  performAction :: T.PerformAction _ State _ Action
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \state -> state{ currentVolId = v }
  performAction _ _ _ = pure unit 

initialState :: Array Volunteer -> Maybe Volunteer -> State
initialState vols currentVol = { vols: map (unwrap >>> \{id, name} -> {id, name}) vols
                               , currentVolId: map (unwrap >>> _.id) currentVol
                               }

changeVols :: Array Volunteer -> State -> State
changeVols vols state = state { vols = map (unwrap >>> \{id, name} -> {id, name}) vols }
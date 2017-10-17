module App.CurrentVolSelector (CurrentVolSelectorState, VolState, CurrentVolSelectorAction(..), currentVolSelectorSpec, currentVolSelectorInitialState) where

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
 
type CurrentVolSelectorState = { vols :: Array VolState
                       , currentVolId :: Maybe VolId
                       } 
 
data CurrentVolSelectorAction = ChangeCurrentVol (Maybe VolId)

currentVolSelectorSpec :: T.Spec _ CurrentVolSelectorState _ CurrentVolSelectorAction
currentVolSelectorSpec = T.simpleSpec performAction render
  where 
  render :: T.Render CurrentVolSelectorState _ CurrentVolSelectorAction
  render dispatch _ state _ =
    [ RD.div [ RP.className "ui form" ]
             [ RD.div [ RP.className "field" ]
                      [ RD.label [ RP.htmlFor "volSelect" ]
                                 [ RD.text "Volunteer" ]
                      , RD.select [ RP.onChange \e -> dispatch (ChangeCurrentVol $ parseVolId $ unsafeEventValue e) ]
                                  ( [ RD.option [ RP.value "" ]
                                                [ RD.text "Select a volunteer" ] ]
                                    <> map (option dispatch state.currentVolId) state.vols
                                  )
                      
                      ]
             ]
    ]
  
  option :: _ -> Maybe VolId -> VolState -> ReactElement
  option dispatch currentVolId {id, name} = RD.option [ RP.selected (maybe false (_ == id) currentVolId) 
                                                      , RP.value $ show id
                                                      ]
                                                      [ RD.text name ]

  performAction :: T.PerformAction _ CurrentVolSelectorState _ CurrentVolSelectorAction
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \state -> state{ currentVolId = v }
  performAction _ _ _ = pure unit 

currentVolSelectorInitialState :: Array Volunteer -> Maybe Volunteer -> CurrentVolSelectorState
currentVolSelectorInitialState vols currentVol = { vols: map (unwrap >>> \{id, name} -> {id, name}) vols
                                         , currentVolId: map (unwrap >>> _.id) currentVol
                                         }
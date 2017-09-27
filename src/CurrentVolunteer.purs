module App.CurrentVolunteer (CurrentVolProps, CurrentVolState, CurrentVolAction(..), currentVolSpec, currentVolInitialState) where

import Prelude

import App.Common (unsafeEventSelectedIndex)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Data (Volunteer(..))

type CurrentVolProps = {  }
 
type CurrentVolState = { vols :: Array Volunteer
                       , currentVol :: Maybe Volunteer } 
 
data CurrentVolAction = ChangeCurrentVol (Maybe Volunteer)

currentVolSpec :: T.Spec _ CurrentVolState _ CurrentVolAction
currentVolSpec = T.simpleSpec performAction render
  where
  render :: T.Render CurrentVolState _ CurrentVolAction
  render dispatch _ state _ =
    [ RD.span [ RP.className "float-right" ]
                [ RD.select [ RP.onChange \e -> dispatch (ChangeCurrentVol $ state.vols !! ((unsafeEventSelectedIndex e) - 1)) ]
                           ([ RD.option [ RP.value "" ]
                                        [ RD.text "Select a volunteer" ] ]
                           <> map (option dispatch state.currentVol) state.vols)
              ]
    ]
  
  option :: _ -> Maybe Volunteer -> Volunteer -> ReactElement
  option dispatch currentVol (Vol v) = RD.option [ RP.selected (maybe false (\(Vol cv) -> cv.id == v.id) currentVol) 
                                                 , RP.value $ show v.id
                                                 ]
                                                 [ RD.text v.name ]

  performAction :: T.PerformAction _ CurrentVolState _ CurrentVolAction
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \state -> state{ currentVol = v }
  performAction _ _ _ = pure unit

currentVolInitialState :: Array Volunteer -> Maybe Volunteer -> CurrentVolState
currentVolInitialState vols currentVol = { vols: vols
                                         , currentVol: currentVol }
module App.CurrentVolunteer (CurrentVolunteerProps, CurrentVolunteerState, CurrentVolunteerAction(..), currentVolunteerSpec, currentVolunteerInitialState) where

import Prelude

import App.Common (unsafeEventSelectedIndex)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Data (Volunteer)

type CurrentVolunteerProps = { volunteers :: Array Volunteer }
 
type CurrentVolunteerState = { currentVolunteer :: Maybe Volunteer } 
 
data CurrentVolunteerAction = ChangeCurrentVolunteer (Maybe Volunteer)

currentVolunteerSpec :: T.Spec _ (Tuple CurrentVolunteerProps CurrentVolunteerState) _ CurrentVolunteerAction
currentVolunteerSpec = T.simpleSpec performAction render
  where
  render :: T.Render (Tuple CurrentVolunteerProps CurrentVolunteerState) _ CurrentVolunteerAction
  render dispatch _ (Tuple props state) _ =
    [ RD.span [ RP.className "float-right" ]
                [ RD.select [ RP.onChange \e -> dispatch (ChangeCurrentVolunteer $ props.volunteers !! ((unsafeEventSelectedIndex e) - 1)) ]
                          ([ RD.option [ RP.value "" ]
                                       [ RD.text "Select a volunteer" ] ]
                          <> map (option dispatch state.currentVolunteer) props.volunteers)
              ]
    ]
  
  option :: _ -> Maybe Volunteer -> Volunteer -> ReactElement
  option dispatch currentVolunteer v = RD.option [ RP.selected (maybe false (\cv -> cv.id == v.id) currentVolunteer) 
                                                 , RP.value $ show v.id
                                                 ]
                                                 [ RD.text v.name ]

  performAction :: T.PerformAction _ (Tuple CurrentVolunteerProps CurrentVolunteerState) _ CurrentVolunteerAction
  performAction (ChangeCurrentVolunteer v) _ _ = void $ T.modifyState \(Tuple props state) -> Tuple props state{ currentVolunteer = v }
  performAction _ _ _ = pure unit

currentVolunteerInitialState :: CurrentVolunteerProps -> Maybe Volunteer -> CurrentVolunteerState
currentVolunteerInitialState volunteers currentVolunteer = { currentVolunteer: currentVolunteer }
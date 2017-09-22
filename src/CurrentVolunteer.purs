module App.CurrentVolunteer (CurrentVolunteerState, Volunteer, CurrentVolunteerAction(..), currentVolunteerSpec) where

import Prelude

import App.Common (unsafeEventSelectedIndex)
import Data.Array ((!!))
import Data.Maybe (Maybe, maybe)
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type Volunteer = { id :: Int
                 , name :: String }
   
type CurrentVolunteerState = { volunteers :: Array Volunteer
                             , currentVolunteer :: Maybe Volunteer }
 
data CurrentVolunteerAction = ChangeCurrentVolunteer (Maybe Volunteer)

currentVolunteerSpec :: T.Spec _ CurrentVolunteerState _ CurrentVolunteerAction
currentVolunteerSpec = T.simpleSpec performAction render
  where
  render :: T.Render CurrentVolunteerState _ CurrentVolunteerAction
  render dispatch _ state _ =
    [ RD.span [ RP.className "float-right" ]
              [ RD.select [ RP.onChange \e -> dispatch (ChangeCurrentVolunteer $ state.volunteers !! ((unsafeEventSelectedIndex e) - 1)) ]
                          ([ RD.option [ RP.value "" ]
                                       [ RD.text "Select a volunteer" ] ]
                          <> map (option dispatch state) state.volunteers)
              ]
    ]
  
  option :: _ -> CurrentVolunteerState -> Volunteer -> ReactElement
  option dispatch state v = RD.option [ RP.selected (maybe false (\cv -> cv.id == v.id) state.currentVolunteer) 
                                      , RP.value $ show v.id
                                      ]
                                      [ RD.text v.name ]

  performAction :: T.PerformAction _ CurrentVolunteerState _ CurrentVolunteerAction
  performAction (ChangeCurrentVolunteer v) _ _ = void $ T.modifyState \state -> state { currentVolunteer = v }

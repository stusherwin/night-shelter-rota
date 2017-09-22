module App.CurrentVolunteer (CurrentVolunteerState, VolunteerState, CurrentVolunteerAction(..), currentVolunteerSpec, buildCurrentVolunteerState, updateCurrentVolunteerState) where

import Prelude

import App.Common (unsafeEventSelectedIndex)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe)
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type VolunteerState = { id :: Int
                      , name :: String }
 
type CurrentVolunteerState = { volunteers :: Array VolunteerState
                             , currentVolunteerId :: Maybe Int } 
 
data CurrentVolunteerAction = ChangeCurrentVolunteer (Maybe Int)

currentVolunteerSpec :: T.Spec _ CurrentVolunteerState _ CurrentVolunteerAction
currentVolunteerSpec = T.simpleSpec performAction render
  where
  render :: T.Render CurrentVolunteerState _ CurrentVolunteerAction
  render dispatch _ state _ =
    [ RD.span [ RP.className "float-right" ]
                [ RD.select [ RP.onChange \e -> dispatch (ChangeCurrentVolunteer $ (_.id) <$> state.volunteers !! ((unsafeEventSelectedIndex e) - 1)) ]
                          ([ RD.option [ RP.value "" ]
                                       [ RD.text "Select a volunteer" ] ]
                          <> map (option dispatch state) state.volunteers)
              ]
    ]
  
  option :: _ -> CurrentVolunteerState -> VolunteerState -> ReactElement
  option dispatch state v = RD.option [ RP.selected (maybe false (_ == v.id) state.currentVolunteerId) 
                                      , RP.value $ show v.id
                                      ]
                                      [ RD.text v.name ]

  performAction :: T.PerformAction _ CurrentVolunteerState _ CurrentVolunteerAction
  performAction _ _ _ = pure unit

buildCurrentVolunteerState :: Array VolunteerState -> Maybe VolunteerState -> CurrentVolunteerState
buildCurrentVolunteerState volunteers currentVolunteer =
  { volunteers: volunteers
  , currentVolunteerId: maybe Nothing (\v -> Just v.id) currentVolunteer
  }

updateCurrentVolunteerState :: Maybe VolunteerState -> CurrentVolunteerState -> CurrentVolunteerState
updateCurrentVolunteerState currentVolunteer state = state { currentVolunteerId = maybe Nothing (\v -> Just v.id) currentVolunteer }
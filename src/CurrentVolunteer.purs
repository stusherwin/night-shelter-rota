module CurrentVolunteer (CurrentVolunteerState, Volunteer, CurrentVolunteerAction(..), currentVolunteerSpec) where

import Prelude

import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..), fromRight, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Time.Duration (Days(..))
import Partial.Unsafe (unsafePartial)

import React (ReactElement)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM 
import Thermite as T

import Common (unsafeEventValue)

type Volunteer = { name :: String }
   
type CurrentVolunteerState = Maybe Volunteer
 
data CurrentVolunteerAction = ChangeCurrentVolunteer Volunteer

currentVolunteerSpec :: T.Spec _ CurrentVolunteerState _ CurrentVolunteerAction
currentVolunteerSpec = T.simpleSpec performAction render
  where
  render :: T.Render CurrentVolunteerState _ CurrentVolunteerAction
  render dispatch _ state _ =
    [ RD.span [ RP.className "float-right" ]
              [ RD.text "Current volunteer: "
              , RD.text (maybe "None" _.name state) ] 
    ]

  performAction :: T.PerformAction _ CurrentVolunteerState _ CurrentVolunteerAction
  performAction _ _ _ = pure unit
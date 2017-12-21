module ShiftListState where

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.DateTime (Date, Weekday(..), weekday)
import App.ShiftRules(ShiftRuleConfig(..))

import App.Types (Volunteer, Shift, VolunteerShift, ShiftType(..), OvernightPreference(..), OvernightGenderPreference(..))

data RowAction = PrevPeriod
               | NextPeriod
               | AddCurrentVol Date ShiftType
               | RemoveCurrentVol Date
               | ChangeCurrentVolShiftType Date ShiftType

data Action = RowAction Int RowAction

type State = { roster :: RosterState
             , config :: ShiftRuleConfig
             , rows :: List RowState
             }

type RosterState = { currentVol :: Maybe Volunteer
                   , shifts :: List Shift
                   , startDate :: Date
                   , endDate :: Date
                   , loading :: Boolean
                   }

data RowState = ShiftRow ShiftRowState
              | HeaderRow HeaderRowState

type HeaderRowState = { text :: String
                      , showActions :: Boolean  
                      }

type ShiftRowState = { date :: Date
                     , currentDate :: Date
                     , status :: ShiftStatus
                     , currentVol :: Maybe CurrentVolState
                     , noOfVols :: Int
                     , maxVols :: Int
                     , volMarkers :: List VolMarkerState
                     , loading :: Boolean
                     }

type VolMarkerState = { name :: String
                      , shiftType :: ShiftType
                      , sharingPrefs :: Array SharingPref
                      }

type CurrentVolState = { name :: String
                       , shiftType :: Maybe ShiftType
                       , canAddOvernight :: Boolean
                       , canAddEvening :: Boolean
                       , canChangeShiftType :: Boolean
                       }  

data SharingPref = GM
                 | GF
                 | P1
                 | P2
                 | N String

data ShiftStatus = Good
                 | Warning String
                 | Error String
                 | Info String
                 | OK
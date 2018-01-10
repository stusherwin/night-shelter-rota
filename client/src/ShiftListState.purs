module ShiftListState where

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.DateTime (Date, Weekday(..), weekday)
import App.ShiftRules(ShiftRuleConfig(..))

import App.Types (Vol, Shift, VolShift, ShiftType(..), OvernightPreference(..), OvernightGenderPreference(..))

data RowAction = PrevPeriod
               | NextPeriod
               | AddCurrentVol Date ShiftType
               | RemoveCurrentVol Date
               | ChangeCurrentVolShiftType Date ShiftType
               | ShowVolInfo Vol

data Action = RowAction Int RowAction

type State = { roster :: RosterState
             , config :: ShiftRuleConfig
             , rows :: List RowState
             }

type RosterState = { currentVol :: Maybe Vol
                   , shifts :: List Shift
                   , startDate :: Date
                   , endDate :: Date
                   , loading :: Boolean
                   }

data RowState = ShiftRow ShiftRowState
              | HeaderRow HeaderRowState

type HeaderRowState = { text :: String
                      , showNext :: Boolean  
                      , showPrev :: Boolean  
                      }

type ShiftRowState = { date :: Date
                     , currentDate :: Date
                     , status :: ShiftStatus
                     , currentVol :: Maybe CurrentVolState
                     , noOfVols :: Int
                     , maxVols :: Int
                     , volMarkers :: List VolShift
                     , loading :: Boolean
                     }

type CurrentVolState = { name :: String
                       , shiftType :: Maybe ShiftType
                       , canAddOvernight :: Boolean
                       , canAddEvening :: Boolean
                       , canChangeShiftType :: Boolean
                       }  

data ShiftStatus = Good
                 | Warning String
                 | Error String
                 | Info String
                 | OK
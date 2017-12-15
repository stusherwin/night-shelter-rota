module App.ShiftList (Action(..), State, RosterState, spec, initialState, changeCurrentVol, shiftUpdated) where
   
import Prelude 

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, Weekday(..))
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), uncurry)
import React.DOM as RD 
import React.DOM.Props as RP
import Thermite as T

import App.Common (tomorrow, modifyWhere, toMonthYearString, isFirstDayOfMonth, addDays, previousWeekday)
import App.Data (Config, updateVolunteer)
import App.Types (Shift, Volunteer, VolunteerShift)
import App.ShiftRow (Action(..), initialState) as SR
import App.Row (Action(..), HeaderRowAction(..), State(..), spec) as R

shiftCount :: Int
shiftCount = 28

data Action = AddShift
            | RowAction Int R.Action

type RosterState = { currentVol :: Maybe Volunteer
                   , shifts :: List Shift
                   , startDate :: Date
                   , endDate :: Date
                   , loading :: Boolean
                   }

type State = { roster :: RosterState
             , config :: Config
             , rows :: List R.State
             }

_rows :: Lens' State (List R.State)
_rows = lens _.rows _{rows = _}

_RowAction :: Prism' Action (Tuple Int R.Action)
_RowAction = prism (uncurry RowAction) unwrap
  where
  unwrap (RowAction i a) = Right (Tuple i a)
  unwrap ta = Left ta

spec :: forall props eff. T.Spec eff State props Action
spec = 
  ( table $ T.focus _rows _RowAction $ T.foreach \_ -> R.spec )
  <> footerSpec
  where
  table :: T.Spec eff State props Action -> T.Spec eff State props Action
  table = over T._render \render d p s c ->
    [ RD.table [ RP.className "ui structured unstackable table" ]
               [ RD.tbody' $ render d p s c
               ]
    ]
      
  footerSpec :: T.Spec _ State _ Action
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ state _ = []

    performAction :: T.PerformAction _ State _ Action
    performAction (RowAction _ (R.HeaderRowAction R.PrevPeriod)) _ _ = void do
      T.modifyState \state -> adjustPeriod (-shiftCount) state
    performAction (RowAction _ (R.HeaderRowAction R.NextPeriod)) _ _ = void do
      T.modifyState \state -> adjustPeriod shiftCount state
    performAction _ _ _ = pure unit
    
initialState :: forall c. Maybe Volunteer -> List Shift -> Config -> State
initialState currentVol shifts config = 
  let startDate = previousWeekday Monday config.currentDate 
      endDate = addDays (shiftCount - 1) startDate
      roster = { currentVol
               , shifts
               , startDate
               , endDate
               , loading: false
               }
  in { roster
     , config
     , rows: rows roster config
     }
 
rows :: RosterState -> Config -> List R.State
rows roster config = rows' roster.startDate
  where   
  rows' :: Date -> List R.State
  rows' date | date > roster.endDate = 
      Cons (R.EndRow { text: "" })
    $ Nil
  rows' date | date == roster.startDate =
      Cons (R.StartRow { text: toMonthYearString date})
    $ Cons (R.ShiftRow $ SR.initialState roster config date)
    $ rows' $ tomorrow date
  rows' date | isFirstDayOfMonth date =
      Cons (R.MonthHeaderRow { text: toMonthYearString date })
    $ Cons (R.ShiftRow $ SR.initialState roster config date)
    $ rows' $ tomorrow date
  rows' date =
      Cons (R.ShiftRow $ SR.initialState roster config date)
    $ rows' $ tomorrow date

preserveLoading :: List R.State -> List R.State -> List R.State
preserveLoading = zipWith row
  where
  row (R.ShiftRow old) (R.ShiftRow new) = R.ShiftRow new { loading = old.loading }
  row _ new = new
 
changeCurrentVol :: Maybe Volunteer -> State -> State
changeCurrentVol currentVol state =
  let roster' = state.roster { currentVol = currentVol
                             , loading = false
                             }
  in state { roster = roster'
           , rows = preserveLoading state.rows $ rows roster' state.config
           }

adjustPeriod :: Int -> State -> State
adjustPeriod adj state = 
  let roster' = state.roster { startDate = addDays adj state.roster.startDate
                             , endDate = addDays adj state.roster.endDate
                             , loading = false
                             }
  in state { roster = roster'
           , rows = rows roster' state.config
           }

shiftUpdated :: List Shift -> Date -> State -> State
shiftUpdated shifts date state =
  let roster' = state.roster { shifts = shifts }

      isShiftOnDate :: R.State -> Boolean
      isShiftOnDate (R.ShiftRow s) = s.date == date
      isShiftOnDate _ = false

      cancelLoading :: R.State -> R.State
      cancelLoading (R.ShiftRow s) = R.ShiftRow s{ loading = false }
      cancelLoading r = r 
  in state { roster = roster'
           , rows = modifyWhere isShiftOnDate cancelLoading $ preserveLoading state.rows $ rows roster' state.config
           }
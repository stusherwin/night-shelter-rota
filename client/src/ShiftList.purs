module App.ShiftList (Action(..), State, RosterState, spec, initialState, changeCurrentVol) where
   
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
import App.Data (Config, Shift, Volunteer, VolunteerShift(Evening, Overnight), addVolunteerShift, changeVolunteerShift, removeVolunteerShift, updateVolunteer) as D
import App.ShiftRow (Action(..), initialState) as SR
import App.Row (Action(..), HeaderRowAction(..), State(..), spec) as R
import App.CurrentVolShiftEdit (Action(..), ShiftType(..)) as CVSE

shiftCount :: Int
shiftCount = 28

data Action = AddShift
            | RowAction Int R.Action

type RosterState = { currentVol :: Maybe D.Volunteer
                   , shifts :: List D.Shift
                   , startDate :: Date
                   , endDate :: Date
                   , loading :: Boolean
                   }

type State = { roster :: RosterState
             , config :: D.Config
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
    performAction (RowAction _ (R.ShiftRowAction (SR.CurrentVolShiftEditAction (CVSE.AddCurrentVol shiftDate CVSE.Overnight)))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.CurrentVolShiftEditAction (CVSE.AddCurrentVol shiftDate CVSE.Evening)))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.CurrentVolShiftEditAction (CVSE.ChangeCurrentVolShiftType shiftDate)))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate cv.id
    performAction (RowAction _ (R.ShiftRowAction (SR.CurrentVolShiftEditAction (CVSE.RemoveCurrentVol shiftDate)))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.removeVolunteerShift shiftDate cv
    performAction (RowAction _ (R.HeaderRowAction R.PrevPeriod)) _ _ = void do
      T.modifyState \state -> adjustPeriod (-shiftCount) state
    performAction (RowAction _ (R.HeaderRowAction R.NextPeriod)) _ _ = void do
      T.modifyState \state -> adjustPeriod shiftCount state
    performAction _ _ _ = pure unit
    
    delay' = lift $ liftAff $ delay (Milliseconds 500.0)

initialState :: forall c. Maybe D.Volunteer -> List D.Shift -> D.Config -> State
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
 
rows :: RosterState -> D.Config -> List R.State
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
 
changeCurrentVol :: Maybe D.Volunteer -> State -> State
changeCurrentVol currentVol state =
  let shifts' = maybe state.roster.shifts (\vol -> D.updateVolunteer vol state.roster.shifts) currentVol
      roster' = state.roster { currentVol = currentVol
                             , shifts = shifts'
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

modifyShifts :: State -> Date -> (List D.Shift -> List D.Shift) -> State
modifyShifts state date modify =
  let shifts = modify state.roster.shifts
      roster' = state.roster { shifts = shifts }

      isShiftOnDate :: R.State -> Boolean
      isShiftOnDate (R.ShiftRow s) = s.date == date
      isShiftOnDate _ = false

      cancelLoading :: R.State -> R.State
      cancelLoading (R.ShiftRow s) = R.ShiftRow s{ loading = false }
      cancelLoading r = r 
  in state { roster = roster'
           , rows = modifyWhere isShiftOnDate cancelLoading $ preserveLoading state.rows $ rows roster' state.config
           }
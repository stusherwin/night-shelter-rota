module App.ShiftList (Action(..), State, RosterState, spec, initialState, changeCurrentVol) where
  
import Prelude

import App.Common (lensOfListWithProps, tomorrow, modifyListWhere, surroundIf, default, toMonthYearString, daysLeftInMonth, isFirstDayOfMonth, sortWith, addDays)
import App.Data (OvernightSharingPrefs(..), Shift(..), Volunteer(..), VolunteerShift(..), RuleResult(..), canAddVolunteer, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType, updateVolunteer) as D
import App.ShiftRow (CurrentVolState, OtherVolState, Action(..), State(..), ShiftStatus(..), ShiftType(..), spec, initialState) as SR
import App.Row (State(..), Action(..), HeaderRowAction(..), HeaderState, spec) as R
import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Trans.Class (lift)
import DOM.HTML.HTMLElement (offsetHeight) 
import Data.Date (diff, lastDayOfMonth, canonicalDate)
import Data.DateTime (Date(..), DateTime(..), Millisecond, Time(..), adjust, canonicalDate, date, day, month, year, Day(..), Year(..))
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int (floor)
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), find, filter, (!!), length, take, foldl, head, snoc, last, zipWith)
import Data.Maybe (Maybe(..), fromJust, maybe, isJust)
import Data.String (length) as S
import Data.Time.Duration (Days(..), Milliseconds(..))
import Data.Tuple (Tuple(..), uncurry, fst, snd)
import Math (e)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React as R
import React.DOM as RD 
import React.DOM.Props as RP
import Thermite as T

data Action = AddShift
            | RowAction Int R.Action

type RosterState = { currentVol :: Maybe D.Volunteer
                   , shifts :: List D.Shift
                   , startDate :: Date
                   , endDate :: Date
                   , currentDate :: Date
                   , loading :: Boolean
                   }

type State = { roster :: RosterState
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
    performAction (RowAction _ (R.ShiftRowAction (SR.AddCurrentVol shiftDate SR.Overnight))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.AddCurrentVol shiftDate SR.Evening))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.ChangeCurrentVolShiftType shiftDate SR.Overnight))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.ChangeCurrentVolShiftType shiftDate SR.Evening))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.RemoveCurrentVol shiftDate))) _ { roster: { currentVol: Just cv } } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.removeVolunteerShift shiftDate cv
    performAction (RowAction _ (R.HeaderRowAction R.PrevPeriod)) _ _ = void do
      T.modifyState \state -> adjustPeriod (-28) state
    performAction (RowAction _ (R.HeaderRowAction R.NextPeriod)) _ _ = void do
      T.modifyState \state -> adjustPeriod 28 state
    performAction _ _ _ = pure unit
    
    delay' = lift $ liftAff $ delay (Milliseconds 500.0)

initialState :: Maybe D.Volunteer -> List D.Shift -> Date -> Date -> Int -> State
initialState currentVol shifts currentDate startDate shiftCount = 
  let endDate = addDays (shiftCount - 1) startDate
      roster = { currentVol
               , shifts
               , currentDate
               , startDate
               , endDate
               , loading: false
               }
  in { roster
     , rows: rows roster
     }

rows :: RosterState -> List R.State
rows roster = rows' roster.startDate
  where 
  maxVols = foldl max 0 $ map (length <<< _.volunteers) roster.shifts
  
  rows' :: Date -> List R.State
  rows' date | date > roster.endDate = 
      Cons (R.EndRow { text: "", noOfCols: maxVols + 7 })
    $ Nil
  rows' date | date == roster.startDate =
      Cons (R.StartRow { text: toMonthYearString date, noOfCols: maxVols + 7 })
    $ Cons (R.ShiftRow $ SR.initialState roster.shifts roster.currentVol roster.currentDate date maxVols)
    $ rows' $ tomorrow date
  rows' date | isFirstDayOfMonth date =
      Cons (R.MonthHeaderRow { text: toMonthYearString date, noOfCols: maxVols + 7 })
    $ Cons (R.ShiftRow $ SR.initialState roster.shifts roster.currentVol roster.currentDate date maxVols)
    $ rows' $ tomorrow date
  rows' date =
      Cons (R.ShiftRow $ SR.initialState roster.shifts roster.currentVol roster.currentDate date maxVols)
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
           , rows = preserveLoading state.rows $ rows roster'
           }

adjustPeriod :: Int -> State -> State
adjustPeriod adj state = 
  let roster' = state.roster { startDate = addDays adj state.roster.startDate
                             , endDate = addDays adj state.roster.endDate
                             , loading = false
                             }
  in state { roster = roster'
           , rows = rows roster'
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
           , rows = modifyListWhere isShiftOnDate cancelLoading $ preserveLoading state.rows $ rows roster'
           }
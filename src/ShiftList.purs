module App.ShiftList (Action(..), State, spec, initialState, changeCurrentVol) where
 
import Prelude
 
import App.Common (lensOfListWithProps, tomorrow, modifyListWhere, surroundIf, default, toMonthYearString, daysLeftInMonth, isFirstDayOfMonth, sortWith, addDays)
import App.Data (OvernightSharingPrefs(..), Shift(..), Volunteer(..), VolunteerShift(..), RuleResult(..), canAddVolunteer, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType, updateVolunteer) as D
import App.ShiftRow (CurrentVolState, OtherVolState, Action(..), State(..), ShiftStatus(..), ShiftType(..), spec) as SR
import App.Row (State(..), Action(..), HeaderRowAction(..), spec) as R
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

type State = { currentVol :: Maybe D.Volunteer
             , shifts :: List D.Shift
             , rows :: List R.State
             , currentDate :: Date
             , startDate :: Date
             , endDate :: Date
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
    performAction (RowAction _ (R.ShiftRowAction (SR.AddCurrentVol shiftDate SR.Overnight))) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.AddCurrentVol shiftDate SR.Evening))) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.ChangeCurrentVolShiftType shiftDate SR.Overnight))) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.ChangeCurrentVolShiftType shiftDate SR.Evening))) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (R.ShiftRowAction (SR.RemoveCurrentVol shiftDate))) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.removeVolunteerShift shiftDate cv
    performAction (RowAction _ (R.HeaderRowAction R.PrevPeriod)) _ _ = void do
      delay'
      T.modifyState \state -> adjustPeriod (-28) state
    performAction (RowAction _ (R.HeaderRowAction R.NextPeriod)) _ _ = void do
      delay'
      T.modifyState \state -> adjustPeriod 28 state
    performAction _ _ _ = pure unit
    
    delay' = lift $ liftAff $ delay (Milliseconds 1000.0)

initialState :: Maybe D.Volunteer -> List D.Shift -> Date -> Date -> Int -> State
initialState currentVol shifts currentDate startDate shiftCount = 
  let endDate = addDays (shiftCount - 1) startDate
  in { currentVol
     , shifts
     , rows: rows currentVol shifts currentDate startDate endDate
     , currentDate
     , startDate
     , endDate
     }

rows :: Maybe D.Volunteer -> List D.Shift -> Date -> Date -> Date -> List R.State
rows currentVol shifts currentDate startDate endDate = rows' startDate
  where 
  rows' :: Date -> List R.State
  rows' date | date > endDate = 
      Cons R.EndRow
    $ Nil
  rows' date | date == startDate =
      Cons (R.StartRow $ toMonthYearString date)
    $ Cons (shiftRow date)
    $ rows' $ tomorrow date
  rows' date | isFirstDayOfMonth date =
      Cons (R.MonthHeaderRow $ toMonthYearString date)
    $ Cons (shiftRow date)
    $ rows' $ tomorrow date
  rows' date =
      Cons (shiftRow date)
    $ rows' $ tomorrow date

  shiftRow :: Date -> R.State
  shiftRow date = 
    R.ShiftRow { date 
               , noOfVols: length shift.volunteers
               , status: status shift currentDate
               , loading: false
               , currentVol: buildCurrentVol shift
               , otherVol1: otherVols !! 0
               , otherVol2: otherVols !! 1
               }
    where
    shift = maybe {date: date, volunteers: Nil} id $ find (\s -> s.date == date) shifts
    otherVols = sortWith _.name $ map buildVol $ case currentVol of
                                                   Just cv -> filter (not <<< D.hasVolWithId $ cv.id) shift.volunteers
                                                   _ -> shift.volunteers

  buildVol :: D.VolunteerShift -> SR.OtherVolState
  buildVol (D.Overnight v) = { name: v.name
                             , shiftType: SR.Overnight
                             , sharingPrefs: sharingPrefs v.overnightSharingPrefs
                             } 
  buildVol (D.Evening v)   = { name: v.name
                             , shiftType: SR.Evening
                             , sharingPrefs: sharingPrefs v.overnightSharingPrefs
                             }

  sharingPrefs :: D.OvernightSharingPrefs -> String
  sharingPrefs prefs = surroundIf " (" ")" $ case prefs of
                                               D.None -> "No sharing"
                                               (D.OnlyGender gender) -> (show gender) <> " only"
                                               (D.Custom text) -> text
                                               _ -> ""

  status :: D.Shift -> Date -> SR.ShiftStatus
  status s currentDate =
    let errors = D.validate s currentDate
        firstErrorStatus = case head errors of
          Just (D.Error e)   -> SR.Error
          Just (D.Warning w) -> SR.Warning
          Just (D.Info i)    -> SR.Info
          Just (D.Neutral)   -> const SR.OK
          _             -> const SR.Good
        
        extractMsg (D.Error e)   = Just e
        extractMsg (D.Warning w) = Just w
        extractMsg (D.Info i)    = Just i
        extractMsg _             = Nothing

        concat ""  (Just m) = "This shift " <> m
        concat msg (Just m) = msg <> ", and also " <> m
        concat msg Nothing  = msg
    in firstErrorStatus $ foldl concat "" $ map extractMsg errors
  
  buildCurrentVol :: D.Shift -> Maybe SR.CurrentVolState
  buildCurrentVol shift = case currentVol of
    (Just cv) -> Just { name: cv.name 
                      , shiftType: currentVolShiftType cv shift.volunteers
                      , canAddOvernight: D.canAddVolunteer (D.Overnight cv) shift
                      , canAddEvening: D.canAddVolunteer (D.Evening cv) shift
                      , canChangeShiftType: D.canChangeVolunteerShiftType cv shift
                      }
    _         -> Nothing

  currentVolShiftType :: D.Volunteer -> List D.VolunteerShift -> Maybe SR.ShiftType
  currentVolShiftType v vols = 
    find (D.hasVolWithId v.id) vols >>= case _ of
      D.Overnight _ -> Just SR.Overnight
      D.Evening   _ -> Just SR.Evening

preserveLoading :: List R.State -> List R.State -> List R.State
preserveLoading = zipWith row
  where
  row (R.ShiftRow old) (R.ShiftRow new) = R.ShiftRow new { loading = old.loading }
  row _ new = new
 
changeCurrentVol :: Maybe D.Volunteer -> State -> State
changeCurrentVol currentVol state =
  let shifts = maybe state.shifts (\vol -> D.updateVolunteer vol state.shifts) currentVol
  in state { currentVol = currentVol
           , shifts = shifts
           , rows = preserveLoading state.rows $ rows currentVol shifts state.currentDate state.startDate state.endDate
           }

adjustPeriod :: Int -> State -> State
adjustPeriod adj state = 
  let startDate' = addDays adj state.startDate
      endDate' = addDays adj state.endDate
  in state { startDate = startDate'
           , endDate = endDate'
           , rows = rows state.currentVol state.shifts state.currentDate startDate' endDate'
           }

modifyShifts :: State -> Date -> (List D.Shift -> List D.Shift) -> State
modifyShifts state date modify =
  let shifts = modify state.shifts
      isShiftOnDate :: R.State -> Boolean
      isShiftOnDate (R.ShiftRow s) = s.date == date
      isShiftOnDate _ = false

      cancelLoading :: R.State -> R.State
      cancelLoading (R.ShiftRow s) = R.ShiftRow s{ loading = false }
      cancelLoading r = r 
  in state { shifts = shifts
           , rows = modifyListWhere isShiftOnDate cancelLoading $ preserveLoading state.rows $ rows state.currentVol shifts state.currentDate state.startDate state.endDate
           }
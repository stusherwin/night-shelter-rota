module App.ShiftList (Action(..), RowAction(..), Row(..), State, spec, initialState, changeCurrentVol) where
 
import Prelude

import App.Common (lensOfListWithProps, tomorrow, modifyListWhere, surroundIf, default, toMonthYearString, daysLeftInMonth, isFirstDayOfMonth, sortWith, addDays)
import App.Data (OvernightSharingPrefs(..), Shift(..), Volunteer(..), VolunteerShift(..), RuleResult(..), canAddVolunteer, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType, updateVolunteer) as D
import App.ShiftRow (CurrentVolState, OtherVolState, Action(..), State(..), ShiftStatus(..), ShiftType(..), spec) as SR
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

data RowAction = ShiftRowAction SR.Action
               | MonthHeaderRowAction Unit

data Action = AddShift
            | RowAction Int RowAction

data Row = ShiftRow SR.State
         | MonthHeaderRow String

type State = { currentVol :: Maybe D.Volunteer
             , shifts :: List D.Shift
             , rows :: List Row
             , currentDate :: Date
             , startDate :: Date
             , endDate :: Date
             }

_rows :: Lens' State (List Row)
_rows = lens _.rows _{rows = _}

_RowAction :: Prism' Action (Tuple Int RowAction)
_RowAction = prism (uncurry RowAction) unwrap
  where
  unwrap (RowAction i a) = Right (Tuple i a)
  unwrap ta = Left ta

_MonthHeaderRowAction :: Prism' RowAction Unit
_MonthHeaderRowAction = prism MonthHeaderRowAction unwrap
  where
  unwrap (MonthHeaderRowAction a) = Right a
  unwrap ra = Left ra

_ShiftRowAction :: Prism' RowAction SR.Action
_ShiftRowAction = prism ShiftRowAction unwrap
  where
  unwrap (ShiftRowAction a) = Right a
  unwrap ra = Left ra

_MonthHeaderRow :: Prism' Row String
_MonthHeaderRow = prism MonthHeaderRow unwrap
  where
  unwrap (MonthHeaderRow s) = Right s
  unwrap r = Left r

_ShiftRow :: Prism' Row SR.State
_ShiftRow = prism ShiftRow unwrap
  where
  unwrap (ShiftRow s) = Right s
  unwrap r = Left r

spec :: forall props eff. T.Spec eff State props Action
spec = 
  ( table $ T.focus _rows _RowAction $ T.foreach \_ ->
    (  T.split _MonthHeaderRow (T.match _MonthHeaderRowAction monthHeaderRow)
    <> T.split _ShiftRow (T.match _ShiftRowAction SR.spec)
    )
  )
  <> footerSpec
  where
  table :: T.Spec eff State props Action -> T.Spec eff State props Action
  table = over T._render \render d p s c ->
    [ RD.table [ RP.className "ui structured unstackable table" ]
               [ RD.tbody' $ render d p s c
               ]
    ]

  monthHeaderRow :: T.Spec _ String _ Unit
  monthHeaderRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render String _ Unit
    render dispatch _ text _ = [ RD.tr [ RP.className "month-header-row" ]
                                       [ RD.td [ RP.colSpan 9 ]
                                               [ RD.text text ]
                                       ]
                               ]
   
  footerSpec :: T.Spec _ State _ Action
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ state _ = []

    performAction :: T.PerformAction _ State _ Action
    performAction (RowAction _ (ShiftRowAction (SR.AddCurrentVol shiftDate SR.Overnight)))             _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (ShiftRowAction (SR.AddCurrentVol shiftDate SR.Evening)))               _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (ShiftRowAction (SR.ChangeCurrentVolShiftType shiftDate SR.Overnight))) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Overnight cv)
    performAction (RowAction _ (ShiftRowAction (SR.ChangeCurrentVolShiftType shiftDate SR.Evening)))   _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Evening cv)
    performAction (RowAction _ (ShiftRowAction (SR.RemoveCurrentVol shiftDate)))                    _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.removeVolunteerShift shiftDate cv
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

rows :: Maybe D.Volunteer -> List D.Shift -> Date -> Date -> Date -> List Row
rows currentVol shifts currentDate startDate endDate = rows' startDate
  where 
  rows' :: Date -> List Row
  rows' date | date > endDate = Nil
  rows' date | date == startDate || isFirstDayOfMonth date =
    Cons (monthHeaderRow date) $ Cons (shiftRow date) $ rows' $ tomorrow date
  rows' date = Cons (shiftRow date) $ rows' $ tomorrow date

  monthHeaderRow :: Date -> Row
  monthHeaderRow = MonthHeaderRow <<< toMonthYearString

  shiftRow :: Date -> Row
  shiftRow date = 
    ShiftRow { date 
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

preserveLoading :: List Row -> List Row -> List Row
preserveLoading = zipWith row
  where
  row (ShiftRow old) (ShiftRow new) = ShiftRow new { loading = old.loading }
  row _ new = new
 
changeCurrentVol :: Maybe D.Volunteer -> State -> State
changeCurrentVol currentVol state =
  let shifts = maybe state.shifts (\vol -> D.updateVolunteer vol state.shifts) currentVol
  in state { currentVol = currentVol
           , shifts = shifts
           , rows = preserveLoading state.rows $ rows currentVol shifts state.currentDate state.startDate state.endDate
           }

modifyShifts :: State -> Date -> (List D.Shift -> List D.Shift) -> State
modifyShifts state date modify =
  let shifts = modify state.shifts
      isShiftOnDate :: Row -> Boolean
      isShiftOnDate (ShiftRow s) = s.date == date
      isShiftOnDate _ = false

      cancelLoading :: Row -> Row
      cancelLoading (ShiftRow s) = ShiftRow s{ loading = false }
      cancelLoading r = r 
  in state { shifts = shifts
           , rows = modifyListWhere isShiftOnDate cancelLoading $ preserveLoading state.rows $ rows state.currentVol shifts state.currentDate state.startDate state.endDate
           }
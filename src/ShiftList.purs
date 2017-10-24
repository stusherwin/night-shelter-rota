module App.ShiftList (Action(..), State, spec, initialState, changeCurrentVol) where
 
import Prelude

import App.Common (lensOfListWithProps, tomorrow, modifyListWhere, surroundIf, default, toMonthYearString, daysLeftInMonth, isFirstDayOfMonth)
import App.Data (OvernightSharingPrefs(..))
import App.Data (Shift(..), Volunteer(..), VolunteerShift(..), RuleResult(..), canAddVolunteer, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType) as D
import App.Shift (CurrentVolState, OtherVolState, Action(..), State(..), ShiftStatus(..), ShiftType(..), spec) as S
import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Trans.Class (lift)
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Array (find, filter, (!!), sortWith, length, take, foldl)
import Data.Date (diff, lastDayOfMonth, canonicalDate)
import Data.DateTime (Date(..), DateTime(..), Millisecond, Time(..), adjust, canonicalDate, date, day, month, year, Day(..), Year(..))
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int (floor)
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), snoc, last, zipWith) as L
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
            | ShiftAction Int S.Action

type State = { currentVol :: Maybe D.Volunteer
             , shifts :: Array D.Shift
             , shiftRows :: L.List S.State
             , currentDate :: Date
             , startDate :: Date
             , noOfRows :: Int
             }

_shiftRows :: Lens' State (L.List S.State)
_shiftRows = lens (\s -> s.shiftRows)
                  (\s a -> s{shiftRows = a})

_ShiftAction :: Prism' Action (Tuple Int S.Action)
_ShiftAction = prism (uncurry ShiftAction) \ta ->
  case ta of
    ShiftAction i a -> Right (Tuple i a)
    _ -> Left ta 

spec :: forall props eff. T.Spec eff State props Action
spec = 
  (table $ T.focus _shiftRows _ShiftAction $ T.foreach \_ -> S.spec)
  <> footerSpec
  where
  table :: T.Spec eff State props Action -> T.Spec eff State props Action
  table = over T._render \render d p s c ->
    [ RD.table [ RP.className "ui structured unstackable table" ]
               [ RD.tbody' $ render d p s c
               ]
    ]
   
  volHeadings :: Maybe D.Volunteer -> Array ReactElement
  volHeadings (Just v) = 
    [ RD.th [ RP.colSpan 2
            , RP.className "left-border collapsing"
            ]
            [ RD.text $ default "Current volunteer" v.name ]
    , RD.th [ RP.colSpan 2
            , RP.className "left-border collapsing"
            ]
            [ RD.text "Other volunteers" ]
    , RD.th' []
    ]
  volHeadings _ =
    [ RD.th [ RP.colSpan 2
            , RP.className "left-border collapsing"
            ]
            [ RD.text "Volunteers" ]
    , RD.th' []
    ]
 
  footerSpec :: T.Spec _ State _ Action
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ state _ = []

    performAction :: T.PerformAction _ State _ Action
    performAction (ShiftAction _ (S.AddCurrentVol shiftDate S.Overnight))             _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Overnight cv)
    performAction (ShiftAction _ (S.AddCurrentVol shiftDate S.Evening))               _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.addVolunteerShift shiftDate (D.Evening cv)
    performAction (ShiftAction _ (S.ChangeCurrentVolShiftType shiftDate S.Overnight)) _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Overnight cv)
    performAction (ShiftAction _ (S.ChangeCurrentVolShiftType shiftDate S.Evening))   _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.changeVolunteerShift shiftDate (D.Evening cv)
    performAction (ShiftAction _ (S.RemoveCurrentVol shiftDate))                    _ { currentVol: Just cv } = void do
      delay'
      T.modifyState \state -> modifyShifts state shiftDate $ D.removeVolunteerShift shiftDate cv
    performAction _ _ _ = pure unit
    
    delay' = lift $ liftAff $ delay (Milliseconds 1000.0)

initialState :: Maybe D.Volunteer -> Array D.Shift -> Date -> Date -> Int -> State
initialState currentVol shifts currentDate startDate noOfRows = 
  { currentVol
  , shifts
  , shiftRows: buildShifts currentVol shifts currentDate startDate noOfRows
  , currentDate
  , startDate
  , noOfRows
  }

buildShifts :: Maybe D.Volunteer -> Array D.Shift -> Date -> Date -> Int -> L.List S.State
buildShifts currentVol shifts currentDate startDate noOfRows = buildShifts' currentDate startDate noOfRows
  where 
  buildShifts' :: Date -> Date -> Int -> L.List S.State
  buildShifts' _ _ 0 = L.Nil
  buildShifts' currentDate date n = L.Cons (buildShift currentVol shifts currentDate date n noOfRows) $ buildShifts' currentDate (tomorrow date) (n - 1)

buildShift :: Maybe D.Volunteer -> Array D.Shift -> Date -> Date -> Int -> Int -> S.State
buildShift currentVol shifts currentDate date row noOfRows =
  { date 
  , noOfVols: length shift.volunteers
  , status: status shift currentDate
  , loading: false
  , currentVol: buildCurrentVol shift
  , otherVol1: otherVols !! 0
  , otherVol2: otherVols !! 1
  , month: if (isFirstDayOfMonth date) || row == noOfRows
             then Just { name: toMonthYearString date
                       , noOfDays: min row (daysLeftInMonth date)
                       }
             else Nothing
  }
  where
  shift = maybe {date: date, volunteers: []} id $ find (\s -> s.date == date) shifts
  otherVols = sortWith _.name $ map buildVol $ case currentVol of
                                                      Just cv -> filter (not <<< D.hasVolWithId $ cv.id) shift.volunteers
                                                      _ -> shift.volunteers

  buildVol :: D.VolunteerShift -> S.OtherVolState
  buildVol (D.Overnight v) = { name: v.name
                             , shiftType: S.Overnight
                             , sharingPrefs: sharingPrefs v.overnightSharingPrefs
                             } 
  buildVol (D.Evening v)   = { name: v.name
                             , shiftType: S.Evening
                             , sharingPrefs: sharingPrefs v.overnightSharingPrefs
                             }

  sharingPrefs :: OvernightSharingPrefs -> String
  sharingPrefs prefs = surroundIf " (" ")" $ case prefs of
                                               None -> "No sharing"
                                               (OnlyGender gender) -> (show gender) <> " only"
                                               (Custom text) -> text
                                               _ -> ""

  status :: D.Shift -> Date -> S.ShiftStatus
  status s currentDate =
    let errors = D.validate s currentDate
        firstErrorStatus = case take 1 errors of
          [D.Error e]   -> S.Error
          [D.Warning w] -> S.Warning
          [D.Info i]    -> S.Info
          [D.Neutral]   -> const S.OK
          _             -> const S.Good
        
        extractMsg (D.Error e)   = Just e
        extractMsg (D.Warning w) = Just w
        extractMsg (D.Info i)    = Just i
        extractMsg _             = Nothing

        concat ""  (Just m) = "This shift " <> m
        concat msg (Just m) = msg <> ", and also " <> m
        concat msg Nothing  = msg
    in firstErrorStatus $ foldl concat "" $ map extractMsg errors
  
  buildCurrentVol :: D.Shift -> Maybe S.CurrentVolState
  buildCurrentVol shift = case currentVol of
    (Just cv) -> Just { name: cv.name 
                      , shiftType: currentVolShiftType cv shift.volunteers
                      , canAddOvernight: D.canAddVolunteer (D.Overnight cv) shift
                      , canAddEvening: D.canAddVolunteer (D.Evening cv) shift
                      , canChangeShiftType: D.canChangeVolunteerShiftType cv shift
                      }
    _         -> Nothing

  currentVolShiftType :: D.Volunteer -> Array D.VolunteerShift -> Maybe S.ShiftType
  currentVolShiftType v vols = 
    find (D.hasVolWithId v.id) vols >>= case _ of
      D.Overnight _ -> Just S.Overnight
      D.Evening   _ -> Just S.Evening

preserveLoading :: L.List S.State -> L.List S.State -> L.List S.State
preserveLoading = L.zipWith \old new ->
  new { loading = old.loading }

changeCurrentVol :: Maybe D.Volunteer -> State -> State
changeCurrentVol currentVol state =
  state { currentVol = currentVol
        , shiftRows = preserveLoading state.shiftRows $ buildShifts currentVol state.shifts state.currentDate state.startDate state.noOfRows
        }

modifyShifts :: State -> Date -> (Array D.Shift -> Array D.Shift) -> State
modifyShifts state date modify =
  let shifts = modify state.shifts
  in state { shifts = shifts
           , shiftRows = modifyListWhere (\s -> s.date == date) (\s -> s{ loading = false }) $ preserveLoading state.shiftRows $ buildShifts state.currentVol shifts state.currentDate state.startDate state.noOfRows
           }

module App.Data (RuleResult(..), Config, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, canAddVolunteer, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType, updateVolunteer, toDate, hasDate, fromDate, hasVId, vId) where

import Prelude
  
import Data.Array ((:))
import Data.Array (toUnfoldable, length, filter, nubBy, any, findIndex, deleteAt, modifyAt, snoc) as A
import App.Common (justIf, sortWith, mkDate)
import Data.List (List, findIndex, find, modifyAt, snoc, deleteAt, length, all, nubBy, filter, catMaybes, any, singleton)
import Data.Date (diff, year, month, day)
import Data.DateTime (Date)
import Data.Enum (fromEnum)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Time.Duration (Days(..))

import ServerTypes (Shift (..), Volunteer(..), OvernightPreference(..), OvernightGenderPreference(..), VolunteerShift(..), ShiftDate (..), ShiftType(..))

hasVolWithId :: Int -> VolunteerShift -> Boolean
hasVolWithId id v = volId v == id

volThat :: forall a. (Volunteer -> a) -> VolunteerShift -> a
volThat f (VolunteerShift { vsVolunteer: v }) = f v
 
volId :: VolunteerShift -> Int
volId (VolunteerShift { vsVolunteer: Volunteer { vId } }) = vId

hasVId :: Int -> Volunteer -> Boolean
hasVId id (Volunteer v) = v.vId == id

vId :: Volunteer -> Int
vId (Volunteer v) = v.vId

addVolunteerShift :: Date -> VolunteerShift -> List Shift -> List Shift
addVolunteerShift date vol shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) -> Shift s{ sVolunteers = (flip A.snoc) vol s.sVolunteers }) shifts
    _      -> snoc shifts $ Shift { sDate: fromDate date, sVolunteers: [vol] }

changeVolunteerShift :: Date -> Int -> List Shift -> List Shift
changeVolunteerShift date volId shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) ->
                case A.findIndex (hasVolWithId volId) s.sVolunteers of
                  Just j -> Shift s{ sVolunteers = fromMaybe s.sVolunteers $ A.modifyAt j otherShiftType s.sVolunteers }
                  _      -> Shift s) shifts
    _      -> shifts

otherShiftType :: VolunteerShift -> VolunteerShift
otherShiftType (VolunteerShift v@{ vsShiftType: Overnight }) = VolunteerShift v { vsShiftType = Evening }
otherShiftType (VolunteerShift v@{ vsShiftType: Evening }) = VolunteerShift v { vsShiftType = Overnight }

hasDate :: Date -> Shift -> Boolean
hasDate date (Shift { sDate: ShiftDate { year: y, month: m, day: d } }) =
     fromEnum (year date) == y
  && fromEnum (month date) == m
  && fromEnum (day date) == d

toDate :: ShiftDate -> Date
toDate (ShiftDate { year: y, month: m, day: d }) = mkDate d m y

fromDate :: Date -> ShiftDate
fromDate date = ShiftDate { day: fromEnum (day date)
                          , month: fromEnum (month date)
                          , year: fromEnum (year date)
                          }

removeVolunteerShift :: Date -> Volunteer -> List Shift -> List Shift
removeVolunteerShift date (Volunteer {vId}) shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) ->
                case A.findIndex (hasVolWithId vId) s.sVolunteers of
                  Just j -> Shift s{ sVolunteers = fromMaybe s.sVolunteers $ A.deleteAt j s.sVolunteers }
                  _      -> Shift s) shifts
    _      -> shifts

updateVolunteer :: Volunteer -> List Shift -> List Shift
updateVolunteer v'@(Volunteer {vId: id'}) = map updateShift
  where
  updateShift (Shift s) = Shift s { sVolunteers = map updateVol s.sVolunteers }
  updateVol (VolunteerShift vs@{ vsVolunteer: Volunteer {vId} }) | vId == id' = VolunteerShift vs { vsVolunteer = v' }
  updateVol v = v

type Config = { currentDate :: Date
              , maxVolsPerShift :: Int
              , urgentPeriodDays :: Int
              }
  
type RuleParams = { shift :: Shift
                  , config :: Config
                  }

type Rule = RuleParams -> Maybe String

data RuleResult = Error String
                | Warning String
                | Info String
                | Neutral

isError :: Maybe RuleResult -> Boolean
isError (Just (Error _)) = true
isError _                = false

isOvernight :: VolunteerShift -> Boolean
isOvernight (VolunteerShift { vsShiftType: Overnight }) = true
isOvernight _ = false

filterOut :: Volunteer -> Array VolunteerShift -> Array VolunteerShift
filterOut (Volunteer v) = A.filter (not <<< hasVolWithId $ v.vId)

isAllowed :: RuleParams -> Boolean
isAllowed params = satisfies params $ A.toUnfoldable [ notHaveSameVolunteerTwice
                                                     ]
  where
  satisfies :: RuleParams -> List Rule -> Boolean
  satisfies p = all id <<< map (not <<< isJust) <<< (flip flap) p

canAddVolunteer :: Config -> VolunteerShift -> Shift -> Boolean
canAddVolunteer config volShift (Shift s) =
  isAllowed { shift: Shift s{ sVolunteers = volShift : s.sVolunteers }
            , config
            }

canChangeVolunteerShiftType :: Config -> Volunteer -> Shift -> Boolean
canChangeVolunteerShiftType config vol@(Volunteer v) (Shift s ) =
  case find (hasVolWithId v.vId) s.sVolunteers of
    Nothing -> false
    (Just volShift) ->
      isAllowed { shift: Shift s{ sVolunteers = otherShiftType volShift : (filterOut vol s.sVolunteers) }
                , config
                }

validate :: Config -> Shift -> List RuleResult
validate config shift =
  collectViolations params $ A.toUnfoldable [ must <<< notHaveSameVolunteerTwice
                                            , ignoreIf (isPast shift) <<< should <<< notExceedMaxVolunteers
                                            , ignoreIf (isPast shift) <<< mustIf (isLooming shift) <<< haveAtLeastOneVolunteer
                                            , ignoreIf (isPast shift) <<< could <<< haveMoreThanOneVolunteer
                                            , ignoreIf (isPast shift) <<< mustIf (isLooming shift) <<< haveAnOvernightVolunteer
                                            , ignoreIf (isPast shift) <<< should <<< notViolateAnyVolsSharingPrefs
                                            ]
  where
  params = { shift
           , config
           }

  isLooming :: Shift -> Boolean
  isLooming (Shift s) =
    let (Days d) = (toDate s.sDate) `diff` config.currentDate
    in d >= 0.0 && d < (toNumber config.urgentPeriodDays)

  isPast :: Shift -> Boolean
  isPast (Shift s) =
    let (Days d) = (toDate s.sDate) `diff` config.currentDate
    in d < 0.0

  collectViolations :: RuleParams -> List (RuleParams -> Maybe RuleResult) -> List RuleResult
  collectViolations params = sortWith priority <<< catMaybes <<< (flip flap) params

  must   = map Error
  should = map Warning
  could  = map Info
  mustIf condition   = map $ if condition then Error else const Neutral
  ignoreIf condition = map $ if condition then const Neutral else id

  priority :: RuleResult -> Int
  priority (Error   _) = 0
  priority (Warning _) = 1
  priority (Info _)    = 2
  priority _           = 3

notExceedMaxVolunteers :: Rule
notExceedMaxVolunteers { shift: Shift s, config: { maxVolsPerShift } } =
  justIf ("has more than " <> show maxVolsPerShift <> " volunteers")
       $ A.length s.sVolunteers > maxVolsPerShift

notHaveSameVolunteerTwice :: Rule
notHaveSameVolunteerTwice { shift: Shift s } =
  justIf "has the same volunteer down twice"
       $ A.length (A.nubBy (\a b -> volId a == volId b) s.sVolunteers) > A.length s.sVolunteers

haveAtLeastOneVolunteer :: Rule
haveAtLeastOneVolunteer { shift: Shift s } =
  justIf "has no volunteers"
       $ A.length s.sVolunteers == 0

haveMoreThanOneVolunteer :: Rule
haveMoreThanOneVolunteer { shift: Shift s } =
  justIf "has only one volunteer"
       $ A.length s.sVolunteers == 1

haveAnOvernightVolunteer :: Rule
haveAnOvernightVolunteer { shift: Shift s, config: { currentDate } } =
  justIf "has no overnight volunteer"
       $ A.length s.sVolunteers /= 0 && (A.length $ A.filter isOvernight s.sVolunteers) == 0

notViolateAnyVolsSharingPrefs :: Rule
notViolateAnyVolsSharingPrefs { shift: Shift s } =
  justIf "goes against a volunteer's preferences"
       $ A.any violatesSharingPrefs s.sVolunteers
  where
  violatesSharingPrefs :: VolunteerShift -> Boolean
  violatesSharingPrefs (VolunteerShift { vsShiftType: Overnight, vsVolunteer: vol@Volunteer { vOvernightPreference: Just PreferToBeAlone }}) = A.any isOvernight $ filterOut vol s.sVolunteers
  violatesSharingPrefs (VolunteerShift { vsShiftType: Overnight, vsVolunteer: vol@Volunteer { vOvernightPreference: Just PreferAnotherVolunteer }}) = (not <<< A.any) isOvernight $ filterOut vol s.sVolunteers
  violatesSharingPrefs _ = false
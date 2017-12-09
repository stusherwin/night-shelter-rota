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

import ServerTypes (Shift (..), Volunteer(..), OvernightPreference(..), OvernightGenderPreference(..), VolunteerShift(..))
import ServerTypes (Date (..)) as ST

hasVolWithId :: Int -> VolunteerShift -> Boolean
hasVolWithId id (Overnight (Volunteer v)) = v.vId == id
hasVolWithId id (Evening (Volunteer v)) = v.vId == id

volThat :: forall a. (Volunteer -> a) -> VolunteerShift -> a
volThat f (Overnight v) = f v
volThat f (Evening v)   = f v
 
volId :: VolunteerShift -> Int
volId (Overnight (Volunteer v)) = v.vId
volId (Evening (Volunteer v)) = v.vId

hasVId :: Int -> Volunteer -> Boolean
hasVId id (Volunteer v) = v.vId == id

vId :: Volunteer -> Int
vId (Volunteer v) = v.vId

addVolunteerShift :: Date -> VolunteerShift -> List Shift -> List Shift
addVolunteerShift date vol shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) -> Shift s{ volunteers = (flip A.snoc) vol s.volunteers }) shifts
    _      -> snoc shifts $ Shift { date: fromDate date, volunteers: [vol] }

changeVolunteerShift :: Date -> Int -> List Shift -> List Shift
changeVolunteerShift date volId shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) ->
                case A.findIndex (hasVolWithId volId) s.volunteers of
                  Just j -> Shift s{ volunteers = fromMaybe s.volunteers $ A.modifyAt j otherShiftType s.volunteers }
                  _      -> Shift s) shifts
    _      -> shifts

otherShiftType :: VolunteerShift -> VolunteerShift
otherShiftType (Overnight v) = Evening v
otherShiftType (Evening v) = Overnight v

hasDate :: Date -> Shift -> Boolean
hasDate date (Shift { date: ST.Date { year: y, month: m, day: d } }) =
     fromEnum (year date) == y
  && fromEnum (month date) == m
  && fromEnum (day date) == d

toDate :: ST.Date -> Date
toDate (ST.Date { year: y, month: m, day: d }) = mkDate d m y

fromDate :: Date -> ST.Date
fromDate date = ST.Date { day: fromEnum (day date)
                        , month: fromEnum (month date)
                        , year: fromEnum (year date)
                        }

removeVolunteerShift :: Date -> Volunteer -> List Shift -> List Shift
removeVolunteerShift date (Volunteer {vId}) shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) ->
                case A.findIndex (hasVolWithId vId) s.volunteers of
                  Just j -> Shift s{ volunteers = fromMaybe s.volunteers $ A.deleteAt j s.volunteers }
                  _      -> Shift s) shifts
    _      -> shifts

updateVolunteer :: Volunteer -> List Shift -> List Shift
updateVolunteer v'@(Volunteer {vId: id'}) = map updateShift
  where
  updateShift (Shift s) = Shift s { volunteers = map updateVol s.volunteers }
  updateVol (Overnight (Volunteer {vId})) | vId == id' = Overnight v'
  updateVol (Evening   (Volunteer {vId})) | vId == id' = Evening v'
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
isOvernight (Overnight _) = true
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
  isAllowed { shift: Shift s{ volunteers = volShift : s.volunteers }
            , config
            }

canChangeVolunteerShiftType :: Config -> Volunteer -> Shift -> Boolean
canChangeVolunteerShiftType config vol@(Volunteer v) (Shift s ) =
  case find (hasVolWithId v.vId) s.volunteers of
    Nothing -> false
    (Just volShift) ->
      let changedShift = case volShift of
                           (Overnight _) -> (Evening vol)
                           (Evening _)   -> (Overnight vol)
      in isAllowed { shift: Shift s{ volunteers = changedShift : (filterOut vol s.volunteers) }
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
    let (Days d) = (toDate s.date) `diff` config.currentDate
    in d >= 0.0 && d < (toNumber config.urgentPeriodDays)

  isPast :: Shift -> Boolean
  isPast (Shift s) =
    let (Days d) = (toDate s.date) `diff` config.currentDate
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
       $ A.length s.volunteers > maxVolsPerShift

notHaveSameVolunteerTwice :: Rule
notHaveSameVolunteerTwice { shift: Shift s } =
  justIf "has the same volunteer down twice"
       $ A.length (A.nubBy (\a b -> volId a == volId b) s.volunteers) > A.length s.volunteers

haveAtLeastOneVolunteer :: Rule
haveAtLeastOneVolunteer { shift: Shift s } =
  justIf "has no volunteers"
       $ A.length s.volunteers == 0

haveMoreThanOneVolunteer :: Rule
haveMoreThanOneVolunteer { shift: Shift s } =
  justIf "has only one volunteer"
       $ A.length s.volunteers == 1

haveAnOvernightVolunteer :: Rule
haveAnOvernightVolunteer { shift: Shift s, config: { currentDate } } =
  justIf "has no overnight volunteer"
       $ A.length s.volunteers /= 0 && (A.length $ A.filter isOvernight s.volunteers) == 0
  where
  isOvernight :: VolunteerShift -> Boolean
  isOvernight (Overnight _) = true
  isOvernight _ = false

notViolateAnyVolsSharingPrefs :: Rule
notViolateAnyVolsSharingPrefs { shift: Shift s } =
  justIf "goes against a volunteer's preferences"
       $ A.any violatesSharingPrefs s.volunteers
  where
  violatesSharingPrefs :: VolunteerShift -> Boolean
  violatesSharingPrefs (Overnight vol@Volunteer { vOvernightPreference: Just PreferToBeAlone }) = A.any isOvernight $ filterOut vol s.volunteers
  violatesSharingPrefs (Overnight vol@Volunteer { vOvernightPreference: Just PreferAnotherVolunteer }) = (not <<< A.any) isOvernight $ filterOut vol s.volunteers
  violatesSharingPrefs _ = false
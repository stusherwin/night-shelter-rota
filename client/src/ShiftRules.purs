module App.ShiftRules (RuleResult(..), ShiftRuleConfig, canAddVolunteer, validateShift, filterOut, canChangeVolunteerShiftType) where

import Prelude

import Data.Array (toUnfoldable)  
import App.Common (justIf, sortWith, mkDate)
import Data.List (List(..), findIndex, find, modifyAt, snoc, deleteAt, length, all, nubBy, filter, catMaybes, any, singleton, length, filter, nubBy, any, findIndex, deleteAt, modifyAt, snoc)
import Data.Date (diff, year, month, day)
import Data.DateTime (Date)
import Data.Enum (fromEnum)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Time.Duration (Days(..))

import App.Types (Shift, Vol, OvernightPreference(..), OvernightGenderPreference(..), VolShift, ShiftType(..), otherShiftType)

type ShiftRuleConfig = { currentDate :: Date
                       , maxVolsPerShift :: Int
                       , urgentPeriodDays :: Int
                       }
  
type RuleParams = { shift :: Shift
                  , config :: ShiftRuleConfig
                  }

type Rule = RuleParams -> Maybe String

data RuleResult = Error String
                | Warning String
                | Info String
                | Neutral

isOvernight :: VolShift -> Boolean
isOvernight { shiftType: Overnight } = true
isOvernight _ = false

filterOut :: Vol -> List VolShift -> List VolShift
filterOut v = filter (\vs -> vs.volunteer.id /= v.id)

isAllowed :: RuleParams -> Boolean
isAllowed params = satisfies params $ toUnfoldable [ notHaveSameVolunteerTwice
                                                   ]
  where
  satisfies :: RuleParams -> List Rule -> Boolean
  satisfies p = all id <<< map (not <<< isJust) <<< (flip flap) p

canAddVolunteer :: ShiftRuleConfig -> VolShift -> Shift -> Boolean
canAddVolunteer config volShift s =
  isAllowed { shift: s{ volunteers = Cons volShift s.volunteers }
            , config
            }

canChangeVolunteerShiftType :: ShiftRuleConfig -> Vol -> Shift -> Boolean
canChangeVolunteerShiftType config v s =
  case find (\vs -> vs.volunteer.id == v.id) s.volunteers of
    Nothing -> false
    (Just vs) ->
      let vs' = vs { shiftType = otherShiftType vs.shiftType }
      in  isAllowed { shift: s{ volunteers = Cons vs' (filterOut v s.volunteers) }
                    , config
                    }

validateShift :: ShiftRuleConfig -> Shift -> List RuleResult
validateShift config shift =
  collectViolations params $ toUnfoldable [ must <<< notHaveSameVolunteerTwice
                                          , ignoreIf (isPast shift) <<< should <<< notExceedMaxVolunteers
                                          , ignoreIf (isPast shift) <<< mustIf (isLooming shift) <<< haveAtLeastOneVolunteer
                                          -- , ignoreIf (isPast shift) <<< could <<< haveMoreThanOneVolunteer
                                          , ignoreIf (isPast shift) <<< mustIf (isLooming shift) <<< haveAnOvernightVolunteer
                                          , ignoreIf (isPast shift) <<< should <<< notViolateAnyVolsSharingPrefs
                                          ]
  where
  params = { shift
           , config
           }

  isLooming :: Shift -> Boolean
  isLooming s =
    let (Days d) = s.date `diff` config.currentDate
    in d >= 0.0 && d < (toNumber config.urgentPeriodDays)

  isPast :: Shift -> Boolean
  isPast s =
    let (Days d) = s.date `diff` config.currentDate
    in d < 0.0

  collectViolations :: RuleParams -> List (RuleParams -> Maybe RuleResult) -> List RuleResult
  collectViolations params = sortWith priority <<< catMaybes <<< (flip flap) params

  must   = map Error
  should = map Warning
  -- could  = map Info
  mustIf condition   = map $ if condition then Error else const Neutral
  ignoreIf condition = map $ if condition then const Neutral else id

  priority :: RuleResult -> Int
  priority (Error   _) = 0
  priority (Warning _) = 1
  priority (Info _)    = 2
  priority _           = 3

notExceedMaxVolunteers :: Rule
notExceedMaxVolunteers { shift: s, config: { maxVolsPerShift } } =
  justIf ("has more than " <> show maxVolsPerShift <> " volunteers")
       $ length s.volunteers > maxVolsPerShift

notHaveSameVolunteerTwice :: Rule
notHaveSameVolunteerTwice { shift: s } =
  justIf "has the same volunteer down twice"
       $ length (nubBy (\a b -> a.volunteer.id == b.volunteer.id) s.volunteers) > length s.volunteers

haveAtLeastOneVolunteer :: Rule
haveAtLeastOneVolunteer { shift: s } =
  justIf "has no volunteers"
       $ length s.volunteers == 0

haveMoreThanOneVolunteer :: Rule
haveMoreThanOneVolunteer { shift: s } =
  justIf "has only one volunteer"
       $ length s.volunteers == 1

haveAnOvernightVolunteer :: Rule
haveAnOvernightVolunteer { shift: s, config: { currentDate } } =
  justIf "has no overnight volunteer"
       $ length s.volunteers /= 0 && (length $ filter isOvernight s.volunteers) == 0

notViolateAnyVolsSharingPrefs :: Rule
notViolateAnyVolsSharingPrefs { shift: s } =
  justIf "goes against a volunteer's preferences"
       $ any violatesSharingPrefs s.volunteers
  where
  violatesSharingPrefs :: VolShift -> Boolean
  violatesSharingPrefs { shiftType: Overnight
                       , volunteer: vol@{ overnightPreference: Just PreferToBeAlone }
                       } =
    any isOvernight $ filterOut vol s.volunteers
  violatesSharingPrefs { shiftType: Overnight
                       , volunteer: vol@{ overnightPreference: Just PreferAnotherVolunteer }
                       } =
    (not <<< any) isOvernight $ filterOut vol s.volunteers
  violatesSharingPrefs _ = false
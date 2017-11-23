module App.Data (Shift (..), Volunteer(..), VolId(..), OvernightPreference(..), OvernightGenderPreference(..), VolunteerShift(..), RuleResult(..), Config, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, canAddVolunteer, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType, parseVolId, nextVolId, updateVolunteer) where

import Prelude
  
import Data.Array (toUnfoldable)
import App.Common (justIf, sortWith)
import Data.List (List, findIndex, find, modifyAt, snoc, deleteAt, length, all, nubBy, (:), filter, catMaybes, any, singleton)
import Data.Date (diff)
import Data.DateTime (Date)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Time.Duration (Days(..))

data OvernightPreference = PreferToBeAlone
                         | PreferAnotherVolunteer 
derive instance overnightPreferenceEq :: Eq OvernightPreference
instance overnightPreferenceShow :: Show OvernightPreference
  where
  show PreferToBeAlone = "PreferToBeAlone"
  show PreferAnotherVolunteer = "PreferAnotherVolunteer" 

data OvernightGenderPreference = Male
                               | Female
derive instance overnightGenderPreferenceEq :: Eq OvernightGenderPreference
instance overnightGenderPreferenceShow :: Show OvernightGenderPreference
  where
  show Male = "Male"
  show Female = "Female" 

newtype VolId = VolId Int
derive instance volIdEq :: Eq VolId
derive instance volIdOrd :: Ord VolId
instance volIdShow :: Show VolId where show (VolId v) = show v

nextVolId :: VolId -> VolId
nextVolId (VolId i) = VolId (i + 1)

parseVolId :: String -> Maybe VolId
parseVolId = map VolId <<< fromString
 
type Volunteer = { id :: VolId
                 , name :: String
                 , overnightPreference :: Maybe OvernightPreference
                 , overnightGenderPreference :: Maybe OvernightGenderPreference
                 , notes :: String
                 }

data VolunteerShift = Overnight Volunteer
                    | Evening Volunteer

type Shift = { date :: Date
             , volunteers :: List VolunteerShift
             }

hasVolWithId :: VolId -> VolunteerShift -> Boolean
hasVolWithId id (Overnight v) = v.id == id
hasVolWithId id (Evening v) = v.id == id

volThat :: forall a. (Volunteer -> a) -> VolunteerShift -> a
volThat f (Overnight v) = f v
volThat f (Evening v)   = f v

volId :: VolunteerShift -> VolId
volId (Overnight v) = v.id
volId (Evening v) = v.id

addVolunteerShift :: Date -> VolunteerShift -> List Shift -> List Shift
addVolunteerShift date vol shifts =
  case findIndex (\s -> s.date == date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\s -> s{ volunteers = (flip snoc) vol s.volunteers }) shifts
    _      -> snoc shifts $ { date, volunteers: singleton vol }

changeVolunteerShift :: Date -> VolId -> List Shift -> List Shift
changeVolunteerShift date volId shifts =
  case findIndex (\s -> s.date == date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\s ->
                case findIndex (hasVolWithId volId) s.volunteers of
                  Just j -> s{ volunteers = fromMaybe s.volunteers $ modifyAt j otherShiftType s.volunteers }
                  _      -> s) shifts
    _      -> shifts

otherShiftType :: VolunteerShift -> VolunteerShift
otherShiftType (Overnight v) = Evening v
otherShiftType (Evening v) = Overnight v

removeVolunteerShift :: Date -> Volunteer -> List Shift -> List Shift
removeVolunteerShift date vol shifts =
  case findIndex (\s -> s.date == date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\s ->
                case findIndex (hasVolWithId vol.id) s.volunteers of
                  Just j -> s{ volunteers = fromMaybe s.volunteers $ deleteAt j s.volunteers }
                  _      -> s) shifts
    _      -> shifts

updateVolunteer :: Volunteer -> List Shift -> List Shift
updateVolunteer vol = map updateShift
  where
  updateShift s = s { volunteers = map updateVol s.volunteers }
  updateVol (Overnight v) | v.id == vol.id = Overnight vol
  updateVol (Evening   v) | v.id == vol.id = Evening vol
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

filterOut :: Volunteer -> List VolunteerShift -> List VolunteerShift
filterOut v = filter (not <<< hasVolWithId $ v.id)

isAllowed :: RuleParams -> Boolean
isAllowed params = satisfies params $ toUnfoldable [ notHaveSameVolunteerTwice
                                                   ]
  where
  satisfies :: RuleParams -> List Rule -> Boolean
  satisfies p = all id <<< map (not <<< isJust) <<< (flip flap) p

canAddVolunteer :: Config -> VolunteerShift -> Shift -> Boolean
canAddVolunteer config volShift s =
  isAllowed { shift: s{ volunteers = volShift:s.volunteers }
            , config
            }

canChangeVolunteerShiftType :: Config -> Volunteer -> Shift -> Boolean
canChangeVolunteerShiftType config v s =
  case find (hasVolWithId v.id) s.volunteers of
    Nothing -> false
    (Just volShift) ->
      let changedShift = case volShift of
                           (Overnight _) -> (Evening v)
                           (Evening _)   -> (Overnight v)
      in isAllowed { shift: s{ volunteers = changedShift:(filterOut v s.volunteers) }
                   , config
                   }

validate :: Config -> Shift -> List RuleResult
validate config shift =
  collectViolations params $ toUnfoldable [ must <<< notHaveSameVolunteerTwice
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
  isLooming shift =
    let (Days d) = shift.date `diff` config.currentDate
    in d >= 0.0 && d < (toNumber config.urgentPeriodDays)

  isPast :: Shift -> Boolean
  isPast shift =
    let (Days d) = shift.date `diff` config.currentDate
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
notExceedMaxVolunteers { shift, config: { maxVolsPerShift } } =
  justIf ("has more than " <> show maxVolsPerShift <> " volunteers")
       $ length shift.volunteers > maxVolsPerShift

notHaveSameVolunteerTwice :: Rule
notHaveSameVolunteerTwice { shift } =
  justIf "has the same volunteer down twice"
       $ length (nubBy (\a b -> volId a == volId b) shift.volunteers) > length shift.volunteers

haveAtLeastOneVolunteer :: Rule
haveAtLeastOneVolunteer { shift } =
  justIf "has no volunteers"
       $ length shift.volunteers == 0

haveMoreThanOneVolunteer :: Rule
haveMoreThanOneVolunteer { shift } =
  justIf "has only one volunteer"
       $ length shift.volunteers == 1

haveAnOvernightVolunteer :: Rule
haveAnOvernightVolunteer { shift, config: { currentDate } } =
  justIf "has no overnight volunteer"
       $ length shift.volunteers /= 0 && (length $ filter isOvernight shift.volunteers) == 0
  where
  isOvernight :: VolunteerShift -> Boolean
  isOvernight (Overnight _) = true
  isOvernight _ = false

notViolateAnyVolsSharingPrefs :: Rule
notViolateAnyVolsSharingPrefs { shift } =
  justIf "goes against a volunteer's preferences"
       $ any violatesSharingPrefs shift.volunteers
  where
  violatesSharingPrefs :: VolunteerShift -> Boolean
  violatesSharingPrefs (Overnight vol@{ overnightPreference: Just PreferToBeAlone }) = any isOvernight $ filterOut vol shift.volunteers
  violatesSharingPrefs (Overnight vol@{ overnightPreference: Just PreferAnotherVolunteer }) = (not <<< any) isOvernight $ filterOut vol shift.volunteers
  violatesSharingPrefs _ = false
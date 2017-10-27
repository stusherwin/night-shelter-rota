module App.Data (Shift (..), Volunteer(..), VolId(..), Gender(..), OvernightSharingPrefs(..), VolunteerShift(..), RuleResult(..), addVolunteerShift, changeVolunteerShift, removeVolunteerShift, canAddVolunteer, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType, parseVolId, nextVolId, updateVolunteer) where

import Prelude

import Data.Array (toUnfoldable)
import App.Common (toDateString, justIf, sortWith)
import Data.List (List(..), findIndex, find, modifyAt, snoc, updateAt, deleteAt, length, all, nub, nubBy, (:), filter, catMaybes, any, singleton)
import Data.Date (diff)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int (fromString)
import Data.Lens (_1)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, fromJust)
import Data.Newtype --(Newtype)
import Data.Time.Duration (Days(..))
import Data.Tuple.Nested (Tuple3(..))
import React.DOM.Dynamic (a)
import Type.Data.Boolean (False)

data Gender = Male | Female | Other String

instance genderShow :: Show (Gender) where
  show Male = "Male"
  show Female = "Female"
  show (Other gender) = gender

data OvernightSharingPrefs = None
                           | OnlyGender Gender
                           | Custom String
                           | Any

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
                 , gender :: Maybe Gender
                 , overnightSharingPrefs :: OvernightSharingPrefs
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

changeVolunteerShift :: Date -> VolunteerShift -> List Shift -> List Shift
changeVolunteerShift date vol shifts =
  case findIndex (\s -> s.date == date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\s ->
                case findIndex (hasVolWithId $ volId vol) s.volunteers of
                  Just j -> s{ volunteers = fromMaybe s.volunteers $ updateAt j vol s.volunteers }
                  _      -> s) shifts
    _      -> shifts

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

  
type RuleParams r = { shift :: Shift | r }

type Rule r = RuleParams r -> Maybe String

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

hasGender :: Gender -> Volunteer -> Boolean
hasGender Male   { gender: Just Male }   = true 
hasGender Female { gender: Just Female } = true
hasGender (Other o1)  { gender: Just (Other o2)} = o1 == o2
hasGender _ _ = false

filterOut :: Volunteer -> List VolunteerShift -> List VolunteerShift
filterOut v = filter (not <<< hasVolWithId $ v.id)

config :: _
config = { maxVolsPerShift: 2
         , urgentPeriodDays: 14.0
         }

isAllowed :: forall r. { shift :: Shift | r } -> Boolean
isAllowed params = satisfies params $ toUnfoldable [ notHaveSameVolunteerTwice
                                                   ]
  where
  satisfies :: RuleParams r -> List (Rule r) -> Boolean
  satisfies p = all id <<< map (not <<< isJust) <<< (flip flap) p

canAddVolunteer :: VolunteerShift -> Shift -> Boolean
canAddVolunteer volShift s =
  isAllowed { shift: s{ volunteers = volShift:s.volunteers }
            }

canChangeVolunteerShiftType :: Volunteer -> Shift -> Boolean
canChangeVolunteerShiftType v s =
  case find (hasVolWithId v.id) s.volunteers of
    Nothing -> false
    (Just volShift) ->
      let changedShift = case volShift of
                           (Overnight _) -> (Evening v)
                           (Evening _)   -> (Overnight v)
      in isAllowed { shift: s{ volunteers = changedShift:(filterOut v s.volunteers) }
                   }

isLooming :: Shift -> Date -> Boolean
isLooming shift currentDate =
  let (Days d) = shift.date `diff` currentDate
  in d >= 0.0 && d < config.urgentPeriodDays

isPast :: Shift -> Date -> Boolean
isPast shift currentDate =
  let (Days d) = shift.date `diff` currentDate
  in d < 0.0

validate :: Shift -> Date -> List RuleResult
validate shift currentDate =
  collectViolations params $ toUnfoldable [ must <<< notHaveSameVolunteerTwice
                                          , ignoreIf (isPast shift currentDate) <<< should <<< notExceedMaxVolunteers
                                          , ignoreIf (isPast shift currentDate) <<< mustIf (isLooming shift currentDate) <<< haveAtLeastOneVolunteer
                                          , ignoreIf (isPast shift currentDate) <<< could <<< haveMoreThanOneVolunteer
                                          , ignoreIf (isPast shift currentDate) <<< mustIf (isLooming shift currentDate) <<< haveAnOvernightVolunteer
                                          , ignoreIf (isPast shift currentDate) <<< should <<< notViolateAnyVolsSharingPrefs
                                          ]
  where
  params = { shift
           , currentDate
           }

  collectViolations :: forall r. RuleParams r -> List (RuleParams r -> Maybe RuleResult) -> List RuleResult
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

notExceedMaxVolunteers :: forall r. Rule r
notExceedMaxVolunteers { shift } =
  justIf ("has more than " <> show config.maxVolsPerShift <> " volunteers")
       $ length shift.volunteers > config.maxVolsPerShift

notHaveSameVolunteerTwice :: forall r. Rule r
notHaveSameVolunteerTwice { shift } =
  justIf "has the same volunteer down twice"
       $ length (nubBy (\a b -> volId a == volId b) shift.volunteers) > length shift.volunteers

haveAtLeastOneVolunteer :: forall r. Rule (currentDate :: Date | r)
haveAtLeastOneVolunteer { shift } =
  justIf "has no volunteers"
       $ length shift.volunteers == 0

haveMoreThanOneVolunteer :: forall r. Rule r
haveMoreThanOneVolunteer { shift } =
  justIf "has only one volunteer"
       $ length shift.volunteers == 1

haveAnOvernightVolunteer :: forall r. Rule (currentDate :: Date | r)
haveAnOvernightVolunteer { shift, currentDate } =
  justIf "has no overnight volunteer"
       $ length shift.volunteers /= 0 && (length $ filter isOvernight shift.volunteers) == 0
  where
  isOvernight :: VolunteerShift -> Boolean
  isOvernight (Overnight _) = true
  isOvernight _ = false

notViolateAnyVolsSharingPrefs :: forall r. Rule r
notViolateAnyVolsSharingPrefs { shift } =
  justIf "goes against a volunteer's sharing preferences"
       $ any violatesSharingPrefs shift.volunteers
  where
  violatesSharingPrefs :: VolunteerShift -> Boolean
  violatesSharingPrefs (Overnight vol@{ overnightSharingPrefs: None }) =         any isOvernight $ filterOut vol shift.volunteers
  violatesSharingPrefs (Overnight vol@{ overnightSharingPrefs: OnlyGender g }) = any ((&&) <$> isOvernight <*> (not <<< (volThat (hasGender g)))) $ filterOut vol shift.volunteers
  violatesSharingPrefs _ = false
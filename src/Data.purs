module App.Data (Shift (..), Volunteer(..), Gender(..), OvernightSharingPrefs(..), VolunteerShift(..), RuleResult(..), addVolunteerShift, changeVolunteerShift, removeVolunteerShift, canAddVolunteer, hasId, hasDate, hasVolWithId, validate, filterOut, canChangeVolunteerShiftType) where

import Prelude

import App.Common (toDateString)
import Data.Array (findIndex, find, modifyAt, snoc, updateAt, deleteAt, length, all, nub, nubBy, (:), filter, catMaybes, sortWith, any)
import Data.Date (diff)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lens (_1)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, fromJust)
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
 
data Volunteer = Vol { id :: Int
                     , name :: String
                     , gender :: Maybe Gender
                     , overnightSharingPrefs :: OvernightSharingPrefs
                     }

data VolunteerShift = Overnight Volunteer
                    | Evening Volunteer

data Shift = Shift { date :: Date, volunteers :: Array VolunteerShift }

hasDate :: Date -> Shift -> Boolean
hasDate date (Shift s) = s.date == date

hasId :: Int -> Volunteer -> Boolean
hasId id (Vol v) = v.id == id

hasVolWithId :: Int -> VolunteerShift -> Boolean
hasVolWithId id (Overnight (Vol v)) = v.id == id
hasVolWithId id (Evening (Vol v)) = v.id == id

volThat :: forall a. (Volunteer -> a) -> VolunteerShift -> a
volThat f (Overnight v) = f v
volThat f (Evening v)   = f v

volId :: VolunteerShift -> Int
volId (Overnight (Vol v)) = v.id
volId (Evening (Vol v)) = v.id

addVolunteerShift :: Date -> VolunteerShift -> Array Shift -> Array Shift
addVolunteerShift date vol shifts =
  case findIndex (hasDate date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) -> Shift s{ volunteers = (flip snoc) vol s.volunteers }) shifts
    _      -> snoc shifts $ Shift {date, volunteers: [vol]}

changeVolunteerShift :: Date -> VolunteerShift -> Array Shift -> Array Shift
changeVolunteerShift shiftDate vol shifts =
  case findIndex (hasDate shiftDate) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) ->
                case findIndex (hasVolWithId $ volId vol) s.volunteers of
                  Just j -> Shift s{ volunteers = fromMaybe s.volunteers $ updateAt j vol s.volunteers }
                  _      -> Shift s) shifts
    _      -> shifts

removeVolunteerShift :: Date -> Volunteer -> Array Shift -> Array Shift
removeVolunteerShift shiftDate (Vol vol) shifts =
  case findIndex (hasDate shiftDate) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) ->
                case findIndex (hasVolWithId vol.id) s.volunteers of
                  Just j -> Shift s{ volunteers = fromMaybe s.volunteers $ deleteAt j s.volunteers }
                  _      -> Shift s) shifts
    _      -> shifts
  
type RuleParams r = { shift :: Shift | r }

type Rule r = RuleParams r -> Maybe String

data RuleSeverity = Must
                  | Should
                  | Could
                  | MustIf Boolean

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
hasGender Male   (Vol { gender: Just Male })   = true
hasGender Female (Vol { gender: Just Female }) = true
hasGender (Other o1)  (Vol { gender: Just (Other o2)}) = o1 == o2
hasGender _ _ = false

filterOut :: Volunteer -> Array VolunteerShift -> Array VolunteerShift
filterOut (Vol v) = filter (not <<< hasVolWithId $ v.id)

config = { maxVolsPerShift: 2
         , urgentPeriodDays: 14.0
         }

isAllowed :: forall r. { shift :: Shift | r } -> Boolean
isAllowed params = satisfies params [ notExceedMaxVolunteers
                                    , notHaveSameVolunteerTwice
                                    ]
  where
  satisfies :: forall r. RuleParams r -> Array (Rule r) -> Boolean
  satisfies p = all id <<< map (not <<< isJust) <<< (flip flap) p

canAddVolunteer :: VolunteerShift -> Shift -> Boolean
canAddVolunteer volShift (Shift s) =
  isAllowed { shift: Shift s{ volunteers = volShift:s.volunteers }
            }

canChangeVolunteerShiftType :: Volunteer -> Shift -> Boolean
canChangeVolunteerShiftType vol@(Vol v) (Shift s) =
  case find (hasVolWithId v.id) s.volunteers of
    Nothing -> false
    (Just volShift) ->
      let changedShift = case volShift of
                           (Overnight _) -> (Evening vol)
                           (Evening _)   -> (Overnight vol)
      in isAllowed { shift: Shift s{ volunteers = changedShift:(filterOut vol s.volunteers) }
                   }

isUrgent :: Shift -> Date -> Boolean
isUrgent (Shift shift) currentDate =
  let (Days d) = shift.date `diff` currentDate
  in d < config.urgentPeriodDays

validate :: Shift -> Date -> Array RuleResult
validate shift currentDate =
  collectViolations params [ wrap Must notExceedMaxVolunteers
                           , wrap Must notHaveSameVolunteerTwice
                           , wrap (MustIf $ isUrgent shift currentDate) haveAtLeastOneVolunteer
                           , wrap Could haveMoreThanOneVolunteer
                           , wrap (MustIf $ isUrgent shift currentDate) haveAnOvernightVolunteer
                           , wrap Should notViolateAnyVolsSharingPrefs
                           ]
  where
  params = { shift
           , currentDate
           }

  collectViolations :: forall r. RuleParams r -> Array (RuleParams r -> Maybe RuleResult) -> Array RuleResult
  collectViolations params severities = sortWith priority
                                      $ catMaybes
                                      $ (flip flap) params
                                      $ severities

  wrap :: forall r. RuleSeverity -> Rule r -> (RuleParams r -> Maybe RuleResult)
  wrap rs r = \p -> map wrap' (r p)
    where wrap' = case rs of
                    Must   -> Error
                    Should -> Warning
                    Could  -> Info
                    (MustIf condition) -> if condition then Error else const Neutral

  priority :: RuleResult -> Int
  priority (Error   _) = 0
  priority (Warning _) = 1
  priority (Info _)    = 2
  priority _           = 3

notExceedMaxVolunteers :: forall r. Rule r
notExceedMaxVolunteers { shift: (Shift s) } =
  if length s.volunteers > config.maxVolsPerShift
    then Just $ "Too many volunteers (max is " <> show config.maxVolsPerShift <> ")"
    else Nothing

notHaveSameVolunteerTwice :: forall r. Rule r
notHaveSameVolunteerTwice { shift: Shift s } =
  if length (nubBy (\a b -> volId a == volId b) s.volunteers) > length s.volunteers
    then Just "The same volunteer is down twice for this shift"
    else Nothing

haveAtLeastOneVolunteer :: forall r. Rule (currentDate :: Date | r)
haveAtLeastOneVolunteer { shift: Shift s } =
  if length s.volunteers == 0
    then Just "This shift has no volunteers"
    else Nothing

haveMoreThanOneVolunteer :: forall r. Rule r
haveMoreThanOneVolunteer { shift: Shift s } =
  if length s.volunteers == 1
    then Just "This shift could do with another volunteer"
    else Nothing

haveAnOvernightVolunteer :: forall r. Rule (currentDate :: Date | r)
haveAnOvernightVolunteer { shift: Shift s, currentDate } =
  let isOvernight :: VolunteerShift -> Boolean
      isOvernight (Overnight _) = true
      isOvernight _ = false
  in if length s.volunteers /= 0 && (length $ filter isOvernight s.volunteers) == 0
     then Just "This shift has no overnight volunteer"
     else Nothing

notViolateAnyVolsSharingPrefs :: forall r. Rule r
notViolateAnyVolsSharingPrefs { shift: Shift s } =
  if any violatesSharingPrefs s.volunteers
    then Just "This shift violates a volunteer's sharing preferences"
    else Nothing
  where
  violatesSharingPrefs :: VolunteerShift -> Boolean
  violatesSharingPrefs (Overnight vol@(Vol { overnightSharingPrefs: None })) =         any isOvernight $ filterOut vol s.volunteers
  violatesSharingPrefs (Overnight vol@(Vol { overnightSharingPrefs: OnlyGender g })) = any ((&&) <$> isOvernight <*> (not <<< (volThat (hasGender g)))) $ filterOut vol s.volunteers
  violatesSharingPrefs _ = false
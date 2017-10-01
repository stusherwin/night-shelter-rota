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

data Gender = Male | Female | Other

data OvernightSharingPrefs = None | OnlySameGender | Any
 
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

type Rule r = RuleParams r -> Maybe RuleResult

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
hasGender Other  (Vol { gender: Just Other })  = true
hasGender _ _ = false

filterOut :: Volunteer -> Array VolunteerShift -> Array VolunteerShift
filterOut (Vol v) = filter (not <<< hasVolWithId $ v.id)

canAddVolunteer :: VolunteerShift -> Shift -> Boolean
canAddVolunteer volShift (Shift s) =
  satisfies params [ mustNotExceedMaxVolunteers
                   , mustNotHaveSameVolunteerTwice
                   , mustNotViolateAnyVolsSharingPrefs
                   ]
  where
  params = { shift: Shift s{ volunteers = volShift:s.volunteers }
           , maxVolsPerShift: 2
           }
  satisfies :: forall r. RuleParams r -> Array (Rule r) -> Boolean
  satisfies p = all id <<< map (not <<< isError) <<< (flip flap) p

canChangeVolunteerShiftType :: Volunteer -> Shift -> Boolean
canChangeVolunteerShiftType vol@(Vol v) (Shift s) =
  case find (hasVolWithId v.id) s.volunteers of
    Nothing -> false
    (Just volShift) ->
      let changedShift = case volShift of
                           (Overnight _) -> (Evening vol)
                           (Evening _)   -> (Overnight vol)
          params = { shift: Shift s{ volunteers = changedShift:(filterOut vol s.volunteers) }
                   , maxVolsPerShift: 2
                   }
      in satisfies params [ mustNotExceedMaxVolunteers
                          , mustNotHaveSameVolunteerTwice
                          , mustNotViolateAnyVolsSharingPrefs
                          ]
  where
  satisfies :: forall r. RuleParams r -> Array (Rule r) -> Boolean
  satisfies p = all id <<< map (not <<< isError) <<< (flip flap) p

validate :: Shift -> Date -> Array RuleResult
validate shift currentDate =
  collectViolations params [ mustNotExceedMaxVolunteers
                           , mustNotHaveSameVolunteerTwice
                           , mustHaveAtLeastOneVolunteer
                           , shouldHaveMoreThanOneVolunteer
                           , mustHaveAnOvernightVolunteer
                           , mustNotViolateAnyVolsSharingPrefs
                           ]
  where
  params = { shift
           , maxVolsPerShift: 2
           , currentDate
           , urgentPeriodDays: 14.0
           }

  collectViolations :: forall r. RuleParams r -> Array (Rule r) -> Array RuleResult
  collectViolations params severities = sortWith priority $ catMaybes $ (flip flap) params $ severities

  priority :: RuleResult -> Int
  priority (Error   _) = 0
  priority (Warning _) = 1
  priority _           = 2

mustNotExceedMaxVolunteers :: forall r. Rule (maxVolsPerShift :: Int | r)
mustNotExceedMaxVolunteers { shift: (Shift s), maxVolsPerShift } =
  if length s.volunteers > maxVolsPerShift
    then Just $ Error $ "Too many volunteers (max is " <> show maxVolsPerShift <> ")"
    else Nothing

mustNotHaveSameVolunteerTwice :: forall r. Rule r
mustNotHaveSameVolunteerTwice { shift: Shift s } =
  if length (nubBy (\a b -> volId a == volId b) s.volunteers) > length s.volunteers
    then Just $ Error "The same volunteer is down twice for this shift"
    else Nothing

mustHaveAtLeastOneVolunteer :: forall r. Rule (currentDate :: Date, urgentPeriodDays :: Number | r)
mustHaveAtLeastOneVolunteer { shift: Shift s, currentDate, urgentPeriodDays } =
  let (Days d) = s.date `diff` currentDate
  in if length s.volunteers == 0
    then (if d < urgentPeriodDays
            then Just $ Error "This shift is happening soon and has no volunteers"
            else Just $ Neutral)
    else Nothing

shouldHaveMoreThanOneVolunteer :: forall r. Rule r
shouldHaveMoreThanOneVolunteer { shift: Shift s } =
  if length s.volunteers == 1
    then Just $ Info "This shift could do with another volunteer"
    else Nothing

mustHaveAnOvernightVolunteer :: forall r. Rule (currentDate :: Date, urgentPeriodDays :: Number | r)
mustHaveAnOvernightVolunteer { shift: Shift s, currentDate, urgentPeriodDays } =
  let (Days d) = s.date `diff` currentDate
      isOvernight :: VolunteerShift -> Boolean
      isOvernight (Overnight _) = true
      isOvernight _ = false
  in if length s.volunteers /= 0 && (length $ filter isOvernight s.volunteers) == 0
    then (if d < urgentPeriodDays
            then Just $ Error "This shift is happening soon and has no overnight volunteer"
            else Just $ Warning "This shift has no overnight volunteer")
    else Nothing

mustNotViolateAnyVolsSharingPrefs :: forall r. Rule r
mustNotViolateAnyVolsSharingPrefs { shift: Shift s } =
  if any violatesSharingPrefs s.volunteers
    then Just $ Error "This shift violates a volunteer's sharing preferences"
    else Nothing
  where
  violatesSharingPrefs :: VolunteerShift -> Boolean
  violatesSharingPrefs (Overnight vol@(Vol { overnightSharingPrefs: None })) = any isOvernight $ filterOut vol s.volunteers
  violatesSharingPrefs (Overnight vol@(Vol { overnightSharingPrefs: OnlySameGender, gender: (Just g) })) = any ((&&) <$> isOvernight <*> (not <<< (volThat (hasGender g)))) $ filterOut vol s.volunteers
  violatesSharingPrefs _ = false
  

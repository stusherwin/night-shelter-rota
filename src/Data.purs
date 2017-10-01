module App.Data (Shift (..), Volunteer(..), VolunteerShift(..), RuleResult(..), addVolunteerShift, changeVolunteerShift, removeVolunteerShift, canAddVolunteer, hasId, hasDate, hasVolWithId, validate) where

import Prelude

import App.Common (toDateString)
import Data.Array (findIndex, find, modifyAt, snoc, updateAt, deleteAt, length, all, nub, nubBy, (:), filter, catMaybes, sortWith)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Time.Duration (Days(..))
import Data.Date (diff)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lens (_1)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, fromJust)
import Data.Tuple.Nested (Tuple3(..))
import Type.Data.Boolean (False)
 
data Volunteer = Vol { id :: Int
                     , name :: String }

data VolunteerShift = Overnight Volunteer
                    | Evening Volunteer

data Shift = Shift { date :: Date, volunteers :: Array VolunteerShift }

-- instance showShift :: Show Shift where
--   show (Shift {date: d, volunteers: v, isOvernight: o}) = show o

-- instance showVolunteer :: Show Volunteer where
--   show (Vol {name: n}) = show n

hasDate :: Date -> Shift -> Boolean
hasDate date (Shift s) = s.date == date

hasId :: Int -> Volunteer -> Boolean
hasId id (Vol v) = v.id == id

hasVolWithId :: Int -> VolunteerShift -> Boolean
hasVolWithId id (Overnight (Vol v)) = v.id == id
hasVolWithId id (Evening (Vol v)) = v.id == id

volId :: VolunteerShift -> Int
volId (Overnight (Vol v)) = v.id
volId (Evening (Vol v)) = v.id

addVolunteerShift :: Date -> VolunteerShift -> Array Shift -> Array Shift
addVolunteerShift shiftDate vol shifts =
  case findIndex (hasDate shiftDate) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (\(Shift s) -> Shift s{ volunteers = (flip snoc) vol s.volunteers }) shifts
    _      -> snoc shifts $ Shift {date: shiftDate, volunteers: [vol]}

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

canAddVolunteer :: VolunteerShift -> Shift -> Boolean
canAddVolunteer volShift (Shift s) =
  satisfies params [ notTooManyVolunteers
                   , noDuplicateVolunteers
                   ]
  where
  params = { shift: Shift s{ volunteers = volShift:s.volunteers }
           , maxVolsPerShift: 2
           }

  satisfies :: forall r. RuleParams r -> Array (Rule r) -> Boolean
  satisfies p = all id <<< map (not <<< isError) <<< (flip flap) p

validate :: Shift -> Date -> Array RuleResult
validate shift date =
  collectViolations params [ notTooManyVolunteers
                           , noDuplicateVolunteers
                           , noVolunteers
                           , notEnoughVolunteers
                           , noOvernightVolunteers
                           ]
  where
  params = { shift: shift
           , maxVolsPerShift: 2
           , currentDate: date
           , urgentPeriodDays: 14.0
           }

  collectViolations :: forall r. RuleParams r -> Array (Rule r) -> Array RuleResult
  collectViolations params severities = sortWith priority $ catMaybes $ (flip flap) params $ severities

  priority :: RuleResult -> Int
  priority (Error   _) = 0
  priority (Warning _) = 1
  priority _           = 2

notTooManyVolunteers :: forall r. Rule (maxVolsPerShift :: Int | r)
notTooManyVolunteers { shift: (Shift s), maxVolsPerShift: max } =
  if length s.volunteers > max
    then Just $ Error $ "Too many volunteers (max is " <> show max <> ")"
    else Nothing

noDuplicateVolunteers :: forall r. Rule r
noDuplicateVolunteers { shift: Shift s } =
  if length (nubBy (\a b -> volId a == volId b) s.volunteers) > length s.volunteers
    then Just $ Error "Duplicate volunteer"
    else Nothing

noVolunteers :: forall r. Rule (currentDate :: Date, urgentPeriodDays :: Number | r)
noVolunteers { shift: Shift s, currentDate, urgentPeriodDays } =
  let (Days d) = s.date `diff` currentDate
  in if length s.volunteers == 0
    then (if d < urgentPeriodDays
            then Just $ Error "This shift is happening soon and has no volunteers"
            else Just $ Neutral)
    else Nothing

notEnoughVolunteers :: forall r. Rule r
notEnoughVolunteers { shift: Shift s } =
  if length s.volunteers == 1
    then Just $ Info "This shift could do with another volunteer"
    else Nothing

noOvernightVolunteers :: forall r. Rule (currentDate :: Date, urgentPeriodDays :: Number | r)
noOvernightVolunteers { shift: Shift s, currentDate, urgentPeriodDays } =
  let (Days d) = s.date `diff` currentDate
      isOvernight :: VolunteerShift -> Boolean
      isOvernight (Overnight _) = true
      isOvernight _ = false
  in if length s.volunteers /= 0 && (length $ filter isOvernight s.volunteers) == 0
    then (if d < urgentPeriodDays
            then Just $ Error "This shift is happening soon and has no overnight volunteer"
            else Just $ Warning "This shift has no overnight volunteer")
    else Nothing
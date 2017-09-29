module App.Data (Shift (..), Volunteer(..), VolunteerShift(..), addVolunteerShift, changeVolunteerShift, removeVolunteerShift, canAddVolunteer, hasId, hasDate, hasVolWithId) where

import Prelude

import Data.Array (findIndex, find, modifyAt, snoc, updateAt, deleteAt, length)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple.Nested (Tuple3(..))
import Type.Data.Boolean (False)

import App.Common (toDateString)
 
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

canAddVolunteer :: Volunteer -> Shift -> Boolean
canAddVolunteer (Vol vol) (Shift shift) = 
  ((length shift.volunteers) < 2) && (isNothing $ find (hasVolWithId vol.id) shift.volunteers)
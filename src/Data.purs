module App.Data (Shift (..), Volunteer(..), VolunteerShift(..), addVolunteer, canAddVolunteer, hasId, hasDate, hasVolWithId) where

import Prelude

import Data.Array (findIndex, find, modifyAt, snoc)
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

addVolunteer :: Date -> VolunteerShift -> Array Shift -> Array Shift
addVolunteer shiftDate vol shifts =
  let mi = findIndex (hasDate shiftDate) shifts
  in case mi of
    (Just i) -> fromMaybe shifts $ modifyAt i (\(Shift s) -> Shift s{ volunteers = (flip snoc) vol s.volunteers }) shifts
    Nothing  -> (flip snoc) (Shift {date: shiftDate, volunteers: [vol]}) shifts

canAddVolunteer :: Shift -> Maybe Volunteer -> Boolean
canAddVolunteer _ Nothing = false
canAddVolunteer (Shift shift) (Just (Vol vol)) = isNothing $ find (hasVolWithId vol.id) shift.volunteers
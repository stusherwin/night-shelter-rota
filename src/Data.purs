module App.Data (Shift (..), Volunteer(..), addVolunteer) where

import Prelude
import Data.Array (findIndex, find, modifyAt, snoc)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (Tuple3(..))

newtype Shift = S { date :: Date, volunteers :: Array Volunteer }

instance showShift :: Show Shift where
  show (S {date: d, volunteers: v}) = show d <> ": " <> foldMap ((_ <> ",") <<< show) v

newtype Volunteer = V { id :: Int
                      , name :: String }

instance showVolunteer :: Show Volunteer where
  show (V {name: n}) = show n

addVolunteer :: Date -> Maybe Volunteer -> Array Shift -> Array Shift
addVolunteer shiftDate maybeVol shifts =
  let mi = findIndex (\(S s) -> s.date == shiftDate) shifts
      ms = find (\(S s) -> s.date == shiftDate) shifts
  in case mi, ms, maybeVol of
    (Just i), (Just shift), (Just vol) -> fromMaybe shifts $ modifyAt i (\(S s) -> S s{ volunteers = (flip snoc) vol s.volunteers }) shifts
    Nothing, Nothing, (Just vol) -> (flip snoc) (S {date: shiftDate, volunteers: [vol]}) shifts
    _, _, _ -> shifts
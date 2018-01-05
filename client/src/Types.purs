module App.Types where

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Prim (Array, Int, String)
import Data.DateTime (Date)

import Prelude

type Volunteer = { id :: Int
                 , name :: String
                 , overnightPreference :: Maybe OvernightPreference
                 , overnightGenderPreference :: Maybe OvernightGenderPreference
                 , notes :: String
                 }

type VolunteerShift = { volunteer :: Volunteer
                      , shiftType :: ShiftType
                      }

type Shift = { date :: Date
             , volunteers :: List VolunteerShift
             }

data OvernightPreference =
    PreferToBeAlone
  | PreferAnotherVolunteer

derive instance eqOvernightPreference :: Eq OvernightPreference

data OvernightGenderPreference =
    Male
  | Female

derive instance eqOvernightGenderPreference :: Eq OvernightGenderPreference

data ShiftType =
    Overnight
  | Evening

derive instance eqShiftType :: Eq ShiftType

type VolunteerDetails = { name :: String
                        , notes :: String
                        , pref :: Maybe OvernightPreference
                        , genderPref :: Maybe OvernightGenderPreference
                        }

otherShiftType :: ShiftType -> ShiftType
otherShiftType Evening   = Overnight
otherShiftType Overnight = Evening

overnightPrefMarker :: OvernightPreference -> String
overnightPrefMarker PreferToBeAlone = "1"
overnightPrefMarker PreferAnotherVolunteer = "2"

overnightPrefDescription :: OvernightPreference -> String
overnightPrefDescription PreferToBeAlone = "I prefer to be on my own"
overnightPrefDescription PreferAnotherVolunteer = "I prefer to work with another volunteer"

overnightGenderPrefMarker :: OvernightGenderPreference -> String
overnightGenderPrefMarker Male = "M"
overnightGenderPrefMarker Female = "F"

overnightGenderPrefDescription :: OvernightGenderPreference -> String
overnightGenderPrefDescription Male = "Males only"
overnightGenderPrefDescription Female = "Females only"
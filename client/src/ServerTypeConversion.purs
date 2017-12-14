module App.ServerTypeConversion where

import Data.DateTime (Date)
import Data.Date (year, month, day)
import Data.List (List(..), fromFoldable)
import App.Common (mkDate)
import Data.Enum (fromEnum)
import ServerTypes as API
import App.Types
import Prelude

fromAPIShiftDate :: API.ShiftDate -> Date
fromAPIShiftDate (API.ShiftDate { year: y, month: m, day: d }) = mkDate d m y

toAPIShiftDate :: Date -> API.ShiftDate
toAPIShiftDate date = API.ShiftDate { day: fromEnum (day date)
                                    , month: fromEnum (month date)
                                    , year: fromEnum (year date)
                                    }

fromAPIShiftType :: API.ShiftType -> ShiftType
fromAPIShiftType API.Overnight = Overnight
fromAPIShiftType API.Evening = Evening

toAPIShiftType :: ShiftType -> API.ShiftType
toAPIShiftType Overnight = API.Overnight
toAPIShiftType Evening = API.Evening

fromAPIOvernightPreference :: API.OvernightPreference -> OvernightPreference
fromAPIOvernightPreference API.PreferToBeAlone = PreferToBeAlone
fromAPIOvernightPreference API.PreferAnotherVolunteer = PreferAnotherVolunteer

toAPIOvernightPreference :: OvernightPreference -> API.OvernightPreference
toAPIOvernightPreference PreferToBeAlone = API.PreferToBeAlone
toAPIOvernightPreference PreferAnotherVolunteer = API.PreferAnotherVolunteer

fromAPIOvernightGenderPreference :: API.OvernightGenderPreference -> OvernightGenderPreference
fromAPIOvernightGenderPreference API.Male = Male
fromAPIOvernightGenderPreference API.Female = Female

toAPIOvernightGenderPreference :: OvernightGenderPreference -> API.OvernightGenderPreference
toAPIOvernightGenderPreference Male = API.Male
toAPIOvernightGenderPreference Female = API.Female

fromAPIVolShifts :: Array API.VolunteerShift -> List VolunteerShift
fromAPIVolShifts volShifts = map fromAPIVolShift $ fromFoldable volShifts

fromAPIVolShift :: API.VolunteerShift -> VolunteerShift
fromAPIVolShift (API.VolunteerShift vs) = { volunteer: fromAPIVol vs.vsVolunteer
                                          , shiftType: fromAPIShiftType vs.vsShiftType
                                          }

fromAPIVols :: Array API.Volunteer -> List Volunteer
fromAPIVols vols = map fromAPIVol $ fromFoldable vols

fromAPIVol :: API.Volunteer -> Volunteer
fromAPIVol (API.Volunteer v) = { id: v.vId
                               , name: v.vName
                               , overnightPreference: fromAPIOvernightPreference <$> v.vOvernightPreference
                               , overnightGenderPreference: fromAPIOvernightGenderPreference <$> v.vOvernightGenderPreference
                               , notes: v.vNotes
                               }

fromAPIShifts :: Array API.Shift -> List Shift
fromAPIShifts shifts = map fromAPIShift $ fromFoldable shifts

fromAPIShift :: API.Shift -> Shift
fromAPIShift (API.Shift s) = { date: fromAPIShiftDate s.sDate
                             , volunteers: fromAPIVolShifts s.sVolunteers
                             }

toAPIVolDetails :: VolunteerDetails -> API.VolunteerDetails
toAPIVolDetails d = API.VolunteerDetails { vdName: d.name
                                         , vdNotes: d.notes
                                         , vdPref: toAPIOvernightPreference <$> d.pref
                                         , vdGenderPref: toAPIOvernightGenderPreference <$> d.genderPref
                                         }
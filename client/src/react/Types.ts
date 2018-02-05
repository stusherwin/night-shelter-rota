export type Vol = { id: number
                  , name: string
                  , intro: string
                  , overnightPreference: '1' | '2' | null
                  , overnightGenderPreference: 'M' | 'F' | null
                  , notes: string
                  }

// type VolShift = { volunteer :: Vol
//                 , shiftType :: ShiftType
//                 }

// type Shift = { date :: Date
//              , volunteers :: List VolShift
//              }

// data OvernightPreference =
//     PreferToBeAlone
//   | PreferAnotherVolunteer

// derive instance eqOvernightPreference :: Eq OvernightPreference

// data OvernightGenderPreference =
//     Male
//   | Female

// derive instance eqOvernightGenderPreference :: Eq OvernightGenderPreference

// data ShiftType =
//     Overnight
//   | Evening

// derive instance eqShiftType :: Eq ShiftType

// type VolunteerDetails = { name :: String
//                         , intro :: String
//                         , pref :: Maybe OvernightPreference
//                         , genderPref :: Maybe OvernightGenderPreference
//                         , notes :: String
//                         }

// otherShiftType :: ShiftType -> ShiftType
// otherShiftType Evening   = Overnight
// otherShiftType Overnight = Evening

// overnightPrefMarker :: OvernightPreference -> String
// overnightPrefMarker PreferToBeAlone = '1'
// overnightPrefMarker PreferAnotherVolunteer = '2'

// overnightPrefDescription :: OvernightPreference -> String
// overnightPrefDescription PreferToBeAlone = 'I prefer to be on my own'
// overnightPrefDescription PreferAnotherVolunteer = 'I prefer to work with another volunteer'

// overnightGenderPrefMarker :: OvernightGenderPreference -> String
// overnightGenderPrefMarker Male = 'M'
// overnightGenderPrefMarker Female = 'F'

// overnightGenderPrefDescription :: OvernightGenderPreference -> String
// overnightGenderPrefDescription Male = 'Males only'
// overnightGenderPrefDescription Female = 'Females only'
// }
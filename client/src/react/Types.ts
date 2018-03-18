export type Vol = { id: number
                  , name: string
                  , intro: string
                  , overnightPreference: OvernightPreference | null
                  , overnightGenderPreference: OvernightGenderPreference | null
                  , notes: string
                  }

export type OvernightPreference = 'PreferToBeAlone' | 'PreferAnotherVolunteer'
export type OvernightGenderPreference = 'Male' | 'Female'
export type ShiftType = 'Overnight'
                      | 'Evening'

export type VolShift = { volunteer: Vol
                       , shiftType: ShiftType
                       }

export type Shift = { date: Date
                    , vols: VolShift[]
                    }

// data OvernightPreference =
//     PreferToBeAlone
//   | PreferAnotherVolunteer

// derive instance eqOvernightPreference :: Eq OvernightPreference

// data OvernightGenderPreference =
//     Male
//   | Female

// derive instance eqOvernightGenderPreference :: Eq OvernightGenderPreference


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
export interface PrefInfo {
  marker: string
  description: string
}

export function info(pref: OvernightPreference | OvernightGenderPreference): PrefInfo {
  switch(pref) {
    case 'PreferToBeAlone':        return { marker: '1', description: 'I prefer to be on my own' }
    case 'PreferAnotherVolunteer': return { marker: '2', description: 'I prefer to work with another volunteer' }
    case 'Male':                   return { marker: 'M', description: 'Males only' }
    case 'Female':
    default:                       return { marker: 'F', description: 'Females only' }
  }
}
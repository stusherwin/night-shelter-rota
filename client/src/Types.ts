import { Util } from './Util'

export type Vol = { id: number
                  , name: string
                  , intro: string
                  , overnightPreference: OvernightPreference | null
                  , overnightGenderPreference: OvernightGenderPreference | null
                  , notes: string
                  , active: boolean
                  }

export type OvernightPreference = 'PreferToBeAlone' | 'PreferAnotherVolunteer'
export type OvernightGenderPreference = 'Male' | 'Female'
export type ShiftType = 'Overnight'
                      | 'Evening'

export type VolShift = { vol: Vol
                       , shiftType: ShiftType
                       }

export type Shift = { date: Date
                    , vols: VolShift[]
                    , loading: boolean
                    }

export type VolDetails = { name: string
                         , intro: string
                         , pref: OvernightPreference | null
                         , genderPref: OvernightGenderPreference | null
                         , notes: string
                         }

export interface PrefInfo {
  marker: string
  description: string
}

export function info(pref: OvernightPreference | OvernightGenderPreference | null): PrefInfo | null {
  switch(pref) {
    case 'PreferToBeAlone':        return { marker: '1', description: 'I prefer to be on my own' }
    case 'PreferAnotherVolunteer': return { marker: '2', description: 'I prefer to work with another volunteer' }
    case 'Male':                   return { marker: 'M', description: 'Males only' }
    case 'Female':                 return { marker: 'F', description: 'Females only' }
    default: return null
  }
}

export function otherShiftType(shiftType: ShiftType) {
  if(shiftType == 'Evening') {
    return 'Overnight'
  } else {
    return 'Evening'
  }
}

export function updateVolDetails(volToUpdate: Vol, newDetails: Vol): Vol {
  return volToUpdate.id == newDetails.id? Util.update(volToUpdate, newDetails) : volToUpdate
}

export function findAndUpdateVolDetails(vols: Vol[], vol: Vol): Vol[] {
  return vols.map(v => updateVolDetails(v, vol))
}

export function findAndUpdateShiftVolDetails(shifts: Shift[], vol: Vol): Shift[] {
  return shifts.map(s => Util.update(s, { vols: s.vols.map(v => v.vol.id == vol.id
                                                                  ? Util.update(v, {vol: vol})
                                                                  : v)
                                        }))
}

export function updateVolShifts(shifts: Shift[], date: Date, vols: VolShift[]): Shift[] {
  let result = shifts.slice()
  let shift = result.find(s => Util.datesEqual(s.date, date))

  if(shift) {
    shift.vols = vols
    shift.loading = false
  } else {
    result.push({date: date, vols: vols, loading: false})
  }

  return result
}

export function createVolDetails(vol: Vol | null): VolDetails {
  return vol
    ? { name: vol.name
      , intro: vol.intro
      , pref: vol.overnightPreference
      , genderPref: vol.overnightGenderPreference
      , notes: vol.notes
      }
    : { name: ''
      , intro: ''
      , pref: null
      , genderPref: null
      , notes: ''
      }
}
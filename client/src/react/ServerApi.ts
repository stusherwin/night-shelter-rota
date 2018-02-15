import { Vol, OvernightPreference, OvernightGenderPreference, Shift, VolShift, ShiftType } from './Types'

type ApiVolunteer = { vId: number
                    , vName: string
                    , vIntro: string
                    , vOvernightPreference: string | null
                    , vOvernightGenderPreference: string | null
                    , vNotes: string
                    }

type ApiShiftDate = { year: number
                    , month: number
                    , day: number
                    }

type ApiVolunteerShift = { vsVolunteer: ApiVolunteer
                         , vsShiftType: string
                         }

type ApiShift = { sDate: ApiShiftDate
                , sVolunteers: ApiVolunteerShift[]
                }

export class ApiError {
  constructor(error: string, message: string) {
    this.error = error
    this.message = message
  }
  error: string
  message: string
}

export class ServerApi {
  static vols(): Promise<Vol[]> {
    return fetch('/api/vols')
      .then(res => {
        if(!res.ok) {
          return res.text().then(txt => { throw new ApiError(`${res.statusText} (${res.status})`, txt) })
        }
        let contentType = res.headers.get('content-type')
        if (contentType == null || !contentType.includes('application/json')) {
          throw new ApiError('Invalid server response', 'Expected response to have content-type application/json.');
        }
        return res.json()
      })
      .then(res => Promise.resolve((res as ApiVolunteer[]).map(toVol)))
  }

  static shifts(): Promise<Shift[]> {
    return fetch('/api/shifts')
      .then(res => {
        if(!res.ok) {
          return res.text().then(txt => { throw new ApiError(`${res.statusText} (${res.status})`, txt) })
        }
        let contentType = res.headers.get('content-type')
        if (contentType == null || !contentType.includes('application/json')) {
          throw new ApiError('Invalid server response', 'Expected response to have content-type application/json.');
        }
        return res.json()
      })
      .then(res => Promise.resolve((res as ApiShift[]).map(s => ({
        date: toDate(s.sDate),
        volunteers: s.sVolunteers.map(toVolShift)
      }))))
  }
}

function toDate(date: ApiShiftDate): Date {
  return new Date(date.year, date.month - 1, date.day)
}

function toVolShift(vs: ApiVolunteerShift): VolShift {
  return {
    volunteer: toVol(vs.vsVolunteer),
    shiftType: constrainShiftType(vs.vsShiftType)
  }
}

function toVol(v: ApiVolunteer): Vol {
  return {
    id: v.vId,
    name: v.vName,
    intro: v.vIntro,
    overnightPreference: constrainOvernightPreference(v.vOvernightPreference),
    overnightGenderPreference: constrainOvernightGenderPreference(v.vOvernightGenderPreference),
    notes: v.vNotes
  }
}

function constrainOvernightPreference(pref: string | null): OvernightPreference | null {
  switch(pref) {
    case 'PreferToBeAlone': return 'PreferToBeAlone';
    case 'PreferAnotherVolunteer': return 'PreferAnotherVolunteer';
    default: return null;
  }
}

function constrainOvernightGenderPreference(pref: string | null): OvernightGenderPreference | null {
  switch(pref) {
    case 'Male': return 'Male';
    case 'Female': return 'Female';
    default: return null;
  }
}

function constrainShiftType(shiftType: string): ShiftType {
  switch(shiftType) {
    case 'Overnight': return 'Overnight';
    default: return 'Evening';
  }
}
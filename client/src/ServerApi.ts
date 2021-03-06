import { Vol, OvernightPreference, OvernightGenderPreference, Shift, VolShift, ShiftType, VolDetails } from './Types'

type ApiVolunteer = { vId: number
                    , vName: string
                    , vIntro: string
                    , vOvernightPreference: string | null
                    , vOvernightGenderPreference: string | null
                    , vNotes: string
                    , vActive: boolean
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

type ApiVolDetails = { vdName: string
                     , vdIntro: string
                     , vdPref: OvernightPreference | null
                     , vdGenderPref: OvernightGenderPreference | null
                     , vdNotes: string
                     }

export class ApiError {
  constructor(error: string, message: string, status: number | null) {
    this.error = error
    this.message = message
    this.status = status
  }

  error: string
  message: string
  status: number | null
}

export class ServerApi {
  static getVols(rotaKey: string | undefined): Promise<Vol[]> {
    const req = new Request(`/api/${rotaKey}/vols`)
    return fetchHttpRequest(req, res => (res as ApiVolunteer[]).map(toVol))
  }

  static putVol(rotaKey: string | undefined, details: VolDetails): Promise<Vol> {
    const req = new Request(`/api/${rotaKey}/vols`,
                              { method: 'PUT'
                              , body: JSON.stringify(fromVolDetails(details))
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => toVol(res as ApiVolunteer))
  }

  static postVol(rotaKey: string | undefined, details: VolDetails, volId: number): Promise<Vol> {
    const req = new Request(`/api/${rotaKey}/vols/${volId}`,
                              { method: 'POST'
                              , body: JSON.stringify(fromVolDetails(details))
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => toVol(res as ApiVolunteer))
  }

  static activateVol(rotaKey: string | undefined, volId: number): Promise<Vol> {
    const req = new Request(`/api/${rotaKey}/vols/active/${volId}`,
                              { method: 'POST'
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => toVol(res as ApiVolunteer))
  }

  static deactivateVol(rotaKey: string | undefined, volId: number): Promise<Vol> {
    const req = new Request(`/api/${rotaKey}/vols/inactive/${volId}`,
                              { method: 'POST'
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => toVol(res as ApiVolunteer))
  }

  static getShifts(rotaKey: string | undefined): Promise<Shift[]> {
    const req = new Request(`/api/${rotaKey}/shifts`)
    return fetchHttpRequest(req, res => (res as ApiShift[]).map(toShift))
  }

  static getVolShifts(rotaKey: string | undefined, date: Date): Promise<VolShift[]> {
    const apiDate = fromDate(date)
    const req = new Request(`/api/${rotaKey}/shifts/${apiDate.year}/${apiDate.month}/${apiDate.day}`)
    return fetchHttpRequest(req, res => (res as ApiVolunteerShift[]).map(toVolShift))
  }

  static putVolShift(rotaKey: string | undefined, shiftType: ShiftType, date: Date, volId: number): Promise<VolShift[]> {
    const apiDate = fromDate(date)
    const req = new Request(`/api/${rotaKey}/shifts/${apiDate.year}/${apiDate.month}/${apiDate.day}/${volId}`,
                              { method: 'PUT'
                              , body: JSON.stringify(shiftType)
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => (res as ApiVolunteerShift[]).map(toVolShift))
  }

  static deleteVolShift(rotaKey: string | undefined, date: Date, volId: number): Promise<VolShift[]> {
    const apiDate = fromDate(date)
    const req = new Request(`/api/${rotaKey}/shifts/${apiDate.year}/${apiDate.month}/${apiDate.day}/${volId}`, 
                              { method: 'DELETE' 
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => (res as ApiVolunteerShift[]).map(toVolShift))
  }

  static postVolShift(rotaKey: string | undefined, shiftType: ShiftType, date: Date, volId: number): Promise<VolShift[]> {
    const apiDate = fromDate(date)
    const req = new Request(`/api/${rotaKey}/shifts/${apiDate.year}/${apiDate.month}/${apiDate.day}/${volId}`,
                              { method: 'POST'
                              , body: JSON.stringify(shiftType)
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => (res as ApiVolunteerShift[]).map(toVolShift))
  }

  static getCurrentVolId(rotaKey: string | undefined): Promise<number | null> {
    const req = new Request(`/api/${rotaKey}/currentvol`)
    return fetchHttpRequest(req, res => res)
  }

  static setCurrentVolId(rotaKey: string | undefined, volId: number): Promise<void> {
    const req = new Request(`/api/${rotaKey}/currentvol/${volId}`,
                              { method: 'POST'
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => {})
  }

  static clearCurrentVolId(rotaKey: string | undefined): Promise<void> {
    const req = new Request(`/api/${rotaKey}/currentvol`,
                              { method: 'POST'
                              , body: ''
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => {})
  }

  static verifyRota(rotaKey: string | undefined): Promise<boolean> {
    const req = new Request(`/api/verify/${rotaKey}`,
                              { method: 'POST'
                              , body: ''
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req, res => res)
  }
}

function fetchHttpRequest<T>(req: Request, process: (res: any) => T): Promise<T> {
  try {
    return fetch(req, {credentials: 'same-origin'})
      .then(res => {
        if(!res.ok) {
          return res.text().then(txt => { throw new ApiError(`${res.statusText} (${res.status})`, txt, res.status) })
        }
        let contentType = res.headers.get('content-type')
        if (contentType == null || !contentType.includes('application/json')) {
          throw new ApiError('Invalid server response', 'Expected response to have content-type application/json.', null);
        }
        return res.json()
      })
      .then(res => Promise.resolve(process(res)))
      .catch(err => Promise.reject(new ApiError('Error from the server', 'Received an unexpected response from the server: ' + err.error, err.status || null)))
  } catch(TypeError) {
    return Promise.reject(new ApiError('Can\'t connect to the server', 'The server seems to be down or busy, please wait a while and try again.', null))
  }
}

function toDate(date: ApiShiftDate): Date {
  return new Date(Date.UTC(date.year, date.month - 1, date.day))
}

function toVolShift(vs: ApiVolunteerShift): VolShift {
  return {
    vol: toVol(vs.vsVolunteer),
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
    notes: v.vNotes,
    active: v.vActive
  }
}

function toShift(s: ApiShift): Shift {
  return {
    date: toDate(s.sDate),
    vols: s.sVolunteers.map(toVolShift),
    loading: false
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

function fromVolDetails(details: VolDetails): ApiVolDetails {
  return {
    vdName: details.name,
    vdIntro: details.intro,
    vdPref: details.pref,
    vdGenderPref: details.genderPref,
    vdNotes: details.notes
  }
}

function fromDate(date: Date): ApiShiftDate {
  return {
    year: date.getUTCFullYear(),
    month: date.getUTCMonth() + 1,
    day: date.getUTCDate()
  }
}
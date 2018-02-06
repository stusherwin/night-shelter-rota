import { Vol, OvernightPreference, OvernightGenderPreference } from './Types'

export type Volunteer = { vId: number
                        , vName: string
                        , vIntro: string
                        , vOvernightPreference: string
                        , vOvernightGenderPreference: string
                        , vNotes: string
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
      .then(res => Promise.resolve((res as Volunteer[]).map(v => ({
        id: v.vId,
        name: v.vName,
        intro: v.vIntro,
        overnightPreference: constrainOvernightPreference(v.vOvernightPreference),
        overnightGenderPreference: constrainOvernightGenderPreference(v.vOvernightGenderPreference),
        notes: v.vNotes
      }))))
  }
}

function constrainOvernightPreference(pref: string): OvernightPreference {
  switch(pref) {
    case '1': return '1';
    case '2': return '2';
    default: return null;
  }
}

function constrainOvernightGenderPreference(pref: string): OvernightGenderPreference {
  switch(pref) {
    case 'M': return 'M';
    case 'F': return 'F';
    default: return null;
  }
}
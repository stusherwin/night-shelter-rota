import { Vol } from './Types'

export type Volunteer = { vId: number
                        , vName: string
                        , vIntro: string
                        , vOvernightPreference: string
                        , vOvernightGenderPreference: string
                        , vNotes: string
                        }

export class ServerAPI {
  static vols(): Promise<Vol[]> {
    return fetch('/api/vols')
      .then(res => {
        if(!res.ok) {
          return res.text().then(txt => { throw new Error(`${res.statusText} (${res.status}): ${txt}`) })
        }
        let contentType = res.headers.get('content-type')
        if (contentType == null) {
          throw new TypeError('Invalid server response (no content-type defined)');
        }
        if (!contentType.includes('application/json')) {
          throw new TypeError('Invalid server response (expected json content-type)');
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

function constrainOvernightPreference(pref: string): '1' | '2' | null {
  switch(pref) {
    case '1': return '1';
    case '2': return '2';
    default: return null;
  }
}

function constrainOvernightGenderPreference(pref: string): 'M' | 'F' | null {
  switch(pref) {
    case 'M': return 'M';
    case 'F': return 'F';
    default: return null;
  }
}
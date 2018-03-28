import { Vol, Shift, VolShift, ShiftType, info } from './Types'
import { Util } from './Util'

export type ShiftRuleConfig = { currentDate: Date
                              , maxVolsPerShift: number
                              , urgentPeriodDays: number
                              }
export type ShiftRuleResultType = 'Error' | 'Warning' | 'Info' | 'Neutral'
export type ShiftRuleResult = { type: ShiftRuleResultType
                              , message: string | null
                              }

type RuleParams = { shift: Shift
                  , config: ShiftRuleConfig
                  }

export class ShiftRules {
  static validateShift(date: Date, vols: VolShift[], config: ShiftRuleConfig): ShiftRuleResult[] {
    let d = Util.dateDiffDays(date, config.currentDate)
    let isPast = d < 0.0
    let isLooming = d >= 0.0 && d < config.urgentPeriodDays

    let results: ShiftRuleResult[] =
      [ must(notHaveSameVolunteerTwice(vols))
      , ignoreIf(isPast, should(notExceedMaxVolunteers(vols, config)))
      , ignoreIf(isPast, mustIf(isLooming, haveAtLeastOneVolunteer(vols)))
      // , ignoreIf (isPast shift) <<< could <<< haveMoreThanOneVolunteer
      , ignoreIf(isPast, mustIf(isLooming, haveAnOvernightVolunteer(vols)))
      , ignoreIf(isPast, should(notViolateAnyVolsSharingPrefs(vols)))
      ]

    let r: ShiftRuleResult[] = results.filter(r => r != null)

    let r2 = r.sort((a, b) => priority(a) - priority(b))
    return r2
  }
}

function priority(result: ShiftRuleResult): number {
  switch(result.type) {
    case 'Error':   return 0
    case 'Warning': return 1
    case 'Info':    return 2
    default: return 3
  }
}

function ignoreIf(condition: boolean, result: ShiftRuleResult): ShiftRuleResult {
  return { type: result.type == 'Neutral' || condition ? 'Neutral' : result.type, message: result.message }
}

function must(error: string | null): ShiftRuleResult {
  return { type: error ? 'Error' : 'Neutral', message: error }
}

function mustIf(condition: boolean, error: string | null): ShiftRuleResult {
  return { type: error && condition ? 'Error' : 'Neutral', message: error }
}

function should(error: string | null): ShiftRuleResult {
  return { type: error ? 'Warning' : 'Neutral', message: error }
}

function could(error: string | null): ShiftRuleResult {
  return { type: error ? 'Info' : 'Neutral', message: error }
}

function notExceedMaxVolunteers(vols: VolShift[], config: ShiftRuleConfig): string | null {
  return vols.length > config.maxVolsPerShift
    ? "has more than " + config.maxVolsPerShift + " volunteers"
    : null
}

function notHaveSameVolunteerTwice(vols: VolShift[]): string | null {
  return Util.dedupeBy(vols, v => v.vol.id).length > vols.length
    ? "has the same volunteer down twice"
    : null
}

function haveAtLeastOneVolunteer(vols: VolShift[]): string | null {
  return vols.length == 0
    ? "has no volunteers"
    : null
}

function haveMoreThanOneVolunteer(vols: VolShift[]): string | null {
  return vols.length == 1
    ? "has only one volunteer"
    : null
}

function haveAnOvernightVolunteer(vols: VolShift[]): string | null {
  return vols.length != 0 && vols.filter(s => s.shiftType == 'Overnight').length == 0
    ? "has no overnight volunteer"
    : null
}

function notViolateAnyVolsSharingPrefs(vols: VolShift[]): string | null {
  let violatesSharingPrefs: (volShift: VolShift) => boolean = (volShift) => {
    let otherVols = vols.filter(v => v.vol.id != volShift.vol.id)

    if(volShift.shiftType == 'Overnight' && volShift.vol.overnightPreference == 'PreferToBeAlone') {
      return !!otherVols.find(v => v.shiftType == 'Overnight')
    }

    if(volShift.shiftType == 'Overnight' && volShift.vol.overnightPreference == 'PreferAnotherVolunteer') {
      return !otherVols.find(v => v.shiftType == 'Overnight')
    }

    return false
  }

  return vols.find(v => violatesSharingPrefs(v))
    ? "goes against a volunteer's preferences"
    : null
}
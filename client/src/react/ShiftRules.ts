import { Vol, Shift, VolShift, ShiftType, info } from './Types'

export type ShiftRuleConfig = { currentDate: Date
                              , maxVolsPerShift: number
                              , urgentPeriodDays: number
                              }
export type ShiftRuleResultType = 'Error' | 'Warning' | 'Info' | 'Neutral'
export type ShiftRuleResult = { type: ShiftRuleResultType
                              , message: string
                              }

export class ShiftRules {
  static validateShift(shift: Shift, config: ShiftRuleConfig): ShiftRuleResult[] {
    return ([{ type: 'Error', message: 'There was error' }])
  }
}
import * as React from 'react';
import { Vol, Shift, VolShift } from './Types'

export interface ShiftRowProps { date: Date
                               , vols: VolShift[]
                               }

export function ShiftRow(props: ShiftRowProps): JSX.Element {
  return <div>{props.date.toDateString()} {props.vols.map(v => v.volunteer.name)}</div>
}
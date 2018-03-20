import * as React from 'react';
import { Vol, Shift, VolShift, ShiftType } from './Types'
import { Util } from './Util'
import { HeaderRow, HeaderRowProps } from './HeaderRow'
import { ShiftRow, ShiftRowProps } from './ShiftRow'
import { ShiftRuleConfig } from './ShiftRules'

export interface RosterProps { visible: boolean
                             , currentVol: Vol | null
                             , shifts: Shift[]
                             , config: ShiftRuleConfig
                             , addCurrentVol: (shiftDate: Date, shiftType: ShiftType) => void
                             , removeCurrentVol: (shiftDate: Date) => void
                             , changeCurrentVolShiftType: (date: Date, shiftType: ShiftType) => void
                             }

export interface RosterState { startDate: Date
                             , endDate: Date
                             }

const SHIFT_COUNT = 28

export class Roster extends React.Component<RosterProps, RosterState> {
  constructor(props: RosterProps) {
    super(props)
    let startDate = Util.previousWeekday(props.config.currentDate, 1)
    this.state = { startDate: startDate
                 , endDate: Util.addDays(startDate, SHIFT_COUNT)
                 }
  }
 
  render() {
    if(!this.props.visible) {
      return null
    }

    return (
      <div className={this.classNames()}>
          {/* Roster today: {this.props.currentDate.toString()}
          start: {this.state.startDate.toString()}
          end: {this.state.endDate.toString()}
          rows: {rows.length} */}
          {this.rows()}
      </div>
    )
  }

  classNames(): string {
    let classNames = ['roster']
    if(this.props.currentVol != null) {
      classNames.push('has-current-vol')
    }
    return classNames.join(' ')
  }

  rows(): JSX.Element[] {
    console.log('rows')
    let rows = []
    let shifts = this.props.shifts.filter(s => s.date >= this.state.startDate && s.date <= this.state.endDate)
                                  .sort((a, b) => a.date.valueOf() - b.date.valueOf())
    let date = this.state.startDate
    let i = 0;

    while(date < this.state.endDate) {
      if(Util.isFirstDayOfMonth(date) || !rows.length) {
        rows.push(
          <HeaderRow showPrev={!rows.length}
                     showNext={false}
                     loadPrevPeriod={this.loadPrevPeriod.bind(this)}
                     loadNextPeriod={this.loadNextPeriod.bind(this)}>
            {Util.monthYearString(date)}
          </HeaderRow>
        )
      }

      let vols = [] as VolShift[]
      let loading = false
      if(shifts.length > i && Util.datesEqual(shifts[i].date, date)) {
        vols = shifts[i].vols
        loading = shifts[i].loading
        i++
      }        

      rows.push(
        <ShiftRow date={date}
                  vols={vols}
                  loading={loading}
                  config={this.props.config}
                  currentVol={this.props.currentVol}
                  addCurrentVol={this.props.addCurrentVol}
                  removeCurrentVol={this.props.removeCurrentVol}
                  changeCurrentVolShiftType={this.props.changeCurrentVolShiftType} />
      )

      date = Util.addDays(date, 1)
    }
    rows.push(
      <HeaderRow showPrev={false}
                 showNext={true}
                 loadPrevPeriod={this.loadPrevPeriod.bind(this)}
                 loadNextPeriod={this.loadNextPeriod.bind(this)} >
      </HeaderRow>
    )
    return rows
  }

  loadPrevPeriod() {
    // if we've already added an extra set of shifts to the roster we don't want to add any more,
    // so just bring the end date back as well
    let daysInRoster = Util.dateDiffDays(this.state.endDate, this.state.startDate)
    // console.log(daysInRoster)
    let endDateDiff = daysInRoster > SHIFT_COUNT ? -SHIFT_COUNT : 0

    this.setState({ startDate: Util.addDays(this.state.startDate, -SHIFT_COUNT)
                  , endDate: Util.addDays(this.state.endDate, endDateDiff)
                  })
  }

  loadNextPeriod() {
    // if we've already added an extra set of shifts to the roster we don't want to add any more,
    // so just bring the start date forward as well
    let daysInRoster = Util.dateDiffDays(this.state.endDate, this.state.startDate)
    // console.log(daysInRoster)
    let startDateDiff = daysInRoster > SHIFT_COUNT ? SHIFT_COUNT : 0

    this.setState({ startDate: Util.addDays(this.state.startDate, startDateDiff)
                  , endDate: Util.addDays(this.state.endDate, SHIFT_COUNT)
                  })
  }
}
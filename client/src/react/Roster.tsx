import * as React from 'react';
import { Vol, Shift, VolShift } from './Types'
import { Util } from './Util'
import { HeaderRow, HeaderRowProps } from './HeaderRow'
import { ShiftRow, ShiftRowProps } from './ShiftRow'

export interface RosterProps { visible: boolean
                             , currentVol: Vol | null
                             , shifts: Shift[]
                             , currentDate: Date
                             }

export interface RosterState { startDate: Date
                             , endDate: Date
                             }

const SHIFT_COUNT = 28

export class Roster extends React.Component<RosterProps, RosterState> {
  constructor(props: RosterProps) {
    super(props)
    let startDate = Util.datePart(props.currentDate)
    this.state = { startDate: startDate
                 , endDate: Util.addDays(startDate, SHIFT_COUNT)
                 }
  }
 
  render() {
    if(!this.props.visible) {
      return null
    }

    return <div className={this.classNames()}>
               {/* Roster today: {this.props.currentDate.toString()}
               start: {this.state.startDate.toString()}
               end: {this.state.endDate.toString()}
               rows: {rows.length} */}
               {this.rows()}
           </div>
  }

  classNames(): string {
    let classNames = ['roster']
    if(this.props.currentVol != null) {
      classNames.push('has-current-vol')
    }
    return classNames.join(' ')
  }

  rows(): JSX.Element[] {
    let rows = []
    let shifts = this.props.shifts.filter(s => s.date >= this.state.startDate && s.date <= this.state.endDate)
                                  .sort((a, b) => a.date.valueOf() - b.date.valueOf())
    let date = this.state.startDate
    let i = 0;
    while(date < this.state.endDate) {
      if(Util.isFirstDayOfMonth(date) || !rows.length) {
        rows.push(<HeaderRow showPrev={!rows.length}
                             showNext={false}
                             loadPrevPeriod={this.loadPrevPeriod.bind(this)}
                             loadNextPeriod={this.loadNextPeriod.bind(this)}>
                    {Util.monthYearString(date)}
                  </HeaderRow>)
      }

      if(shifts.length > i && Util.datesEqual(shifts[i].date, date)) {
        rows.push(<ShiftRow date={date} vols={shifts[i].volunteers} />)
        i++
      } else {
        rows.push(<ShiftRow date={date} vols={[]} />)
      }

      date = Util.addDays(date, 1)
    }
    rows.push(<HeaderRow showPrev={false}
                         showNext={true}
                         loadPrevPeriod={this.loadPrevPeriod.bind(this)}
                         loadNextPeriod={this.loadNextPeriod.bind(this)} >
              </HeaderRow>)
    return rows
  }

  loadPrevPeriod() {
    // if we've already added an extra set of shifts to the roster we don't want to add any more,
    // so just bring the end date back as well
    let daysInRoster = Util.dateDiffDays(this.state.endDate, this.state.startDate)
    console.log(daysInRoster)
    let endDateDiff = daysInRoster > SHIFT_COUNT ? -SHIFT_COUNT : 0

    this.setState({ startDate: Util.addDays(this.state.startDate, -SHIFT_COUNT)
                  , endDate: Util.addDays(this.state.endDate, endDateDiff)
                  })
  }

  loadNextPeriod() {
    // if we've already added an extra set of shifts to the roster we don't want to add any more,
    // so just bring the start date forward as well
    let daysInRoster = Util.dateDiffDays(this.state.endDate, this.state.startDate)
    console.log(daysInRoster)
    let startDateDiff = daysInRoster > SHIFT_COUNT ? SHIFT_COUNT : 0

    this.setState({ startDate: Util.addDays(this.state.startDate, startDateDiff)
                  , endDate: Util.addDays(this.state.endDate, SHIFT_COUNT)
                  })
  }
}
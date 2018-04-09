import * as React from 'react';
import { Vol, Shift, VolShift, ShiftType } from './Types'
import { Util } from './Util'
import { HeaderRow, HeaderRowProps } from './HeaderRow'
import { ShiftRow, ShiftRowProps } from './ShiftRow'
import { ShiftRuleConfig } from './ShiftRules'
import { ServerApi, ApiError } from './ServerApi'

export interface RosterProps { visible: boolean
                             , currentVol: Vol | null
                             , shifts: Shift[]
                             , config: ShiftRuleConfig
                             , apiRequest: (req: Promise<any>) => void
                             , updateShifts: (date: Date, vols: VolShift[]) => void
                             , showVolInfo: (vol: Vol) => void
                             }

export interface RosterState { startDate: Date
                             , endDate: Date
                             , fixedMessage: Date | null
                             }

const SHIFT_COUNT = 28

export class Roster extends React.Component<RosterProps, RosterState> {
  constructor(props: RosterProps) {
    super(props)
    let startDate = Util.previousWeekday(props.config.currentDate, 1)
    this.state = { startDate: startDate
                 , endDate: Util.addDays(startDate, SHIFT_COUNT)
                 , fixedMessage: null
                 }
  }
 
  render() {
    if(!this.props.visible) {
      return null
    }

    return (
      <div className={this.classNames()}>
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
                     loadNextPeriod={this.loadNextPeriod.bind(this)}
                     key={`header-${Util.toDateString(date)}`}>
            {Util.monthYearString(date)}
          </HeaderRow>
        )
      }

      let vols = [] as VolShift[]
      if(shifts.length > i && Util.datesEqual(shifts[i].date, date)) {
        vols = shifts[i].vols
        i++
      }     
      
      rows.push(
        <ShiftRow date={date}
                  vols={vols}
                  config={this.props.config}
                  currentVol={this.props.currentVol}
                  apiRequest={this.props.apiRequest}
                  updateShifts={this.props.updateShifts}
                  showVolInfo={this.props.showVolInfo}
                  otherFixedMessage={!!this.state.fixedMessage && !Util.datesEqual(this.state.fixedMessage, date)}
                  messageFixedStateChanged={this.messageFixedStateChanged.bind(this)}
                  key={`shift-${Util.toDateString(date)}`} />
      )

      date = Util.addDays(date, 1)
    }
    rows.push(
      <HeaderRow showPrev={false}
                 showNext={true}
                 loadPrevPeriod={this.loadPrevPeriod.bind(this)}
                 loadNextPeriod={this.loadNextPeriod.bind(this)}
                 key="header-end" >
      </HeaderRow>
    )
    return rows
  }

  messageFixedStateChanged(date: Date, fixed: boolean) {
    if(fixed) {
      this.setState({fixedMessage: date})
    } else if(this.state.fixedMessage && Util.datesEqual(this.state.fixedMessage, date)) {
      this.setState({fixedMessage: null})
    }
  }

  loadPrevPeriod() {
    // if we've already added an extra set of shifts to the roster we don't want to add any more,
    // so just bring the end date back as well
    let daysInRoster = Util.dateDiffDays(this.state.endDate, this.state.startDate)
    let endDateDiff = daysInRoster > SHIFT_COUNT ? -SHIFT_COUNT : 0

    this.setState({ startDate: Util.addDays(this.state.startDate, -SHIFT_COUNT)
                  , endDate: Util.addDays(this.state.endDate, endDateDiff)
                  })
  }

  loadNextPeriod() {
    // if we've already added an extra set of shifts to the roster we don't want to add any more,
    // so just bring the start date forward as well
    let daysInRoster = Util.dateDiffDays(this.state.endDate, this.state.startDate)
    let startDateDiff = daysInRoster > SHIFT_COUNT ? SHIFT_COUNT : 0

    this.setState({ startDate: Util.addDays(this.state.startDate, startDateDiff)
                  , endDate: Util.addDays(this.state.endDate, SHIFT_COUNT)
                  })
  }
}
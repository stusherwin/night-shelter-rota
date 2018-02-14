import * as React from 'react';
import { Vol, Shift, VolShift } from './Types'
import { Util } from './Util'

export interface ShiftRowProps { date: Date
                               , currentDate: Date
                               , currentVol: Vol | null
                               , vols: VolShift[]
                               }

export interface CurrentVolSignedUpState { shiftType: string | null
                                         }

export interface ShiftRowState { loading: boolean
                               , status: 'Error' | 'Warning' | null
                               , currentVolSignedUp: CurrentVolSignedUpState | null
                               }

export class ShiftRow extends React.Component<ShiftRowProps, ShiftRowState> {
  constructor(props: ShiftRowProps) {
    super(props)
    let startDate = Util.datePart(props.currentDate)
    this.state = { loading: false
                 , status: null
                 , currentVolSignedUp: null
                 }
  }

  render() {
    return (
      <div className={this.classNames()}
           id={`shift-row-${this.props.date}`}>
         <div className="shift-info"> {/*hasMessage state.status */}
              {/* , RP.onClick \e -> do
                     _ <- R.preventDefault e
                     _ <- dispatch $ MessageBubbleAction ToggleFixed
                     R.stopPropagation e
                  , RP.onMouseOver $ const $ dispatch $ MessageBubbleAction ShowTransitory
                  , RP.onMouseOut $ const $ dispatch $ MessageBubbleAction HideTransitory */}
            <div className="row-item shift-date">
              <div className="shift-date-part shift-date-day collapsing">
                 {Util.weekdayName(this.props.date).substr(0, 3).toUpperCase()}
               </div>
               <div className="shift-date-part shift-date-month collapsing">
                 {Util.monthName(this.props.date).substr(0, 3).toUpperCase()}
               </div>
               <div className="shift-date-part shift-date-date collapsing">
                 {Util.day(this.props.date)}
                 <span className="shift-date-postfix">
                   {Util.positionalPostfix(Util.day(this.props.date))}
                 </span>
               </div>
             </div>
             <div className="row-item shift-status">
               <div className="shift-status-part shift-status-vol-count collapsing">
                                        {/* [ RD.text $ "" <> show state.noOfVols <> "/" <> show state.maxVols ] */}
               </div>
               <div className="shift-status-part shift-status-icon collapsing">
                                      {/* $ statusIcon state */}
               </div>
             </div>
         </div>
        {this.props.date.toDateString()}
        {this.props.vols.map(v => v.volunteer.name)}
      </div>
    )
  }

  classNames() {
    let classNames = ['row shift-row']

    if(!this.state.loading && this.state.currentVolSignedUp != null && this.state.currentVolSignedUp.shiftType == null) {
      classNames.push('clickable')
    }

    if(Util.isWeekend(this.props.date)) {
      classNames.push('weekend')
    }

    if(this.state.loading) {
      classNames.push('loading')
    }

    if(this.props.date < this.props.currentDate) {
      console.log(this.props.date)
      console.log(this.props.currentDate)
      classNames.push('past')
    }

    if(Util.datesEqual(this.props.date, this.props.currentDate)) {
      classNames.push('today')
    }

    if(this.state.status == 'Error') {
      classNames.push('negative1')
    } else if(this.state.status == 'Warning') {
      classNames.push('warning1')
    }
    
    return classNames.join(' ')
  }
}
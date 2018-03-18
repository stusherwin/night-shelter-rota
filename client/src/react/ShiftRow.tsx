import * as React from 'react';
import { Vol, Shift, VolShift, ShiftType, info } from './Types'
import { Util } from './Util'
import { ShiftRules, ShiftRuleConfig, ShiftRuleResultType } from './ShiftRules'

export interface ShiftRowProps { shift: Shift
                               , currentVol: Vol | null
                               , config: ShiftRuleConfig
                               }

export interface CurrentVolSignedUpState { shiftType: string | null
                                         }

export interface ShiftRowState { loading: boolean
                               , status: ShiftRuleResultType
                               , currentVolSignedUp: CurrentVolSignedUpState | null
                               }

export class ShiftRow extends React.Component<ShiftRowProps, ShiftRowState> {
  constructor(props: ShiftRowProps) {
    super(props)
    
    let results = ShiftRules.validateShift(props.shift, props.config)
    
    this.state = { loading: false
                 , status: results[0].type
                 , currentVolSignedUp: null
                 }
  }

  render() {
    return (
      <div className={this.classNames()}
           id={`shift-row-${this.props.shift.date}`}>
         <div className="shift-info"> {/*hasMessage state.status */}
              {/* , RP.onClick \e -> do
                     _ <- R.preventDefault e
                     _ <- dispatch $ MessageBubbleAction ToggleFixed
                     R.stopPropagation e
                  , RP.onMouseOver $ const $ dispatch $ MessageBubbleAction ShowTransitory
                  , RP.onMouseOut $ const $ dispatch $ MessageBubbleAction HideTransitory */}
             <ShiftDate date={this.props.shift.date} />
             <ShiftStatus noOfVols={this.props.shift.vols.length}
                          status={this.state.status}
                          date={this.props.shift.date}
                          config={this.props.config} />
             <CurrentVolSignUp />
             <VolMarkers vols={this.props.shift.vols} />
         </div>
      </div>
    )
  }

  classNames() {
    let classNames = ['row shift-row']

    if(!this.state.loading && this.state.currentVolSignedUp != null && this.state.currentVolSignedUp.shiftType == null) {
      classNames.push('clickable')
    }

    if(Util.isWeekend(this.props.shift.date)) {
      classNames.push('weekend')
    }

    if(this.state.loading) {
      classNames.push('loading')
    }

    if(this.props.shift.date < this.props.config.currentDate) {
      console.log(this.props.shift.date)
      console.log(this.props.config.currentDate)
      classNames.push('past')
    } else {
      if(Util.datesEqual(this.props.shift.date, this.props.config.currentDate)) {
        classNames.push('today')
      }
    
      if(this.state.status == 'Error') {
        classNames.push('negative1')
      } else if(this.state.status == 'Warning') {
        classNames.push('warning1')
      }
    }

    return classNames.join(' ')
  }
}

function ShiftDate(props: {date: Date}): JSX.Element {
  return (
    <div className="row-item shift-date">
      <div className="shift-date-part shift-date-day collapsing">
        {Util.weekdayName(props.date).substr(0, 3).toUpperCase()}
      </div>
      <div className="shift-date-part shift-date-month collapsing">
        {Util.monthName(props.date).substr(0, 3).toUpperCase()}
      </div>
      <div className="shift-date-part shift-date-date collapsing">
        {Util.day(props.date)}
        <span className="shift-date-postfix">
          {Util.positionalPostfix(Util.day(props.date))}
        </span>
      </div>
    </div>
  )
}

function ShiftStatus(props: {date: Date, noOfVols: number, status: ShiftRuleResultType, config: ShiftRuleConfig}): JSX.Element {
  return (
    <div className="row-item shift-status">
      <div className="shift-status-part shift-status-vol-count collapsing">
        {props.noOfVols}/{props.config.maxVolsPerShift}
      </div>
      <div className="shift-status-part shift-status-icon collapsing">
         <ShiftStatusIcon {...props} />
      </div>
    </div>
  )
}

function ShiftStatusIcon(props: {date: Date, status: ShiftRuleResultType, config: ShiftRuleConfig}): JSX.Element | null {
  if(props.date < props.config.currentDate) {
    return <i className="icon-clock"></i>
  }

  switch(props.status) {
    case 'Error':   return <i className="icon-warning"></i>
    case 'Warning': return <i className="icon-info"></i>
    case 'Info':    return <i className="icon-info"></i>
  }

  return null
}

function CurrentVolSignUp(props: {}): JSX.Element {
  return (
    <div className="row-item current-vol collapsing right aligned">
                    {/* $ renderCurrentVol state.currentVol */}
    </div>
  )
}

function VolMarkers(props: {vols: VolShift[]}): JSX.Element {
  return (
    <div className="row-item vol-markers collapsing">
      {props.vols.map(v => <VolMarker volShift={v} />)}
    </div>
  )
}

function VolMarker(props: {volShift: VolShift}): JSX.Element {
  return (
    <span className="vol-marker">
      {props.volShift.volunteer.notes.length
        ? <span className="sharing-pref icon">
            <i className="icon-info"></i>&nbsp;
          </span>
        : null}
      {props.volShift.volunteer.overnightGenderPreference
        ? <span className="sharing-pref gender">
            <span>{info(props.volShift.volunteer.overnightGenderPreference).marker}</span>
          </span>
        : null}
      {props.volShift.volunteer.overnightPreference
        ? <span className="sharing-pref alone">
            <span>{info(props.volShift.volunteer.overnightPreference).marker}</span>
          </span>
        : null}
      <span className="vol-name"
                      // , RP.onClick \e -> do
                      //     _ <- R.preventDefault e
                      //     _ <- dispatch $ ShowVolInfo s.volunteer
                      //     R.stopPropagation e
                      >
        <ShiftTypeIcon shiftType={props.volShift.shiftType} />
        {props.volShift.volunteer.name}
      </span>
    </span>
  )
}

function ShiftTypeIcon(props: {shiftType: ShiftType}): JSX.Element {
  switch(props.shiftType) {
    case 'Evening':
      return <i className="vol-icon icon-no-bed"></i>
    case 'Overnight':
    default:
      return <i className="vol-icon icon-bed"></i>
  }
}
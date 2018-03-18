import * as React from 'react';
import { Vol, Shift, VolShift, ShiftType, info } from './Types'
import { Util } from './Util'
import { ShiftRules, ShiftRuleConfig, ShiftRuleResult, ShiftRuleResultType } from './ShiftRules'

export interface ShiftRowProps { shift: Shift
                               , currentVol: Vol | null
                               , config: ShiftRuleConfig
                               , addCurrentVol: (shiftDate: Date, shiftType: ShiftType) => void
                               , removeCurrentVol: (shiftDate: Date) => void
                               }

export interface CurrentVolSignedUpState { shiftType: string | null
                                         }

export interface ShiftRowState { loading: boolean
                               , ruleResult: ShiftRuleResult
                               , currentVolSignedUp: CurrentVolSignedUpState | null
                               }

export class ShiftRow extends React.Component<ShiftRowProps, ShiftRowState> {
  constructor(props: ShiftRowProps) {
    super(props)
    
    let results = ShiftRules.validateShift(props.shift, props.config)
    
    this.state = { loading: false
                 , ruleResult: results[0]
                 , currentVolSignedUp: null
                 }
  }

  render() {
    return (
      <div className={this.classNames()}
           id={`shift-row-${this.props.shift.date}`}>
         <ShiftInfo shift={this.props.shift}
                    config={this.props.config}
                    ruleResult={this.state.ruleResult} />
         <CurrentVolSignUp shift={this.props.shift}
                           currentVol={this.props.currentVol}
                           loading={this.state.loading}
                           addCurrentVol={this.props.addCurrentVol}
                           removeCurrentVol={this.props.removeCurrentVol} />
         <VolMarkers vols={this.props.shift.vols} />
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
    
      if(this.state.ruleResult.type == 'Error') {
        classNames.push('negative1')
      } else if(this.state.ruleResult.type == 'Warning') {
        classNames.push('warning1')
      }
    }

    return classNames.join(' ')
  }
}

export class ShiftInfo extends React.Component<{ shift: Shift
                                               , config: ShiftRuleConfig
                                               , ruleResult: ShiftRuleResult
                                               }, {}> {
  render() {
    return (
      <div className={this.classNames()}>
            {/* , RP.onClick \e -> do
                   _ <- R.preventDefault e
                   _ <- dispatch $ MessageBubbleAction ToggleFixed
                   R.stopPropagation e
                , RP.onMouseOver $ const $ dispatch $ MessageBubbleAction ShowTransitory
                , RP.onMouseOut $ const $ dispatch $ MessageBubbleAction HideTransitory */}
           <ShiftDate date={this.props.shift.date} />
           <ShiftStatus noOfVols={this.props.shift.vols.length}
                        ruleResult={this.props.ruleResult}
                        date={this.props.shift.date}
                        config={this.props.config} />
         </div>
    )
  }

  classNames(): string {
    let classNames = ['shift-info']

    if(this.props.ruleResult.message) {
      classNames.push('has-message')
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

function ShiftStatus(props: { date: Date
                            , noOfVols: number
                            , ruleResult: ShiftRuleResult
                            , config: ShiftRuleConfig
                            }): JSX.Element {
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

function ShiftStatusIcon(props: { date: Date
                                , ruleResult: ShiftRuleResult
                                , config: ShiftRuleConfig
                                }): JSX.Element | null {
  if(props.date < props.config.currentDate) {
    return <i className="icon-clock"></i>
  }

  switch(props.ruleResult.type) {
    case 'Error':   return <i className="icon-warning"></i>
    case 'Warning': return <i className="icon-info"></i>
    case 'Info':    return <i className="icon-info"></i>
  }

  return null
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

function CurrentVolSignUp(props: { shift: Shift
                                 , currentVol: Vol | null
                                 , loading: boolean
                                 , addCurrentVol: (shiftDate: Date, shiftType: ShiftType) => void
                                 , removeCurrentVol: (shiftDate: Date) => void
                                 }): JSX.Element {
  if(props.currentVol == null) {
    return (
      <div className="row-item current-vol collapsing right aligned">
      </div>
    )
  }

  return (
    <div className="row-item current-vol collapsing right aligned">
      <CurrentVolSelected shift={props.shift} currentVol={props.currentVol} loading={props.loading} addCurrentVol={props.addCurrentVol} removeCurrentVol={props.removeCurrentVol} />
      <CurrentVolShiftType shift={props.shift} currentVol={props.currentVol} loading={props.loading} />
    </div>
  )
}

function CurrentVolSelected(props: { shift: Shift
                                   , currentVol: Vol
                                   , loading: boolean
                                   , addCurrentVol: (shiftDate: Date, shiftType: ShiftType) => void
                                   , removeCurrentVol: (shiftDate: Date) => void
                                   }): JSX.Element | null {
  if(props.loading) {
    return <i className="icon-spin animate-spin loading"></i>
  }

  let checked = false
  let onChange = () => props.addCurrentVol(props.shift.date, 'Overnight')
  let disabled = props.loading
 
  if(props.currentVol && props.shift.vols.find(s => s.volunteer.id == props.currentVol.id)) {
    checked = true
    onChange = () => props.removeCurrentVol(props.shift.date)
    disabled = disabled /* || ShiftRules.canAddVolunteer(shift, )
    , canAddOvernight: canAddVolunteer config { shiftType: Overnight, volunteer: cv} shift
                       , canAddEvening: canAddVolunteer config { shiftType: Evening, volunteer: cv} shift
                       */
  }

  return (
    <span className="current-vol-selected ui fitted checkbox">
      <input type="checkbox"
             checked={checked}
             disabled={disabled}
             onChange={e => {onChange();}} />
      <label></label>
    </span>
  )
}

function CurrentVolShiftType(props: {shift: Shift, currentVol: Vol | null, loading: boolean}): JSX.Element | null {
  return null
}
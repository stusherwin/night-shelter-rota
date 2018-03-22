import * as React from 'react';
import { Vol, Shift, VolShift, ShiftType, info } from './Types'
import { Util } from './Util'
import { ShiftRules, ShiftRuleConfig, ShiftRuleResult, ShiftRuleResultType } from './ShiftRules'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubble, MessageBubbleProps, MessageBubbleAction, Message, MessageBubblePosition } from './MessageBubble'

export interface ShiftRowProps { date: Date
                               , vols: VolShift[]
                               , currentVol: Vol | null
                               , config: ShiftRuleConfig
                               , requestStarted: () => void
                               , requestFailed: (error: ApiError) => void
                               , requestSucceeded: () => void
                               , updateShifts: (date: Date, vols: VolShift[]) => void
                               }

export interface CurrentVolSignedUpState { shiftType: string | null
                                         }

export interface ShiftRowState { ruleResult: ShiftRuleResult
                               , currentVolSignedUp: CurrentVolSignedUpState | null
                               , loading: boolean
                               , messageBubble: MessageBubbleProps
                               }

export class ShiftRow extends React.Component<ShiftRowProps, ShiftRowState> {
  constructor(props: ShiftRowProps) {
    super(props)
    
    let results = ShiftRules.validateShift(props.date, props.vols, props.config)
    let resultsWithMessages = results.filter(r => r.message && r.message.length)

    let message = { header: null
                  , body: ''
                  , position: 'Over'
                  , icon: null
                  } as Message

    if(this.props.date < this.props.config.currentDate) {
      message.body = 'This shift is in the past'
      message.icon = 'clock'
    } else if(resultsWithMessages.length) {
      switch(results[0].type) {
        case 'Error':
          message.icon = 'warning'
          break
        case 'Warning':
        case 'Info':
          message.icon = 'info'
      }

      for(let r of resultsWithMessages) {
        if(!message.body.length) {
          message.body = 'This shift ' + r.message
        } else {
          message.body += ', and also ' + r.message
        }
      }
    }
    
    this.state = { ruleResult: results[0]
                 , currentVolSignedUp: null
                 , loading: false
                 , messageBubble: new MessageBubbleProps(message)
                 }
  }

  render() {
    return (
      <div className={this.classNames()}
           id={`shift-row-${this.props.date}`}>
         <ShiftInfo date={this.props.date}
                    vols={this.props.vols}
                    config={this.props.config}
                    ruleResult={this.state.ruleResult}
                    messageBubble={this.state.messageBubble}
                    messageBubbleAction={this.messageBubbleAction.bind(this)} />
         <CurrentVolSignUp date={this.props.date}
                           vols={this.props.vols}
                           currentVol={this.props.currentVol}
                           loading={this.state.loading}
                           addCurrentVol={this.addCurrentVol.bind(this)}
                           removeCurrentVol={this.removeCurrentVol.bind(this)}
                           changeCurrentVolShiftType={this.changeCurrentVolShiftType.bind(this)} />
         <VolMarkers vols={this.props.vols} />
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

    if(this.props.date < this.props.config.currentDate) {
      classNames.push('past')
    } else {
      if(Util.datesEqual(this.props.date, this.props.config.currentDate)) {
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

  addCurrentVol(shiftType: ShiftType) {
    console.log('addCurrentVol')
    
    if(!this.props.currentVol) {
      return
    }

    this.props.requestStarted();
    this.setState({ loading: true })
    ServerApi.putVolShift(shiftType, this.props.date, this.props.currentVol.id)
      .then(volShifts => {
        this.props.updateShifts(this.props.date, volShifts);
        this.props.requestSucceeded();
        this.setState({ loading: false })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.props.requestFailed(apiError);
        this.setState({ loading: false })
      })
  }

  removeCurrentVol() {
    console.log('removeCurrentVol')
    
    if(!this.props.currentVol) {
      return
    }

    this.props.requestStarted();
    this.setState({ loading: true })
    ServerApi.deleteVolShift(this.props.date, this.props.currentVol.id)
      .then(volShifts => {
        this.props.updateShifts(this.props.date, volShifts);
        this.props.requestSucceeded();
        this.setState({ loading: false })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.props.requestFailed(apiError);
        this.setState({ loading: false })
      })
  }

  changeCurrentVolShiftType(shiftType: ShiftType) {
    console.log('change current vol shift type')
    
    if(!this.props.currentVol) {
      return
    }

    this.props.requestStarted();
    this.setState({ loading: true })
    ServerApi.postVolShift(shiftType, this.props.date, this.props.currentVol.id)
      .then(volShifts => {
        this.props.updateShifts(this.props.date, volShifts);
        this.props.requestSucceeded();
        this.setState({ loading: false })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.props.requestFailed(apiError);
        this.setState({ loading: false })
      })
  }
  
  messageBubbleAction(action: MessageBubbleAction) {
    console.log('messageBubbleAction: ' + action)
    this.setState({ messageBubble: this.state.messageBubble.afterAction(action)
                  })
  }
}

export class ShiftInfo extends React.Component<{ date: Date
                                               , vols: VolShift[]
                                               , config: ShiftRuleConfig
                                               , ruleResult: ShiftRuleResult
                                               , messageBubble: MessageBubbleProps
                                               , messageBubbleAction: (action: MessageBubbleAction) => void
                                               }, {}> {
  render() {
    return (
      <div className={this.classNames()}
           onClick={e => { e.preventDefault(); this.props.messageBubbleAction('ToggleFixed'); e.stopPropagation() }}
           onMouseOver={e => this.props.messageBubbleAction('ShowTransitory')}
           onMouseOut={e => this.props.messageBubbleAction('HideTransitory')} >
        <ShiftDate date={this.props.date} />
        <ShiftStatus noOfVols={this.props.vols.length}
                     ruleResult={this.props.ruleResult}
                     date={this.props.date}
                     config={this.props.config} />
        <MessageBubble {...this.props.messageBubble}
                       action={this.props.messageBubbleAction}>
        </MessageBubble>
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

function CurrentVolSignUp(props: { date: Date
                                 , vols: VolShift[]
                                 , currentVol: Vol | null
                                 , loading: boolean
                                 , addCurrentVol: (shiftType: ShiftType) => void
                                 , removeCurrentVol: () => void
                                 , changeCurrentVolShiftType: (shiftType: ShiftType) => void
                                 }): JSX.Element {
  if(props.currentVol == null) {
    return (
      <div className="row-item current-vol collapsing right aligned">
      </div>
    )
  }

  return (
    <div className="row-item current-vol collapsing right aligned">
      <CurrentVolSelected vols={props.vols}
                          currentVol={props.currentVol}
                          loading={props.loading}
                          addCurrentVol={props.addCurrentVol}
                          removeCurrentVol={props.removeCurrentVol} />
      <CurrentVolShiftType date={props.date}
                           vols={props.vols}
                           currentVol={props.currentVol}
                           loading={props.loading}
                           changeCurrentVolShiftType={props.changeCurrentVolShiftType} />
    </div>
  )
}

function CurrentVolSelected(props: { vols: VolShift[]
                                   , currentVol: Vol
                                   , loading: boolean
                                   , addCurrentVol: (shiftType: ShiftType) => void
                                   , removeCurrentVol: () => void
                                   }): JSX.Element | null {
  if(props.loading) {
    return <i className="icon-spin animate-spin loading"></i>
  }

  let checked = false
  let onChange = () => props.addCurrentVol('Overnight')
  let disabled = props.loading
 
  if(props.currentVol && props.vols.find(s => s.volunteer.id == props.currentVol.id)) {
    checked = true
    onChange = () => props.removeCurrentVol()
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
             onClick={e => {e.stopPropagation()}}
             onChange={e => {e.preventDefault(); onChange(); e.stopPropagation()}} />
      <label></label>
    </span>
  )
}

function CurrentVolShiftType(props: { date: Date
                                    , vols: VolShift[]
                                    , currentVol: Vol | null
                                    , loading: boolean
                                    , changeCurrentVolShiftType: (shiftType: ShiftType) => void
                                    }): JSX.Element | null {
  if(!props.currentVol) {
    return null
  }
  
  let currentVolId = props.currentVol.id
  let volShift = props.vols.find(s => s.volunteer.id == currentVolId)
  
  if(!volShift) {
    return null
  }

  let st = volShift.shiftType
  
  return (
    <span>
      <span className="current-vol-shift-type radio media-large-screen media-larger-screen">
        <ShiftTypeRadio shiftType="Overnight"
                        currentShiftType={st}
                        date={props.date}
                        changeCurrentVolShiftType={props.changeCurrentVolShiftType} />
        <ShiftTypeRadio shiftType="Evening"
                        currentShiftType={st}
                        date={props.date}
                        changeCurrentVolShiftType={props.changeCurrentVolShiftType} />
      </span>
      <span className="current-vol-shift-type toggle media-medium-screen media-small-screen">
        <div className="ui toggle checkbox">
          <input tabIndex={0}
                 className="hidden"
                 type="checkbox"
                 id={`shift-type-${Util.toDateString(props.date)}`}
                 checked={st == 'Overnight'} 
                 onClick={e => e.stopPropagation()}
                 onChange={e => { e.preventDefault(); props.changeCurrentVolShiftType(otherShiftType(st)); e.stopPropagation() }} />
          <label htmlFor={`shift-type-${Util.toDateString(props.date)}`}></label>
          <span className="current-vol-shift-type-toggle-description">
            {st}
          </span>
        </div>
      </span>
    </span>
  )
}

function otherShiftType(shiftType: ShiftType) {
  if(shiftType == 'Evening') {
    return 'Overnight'
  } else {
    return 'Evening'
  }
}

function ShiftTypeRadio(props: { shiftType: ShiftType
                               , currentShiftType: ShiftType
                               , date: Date
                               , changeCurrentVolShiftType: (shiftType: ShiftType) => void
                               }): JSX.Element {
  return (
    <span className="current-vol-shift-type-option">
      <input type="radio"
             id={`shift-type-${Util.toDateString(props.date)}-${props.shiftType.toLowerCase()}`}
             name={`shift-type-${Util.toDateString(props.date)}`}
             checked={props.currentShiftType == props.shiftType}
             onClick={e => {e.stopPropagation()}}
             onChange={e => {e.preventDefault(); props.changeCurrentVolShiftType(props.shiftType); e.stopPropagation()}} />
      <label className="action-label"
             htmlFor={`shift-type-${Util.toDateString(props.date)}-${props.shiftType.toLowerCase()}`}>
        <ShiftTypeIcon shiftType={props.shiftType} />
        {props.shiftType}
      </label>
    </span>
  )
}
import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, ShiftType, VolShift, Shift } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { Util } from './Util'
import { ShiftRuleConfig } from './ShiftRules'

export interface ShelterRotaProps {}
export interface ShelterRotaState { initialDataLoaded: boolean
                                  , vols: Vol[]
                                  , shifts: Shift[]
                                  , currentVol: Vol | null
                                  , config: ShiftRuleConfig
                                  , rosterVisible: boolean
                                  , reqInProgress: boolean
                                  , error: ApiError | null
                                  }

export class ShelterRota extends React.Component<ShelterRotaProps, ShelterRotaState> {
  constructor(props: ShelterRotaProps) {
    super(props)
    this.state = { initialDataLoaded: false
                 , vols: []
                 , shifts: []
                 , currentVol: null
                 , config: { maxVolsPerShift: 2
                           , currentDate: Util.today()
                           , urgentPeriodDays: 14
                           }
                 , rosterVisible: false
                 , reqInProgress: true
                 , error: null
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.getVols(), ServerApi.getShifts()])
      .then(results => {
        console.log(results);
        this.setState({ reqInProgress: false
                      , vols: results[0]
                      , initialDataLoaded: true
                      , rosterVisible: true
                      , shifts: results[1]
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ reqInProgress: false
                      , error: apiError
                      })
      })
  }

  render() {
    return (
      <div>
        <Header reqInProgress={this.state.reqInProgress}
                initialDataLoaded={this.state.initialDataLoaded}
                vols={this.state.vols}
                error={this.state.error}
                changeCurrentVol={this.changeCurrentVol.bind(this)}
                editNewVol={this.editNewVol.bind(this)}
                editCurrentVol={this.editCurrentVol.bind(this)} />
        <div className="container">
          <Roster visible={this.state.rosterVisible}
                  currentVol={this.state.currentVol}
                  shifts={this.state.shifts}
                  config={this.state.config}
                  addCurrentVol={this.addCurrentVol.bind(this)}
                  removeCurrentVol={this.removeCurrentVol.bind(this)}
                  changeCurrentVolShiftType={this.changeCurrentVolShiftType.bind(this)} />
        </div>
      </div>
    )
  }

  changeCurrentVol(vol: Vol | null) {
    console.log('current vol: ' + (vol? vol.name : 'none'))
    this.setState({ currentVol: vol
                  })
  }

  editNewVol() {
    console.log('edit new vol')
  }

  editCurrentVol() {
    console.log('edit current vol')
  }

  addCurrentVol(shiftDate: Date, shiftType: ShiftType) {
    console.log('addCurrentVol')
    
    if(!this.state.currentVol) {
      return
    }

    this.setState({ reqInProgress: true
                  , error: null
                  , shifts: this.setShiftLoading(shiftDate, this.state.shifts)
                  })
    ServerApi.putVolShift(shiftType, shiftDate, this.state.currentVol.id)
      .then(volShifts => {
        this.setState({ reqInProgress: false
                      , shifts: this.addOrUpdateShift(shiftDate, volShifts, this.state.shifts)
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ reqInProgress: false
                      , error: apiError
                      })
      })
  }

  removeCurrentVol(shiftDate: Date) {
    console.log('removeCurrentVol')
    
    if(!this.state.currentVol) {
      return
    }

    this.setState({ reqInProgress: true
                  , error: null
                  , shifts: this.setShiftLoading(shiftDate, this.state.shifts)
                  })
    ServerApi.deleteVolShift(shiftDate, this.state.currentVol.id)
      .then(volShifts => {
        this.setState({ reqInProgress: false
                      , shifts: this.addOrUpdateShift(shiftDate, volShifts, this.state.shifts)
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ reqInProgress: false
                      , error: apiError
                      })
      })
  }

  changeCurrentVolShiftType(shiftDate: Date, shiftType: ShiftType) {
    console.log('change current vol shift type')
  }

  setShiftLoading(date: Date, shifts: Shift[]): Shift[] {
    let result = shifts.slice()
    let shift = result.find(s => Util.datesEqual(s.date, date))

    if(shift) {
      shift.loading = true
    }

    return result
  }

  addOrUpdateShift(date: Date, vols: VolShift[], shifts: Shift[]): Shift[] {
    let result = shifts.slice()
    let shift = result.find(s => Util.datesEqual(s.date, date))

    if(shift) {
      shift.vols = vols
      shift.loading = false
    } else {
      result.push({date: date, vols: vols, loading: false})
    }

    return result
  }
}
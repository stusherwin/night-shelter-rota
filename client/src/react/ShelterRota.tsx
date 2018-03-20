import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, ShiftType, VolShift, Shift } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { Util } from './Util'

export interface ShelterRotaProps {}
export interface ShelterRotaState { header: HeaderProps
                                  , roster: RosterProps
                                  }

export class ShelterRota extends React.Component<ShelterRotaProps, ShelterRotaState> {
  constructor(props: ShelterRotaProps) {
    super(props)
    this.state = { header: { vols: []
                           , reqInProgress: true
                           , error: null
                           , initialDataLoaded: false
                           , changeCurrentVol: this.changeCurrentVol.bind(this)
                           , editNewVol: this.editNewVol.bind(this)
                           , editCurrentVol: this.editCurrentVol.bind(this)
                           }
                 , roster: { visible: false
                           , currentVol: null
                           , config: { maxVolsPerShift: 2
                                     , currentDate: Util.today()
                                     , urgentPeriodDays: 14
                                     }
                           , shifts: []
                           , addCurrentVol: this.addCurrentVol.bind(this)
                           , removeCurrentVol: this.removeCurrentVol.bind(this)
                           , changeCurrentVolShiftType: this.changeCurrentVolShiftType.bind(this)
                           }
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.getVols(), ServerApi.getShifts()])
      .then(results => {
        console.log(results);
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 , vols: results[0]
                                                                 , initialDataLoaded: true
                                                                 })
                      , roster: Object.assign(this.state.roster, { visible: true
                                                                 , shifts: results[1]
                                                                 })
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 , error: apiError
                                                                 })
                      })
      })
  }

  render() {
    return (
      <div>
        <Header {...this.state.header} />
        <div className="container">
          <Roster {...this.state.roster} />
        </div>
      </div>
    )
  }

  changeCurrentVol(vol: Vol | null) {
    console.log('current vol: ' + (vol? vol.name : 'none'))
    this.setState({ roster: Object.assign(this.state.roster, { currentVol: vol
                                                             })
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
    
    if(!this.state.roster.currentVol) {
      return
    }

    this.setState({ header: Object.assign(this.state.header, { reqInProgress: true
                                                             , error: null
                                                             }),
                    roster: Object.assign(this.state.roster, { shifts: this.setShiftLoading(shiftDate, this.state.roster.shifts)})
                  })
    ServerApi.putVolShift(shiftType, shiftDate, this.state.roster.currentVol.id)
      .then(volShifts => {
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 }),
                        roster: Object.assign(this.state.roster, { shifts: this.addOrUpdateShift(shiftDate, volShifts, this.state.roster.shifts)
                                                                 })
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 , error: apiError
                                                                 })
                      })
      })
  }

  removeCurrentVol(shiftDate: Date) {
    console.log('removeCurrentVol')
    
    if(!this.state.roster.currentVol) {
      return
    }

    this.setState({ header: Object.assign(this.state.header, { reqInProgress: true
                                                             , error: null
                                                             }),
                    roster: Object.assign(this.state.roster, { shifts: this.setShiftLoading(shiftDate, this.state.roster.shifts)})
                  })
    ServerApi.deleteVolShift(shiftDate, this.state.roster.currentVol.id)
      .then(volShifts => {
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 }),
                        roster: Object.assign(this.state.roster, { shifts: this.addOrUpdateShift(shiftDate, volShifts, this.state.roster.shifts)
                                                                 })
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 , error: apiError
                                                                 })
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
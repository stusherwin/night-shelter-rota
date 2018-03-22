import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, ShiftType, VolShift, Shift } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { Util } from './Util'
import { ShiftRuleConfig } from './ShiftRules'
import { VolInfo } from './VolInfo'

export interface ShelterRotaProps {}
export interface ShelterRotaState { initialDataLoaded: boolean
                                  , vols: Vol[]
                                  , shifts: Shift[]
                                  , currentVol: Vol | null
                                  , config: ShiftRuleConfig
                                  , rosterVisible: boolean
                                  , reqInProgress: boolean
                                  , error: ApiError | null
                                  , volInfo: Vol | null
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
                 , volInfo : null
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
                  requestStarted={this.requestStarted.bind(this)}
                  requestFailed={this.requestFailed.bind(this)}
                  requestSucceeded={this.requestSucceeded.bind(this)}
                  updateShifts={this.updateShifts.bind(this)}
                  showVolInfo={this.showVolInfo.bind(this)} />
        </div>
        <VolInfo vol={this.state.volInfo}
                 close={this.hideVolInfo.bind(this)} />
      </div>
    )
  }

  requestStarted() {
    this.setState({ reqInProgress: true
                  , error: null
                  })
  }

  requestFailed(error: ApiError) {
    this.setState({ reqInProgress: false
                  , error: error
                  })
  }

  requestSucceeded() {
    this.setState({ reqInProgress: false
                  , error: null
                  })
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

  updateShifts(date: Date, vols: VolShift[]) {
    let result = this.state.shifts.slice()
    let shift = result.find(s => Util.datesEqual(s.date, date))

    if(shift) {
      shift.vols = vols
      shift.loading = false
    } else {
      result.push({date: date, vols: vols, loading: false})
    }

    this.setState({ shifts: result })
  }

  showVolInfo(vol: Vol) {
    this.setState({volInfo: vol})
  }

  hideVolInfo() {
    this.setState({volInfo: null})
  }
}
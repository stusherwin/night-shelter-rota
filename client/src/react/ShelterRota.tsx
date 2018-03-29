import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, VolDetails, ShiftType, VolShift, Shift, updateVolShifts, updateVolDetails, updateShiftVolDetails } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { Util } from './Util'
import { ShiftRuleConfig } from './ShiftRules'
import { VolInfo } from './VolInfo'
import { VolDetailsForm } from './VolDetailsForm'

export interface ShelterRotaProps {}
export interface ShelterRotaState { initialDataLoaded: boolean
                                  , vols: Vol[]
                                  , shifts: Shift[]
                                  , currentVol: Vol | null
                                  , config: ShiftRuleConfig
                                  , reqInProgress: boolean
                                  , error: ApiError | null
                                  , volInfo: Vol | null
                                  , editingVolDetails: boolean
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
                 , reqInProgress: true
                 , error: null
                 , volInfo : null
                 , editingVolDetails: false
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.getVols(), ServerApi.getShifts()])
      .then(results => {
        this.setState({ reqInProgress: false
                      , vols: results[0]
                      , initialDataLoaded: true
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
        <Header currentVol={this.state.currentVol}
                editingVolDetails={this.state.editingVolDetails}
                reqInProgress={this.state.reqInProgress}
                initialDataLoaded={this.state.initialDataLoaded}
                vols={this.state.vols}
                error={this.state.error}
                changeCurrentVol={this.changeCurrentVol.bind(this)}
                editNewVol={this.editNewVol.bind(this)}
                editCurrentVol={this.editCurrentVol.bind(this)} />
        <div className="container">
          <Roster visible={this.state.initialDataLoaded && !this.state.editingVolDetails}
                  currentVol={this.state.currentVol}
                  shifts={this.state.shifts}
                  config={this.state.config}
                  requestStarted={this.requestStarted.bind(this)}
                  requestFailed={this.requestFailed.bind(this)}
                  requestSucceeded={this.requestSucceeded.bind(this)}
                  updateShifts={this.updateShifts.bind(this)}
                  showVolInfo={this.showVolInfo.bind(this)} />
          <VolDetailsForm visible={this.state.editingVolDetails}
                          currentVol={this.state.currentVol}
                          readOnly={this.state.reqInProgress}
                          save={this.saveVolDetails.bind(this)}
                          cancel={this.cancelEditingVolDetails.bind(this)} />
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
    this.setState({ currentVol: vol
                  , editingVolDetails: false
                  , error: null
                  })
  }

  editNewVol() {
    this.setState({ currentVol: null
                  , editingVolDetails: true
                  , error: null
                  })
  }

  editCurrentVol() {
    this.setState({ editingVolDetails: true
                  , error: null
                  })
  }

  saveVolDetails(details: VolDetails) {
    this.requestStarted();

    let req = this.state.currentVol
      ? ServerApi.postVol(details, this.state.currentVol.id)
        .then(vol => {
          this.setState({ currentVol: vol
                        , vols: updateVolDetails(this.state.vols, vol)
                        , shifts: updateShiftVolDetails(this.state.shifts, vol)
                        })
        })
      : ServerApi.putVol(details)
        .then(vol => {
          this.setState({ currentVol: vol
                        , vols: this.state.vols.concat([vol])
                        })      
        })

    req
    .then(vol => {
      this.requestSucceeded();
      this.cancelEditingVolDetails();
    })
    .catch(err => {
      let apiError = err as ApiError
      console.log(err)
      this.requestFailed(apiError);
    })
  }

  cancelEditingVolDetails() {
    this.setState({editingVolDetails: false})
  }

  updateShifts(date: Date, vols: VolShift[]) {
    this.setState({ shifts: updateVolShifts(this.state.shifts, date, vols) })
  }

  showVolInfo(vol: Vol) {
    this.setState({volInfo: vol})
  }

  hideVolInfo() {
    this.setState({volInfo: null})
  }
}
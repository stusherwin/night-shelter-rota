import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, VolDetails, ShiftType, VolShift, Shift, updateVolShifts, updateVolDetails, updateShiftVolDetails } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { Util } from './Util'
import { ShiftRuleConfig } from './ShiftRules'
import { VolInfo } from './VolInfo'
import { VolDetailsForm, VolDetailsState } from './VolDetailsForm'

export interface ShelterRotaProps {}
export interface ShelterRotaState { initialDataLoaded: boolean
                                  , vols: Vol[]
                                  , shifts: Shift[]
                                  , currentVol: Vol | null
                                  , config: ShiftRuleConfig
                                  , reqInProgress: boolean
                                  , error: ApiError | null
                                  , volInfo: Vol | null
                                  , volDetailsState: VolDetailsState
                                  , volDetails: Vol | null
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
                 , volDetailsState: 'NotEditing'
                 , volDetails: null
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.getVols(), ServerApi.getShifts()])
      .then(results => {
        console.log(results);
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
                volDetailsState={this.state.volDetailsState}
                reqInProgress={this.state.reqInProgress}
                initialDataLoaded={this.state.initialDataLoaded}
                vols={this.state.vols}
                error={this.state.error}
                changeCurrentVol={this.changeCurrentVol.bind(this)}
                editNewVol={this.editNewVol.bind(this)}
                editCurrentVol={this.editCurrentVol.bind(this)} />
        <div className="container">
          <Roster visible={this.state.initialDataLoaded && this.state.volDetailsState == 'NotEditing'}
                  currentVol={this.state.currentVol}
                  shifts={this.state.shifts}
                  config={this.state.config}
                  requestStarted={this.requestStarted.bind(this)}
                  requestFailed={this.requestFailed.bind(this)}
                  requestSucceeded={this.requestSucceeded.bind(this)}
                  updateShifts={this.updateShifts.bind(this)}
                  showVolInfo={this.showVolInfo.bind(this)} />
          <VolDetailsForm state={this.state.volDetailsState}
                          vol={this.state.volDetails}
                          readOnly={false}
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
    console.log('current vol: ' + (vol? vol.name : 'none'))
    this.setState({ currentVol: vol
                  , volDetailsState: 'NotEditing'
                  , error: null
                  })
  }

  editNewVol() {
    console.log('edit new vol')
    this.setState({ volDetails: null
                  , volDetailsState: 'EditingNewVol'
                  , error: null
                  })
  }

  editCurrentVol() {
    console.log('edit current vol')
    this.setState({ volDetails: this.state.currentVol
                  , volDetailsState: 'EditingCurrentVol'
                  , error: null
                  })
  }

  saveVolDetails(details: VolDetails) {
    console.log('save vol details')
    console.log(details)

    if(this.state.currentVol) {
      ServerApi.postVol(details, this.state.currentVol.id)
      .then(vol => {
        this.requestSucceeded();
        this.setState({ currentVol: vol
                      , vols: updateVolDetails(this.state.vols, vol)
                      , shifts: updateShiftVolDetails(this.state.shifts, vol)
                      , volDetails: null
                      , volDetailsState: 'NotEditing'
                      })      
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.requestFailed(apiError);
      })
    } else {
      ServerApi.putVol(details)
      .then(vol => {
        this.requestSucceeded();
        this.setState({ currentVol: vol
                      , vols: this.state.vols.concat([vol])
                      , volDetails: null
                      , volDetailsState: 'NotEditing'
                      })      
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.requestFailed(apiError);
      })
    }
  }

  cancelEditingVolDetails() {
    console.log('cancel edit vol details')
    this.setState({ volDetails: null
                  , volDetailsState: 'NotEditing'
                  })
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
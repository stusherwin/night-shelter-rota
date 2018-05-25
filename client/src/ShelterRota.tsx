import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, VolDetails, ShiftType, VolShift, Shift, updateVolShifts, updateVolDetails, findAndUpdateVolDetails, findAndUpdateShiftVolDetails } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps } from './MessageBubble'
import { Util, pure } from './Util'
import { ShiftRuleConfig } from './ShiftRules'
import { VolInfo } from './VolInfo'
import { VolDetailsForm } from './VolDetailsForm'
import { CurrentVolSelector } from './CurrentVolSelector'

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
                                  , active: boolean
                                  , rotaKey: string | undefined
                                  , rotaExists: boolean
                                  }

export class ShelterRota extends React.Component<ShelterRotaProps, ShelterRotaState> {
  constructor(props: ShelterRotaProps) {
    super(props)
    let pathParts = window.location.pathname.split('/')
    let rotaKey = pathParts[pathParts.indexOf('rota') + 1]

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
                 , active: true
                 , rotaKey
                 , rotaExists: false
                 }
  }

  componentDidMount() {
    if(!this.state.rotaKey) {
      this.setState({ initialDataLoaded: true
                    , reqInProgress: false
                    , rotaExists: false
                    })
      return
    }

    ServerApi.verifyRota(this.state.rotaKey)
      .then(result => {
        if(result) {
          this.setState({ rotaExists: true })
          Promise.all([ServerApi.getVols(this.state.rotaKey), ServerApi.getShifts(this.state.rotaKey), ServerApi.getCurrentVolId(this.state.rotaKey)])
            .then(results => {
              let vols = results[0]
              let shifts = results[1]
              let currentVolId = results[2]
              let currentVol = vols.find(v => v.id == currentVolId) || null

              this.setState({ reqInProgress: false
                            , vols
                            , initialDataLoaded: true
                            , shifts
                            , currentVol
                            , rotaExists: true
                            })
            })
            .catch(err => {
              let apiError = err as ApiError
              console.log(err)
              this.setState({ reqInProgress: false
                            , error: apiError
                            })
            })
        } else {
          this.setState({ reqInProgress: false
                        , initialDataLoaded: true
                        , rotaExists: false
                        })  
        }
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
                active={this.state.active}
                apiRequest={this.apiRequest.bind(this)}
                clearCurrentVol={this.clearCurrentVol.bind(this)}
                editCurrentVol={this.editCurrentVol.bind(this)}
                setActive={this.setActive.bind(this)}
                rotaKey={this.state.rotaKey}
                rotaExists={this.state.rotaExists} />
        <div className="container">
          <RotaNotFound visible={this.state.initialDataLoaded && !this.state.rotaExists} />
          <CurrentVolSelector visible={this.state.initialDataLoaded && this.state.rotaExists && !this.state.currentVol && !this.state.editingVolDetails}
                              vols={this.state.vols}
                              active={this.state.active}
                              apiRequest={this.apiRequest.bind(this)}
                              changeCurrentVol={this.changeCurrentVol.bind(this)}
                              editCurrentVol={this.editCurrentVol.bind(this)}
                              editNewVol={this.editNewVol.bind(this)}
                              updateVolDetails={this.updateVolDetails.bind(this)}
                              rotaKey={this.state.rotaKey} />
          <Roster visible={this.state.initialDataLoaded && this.state.rotaExists && !!this.state.currentVol && !this.state.editingVolDetails}
                  currentVol={this.state.currentVol}
                  shifts={this.state.shifts}
                  config={this.state.config}
                  apiRequest={this.apiRequest.bind(this)}
                  updateShifts={this.updateShifts.bind(this)}
                  showVolInfo={this.showVolInfo.bind(this)}
                  rotaKey={this.state.rotaKey} />
          <VolDetailsForm visible={this.state.editingVolDetails}
                          currentVol={this.state.currentVol}
                          readOnly={this.state.reqInProgress}
                          apiRequest={this.apiRequest.bind(this)}
                          addNewVol={this.addNewVol.bind(this)}
                          updateVolDetails={this.updateVolDetails.bind(this)}
                          cancel={this.cancelEditingVolDetails.bind(this)}
                          rotaKey={this.state.rotaKey} />
        </div>
        <VolInfo vol={this.state.volInfo}
                 close={this.hideVolInfo.bind(this)} />
      </div>
    )
  }

  clearCurrentVol() {
    this.setState({ currentVol: null
                  , editingVolDetails: false
                  , error: null
                  })
  }

  changeCurrentVol(vol: Vol) {
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

  updateVolDetails(vol: Vol) {
    this.setState({ currentVol: this.state.currentVol && updateVolDetails(this.state.currentVol, vol)
                  , vols: findAndUpdateVolDetails(this.state.vols, vol)
                  , shifts: findAndUpdateShiftVolDetails(this.state.shifts, vol)
                  , editingVolDetails: false
                  })
  }

  addNewVol(vol: Vol) {
    this.setState({ currentVol: vol
                  , vols: this.state.vols.concat([vol])
                  , editingVolDetails: false
                  })      
  }

  allVols() {
    this.setState({ currentVol: null
                  , error: null
                  })
  }

  setActive(active: boolean) {
    this.setState({ active: active
                  })
  }

  apiRequest(req: Promise<any>) {
    this.setState({ reqInProgress: true
                  , error: null
                  })

    req
      .then(_ => {
        this.setState({ reqInProgress: false
                      , error: null
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

const RotaNotFound = pure((props: { visible: boolean
                                  }) => !props.visible ? null : (
  <div className="current-vol-selector">
    <h2>Rota not found</h2>
    <p>The rota you are looking for cannot be found. Please make sure you are using the most up-to-date link.</p>
    <p>If you don't have a link and you're curious, why don't you have a play with the <a href="/rota/DEMO">demo rota</a>?</p>
  </div>
))
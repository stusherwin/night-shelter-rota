import * as React from 'react';
import { pure } from './Util'
import { Vol } from './Types'
import { ServerApi, ApiError } from './ServerApi'

export interface CurrentVolSelectorProps { visible: boolean
                                         , vols: Vol[]
                                         , apiRequest: (req: Promise<any>) => void
                                         , changeCurrentVol: (vol: Vol) => void
                                         , editCurrentVol: () => void
                                         , editNewVol: () => void
                                         , updateVolDetails: (vol: Vol) => void
                                         }

export interface CurrentVolSelectorState { active: boolean
                                         }

export class CurrentVolSelector extends React.Component<CurrentVolSelectorProps, CurrentVolSelectorState> {
  constructor(props: CurrentVolSelectorProps) {
    super(props)
    
    this.state = { active: true }
  }

  render() {
    if(!this.props.visible) {
      return null
    }

    let buttons = [];
    if(this.state.active) {
        buttons.push(<button className={`ui button media-large-screen`}
                             onClick={e => this.setState({active: false})}>
                       <i className={`icon icon-user-times`}></i>
                       Show inactive volunteers
                     </button>)
        buttons.push(<button className={`ui button media-larger-screen media-medium-screen`}
                             onClick={e => this.setState({active: false})}>
                       <i className={`icon icon-user-times`}></i>
                       Inactive
                     </button>)
        buttons.push(<button className={`ui button mini icon media-small-screen`}
                             onClick={e => this.setState({active: false})}>
                       <i className={`icon icon-user-times`}></i>
                     </button>)
    } else {
        buttons.push(<button className={`ui button media-large-screen`}
                             onClick={e => this.setState({active: true})}>
                       <i className={`icon icon-user`}></i>
                       Show active volunteers
                     </button>)
        buttons.push(<button className={`ui button media-larger-screen media-medium-screen`}
                             onClick={e => this.setState({active: true})}>
                       <i className={`icon icon-user`}></i>
                       Active
                     </button>)
        buttons.push(<button className={`ui button mini icon media-small-screen`}
                             onClick={e => this.setState({active: true})}>
                       <i className={`icon icon-user`}></i>
                     </button>)
    }

    return (
      <div className="current-vol-selector">
        <div style={{float: 'right'}}>{buttons}</div>
        <h3>{this.state.active? 'Volunteers' : 'Inactive volunteers'}</h3>
        
        {!this.state.active? null : <p>Please choose your name from the list:</p>}

        <div className="vols-list">
          {this.props.vols
             .filter(v => v.active == this.state.active)
             .sort((a, b) => a.name.localeCompare(b.name))
             .map(vol =>
            <div className="vol" key={vol.id}>
              {this.state.active 
                ? <span>
                    <button className={`ui button icon mini`}
                            onClick={e => this.deactivateVol(vol)}>
                      <i className={`icon icon-user-times`}></i>
                    </button>
                    <button className={`ui button icon mini`}
                            onClick={e => this.changeCurrentVolAndEdit(vol)}>
                      <i className={`icon icon-edit`}></i>
                    </button>
                  </span>
                : <span>
                    <button className={`ui button icon mini`}
                            onClick={e => this.activateVol(vol)}>
                      <i className={`icon icon-user`}></i>
                    </button>
                  </span>
              }
              {this.state.active 
                ? <a href="#"
                     className="vol-name"
                     onClick={e => { this.changeCurrentVol(vol) }}>
                    {vol.name}
                  </a>
                : <span className="vol-name">{vol.name}</span>
              }
            </div>
          )}
        </div>

        {!this.state.active? null :
          <div className="vols-add">
            <p>Can't find your name? You can add yourself here:</p>
            <button className={`ui button primary`}
                    onClick={e => this.props.editNewVol()}>
              <i className={`icon icon-add`}></i>
              Add your details
            </button>
          </div>
        }
      </div>
    )
  }

  changeCurrentVol(vol: Vol) {
    this.props.apiRequest(
      ServerApi.setCurrentVolId(vol.id)
        .then(() => {
          this.props.changeCurrentVol(vol)
        }))
  }

  changeCurrentVolAndEdit(vol: Vol) {
    this.props.apiRequest(
      ServerApi.setCurrentVolId(vol.id)
        .then(() => {
          this.props.changeCurrentVol(vol)
          this.props.editCurrentVol()
        }))
  }

  deactivateVol(vol: Vol) {
    this.props.apiRequest(
      ServerApi.deactivateVol(vol.id)
        .then(vol => {
          this.props.updateVolDetails(vol)
        }))
  }

  activateVol(vol: Vol) {
    this.props.apiRequest(
      ServerApi.activateVol(vol.id)
        .then(vol => {
          this.props.updateVolDetails(vol)
        }))
  }
}
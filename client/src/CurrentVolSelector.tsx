import * as React from 'react';
import { pure } from './Util'
import { Vol } from './Types'
import { ServerApi, ApiError } from './ServerApi'

export interface CurrentVolSelectorProps { visible: boolean
                                         , vols: Vol[]
                                         , active: boolean
                                         , apiRequest: (req: Promise<any>) => void
                                         , changeCurrentVol: (vol: Vol) => void
                                         , editCurrentVol: () => void
                                         , editNewVol: () => void
                                         , updateVolDetails: (vol: Vol) => void
                                         }

export interface CurrentVolSelectorState { 
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

    return (
      <div className="current-vol-selector">
        {this.props.active
          ? <h3><i className="icon-users"></i>Volunteers</h3>
          : <h3><i className="icon-history"></i>Inactive volunteers</h3>
        }
        
        {!this.props.active? null : <p>Please choose your name from the list:</p>}

        <div className="vols-list">
          {this.props.vols
             .filter(v => v.active == this.props.active)
             .sort((a, b) => a.name.localeCompare(b.name))
             .map(vol =>
            <div className={`vol ${!this.props.active && 'inactive'}`} key={vol.id}>
              {this.props.active 
                ? <span>
                    <button className={`ui button icon mini`}
                            onClick={e => this.deactivateVol(vol)}>
                      <i className={`icon icon-history`}></i>
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
              {this.props.active 
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

        {!this.props.active? null :
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
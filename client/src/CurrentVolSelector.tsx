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
                                         , rotaKey: string | undefined
                                         }

export interface CurrentVolSelectorState { active: boolean
                                         , activeVols: boolean
                                         }

export class CurrentVolSelector extends React.Component<CurrentVolSelectorProps, CurrentVolSelectorState> {
  constructor(props: CurrentVolSelectorProps) {
    super(props)
    
    this.state = { active: true
                 , activeVols: !!this.props.vols.filter(v => v.active).length
                 }
  }

  render() {
    if(!this.props.visible) {
      return null
    }

    let vols = this.props.vols.filter(v => v.active == this.props.active)

    return (
      <div className="current-vol-selector">
        {this.props.active
          ? <h3><i className="icon-users"></i>Volunteers</h3>
          : <h3><i className="icon-history"></i>Inactive volunteers</h3>
        }

        {this.props.active && vols.length
          ? <p>Hi! Please choose your name from the list:</p>
          : null}
        
        {!this.props.active && !vols.length
          ? <p>There aren't any inactive volunteers.</p>
          : null}
        
        <VolsList vols={vols}
                  active={this.props.active}
                  deactivateVol={this.deactivateVol.bind(this)}
                  changeCurrentVolAndEdit={this.changeCurrentVolAndEdit.bind(this)}
                  activateVol={this.activateVol.bind(this)}
                  changeCurrentVol={this.changeCurrentVol.bind(this)} />

        {this.props.active
          ? <div className="vols-add">
              {vols.length
                ? <p>Can't find your name? You can add yourself here:</p>
                : <p>There aren't any active volunteers on this rota. You can add yourself here:</p>}
              <button className={`ui button primary`}
                      onClick={e => this.props.editNewVol()}>
                <i className={`icon icon-add`}></i>
                Add your details
              </button>
            </div>
          : null}
      </div>
    )
  }

  changeCurrentVol(vol: Vol) {
    this.props.apiRequest(
      ServerApi.setCurrentVolId(this.props.rotaKey, vol.id)
        .then(() => {
          this.props.changeCurrentVol(vol)
        }))
  }

  changeCurrentVolAndEdit(vol: Vol) {
    this.props.apiRequest(
      ServerApi.setCurrentVolId(this.props.rotaKey, vol.id)
        .then(() => {
          this.props.changeCurrentVol(vol)
          this.props.editCurrentVol()
        }))
  }

  deactivateVol(vol: Vol) {
    this.props.apiRequest(
      ServerApi.deactivateVol(this.props.rotaKey, vol.id)
        .then(vol => {
          this.props.updateVolDetails(vol)
        }))
  }

  activateVol(vol: Vol) {
    this.props.apiRequest(
      ServerApi.activateVol(this.props.rotaKey, vol.id)
        .then(vol => {
          this.props.updateVolDetails(vol)
        }))
  }
}

const VolsList = pure((props: { vols: Vol[]
                              , active: boolean
                              , deactivateVol: (vol: Vol) => void
                              , changeCurrentVolAndEdit: (vol: Vol) => void
                              , activateVol: (vol: Vol) => void
                              , changeCurrentVol: (vol: Vol) => void
                              }) => !props.vols.length ? null : (
  <div className="vols-list">
    {props.vols
       .sort((a, b) => a.name.localeCompare(b.name))
       .map(vol =>
      <div className={`vol ${!props.active && 'inactive'}`} key={vol.id}>
        {props.active 
          ? <span>
              <button className={`ui button icon mini`}
                      onClick={e => props.deactivateVol(vol)}>
                <i className={`icon icon-history`}></i>
              </button>
              <button className={`ui button icon mini`}
                      onClick={e => props.changeCurrentVolAndEdit(vol)}>
                <i className={`icon icon-edit`}></i>
              </button>
            </span>
          : <span>
              <button className={`ui button icon mini`}
                      onClick={e => props.activateVol(vol)}>
                <i className={`icon icon-user`}></i>
              </button>
            </span>
        }
        {props.active 
          ? <a href="#"
               className="vol-name"
               onClick={e => { props.changeCurrentVol(vol) }}>
              {vol.name}
            </a>
          : <span className="vol-name">{vol.name}</span>
        }
      </div>
    )}
  </div>
))
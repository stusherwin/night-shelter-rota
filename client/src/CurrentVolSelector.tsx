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

export class CurrentVolSelector extends React.Component<CurrentVolSelectorProps, {}> {
  render() {
    if(!this.props.visible) {
      return null
    }

    return (
      <div className="current-vol-selector">
        <h3>Volunteers</h3>
        
        <p>Hi, who are you? Please choose your name:</p>

        <div className="vols-list">
          {this.props.vols
             .filter(v => v.active)
             .sort((a, b) => a.name.localeCompare(b.name))
             .map(vol =>
            <div className="vol" key={vol.id}>
              <button className={`ui button icon mini`}
                      onClick={e => this.deactivateVol(vol)}>
                <i className={`icon icon-user-times`}></i>
              </button>
              <button className={`ui button icon mini`}
                      onClick={e => this.changeCurrentVolAndEdit(vol)}>
                <i className={`icon icon-edit`}></i>
              </button>
              <a href="#"
                 className="vol-name"
                 onClick={e => { this.changeCurrentVol(vol) }}>
                {vol.name}
              </a>
            </div>
          )}
        </div>

        <div className="vols-add">
          <p>Can't find your name? You can add yourself here:</p>
          <button className={`ui button primary`}
                  onClick={e => this.props.editNewVol()}>
            <i className={`icon icon-add`}></i>
            Add your details
          </button>
        </div>
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
}
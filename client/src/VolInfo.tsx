import * as React from 'react';
import { Vol, OvernightPreference, OvernightGenderPreference, PrefInfo, info } from './Types'
import { pure } from './Util'

export interface VolInfoProps { vol: Vol | null
                              , close: () => void
                              }

export class VolInfo extends React.PureComponent<VolInfoProps> {
  render() {
    if(!this.props.vol) {
      return null
    }

    return (
      <div className="vol-info-fadeout"
           onClick={e => {e.preventDefault(); this.props.close()}}>
        <div className="vol-info"
             onClick={e => { e.preventDefault(); this.props.close() }}>
          <div className="vol-info-wrapper">
            <h2>{this.props.vol.name}</h2>
            <div className="vol-info-content">
              <Intro name={this.props.vol.name}
                     intro={this.props.vol.intro} />
              <Preferences prefs={[ info(this.props.vol.overnightPreference)
                                  , info(this.props.vol.overnightGenderPreference)
                                  ]} />
              <Notes notes={this.props.vol.notes} />
            </div>
          </div>
          <a href="#"
             onClick={e => {e.preventDefault(); this.props.close()}}>
            <i className="icon-cancel"></i>
          </a>
        </div>
      </div>
    )
  }
}

const Intro = pure((props: { name: string
                           , intro: string
                           }) => {

  if(!props.intro.length) {
    return (
      <div className="vol-info-no-intro">
        {props.name} doesn't have an intro yet.
      </div>
    )
  }

  let paras = props.intro.split(/[\r\n]+/g).map(p => <p>{p}</p>)
  return (
    <div className="vol-info-intro">
      {paras}
    </div>
  )
})

const Preferences = pure((props: {prefs: (PrefInfo | null)[]})  => {
  if(!props.prefs.filter(p => !!p).length) {
    return null
  }

  return (
    <div>
      <h3>My preferences</h3>
      {props.prefs.map(p => <Preference pref={p} />)}
    </div>
  )
})

const Preference = pure((props: {pref: PrefInfo | null}) => {
  if(!props.pref) {
    return null
  }

  return (
    <div className="vol-info-pref">
      <div className="vol-info-pref-marker">{props.pref.marker}</div>
      <div className="vol-info-pref-description">{props.pref.description}</div>
    </div>
  )
})

const Notes = pure((props: {notes: string}) => {
  if(!props.notes.length) {
    return null
  }

  return (
    <div>
      <h3>Notes for other volunteers</h3>
      <div className="vol-info-pref">
        <div className="vol-info-pref-marker">
          <i className="icon-info"></i>
        </div>
        <div className="vol-info-pref-description">
          {props.notes}
        </div>
      </div>
    </div>
  )
})
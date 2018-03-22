import * as React from 'react';
import { Vol, OvernightPreference, OvernightGenderPreference } from './Types'

export interface VolInfoProps { vol: Vol | null
                              , close: () => void
                              }

export class VolInfo extends React.Component<VolInfoProps, {}> {
  constructor(props: VolInfoProps) {
    super(props)
  }

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
              <Preferences overnightPreference={this.props.vol.overnightPreference}
                           overnightGenderPreference={this.props.vol.overnightGenderPreference} />
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

function Intro(props: { name: string
                      , intro: string
                      }): JSX.Element {

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
}

function Preferences(props: { overnightPreference: OvernightPreference | null
                            , overnightGenderPreference: OvernightGenderPreference | null
                            }): JSX.Element {
  return (
    <div></div>
  )
      // renderPreferences { overnightPreference: Nothing, overnightGenderPreference: Nothing } = []
      // renderPreferences v = [ RD.h3' [ RD.text "My preferences" ] ]
      //                       <> renderOvernight v.overnightPreference
      //                       <> renderGender v.overnightGenderPreference
      //   where
      //   renderOvernight :: Maybe OvernightPreference -> Array R.ReactElement
      //   renderOvernight Nothing = []
      //   renderOvernight (Just p) = [ RD.div [ RP.className "vol-info-pref" ]
      //                                       [ RD.div [ RP.className "vol-info-pref-marker" ]
      //                                                [ RD.text $ overnightPrefMarker p ]
      //                                       , RD.div [ RP.className "vol-info-pref-description" ]
      //                                                [ RD.text $ overnightPrefDescription p ]
      //                                       ] 
      //                              ]
    
      //   renderGender :: Maybe OvernightGenderPreference -> Array R.ReactElement
      //   renderGender Nothing = []
      //   renderGender (Just p) = [ RD.div [ RP.className "vol-info-pref" ]
      //                                    [ RD.div [ RP.className "vol-info-pref-marker" ]
      //                                             [ RD.text $ overnightGenderPrefMarker p ]
      //                                    , RD.div [ RP.className "vol-info-pref-description" ]
      //                                             [ RD.text $ overnightGenderPrefDescription p ]
      //                                    ] 
      //                           ]
}

function Notes(props: { notes: string
                      }): JSX.Element | null {
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
}
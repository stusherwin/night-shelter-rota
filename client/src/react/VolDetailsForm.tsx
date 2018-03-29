import * as React from 'react';
import { Vol, OvernightPreference, OvernightGenderPreference, PrefInfo, info, VolDetails, createVolDetails } from './Types'
import { Util } from './Util'
import { ServerApi } from './ServerApi'

export class VolDetailsFormProps {
  visible: boolean
  currentVol: Vol | null
  readOnly: boolean
  apiRequest: (req: Promise<any>) => void
  updateCurrentVol: (vol: Vol) => void
  addNewVol: (vol: Vol) => void
  cancel: () => void
}

export class VolDetailsFormState {
  details: VolDetails
  formSubmitted: boolean
  formValid: boolean
}

export class VolDetailsForm extends React.Component<VolDetailsFormProps, VolDetailsFormState> {
  constructor(props: VolDetailsFormProps) {
    super(props)

    let details = createVolDetails(props.currentVol)
    this.state = { details
                 , formSubmitted: false
                 , formValid: this.isValid(details)
                 }
  }

  componentWillReceiveProps(props: VolDetailsFormProps) {
    if(props.visible != this.props.visible) {
      let details = createVolDetails(props.currentVol)
      this.setState({ details
                    , formSubmitted: false
                    , formValid: this.isValid(details)
                    })
    }
  }

  setDetails(fields: any) {
    let details = Util.update(this.state.details, fields)
    this.setState({ details
                  , formValid: this.isValid(details) 
                  })
  }

  isValid(details: VolDetails) {
    if(details.name.length == 0) {
      return false
    }
    return true
  }

  save() {
    if(this.props.currentVol) {
      let currentVolId = this.props.currentVol.id
      this.props.apiRequest(ServerApi.postVol(this.state.details, currentVolId)
          .then(vol => this.props.updateCurrentVol(vol)))
    } else {
      this.props.apiRequest(ServerApi.putVol(this.state.details)
          .then(vol => this.props.addNewVol(vol)))
    }
  }

  render() {
    if(!this.props.visible) {
      return null
    }

    let formError = this.state.formSubmitted && !this.state.formValid

    return (
      <form className={`ui form details ${formError? "error": ""}`}>
        <h3>{this.props.currentVol? this.props.currentVol.name + "'s details" : "Add new volunteer"}</h3>
        <div className={`required field ${formError? "error" : ""}`}>
          <label htmlFor="volName">Name</label>
          <input type="text"
                 id="volName"
                 autoFocus={true}
                 value={this.state.details.name}
                 onChange={e => {this.setDetails({name: e.target.value})}}
                 disabled={this.props.readOnly} />
        </div>
        <div className="field">
          <label htmlFor="volIntro">A short intro about yourself</label>
          <textarea id="volIntro"
                    onChange={e => {this.setDetails({intro: e.target.value})}}
                    disabled={this.props.readOnly}>
            {this.state.details.intro}
          </textarea>
        </div>
        <div className="field">
          <label>Would you prefer to work with another volunteer?</label>
          <PreferenceRadio name="alone"
                           value="PreferAnotherVolunteer"
                           checked={this.state.details.pref == 'PreferAnotherVolunteer'}
                           onChecked={() => this.setDetails({pref: 'PreferAnotherVolunteer'})}
                           readOnly={this.props.readOnly} />
          <PreferenceRadio name="alone"
                           value="PreferToBeAlone"
                           checked={this.state.details.pref == 'PreferToBeAlone'}
                           onChecked={() => this.setDetails({pref: 'PreferToBeAlone'})}
                           readOnly={this.props.readOnly} />
          <PreferenceRadio name="alone"
                           value={null}
                           checked={this.state.details.pref == null}
                           onChecked={() => this.setDetails({pref: null})}
                           readOnly={this.props.readOnly} />
        </div>
        <div className="field">
          <label>Who would you prefer to share the volunteers' room with?</label>
          <PreferenceRadio name="gender"
                           value="Male"
                           checked={this.state.details.genderPref == 'Male'}
                           onChecked={() => this.setDetails({genderPref: 'Male'})}
                           readOnly={this.props.readOnly} />
          <PreferenceRadio name="gender"
                           value="Female"
                           checked={this.state.details.genderPref == 'Female'}
                           onChecked={() => this.setDetails({genderPref: 'Female'})}
                           readOnly={this.props.readOnly} />
          <PreferenceRadio name="gender"
                           value={null}
                           checked={this.state.details.genderPref == null}
                           onChecked={() => this.setDetails({genderPref: null})}
                           readOnly={this.props.readOnly} />
        </div>
        <div className="field">
          <label htmlFor="volNotes">Any other preferences?</label>
          <input type="text"
                 value={this.state.details.notes}
                 onChange={e => {this.setDetails({notes: e.target.value})}}
                 disabled={this.props.readOnly} />
        </div>
        <div className="ui error message">
          <div className="error-message-header">Please check these fields and try again:</div>
          <p>Volunteer name should not be empty.</p>
        </div>
        <div className="buttons">
          <button className="ui button"
                  disabled={this.props.readOnly}
                  onClick={e => {e.preventDefault(); this.props.cancel()}}>
            <i className="icon icon-cancel"></i>
            Cancel
          </button>
          <button className="ui primary button"
                  type="submit"
                  disabled={formError || this.props.readOnly}
                  onClick={e => {
                    e.preventDefault(); 
                    if(this.state.formValid) {
                      this.save()
                    } else {
                      this.setState({formSubmitted: true})
                    }
                  }}>
            <i className="icon icon-ok"></i>
            Save
          </button>
        </div>
      </form>
    )
  }
}

function PreferenceRadio(props: { name: string
                                , value: OvernightPreference | OvernightGenderPreference | null
                                , checked: boolean
                                , onChecked: () => void
                                , readOnly: boolean
                                }): JSX.Element {
  let prefInfo = info(props.value)
  let marker = prefInfo? prefInfo.marker : 'none'

  return (
    <div className="ui radio checkbox">
      <input type="radio"
             id={`pref-${props.name}-${marker}`}
             name={`pref-${props.name}`}
             checked={props.checked}
             onChange={e => { if(e.target.checked) { props.onChecked() } }}
             disabled={props.readOnly} />
      <label className="action-label"
             htmlFor={`pref-${props.name}-${marker}`}>
        <Preference className={props.name} pref={prefInfo} />
      </label>
    </div>
  )
}

function Preference(props: {pref: PrefInfo | null, className: string}): JSX.Element | null {
  if(!props.pref) {
    return <span>I don't mind</span>      
  }

  return (
    <span>
      {props.pref.description} (<span className={`sharing-pref ${props.className}`}><span>{props.pref.marker}</span></span>)
    </span>
  )
}
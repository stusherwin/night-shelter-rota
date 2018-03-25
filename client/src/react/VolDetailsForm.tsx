import * as React from 'react';
import { Vol, OvernightPreference, OvernightGenderPreference, PrefInfo, info, VolDetails } from './Types'

export type VolDetailsState = 'NotEditing' 
                            | 'EditingNewVol'
                            | 'EditingCurrentVol'

export class VolDetailsFormProps {
  state: VolDetailsState
  vol: Vol | null
  readOnly: boolean
  save: (details: VolDetails) => void
  cancel: () => void
}

export class VolDetailsFormState {
  name: string
  intro: string
  pref: OvernightPreference | null
  genderPref: OvernightGenderPreference | null
  notes: string
  formSubmitted: boolean
  formValid: boolean
}

export class VolDetailsForm extends React.Component<VolDetailsFormProps, VolDetailsFormState> {
  constructor(props: VolDetailsFormProps) {
    super(props)
    
    this.state = { name: props.vol? props.vol.name : ''
                 , intro: props.vol? props.vol.intro : ''
                 , pref: props.vol? props.vol.overnightPreference : null
                 , genderPref: props.vol? props.vol.overnightGenderPreference : null
                 , notes: props.vol? props.vol.notes : ''
                 , formSubmitted: false
                 , formValid: false
                 }
  }

  componentWillReceiveProps(props: VolDetailsFormProps) {
    if(props.vol != this.props.vol) {
      this.setState({ name: props.vol? props.vol.name : ''
                    , intro: props.vol? props.vol.intro : ''
                    , pref: props.vol? props.vol.overnightPreference : null
                    , genderPref: props.vol? props.vol.overnightGenderPreference : null
                    , notes: props.vol? props.vol.notes : ''
                    , formSubmitted: false
                    , formValid: false
                    })
    }
  }

  render() {
    if(this.props.state == 'NotEditing') {
      return null
    }

    let formError = this.state.formSubmitted && !this.state.formValid

    return (
      <form className={`ui form details ${formError? "error": ""}`}>
        <h3>{this.props.vol? this.props.vol.name + "'s details" : "Add new volunteer"}</h3>
        <div className={`required field ${formError? "error" : ""}`}>
          <label htmlFor="volName">Name</label>
          <input type="text"
                 autoFocus={true}
                 value={this.state.name}
                 onChange={e => {this.setState({name: e.target.value})}}
                 disabled={this.props.readOnly} />
        </div>
        <div className="field">
          <label htmlFor="volIntro">A short intro about yourself</label>
          <textarea onChange={e => {this.setState({intro: e.target.value})}}
                    disabled={this.props.readOnly}>
            {this.state.intro}
          </textarea>
        </div>
        <div className="field">
          <label>Would you prefer to work with another volunteer?</label>
          <div className="ui radio checkbox">
            <input type="radio"
                   id="pref-alone-2"
                   name="pref-alone"
                   checked={this.state.pref == 'PreferAnotherVolunteer'}
                   onChange={e => { if(e.target.checked) { this.setState({pref: 'PreferAnotherVolunteer'}) } }}
                   disabled={this.props.readOnly} />
            <label className="action-label"
                   htmlFor="pref-alone-2">
              <Preference className="alone" pref={info('PreferAnotherVolunteer')} />
            </label>
          </div>
          <div className="ui radio checkbox">
            <input type="radio"
                   id="pref-alone-1"
                   name="pref-alone"
                   checked={this.state.pref == 'PreferToBeAlone'}
                   onChange={e => { if(e.target.checked) { this.setState({pref: 'PreferToBeAlone'}) } }}
                   disabled={this.props.readOnly} />
            <label className="action-label"
                   htmlFor="pref-alone-1">
              <Preference className="alone" pref={info('PreferToBeAlone')} />
            </label>
          </div>
          <div className="ui radio checkbox">
            <input type="radio"
                   id="pref-alone-none"
                   name="pref-alone"
                   checked={this.state.pref == null}
                   onChange={e => { if(e.target.checked) { this.setState({pref: null}) } }}
                   disabled={this.props.readOnly} />
            <label className="action-label"
                   htmlFor="pref-alone-none">
              I don't mind
            </label>
          </div>
        </div>
        <div className="field ">
          <label>Who would you prefer to share the volunteers' room with?</label>
          <div className="ui radio checkbox">
            <input type="radio"
                   id="pref-gender-f"
                   name="pref-gender"
                   checked={this.state.genderPref == 'Female'}
                   onChange={e => { if(e.target.checked) { this.setState({genderPref: 'Female'}) } }}
                   disabled={this.props.readOnly} />
            <label className="action-label"
                   htmlFor="pref-gender-f">
              <Preference className="gender" pref={info('Female')} />
            </label>
          </div>
          <div className="ui radio checkbox">
            <input type="radio"
                   id="pref-gender-m"
                   name="pref-gender"
                   onChange={e => { if(e.target.checked) { this.setState({genderPref: 'Male'}) } }}
                   disabled={this.props.readOnly} />
            <label className="action-label"
                   htmlFor="pref-gender-m">
              <Preference className="gender" pref={info('Male')} />
            </label>
          </div>
          <div className="ui radio checkbox">
            <input type="radio"
                   id="pref-gender-none"
                   name="pref-gender"
                   checked={this.state.genderPref == null}
                   onChange={e => { if(e.target.checked) { this.setState({genderPref: null}) } }}
                   disabled={this.props.readOnly} />
            <label className="action-label"
                   htmlFor="pref-gender-none">
              I don't mind
            </label>
          </div>
        </div>
        <div className="field">
          <label htmlFor="volNotes">Any other preferences?</label>
          <input type="text"
                 value={this.state.notes}
                 onChange={e => {this.setState({notes: e.target.value})}}
                 disabled={this.props.readOnly} />
        </div>
        <div className="ui error message">
          <div className="header">Please check these fields and try again:</div>
          <p>"Volunteer name should not be empty.</p>
        </div>
        <div className="buttons">
          <button className="ui button"
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
                      this.props.save({
                        name: this.state.name,
                        intro: this.state.intro,
                        pref: this.state.pref,
                        genderPref: this.state.genderPref,
                        notes: this.state.notes
                      })
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

function Preference(props: {pref: PrefInfo | null, className: string}): JSX.Element | null {
  if(!props.pref) {
    return null
  }

  return (
    <span>
      {props.pref.description}
      (<span className={`sharing-pref ${props.className}`}>
        <span>{props.pref.marker}</span>
      </span>)
    </span>
  )
}
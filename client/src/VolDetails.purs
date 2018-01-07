module App.VolDetails (State, Action(..), spec, initialState, disable, enable) where
 
import Prelude

import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.String (length)
import React (ReactElement, preventDefault)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
 
import App.Common (unsafeEventValue, classNames, onlyIf, unsafeChecked)
import App.Types (OvernightPreference(..), OvernightGenderPreference(..), Volunteer, VolunteerDetails, overnightPrefMarker, overnightPrefDescription, overnightGenderPrefMarker, overnightGenderPrefDescription)

type State = { details :: VolunteerDetails
             , title :: String
             , formValid :: Boolean
             , formSubmitted :: Boolean
             , readOnly :: Boolean
             }
 
data Action = SetName String
            | SetIntro String
            | SetPref (Maybe OvernightPreference)
            | SetGenderPref (Maybe OvernightGenderPreference)
            | SetNotes String
            | SetSubmitted
            | Save VolunteerDetails
            | Cancel

hasPref :: OvernightPreference -> VolunteerDetails -> Boolean
hasPref PreferToBeAlone { pref: Just PreferToBeAlone } = true
hasPref PreferAnotherVolunteer { pref: Just PreferAnotherVolunteer } = true
hasPref _ _ = false

hasGenderPref :: OvernightGenderPreference -> VolunteerDetails -> Boolean
hasGenderPref Male { genderPref: Just Male } = true
hasGenderPref Female { genderPref: Just Female } = true
hasGenderPref _ _ = false

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.form [ classNames [ "ui form details", onlyIf formError "error" ] ]
              [ RD.h3' [ RD.text state.title ]
              , RD.div [ classNames [ "required field ", onlyIf formError "error" ] ]
                       [ RD.label [ RP.htmlFor "volName" ]
                                  [ RD.text "Name" ]
                       , RD.input [ RP._type "text"
                                  , RP.autoFocus true
                                  , RP.value state.details.name
                                  , RP.onChange $ dispatch <<< SetName <<< unsafeEventValue
                                  , RP.disabled state.readOnly
                                  ]
                                  []
                       ]
              , RD.div [ classNames [ "field " ] ]
                       [ RD.label [ RP.htmlFor "volIntro" ]
                                  [ RD.text "A short intro about yourself"
                                  ]
                       , RD.textarea [ RP.onChange $ dispatch <<< SetIntro <<< unsafeEventValue
                                     , RP.disabled state.readOnly
                                     ]
                                     [ RD.text state.details.intro ]
                       ]
              , RD.div [ classNames [ "field " ] ]
                       [ RD.label' [ RD.text "Would you prefer to work with another volunteer?" ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-alone-2"
                                           , RP.name "pref-alone"
                                           , RP.checked $ hasPref PreferAnotherVolunteer state.details
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetPref (Just PreferAnotherVolunteer)
                                           , RP.disabled state.readOnly
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-alone-2" ]
                                           [ RD.text $ overnightPrefDescription PreferAnotherVolunteer
                                           , RD.text " ("
                                           , renderPref PreferAnotherVolunteer 
                                           , RD.text ")"
                                           ]
                                ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-alone-1"
                                           , RP.name "pref-alone"
                                           , RP.checked $ hasPref PreferToBeAlone state.details
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetPref (Just PreferToBeAlone)
                                           , RP.disabled state.readOnly
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-alone-1" ]
                                           [ RD.text $ overnightPrefDescription PreferToBeAlone
                                           , RD.text " ("
                                           , renderPref PreferToBeAlone
                                           , RD.text ")"
                                           ]
                                ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-alone-none"
                                           , RP.name "pref-alone"
                                           , RP.checked $ isNothing state.details.pref
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetPref Nothing
                                           , RP.disabled state.readOnly
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-alone-none" ]
                                           [ RD.text "I don't mind"
                                           ]
                                ]
                       ]
              , RD.div [ classNames [ "field " ] ]
                       [ RD.label' [ RD.text "Who would you prefer to share the volunteers' room with?" ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-gender-f"
                                           , RP.name "pref-gender"
                                           , RP.checked $ hasGenderPref Female state.details
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetGenderPref (Just Female)
                                           , RP.disabled state.readOnly
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-gender-f" ]
                                           [ RD.text $ overnightGenderPrefDescription Female
                                           , RD.text " ("
                                           , renderGenderPref Female
                                           , RD.text ")"
                                           ]
                                ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-gender-m"
                                           , RP.name "pref-gender"
                                           , RP.checked $ hasGenderPref Male state.details
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetGenderPref (Just Male)
                                           , RP.disabled state.readOnly
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-gender-m" ]
                                           [ RD.text $ overnightGenderPrefDescription Male
                                           , RD.text " ("
                                           , renderGenderPref Male 
                                           , RD.text ")"
                                           ]
                                ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-gender-none"
                                           , RP.name "pref-gender"
                                           , RP.checked $ isNothing state.details.genderPref
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetGenderPref Nothing
                                           , RP.disabled state.readOnly
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-gender-none" ]
                                           [ RD.text "I don't mind"
                                           ]
                                ]
                       ]
              , RD.div [ classNames [ "field " ] ]
                       [ RD.label [ RP.htmlFor "volNotes" ]
                                  [ RD.text "Any other preferences?"
                                  ]
                       , RD.input [ RP._type "text"
                                  , RP.value state.details.notes
                                  , RP.onChange $ dispatch <<< SetNotes <<< unsafeEventValue
                                  , RP.disabled state.readOnly
                                  ]
                                  []
                       ]
              , RD.div [ RP.className "ui error message" ]
                       [ RD.div [ RP.className "header" ]
                                [ RD.text "Please check these fields and try again:" ]
                       , RD.p' [ RD.text "Volunteer name should not be empty." ]
                       ]
              , RD.div [ RP.className "buttons" ]
                       [ RD.button [ RP.className "ui button"
                                   , RP.disabled formError
                                   , RP.onClick \e -> do
                                       _ <- preventDefault e
                                       dispatch $ Cancel
                                   , RP.disabled state.readOnly
                                   ]
                                   [ RD.i [ RP.className "icon icon-cancel" ] []
                                   , RD.text "Cancel" ]
                       , RD.button [ RP.className "ui primary button"
                                   , RP._type "submit"
                                   , RP.disabled formError
                                   , RP.onClick \e -> do
                                       _ <- preventDefault e
                                       dispatch $ if state.formValid then Save state.details else SetSubmitted
                                   , RP.disabled state.readOnly
                                   ]
                                   [ RD.i [ RP.className "icon icon-ok" ] []
                                   , RD.text "Save" ]
                       ]
              ]
    ]
    where
    formError :: Boolean
    formError = state.formSubmitted && not state.formValid
  
  performAction :: T.PerformAction _ State _ Action
  performAction (SetName name) _ _ = void $ T.modifyState \state ->
    let details' = state.details { name = name }
    in state { details = details'
             , formValid = isValid details'
             }
  performAction (SetIntro intro) _ _ = void $ T.modifyState \state ->
    let details' = state.details { intro = intro }
    in state { details = details'
             , formValid = isValid details'
             }
  performAction (SetPref pref) _ _ = void $ T.modifyState \state ->
    let details' = state.details { pref = pref }
    in state { details = details'
             , formValid = isValid details'
             }
  performAction (SetGenderPref pref) _ _ = void $ T.modifyState \state ->
    let details' = state.details { genderPref = pref }
    in state { details = details'
             , formValid = isValid details'
             }
  performAction (SetNotes notes) _ _ = void $ T.modifyState \state ->
    let details' = state.details { notes = notes }
    in state { details = details'
             , formValid = isValid details'
             }
  performAction SetSubmitted _ _ = void $ T.modifyState \state -> state { formSubmitted = true }
  performAction _ _ _ = pure unit 

initialState :: Maybe Volunteer -> State
initialState currentVol =
  let defaultDetails = { name: ""
                       , intro: ""
                       , pref: Nothing
                       , genderPref: Nothing
                       , notes: ""
                       }
      currentVolDetails cv = { name: cv.name
                             , intro: cv.intro
                             , pref: cv.overnightPreference
                             , genderPref: cv.overnightGenderPreference
                             , notes: cv.notes
                             }
      details = maybe defaultDetails currentVolDetails currentVol
  in { details
     , title: maybe "Add new volunteer" (\cv -> cv.name <> "'s details") currentVol
     , formValid: isValid details
     , formSubmitted: false
     , readOnly: false
     }

disable :: State -> State
disable = _ { readOnly = true }

enable :: State -> State
enable = _ { readOnly = false }

isValid :: VolunteerDetails -> Boolean
isValid { name } | length name == 0 = false
isValid _ = true

renderPref :: OvernightPreference -> ReactElement
renderPref p = RD.span [ RP.className "sharing-pref alone" 
                       ] 
                       [ RD.span' [ RD.text $ overnightPrefMarker p ] ]

renderGenderPref :: OvernightGenderPreference -> ReactElement
renderGenderPref p = RD.span [ RP.className "sharing-pref gender" 
                             ] 
                             [ RD.span' [ RD.text $ overnightGenderPrefMarker p ] ]
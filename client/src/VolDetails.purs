module App.VolDetails (State, Details, Action(..), spec, initialState) where

import Prelude

import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.String (length)
import React (ReactElement, preventDefault)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
 
import App.Common (unsafeEventValue, className, onlyIf, unsafeChecked)
import ServerTypes (OvernightPreference(..), OvernightGenderPreference(..), Volunteer (..))

type Details = { name :: String 
               , notes :: String
               , pref :: Maybe OvernightPreference
               , genderPref :: Maybe OvernightGenderPreference
               }

type State = { details :: Details
             , title :: String
             , formValid :: Boolean
             , formSubmitted :: Boolean
             }
 
data Action = SetName String
            | SetNotes String
            | SetPref (Maybe OvernightPreference)
            | SetGenderPref (Maybe OvernightGenderPreference)
            | SetSubmitted
            | Save Details
            | Cancel

hasPref :: OvernightPreference -> Details -> Boolean
hasPref PreferToBeAlone { pref: Just PreferToBeAlone } = true
hasPref PreferAnotherVolunteer { pref: Just PreferAnotherVolunteer } = true
hasPref _ _ = false

hasGenderPref :: OvernightGenderPreference -> Details -> Boolean
hasGenderPref Male { genderPref: Just Male } = true
hasGenderPref Female { genderPref: Just Female } = true
hasGenderPref _ _ = false

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.form [ className [ "ui form", onlyIf formError "error" ] ]
              [ RD.h3' [ RD.text state.title ]
              , RD.div [ className [ "required field ", onlyIf formError "error" ] ]
                       [ RD.label [ RP.htmlFor "volName" ]
                                  [ RD.text "Name" ]
                       , RD.input [ RP._type "text"
                                  , RP.autoFocus true
                                  , RP.value state.details.name
                                  , RP.onChange $ dispatch <<< SetName <<< unsafeEventValue
                                  ]
                                  []
                       ]
              , RD.div [ className [ "field " ] ]
                       [ RD.label' [ RD.text "Would you prefer to work with another volunteer?" ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-alone-2"
                                           , RP.name "pref-alone"
                                           , RP.checked $ hasPref PreferAnotherVolunteer state.details
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetPref (Just PreferAnotherVolunteer)
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-alone-2" ]
                                           [ renderPref PreferAnotherVolunteer 
                                           , RD.text "I prefer to work with a second volunteer"
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
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-alone-1" ]
                                           [ renderPref PreferToBeAlone
                                           , RD.text "I prefer to be on my own"
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
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-alone-none" ]
                                           [ RD.text "I don't mind"
                                           ]
                                ]
                       ]
              , RD.div [ className [ "field " ] ]
                       [ RD.label' [ RD.text "Who would you prefer to share the volunteers' room with?" ]
                       , RD.div [ RP.className "ui radio checkbox" ]
                                [ RD.input [ RP._type "radio"
                                           , RP._id "pref-gender-f"
                                           , RP.name "pref-gender"
                                           , RP.checked $ hasGenderPref Female state.details
                                           , RP.onChange \e -> do
                                               let checked = unsafeChecked e
                                               when checked $ dispatch $ SetGenderPref (Just Female)
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-gender-f" ]
                                           [ renderGenderPref Female
                                           , RD.text "Females only"
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
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-gender-m" ]
                                           [ renderGenderPref Male 
                                           , RD.text "Males only"
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
                                           ]
                                           [ ]
                                , RD.label [ RP.className "action-label"
                                           , RP.htmlFor "pref-gender-none" ]
                                           [ RD.text "I don't mind"
                                           ]
                                ]
                       ]
              , RD.div [ className [ "field " ] ]
                       [ RD.label [ RP.htmlFor "volNotes" ]
                                  [ RD.text "Any other preferences?"
                                  ]
                       , RD.input [ RP._type "text"
                                  , RP.value state.details.notes
                                  , RP.onChange $ dispatch <<< SetNotes <<< unsafeEventValue
                                  ]
                                  []
                       ]
               , RD.div [ RP.className "ui error message" ]
                        [ RD.div [ RP.className "header" ]
                                 [ RD.text "Please check these fields and try again:" ]
                        , RD.p' [ RD.text "Volunteer name should not be empty." ]
                        ]
              , RD.button [ RP.className "ui primary button"
                          , RP._type "submit"
                          , RP.disabled formError
                          , RP.onClick \e -> do
                              _ <- preventDefault e
                              dispatch $ if state.formValid then Save state.details else SetSubmitted
                          ]
                          [ RD.i [ RP.className "icon icon-ok" ] []
                          , RD.text "Save" ]
              , RD.button [ RP.className "ui button"
                          , RP.disabled formError
                          , RP.onClick \e -> do
                              _ <- preventDefault e
                              dispatch $ Cancel
                          ]
                          [ RD.i [ RP.className "icon icon-cancel" ] []
                          , RD.text "Cancel" ]
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
  performAction (SetNotes notes) _ _ = void $ T.modifyState \state ->
    let details' = state.details { notes = notes }
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
  performAction SetSubmitted _ _ = void $ T.modifyState \state -> state { formSubmitted = true }
  performAction _ _ _ = pure unit 

initialState :: Maybe Volunteer -> State
initialState currentVol =
  let defaultDetails = { name: ""
                       , notes: ""
                       , pref: Nothing
                       , genderPref: Nothing
                       }
      currentVolDetails (Volunteer cv) = { name: cv.vName
                             , notes: cv.vNotes
                             , pref: cv.vOvernightPreference
                             , genderPref: cv.vOvernightGenderPreference
                             }
      details = maybe defaultDetails currentVolDetails currentVol
  in { details
     , title: maybe "Add new volunteer" (\(Volunteer cv) -> cv.vName <> "'s details") currentVol
     , formValid: isValid details
     , formSubmitted: false
     }

isValid :: Details -> Boolean
isValid { name } | length name == 0 = false
isValid _ = true

renderGenderPref :: OvernightGenderPreference -> ReactElement
renderGenderPref Male   = RD.div [ RP.className "sharing-pref gender" 
                                 ] 
                                 [ RD.span' [ RD.text "M" ] ]
renderGenderPref Female = RD.div [ RP.className "sharing-pref gender" 
                                 ] 
                                 [ RD.span' [ RD.text "F" ] ]
renderPref :: OvernightPreference -> ReactElement
renderPref PreferToBeAlone        = RD.div [ RP.className "sharing-pref alone" 
                                           ] 
                                           [ RD.span' [ RD.text "1" ] ]
renderPref PreferAnotherVolunteer = RD.div [ RP.className "sharing-pref alone" 
                                           ] 
                                           [ RD.span' [ RD.text "2" ] ]
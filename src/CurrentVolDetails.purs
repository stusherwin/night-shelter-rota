module App.CurrentVolDetails (State, VolDetails, Action(..), spec, initialState) where

import Prelude

import App.Common (unsafeEventValue, className, onlyIf)
import App.Data (OvernightSharingPrefs(..), VolId(..), Volunteer(..), parseVolId)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Math (e)
import React (ReactElement, preventDefault)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type VolDetails = { name :: String 
                  , notes :: String
                  }

type State = { volDetails :: VolDetails
             , formValid :: Boolean
             , formSubmitted :: Boolean
             }
 
data Action = SetName String
            | SetNotes String
            | SetSubmitted
            | Save VolDetails
            | Cancel

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.form [ className [ "ui form", onlyIf formError "error" ] ]
              [ RD.div [ className [ "required field ", onlyIf formError "error" ] ]
                       [ RD.label [ RP.htmlFor "volName" ]
                                  [ RD.text "Name" ]
                       , RD.input [ RP._type "text"
                                  , RP.autoFocus true
                                  , RP.value state.volDetails.name
                                  , RP.onChange $ dispatch <<< SetName <<< unsafeEventValue
                                  ]
                                  []
                       ]
              , RD.div [ className [ "field " ] ]
                       [ RD.label [ RP.htmlFor "volNotes" ]
                                  [ RD.text "Notes"
                                  , RD.span [ RP.className "notes" ]
                                            [ RD.text "e.g. "
                                            , RD.b' [ RD.text "M" ]
                                            , RD.text " for Male only, "
                                            , RD.b' [ RD.text "F" ]
                                            , RD.text " for Female only, "
                                            , RD.b' [ RD.text "(1)" ]
                                            , RD.text " if you prefer to be on your own or "
                                            , RD.b' [ RD.text "(2)" ]
                                            , RD.text " if you prefer to share with another volunteer"
                                            ]
                                  ]
                       , RD.input [ RP._type "text"
                                  , RP.value state.volDetails.notes
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
                              dispatch $ if state.formValid then Save state.volDetails else SetSubmitted
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
  render _ _ _ _ = []
  
  performAction :: T.PerformAction _ State _ Action
  performAction (SetName name) _ _ = void $ T.modifyState \state ->
    let volDetails' = state.volDetails { name = name }
    in state { volDetails = volDetails'
             , formValid = isValid volDetails'
             }
  performAction (SetNotes notes) _ _ = void $ T.modifyState \state ->
    let volDetails' = state.volDetails { notes = notes }
    in state { volDetails = volDetails'
             , formValid = isValid volDetails'
             }
  performAction SetSubmitted _ _ = void $ T.modifyState \state -> state { formSubmitted = true }
  performAction _ _ _ = pure unit 

initialState :: Maybe Volunteer -> State
initialState currentVol =
  let volDetails = maybe { name: "", notes: "" } (\cv -> { name: cv.name, notes: notes cv }) currentVol
  in { volDetails: volDetails
     , formValid: isValid volDetails
     , formSubmitted: false
     }

isValid :: VolDetails -> Boolean
isValid { name } | length name == 0 = false
isValid _ = true

notes :: Volunteer -> String
notes { overnightSharingPrefs: (Custom n)} = n
notes _ = ""
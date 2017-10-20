module App.CurrentVolDetails (State, Action(..), spec, initialState, changeCurrentVol) where

import Prelude 

import App.Common (unsafeEventValue, className, onlyIf)
import App.Data (Volunteer(..), VolId(..), parseVolId)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Newtype (unwrap)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Math (e)
import React (ReactElement, preventDefault)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type State = { hasCurrentVol :: Boolean
             , name :: String
             , formValid :: Boolean
             , formSubmitted :: Boolean
             } 
 
data Action = UpdateName String
            | SetSubmitted
            | ChangeCurrentVolName String

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state@{ hasCurrentVol } _ | hasCurrentVol =
    [ RD.form [ className [ "ui form", onlyIf formError "error" ] ]
              [ RD.div [ className [ "required field ", onlyIf formError "error" ] ]
                       [ RD.label [ RP.htmlFor "volName" ]
                                  [ RD.text "Name" ]
                       , RD.input [ RP._type "text"
                                  , RP.value state.name
                                  , RP.onChange $ dispatch <<< UpdateName <<< unsafeEventValue
                                  ]
                                  []
                       , RD.div [ RP.className "ui error message" ]
                                [ RD.div [ RP.className "header" ]
                                         [ RD.text "Please check these fields and try again:" ]
                                , RD.p' [ RD.text "Volunteer name should not be empty." ]
                                ]
                       ]
              , RD.button [ RP.className "ui button"
                          , RP._type "submit"
                          , RP.disabled formError
                          , RP.onClick \e -> do
                              _ <- preventDefault e
                              dispatch $ if state.formValid then ChangeCurrentVolName state.name
                                                            else SetSubmitted
                          ]
                          [ RD.text "Save" ]
              ]
    ]
    where
    formError :: Boolean
    formError = state.formSubmitted && not state.formValid
  render _ _ _ _ = []

  
  
  performAction :: T.PerformAction _ State _ Action
  performAction (UpdateName name) _ _ = void $ T.modifyState \state -> state { name = name, formValid = length name > 0 }
  performAction SetSubmitted _ _ = void $ T.modifyState \state -> state { formSubmitted = true }
  performAction (ChangeCurrentVolName _) _ _ = void $ T.modifyState \state -> state { formSubmitted = false }
  performAction _ _ _ = pure unit 

initialState :: Maybe Volunteer -> State
initialState currentVol = { hasCurrentVol: isJust currentVol
                          , name: maybe "" (\(Vol v) -> v.name) currentVol
                          , formValid: true
                          , formSubmitted: false
                          }

changeCurrentVol :: Maybe Volunteer -> State -> State
changeCurrentVol currentVol state = state { hasCurrentVol = isJust currentVol
                                          , name = maybe "" (\(Vol v) -> v.name) currentVol
                                          }


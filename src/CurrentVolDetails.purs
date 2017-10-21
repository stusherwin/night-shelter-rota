module App.CurrentVolDetails (State, VolType(..), VolDetails, Action(..), spec, initialState, changeCurrentVol, defineNewVol) where

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

data VolType = CurrentVol
             | NewVol

type VolDetails = { name :: String 
                  , notes :: String
                  }

type State = { volType :: VolType
             , volDetails :: Maybe VolDetails
             , formValid :: Boolean
             , formSubmitted :: Boolean
             }
 
data Action = UpdateName String
            | UpdateNotes String
            | SetSubmitted
            | ChangeCurrentVolDetails VolDetails
            | CreateNewVol VolDetails

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state@{ volDetails: Just v } _ =
    [ RD.form [ className [ "ui form", onlyIf formError "error" ] ]
              [ RD.div [ className [ "required field ", onlyIf formError "error" ] ]
                       [ RD.label [ RP.htmlFor "volName" ]
                                  [ RD.text "Name" ]
                       , RD.input [ RP._type "text"
                                  , RP.value v.name
                                  , RP.onChange $ dispatch <<< UpdateName <<< unsafeEventValue
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
                                  , RP.value v.notes
                                  , RP.onChange $ dispatch <<< UpdateNotes <<< unsafeEventValue
                                  ]
                                  []
                       ]
               , RD.div [ RP.className "ui error message" ]
                        [ RD.div [ RP.className "header" ]
                                 [ RD.text "Please check these fields and try again:" ]
                        , RD.p' [ RD.text "Volunteer name should not be empty." ]
                        ]
              , RD.button [ RP.className "ui button"
                          , RP._type "submit"
                          , RP.disabled formError
                          , RP.onClick \e -> do
                              _ <- preventDefault e
                              dispatch $ case state.formValid, state.volType of
                                           true, CurrentVol -> ChangeCurrentVolDetails v
                                           true, NewVol     -> CreateNewVol v
                                           _, _             -> SetSubmitted
                          ]
                          [ RD.text "Save" ]
              ]
    ]
    where
    formError :: Boolean
    formError = state.formSubmitted && not state.formValid
  render _ _ _ _ = []
  
  performAction :: T.PerformAction _ State _ Action
  performAction (UpdateName name) _ _ = void $ T.modifyState \state ->
    let volDetails' = case state.volDetails of
                        Just d -> Just d{ name = name }
                        _ -> Nothing
    in state { volDetails = volDetails'
             , formValid = isValid volDetails'
             }
  performAction (UpdateNotes notes) _ _ = void $ T.modifyState \state ->
    let volDetails' = case state.volDetails of
                        Just d -> Just d{ notes = notes }
                        _ -> Nothing
    in state { volDetails = volDetails'
             , formValid = isValid volDetails'
             }
  performAction SetSubmitted _ _ = void $ T.modifyState \state -> state { formSubmitted = true }
  performAction _ _ _ = pure unit 
 
initialState :: Maybe Volunteer -> State 
initialState currentVol =
 let volDetails = Nothing
 in { volType: CurrentVol
    , volDetails: volDetails
    , formValid: isValid volDetails
    , formSubmitted: false
    }

changeCurrentVol :: Maybe Volunteer -> State -> State
changeCurrentVol currentVol state =
  let volDetails = (\v -> { name: v.name, notes: notes v }) <$> currentVol
  in state { volType = CurrentVol
           , volDetails = volDetails
           , formValid = isValid volDetails
           , formSubmitted = false
           }

defineNewVol :: State -> State
defineNewVol state =
  let volDetails = Just { name: "", notes: "" }
  in state { volType = NewVol
           , volDetails = volDetails
           , formValid = isValid volDetails
           , formSubmitted = false
           }

isValid :: Maybe VolDetails -> Boolean
isValid (Just { name }) | length name == 0 = false
isValid _ = true

notes :: Volunteer -> String
notes { overnightSharingPrefs: (Custom n)} = n
notes _ = ""
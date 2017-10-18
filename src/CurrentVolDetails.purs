module App.CurrentVolDetails (State, Action(..), spec, initialState, changeCurrentVol) where

import Prelude

import App.Common (unsafeEventValue)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Data (Volunteer(..), VolId(..), parseVolId)

type State = { hasCurrentVol :: Boolean
                              , name :: String
                              } 
 
data Action = UpdateName String
                             | ChangeCurrentVolName String

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    if state.hasCurrentVol then [ RD.h4 [ RP.className "ui dividing header" ] [ RD.text "Volunteer Details" ]
                                , RD.div [ RP.className "field" ]
                                         [ RD.label [ RP.htmlFor "volName" ]
                                                    [ RD.text "Name" ]
                                         , RD.input [ RP._type "text"
                                                    , RP.value state.name
                                                    , RP.onChange $ dispatch <<< UpdateName <<< unsafeEventValue
                                                    ]
                                                    []
                                         ]
                                , RD.button [ RP.className "ui button"
                                            , RP._type "submit"
                                            , RP.onClick $ const $ dispatch $ ChangeCurrentVolName state.name
                                            ]
                                            [ RD.text "Save" ]
                                ]
                                
                           else []
  
  performAction :: T.PerformAction _ State _ Action
  performAction (UpdateName name) _ _ = void $ T.modifyState \state -> state { name = name }
  performAction _ _ _ = pure unit 

initialState :: Maybe Volunteer -> State
initialState currentVol = { hasCurrentVol: isJust currentVol
                          , name: maybe "" (\(Vol v) -> v.name) currentVol
                          }

changeCurrentVol :: Maybe Volunteer -> State -> State
changeCurrentVol currentVol state = state { hasCurrentVol = isJust currentVol
                                          , name = maybe "" (\(Vol v) -> v.name) currentVol
                                          }

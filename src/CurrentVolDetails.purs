module App.CurrentVolDetails (CurrentVolDetailsState, CurrentVolDetailsAction(..), currentVolDetailsSpec, currentVolDetailsInitialState, changeCurrentVol') where

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

type CurrentVolDetailsState = { hasCurrentVol :: Boolean
                              , name :: String
                              } 
 
data CurrentVolDetailsAction = UpdateName String

currentVolDetailsSpec :: T.Spec _ CurrentVolDetailsState _ CurrentVolDetailsAction
currentVolDetailsSpec = T.simpleSpec performAction render
  where 
  render :: T.Render CurrentVolDetailsState _ CurrentVolDetailsAction
  render dispatch _ state _ =
    if state.hasCurrentVol then [ RD.div [ RP.className "ui form" ]
                                         [ RD.div [ RP.className "field" ]
                                                  [ RD.label [ RP.htmlFor "volName" ]
                                                             [ RD.text "Name" ]
                                                  , RD.input [ RP._type "text"
                                                             , RP.value state.name
                                                             , RP.onChange \e -> dispatch $ UpdateName $ unsafeEventValue e
                                                             ]
                                                             []
                                                  ]
                                         ]
                                ]
                           else []
  
  performAction :: T.PerformAction _ CurrentVolDetailsState _ CurrentVolDetailsAction
  performAction (UpdateName name) _ _ = void $ T.modifyState \state -> state { name = name }
  performAction _ _ _ = pure unit 

currentVolDetailsInitialState :: Maybe Volunteer -> CurrentVolDetailsState
currentVolDetailsInitialState currentVol = { hasCurrentVol: isJust currentVol
                                           , name: maybe "" (\(Vol v) -> v.name) currentVol
                                           }

changeCurrentVol' :: Maybe Volunteer -> CurrentVolDetailsState -> CurrentVolDetailsState
changeCurrentVol' currentVol state = state { hasCurrentVol = isJust currentVol
                                           , name = maybe "" (\(Vol v) -> v.name) currentVol
                                           }
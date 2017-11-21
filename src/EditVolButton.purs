module App.EditVolButton (State, Action, spec, initialState) where

import Prelude

import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM (render)
import ReactDOM as RDOM
import Thermite as T

type State = { currentVolName :: String }

type Action = Unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.button [ RP.className "ui primary button"
                , RP.style { float: "right" }
                , RP.onClick \e -> do
                    _ <- R.preventDefault e
                    dispatch unit
                ]
                [ RD.i [ RP.className "icon icon-edit" ] []
                , RD.text $ "Edit " <> state.currentVolName <> "'s details"
                ]
    ]
  render _ _ _ _ = []

initialState :: String -> State
initialState = { currentVolName: _ }
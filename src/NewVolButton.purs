module App.NewVolButton (State, Action, spec, initialState) where

import Prelude

import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM (render)
import ReactDOM as RDOM
import Thermite as T

type State = {}

type Action = Unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Action
  render dispatch _ _ _ =
    [ RD.button [ RP.className "ui button"
                , RP.style { float: "right" }
                , RP.onClick \e -> do
                    _ <- R.preventDefault e
                    dispatch unit
                ]
                [ RD.i [ RP.className "icon icon-add" ] []
                , RD.text "New volunteer"
                ]
    ]

initialState :: State
initialState = {}
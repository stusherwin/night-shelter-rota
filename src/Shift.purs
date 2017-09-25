module App.Shift (ShiftProps, ShiftState, VolunteerState(..), ShiftAction(..), shiftSpec) where

import Prelude

import App.Common (unsafeEventValue, toDateString)
import App.Data (Volunteer(..))
import Data.DateTime (Date)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type ShiftProps = { currentVolName :: Maybe String }

data VolunteerState = CurrentVol String
                    | OtherVol String

type ShiftState = { date :: Date
                  , vols :: Array VolunteerState }

data ShiftAction = AddCurrentVol Date

shiftSpec :: T.Spec _ (Tuple ShiftProps ShiftState) _ ShiftAction
shiftSpec = T.simpleSpec performAction render
  where
  render :: T.Render (Tuple ShiftProps ShiftState) _ ShiftAction
  render dispatch _ (Tuple props state) _ =
    [ RD.div [ RP.className "alert alert-primary mt-3" ]
            ([ RD.text $ toDateString state.date
            ] <> renderAdd dispatch state.date props.currentVolName
            <> [ RD.span [ RP.className "float-right" ]
                      (map renderVol state.vols)
             ])
    ]

  renderAdd :: _ -> Date -> Maybe String -> Array ReactElement
  renderAdd dispatch date (Just volName)
    = [ RD.a [ RP.onClick \_ -> dispatch (AddCurrentVol date)
           , RP.href "#"
           , RP.role "button"
           , RP.className "btn btn-primary btn-sm float-right"
           ]
           [ RD.text $ "Add " <> volName ]
      ] 
  renderAdd _ _ Nothing = [] 
  
  renderVol :: VolunteerState -> ReactElement
  renderVol (CurrentVol v) = RD.strong' [ RD.text v ]
  renderVol (OtherVol v)   = RD.text v

  performAction :: T.PerformAction _ (Tuple ShiftProps ShiftState) _ ShiftAction
  performAction _ _ _ = pure unit

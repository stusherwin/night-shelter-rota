module App.Shift (ShiftProps, ShiftState, VolunteerState(..), ShiftAction(..), shiftSpec) where

import Prelude

import App.Common (unsafeEventValue, toDateString)
import App.Data (Volunteer(..))
import Data.DateTime (Date)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type ShiftProps = {  }

type VolunteerState = { name :: String
                      , isCurrentVol :: Boolean
                      , isOvernight :: Boolean }

type ShiftState = { currentVolName :: Maybe String
                  , date :: Date
                  , vols :: Array VolunteerState
                  , canAddCurrentVol :: Boolean
                  }

data ShiftAction = AddOvernightVol Date
                 | AddEveningVol Date

shiftSpec :: T.Spec _ ShiftState _ ShiftAction
shiftSpec = T.simpleSpec performAction render
  where
  render :: T.Render ShiftState _ ShiftAction
  render dispatch _ state _ =
    [ RD.div [ RP.className "alert alert-primary mt-3" ]
             (  [ RD.text $ toDateString state.date ]
             <> renderAdd dispatch state
             -- <> renderBtn dispatch state
             <> map renderVol state.vols
             )
    ]

  renderAdd :: _ -> ShiftState -> Array ReactElement
  renderAdd dispatch state@{ currentVolName: (Just currentVolName) } | state.canAddCurrentVol =
    [
    -- = [ RD.div [ RP.className "mini ui buttons"
    --            ]
    --            [ RD.a [ RP.className "mini ui icon button"
    --                   ]
    --                   [ RD.i [ RP.className "icon-no-bed" ] []
    --                   ]
    --            , RD.span [ RP.className "mini ui basic button"
    --                      ]
    --                      [ RD.text volName
    --                      ]
    --            , RD.a [ RP.className "mini ui icon button"
    --                   ]
    --                   [ RD.i [ RP.className "icon-ok" ] []
    --                   ]
    --            ]
    --   ]

       RD.a [ RP.onClick \_ -> dispatch (AddEveningVol state.date)
             , RP.href "#"
             , RP.role "button"
            --  , RP.className $ "mini ui primary button right floated"
             , RP.className $ "btn btn-small btn-primary float-right"
             ] 
             [ RD.i [ RP.className "icon-no-bed" ] []
             ] --, RD.text $ "Evening only" ]
      , RD.a [ RP.onClick \_ -> dispatch (AddOvernightVol state.date) 
             , RP.href "#"
             , RP.role "button"
            --  , RP.className $ "mini ui primary button right floated"
             , RP.className $ "btn btn-small btn-primary float-right"
             ]
             [ RD.i [ RP.className "icon-bed" ] []
             ] --, RD.text $ "Overnight" ]
      ] 
  renderAdd _ _ = []

  renderVol :: VolunteerState -> ReactElement
  renderVol v | v.isCurrentVol =
    RD.span [ RP.className "float-right mr-1" ]
            [ RD.i [ RP.className $ if v.isOvernight then "icon-bed mr-1" else "icon-no-bed mr-1" ]
                   []
            , RD.strong' [ RD.text v.name ]
            ]
  renderVol v =
    RD.span [ RP.className "float-right mr-1" ]
            [ RD.i [ RP.className $ if v.isOvernight then "icon-bed mr-1" else "icon-no-bed mr-1" ]
                   []
            , RD.text v.name
            ]

  volText :: VolunteerState -> ReactElement
  volText v = RD.text $ v.name <> if v.isOvernight then " (overnight)" else " (evening)"

  performAction :: T.PerformAction _ ShiftState _ ShiftAction
  performAction _ _ _ = pure unit
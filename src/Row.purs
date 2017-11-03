module App.Row (State(..), StartRowState, Action(..), HeaderRowAction(..), spec) where

import Prelude

import Data.Lens (Lens', lens, Prism', prism, over)
import Data.Either (Either(..))
import React (ReactElement)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.ShiftRow (CurrentVolState, OtherVolState, Action(..), State(..), ShiftStatus(..), ShiftType(..), spec) as SR

data HeaderRowAction = PrevPeriod
                     | NextPeriod

data Action = ShiftRowAction SR.Action
            | HeaderRowAction HeaderRowAction

type StartRowState = { monthName :: String
                     , loading :: Boolean
                     }

data State = ShiftRow SR.State
           | StartRow StartRowState
           | MonthHeaderRow String
           | EndRow

_HeaderRowAction :: Prism' Action HeaderRowAction
_HeaderRowAction = prism HeaderRowAction unwrap
  where
  unwrap (HeaderRowAction a) = Right a
  unwrap ra = Left ra

_ShiftRowAction :: Prism' Action SR.Action
_ShiftRowAction = prism ShiftRowAction unwrap
  where
  unwrap (ShiftRowAction a) = Right a
  unwrap ra = Left ra

_StartRow :: Prism' State StartRowState
_StartRow = prism StartRow unwrap
  where
  unwrap (StartRow s) = Right s
  unwrap r = Left r

_MonthHeaderRow :: Prism' State String
_MonthHeaderRow = prism MonthHeaderRow unwrap
  where
  unwrap (MonthHeaderRow s) = Right s
  unwrap r = Left r

_EndRow :: Prism' State Unit
_EndRow = prism (const EndRow) unwrap
  where
  unwrap EndRow = Right unit
  unwrap r = Left r

_ShiftRow :: Prism' State SR.State
_ShiftRow = prism ShiftRow unwrap
  where
  unwrap (ShiftRow s) = Right s
  unwrap r = Left r

spec :: forall props eff. T.Spec eff State props Action
spec = 
  (  T.split _StartRow (T.match _HeaderRowAction startRow)
  <> T.split _MonthHeaderRow (T.match _HeaderRowAction monthHeaderRow)
  <> T.split _ShiftRow (T.match _ShiftRowAction SR.spec)
  <> T.split _EndRow (T.match _HeaderRowAction endRow)
  )

  where
  startRow :: T.Spec _ StartRowState _ HeaderRowAction
  startRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render StartRowState _ HeaderRowAction
    render dispatch _ state _ = [ RD.tr [ RP.className "month-header-row" ]
                                        [ RD.td [ RP.colSpan 9 ]
                                                [ RD.text state.monthName
                                                , ( if state.loading
                                                      then RD.i [ RP.className "icon-spin animate-spin loading float-right"
                                                                ]
                                                                []
                                                      else RD.a [ RP.href "#"
                                                                , RP.className "action float-right"
                                                                , RP.onClick \_ -> dispatch PrevPeriod
                                                                ]
                                                                [ RD.i [ RP.className "icon-up-open"] []
                                                                , RD.span' [ RD.text "previous 4 weeks" ]
                                                                ]
                                                  )
                                                ]
                                        ]
                                ]
  
  monthHeaderRow :: T.Spec _ String _ HeaderRowAction
  monthHeaderRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render String _ HeaderRowAction
    render dispatch _ text _ = [ RD.tr [ RP.className "month-header-row" ]
                                       [ RD.td [ RP.colSpan 9 ]
                                               [ RD.text text
                                               ]
                                       ]
                               ]
   
  endRow :: T.Spec _ Unit _ HeaderRowAction
  endRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render Unit _ HeaderRowAction
    render dispatch _ state _ = [ RD.tr [ RP.className "month-header-row" ]
                                        [ RD.td [ RP.colSpan 9 ]
                                                [ RD.a [ RP.href "#"
                                                       , RP.className "action float-right"
                                                       , RP.onClick \_ -> dispatch NextPeriod
                                                       ]
                                                       [ RD.i [ RP.className "icon-down-open"] []
                                                       , RD.span' [ RD.text "next 4 weeks" ]
                                                       ]
                                                ]
                                        ]
                                ]
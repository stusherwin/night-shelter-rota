module App.Row (State(..), HeaderState, Action(..), HeaderRowAction(..), spec) where

import Prelude
 
import Data.Lens (Prism', prism)
import Data.Either (Either(..))
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.ShiftRow (Action, State, spec) as SR

data HeaderRowAction = PrevPeriod
                     | NextPeriod

data Action = ShiftRowAction SR.Action
            | HeaderRowAction HeaderRowAction

type HeaderState = { text :: String 
                   }

data State = ShiftRow SR.State
           | StartRow HeaderState
           | MonthHeaderRow HeaderState
           | EndRow HeaderState

noOfCols :: Int
noOfCols = 8

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

_StartRow :: Prism' State HeaderState
_StartRow = prism StartRow unwrap
  where
  unwrap (StartRow s) = Right s
  unwrap r = Left r

_MonthHeaderRow :: Prism' State HeaderState
_MonthHeaderRow = prism MonthHeaderRow unwrap
  where
  unwrap (MonthHeaderRow s) = Right s
  unwrap r = Left r

_EndRow :: Prism' State HeaderState
_EndRow = prism EndRow unwrap
  where
  unwrap (EndRow s) = Right s
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
  startRow :: T.Spec _ HeaderState _ HeaderRowAction
  startRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderState _ HeaderRowAction
    render dispatch _ state _ = [ RD.tr [ RP.className "month-header-row" ]
                                        [ RD.td [ RP.colSpan noOfCols ]
                                                [ RD.text state.text
                                                , RD.a [ RP.href "#"
                                                       , RP.className "action"
                                                       , RP.onClick \_ -> dispatch NextPeriod
                                                       ]
                                                       [ RD.span' [ RD.text "next 4 weeks" ]
                                                       , RD.i [ RP.className "icon-right-open"] []
                                                       ]
                                                , RD.a [ RP.href "#"
                                                       , RP.className "action"
                                                       , RP.onClick \_ -> dispatch PrevPeriod
                                                       ]
                                                       [ RD.i [ RP.className "icon-left-open"] []
                                                       , RD.span' [ RD.text "previous 4 weeks" ]
                                                       ]
                                                ]
                                        ]
                                ]
  
  monthHeaderRow :: T.Spec _ HeaderState _ HeaderRowAction
  monthHeaderRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderState _ HeaderRowAction
    render dispatch _ state _ = [ RD.tr [ RP.className "month-header-row" ]
                                        [ RD.td [ RP.colSpan noOfCols ]
                                                [ RD.text state.text
                                                ]
                                        ]
                                ]
   
  endRow :: T.Spec _ HeaderState _ HeaderRowAction
  endRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderState _ HeaderRowAction
    render dispatch _ state _ = [ RD.tr [ RP.className "month-header-row" ]
                                        [ RD.td [ RP.colSpan noOfCols ]
                                                [ RD.a [ RP.href "#"
                                                       , RP.className "action"
                                                       , RP.onClick \_ -> dispatch NextPeriod
                                                       ]
                                                       [ RD.span' [ RD.text "next 4 weeks" ]
                                                       , RD.i [ RP.className "icon-right-open"] []
                                                       ]
                                                , RD.a [ RP.href "#"
                                                       , RP.className "action"
                                                       , RP.onClick \_ -> dispatch PrevPeriod
                                                       ]
                                                       [ RD.i [ RP.className "icon-left-open"] []
                                                       , RD.span' [ RD.text "previous 4 weeks" ]
                                                       ]
                                                ]
                                        ]
                                ]
module App.Row (spec) where

import Prelude
 
import Data.Lens (Prism', prism)
import Data.Either (Either(..))
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.ShiftRow (spec) as SR
import ShiftListState

noOfCols :: Int
noOfCols = 8

_StartRow :: Prism' RowState HeaderRowState
_StartRow = prism StartRow unwrap
  where
  unwrap (StartRow s) = Right s
  unwrap r = Left r

_MonthHeaderRow :: Prism' RowState HeaderRowState
_MonthHeaderRow = prism MonthHeaderRow unwrap
  where
  unwrap (MonthHeaderRow s) = Right s
  unwrap r = Left r

_EndRow :: Prism' RowState HeaderRowState
_EndRow = prism EndRow unwrap
  where
  unwrap (EndRow s) = Right s
  unwrap r = Left r

_ShiftRow :: Prism' RowState ShiftRowState
_ShiftRow = prism ShiftRow unwrap
  where
  unwrap (ShiftRow s) = Right s
  unwrap r = Left r

spec :: forall props eff. T.Spec eff RowState props RowAction
spec = 
  (  T.split _StartRow startRow
  <> T.split _MonthHeaderRow monthHeaderRow
  <> T.split _ShiftRow SR.spec
  <> T.split _EndRow endRow
  )

  where
  startRow :: T.Spec _ HeaderRowState _ RowAction
  startRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderRowState _ RowAction
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
  
  monthHeaderRow :: T.Spec _ HeaderRowState _ RowAction
  monthHeaderRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderRowState _ RowAction
    render dispatch _ state _ = [ RD.tr [ RP.className "month-header-row" ]
                                        [ RD.td [ RP.colSpan noOfCols ]
                                                [ RD.text state.text
                                                ]
                                        ]
                                ]
   
  endRow :: T.Spec _ HeaderRowState _ RowAction
  endRow = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderRowState _ RowAction
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
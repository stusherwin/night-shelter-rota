module App.Row (spec) where

import Prelude
 
import Data.Lens (Prism', prism)
import Data.Either (Either(..))
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
import React as R

import App.ShiftRow (spec) as SR
import ShiftListState


_HeaderRow :: Prism' RowState HeaderRowState
_HeaderRow = prism HeaderRow unwrap
  where
  unwrap (HeaderRow s) = Right s
  unwrap r = Left r

_ShiftRow :: Prism' RowState ShiftRowState
_ShiftRow = prism ShiftRow unwrap
  where
  unwrap (ShiftRow s) = Right s
  unwrap r = Left r

spec :: forall props eff. T.Spec eff RowState props RowAction
spec = T.split _HeaderRow headerRowSpec
    <> T.split _ShiftRow SR.spec

  where
  headerRowSpec :: T.Spec _ HeaderRowState _ RowAction
  headerRowSpec = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render HeaderRowState _ RowAction
    render dispatch _ state _ = [ RD.div [ RP.className "row row-header" ]
                                         $ actions <> [ RD.text state.text ]
                                ]
      where
      actions :: Array R.ReactElement
      actions = if state.showActions then
                  [ RD.div [ RP.className "row-header-actions" ]
                           [ RD.a [ RP.href "#"
                                  , RP.className "action"
                                  , RP.onClick \_ -> dispatch PrevPeriod
                                  ]
                                  [ RD.i [ RP.className "icon-left-open"] []
                                  , RD.span' [ RD.text "previous 4 weeks" ]
                                  ]
                           , RD.a [ RP.href "#"
                                  , RP.className "action"
                                  , RP.onClick \_ -> dispatch NextPeriod
                                  ]
                                  [ RD.span' [ RD.text "next 4 weeks" ]
                                  , RD.i [ RP.className "icon-right-open"] []
                                  ]
                           ]
                  ]
                else
                  []
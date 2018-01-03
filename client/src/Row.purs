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
      actions = [ RD.div [ RP.className "row-header-actions" ]
                         $ if state.showPrev then
                             [ RD.a [ RP.href "#"
                                    , RP.className "action"
                                    , RP.onClick \e -> do
                                       _ <- R.preventDefault e
                                       dispatch PrevPeriod
                                    ]
                                    [ RD.i [ RP.className "icon-up-open"] []
                                    , RD.span' [ RD.text "previous 4 weeks" ]
                                    ]
                             ]
                           else []
                         <> if state.showNext then 
                             [ RD.a [ RP.href "#"
                                    , RP.className "action"
                                    , RP.onClick \e -> do
                                       _ <- R.preventDefault e
                                       dispatch NextPeriod
                                    ]
                                    [ RD.i [ RP.className "icon-down-open"] []
                                    , RD.span' [ RD.text "next 4 weeks" ]
                                    ]
                             ]
                            else []
                ]
module App.ShiftList where

import Prelude 

import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List, snoc, last) as L
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import Thermite as T
import React as R
import React.DOM as RD
import React.DOM.Props as RP

import App.Common (Volunteer)
import App.Shift (ShiftState, ShiftAction, shiftSpec, buildShifts, tomorrow)
  
data ShiftListAction = AddShift
                     | ShiftAction Int ShiftAction
 
type ShiftListState = { shifts :: L.List ShiftState
                      , currentVolunteer :: Maybe Volunteer
                      , currentDate :: Date }

_shifts :: Lens' ShiftListState (L.List ShiftState)
_shifts = lens _.shifts (_ { shifts = _ })

_ShiftAction :: Prism' ShiftListAction (Tuple Int ShiftAction)
_ShiftAction = prism (uncurry ShiftAction) \ta ->
  case ta of 
    ShiftAction i a -> Right (Tuple i a)
    _ -> Left ta 

shiftListSpec :: forall props eff. T.Spec eff ShiftListState props ShiftListAction
shiftListSpec = 
  (T.focus _shifts _ShiftAction (T.foreach \_ -> shiftSpec))
  <> footerSpec
  where
  footerSpec :: T.Spec _ ShiftListState _ ShiftListAction
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render ShiftListState _ ShiftListAction
    render dispatch _ state _ =
      [ RD.a [ RP.onClick \_ -> dispatch AddShift
            , RP.href "#"
            , RP.role "button"
            , RP.className "btn btn-primary"
            ]
            [ RD.text "Add shift" ]
      ]

    performAction :: T.PerformAction _ ShiftListState _ ShiftListAction
    performAction AddShift _ _ = void $ T.modifyState \state -> state { shifts = L.snoc state.shifts {shift:(tomorrow $ _.shift $ unsafePartial $ fromJust $ L.last state.shifts)} }
    performAction _ _ _ = pure unit
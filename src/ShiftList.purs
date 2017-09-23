module App.ShiftList (ShiftListProps, ShiftListAction, ShiftListState, shiftListSpec, shiftListInitialState) where

import Prelude

import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..), uncurry, snd)
import Partial.Unsafe (unsafePartial)
import Thermite as T
import React as R
import React.DOM as RD
import React.DOM.Props as RP

import App.Shift (ShiftProps, ShiftState, ShiftAction, shiftSpec, tomorrow)
import App.Data (Shift, Volunteer)
  
data ShiftListAction = AddShift
                     | ShiftAction Int ShiftAction

type ShiftListProps = { currentVolunteer :: Maybe Volunteer
                      , shifts :: Array Shift
                      }

type ShiftListState = { shifts :: L.List ShiftState
                      , currentDate :: Date }
 
_shifts :: Lens' (Tuple ShiftListProps ShiftListState) (L.List (Tuple ShiftProps ShiftState))
_shifts = lens getter setter 
  where 
  getter :: Tuple ShiftListProps ShiftListState -> L.List (Tuple ShiftProps ShiftState)
  getter (Tuple props state) = map (Tuple {currentVolunteer: props.currentVolunteer}) state.shifts

  setter :: Tuple ShiftListProps ShiftListState -> L.List (Tuple ShiftProps ShiftState) -> Tuple ShiftListProps ShiftListState
  setter (Tuple props state) shifts = Tuple props state{ shifts = map snd shifts }

_ShiftAction :: Prism' ShiftListAction (Tuple Int ShiftAction)
_ShiftAction = prism (uncurry ShiftAction) \ta ->
  case ta of
    ShiftAction i a -> Right (Tuple i a)
    _ -> Left ta 

shiftListSpec :: forall props eff. T.Spec eff (Tuple ShiftListProps ShiftListState) props ShiftListAction
shiftListSpec = 
  (T.focus _shifts _ShiftAction (T.foreach \_ -> shiftSpec))
  <> footerSpec
  where
  footerSpec :: T.Spec _ (Tuple ShiftListProps ShiftListState) _ ShiftListAction
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render (Tuple ShiftListProps ShiftListState) _ ShiftListAction
    render dispatch _ (Tuple props state) _ =
      [ RD.a [ RP.onClick \_ -> dispatch AddShift
            , RP.href "#"
            , RP.role "button"
            , RP.className "btn btn-primary"
            ]
            [ RD.text "Add shift" ]
      ]

    performAction :: T.PerformAction _ (Tuple ShiftListProps ShiftListState) _ ShiftListAction
    performAction _ _ _ = pure unit

shiftListInitialState :: ShiftListProps -> Date -> ShiftListState
shiftListInitialState props currentDate = 
  { shifts: buildShifts currentDate 7
  , currentDate: currentDate }
  where
  buildShifts :: Date -> Int -> L.List ShiftState
  buildShifts date 0 = L.Nil
  buildShifts date n = L.Cons { shift: date, volunteers: ["Stu"] } $ buildShifts (tomorrow date) (n - 1)
module App.ShiftList (ShiftListProps, ShiftListAction(..), ShiftListState, shiftListSpec, shiftListInitialState, changeCurrentVol) where

import Prelude

import App.Common (lensOfListWithProps, tomorrow)
import App.Data (Shift(..), Volunteer(..), VolunteerShift(..), canAddVolunteer, addVolunteer, hasId, hasDate, hasVolWithId)
import App.Shift (ShiftAction(..), ShiftProps, ShiftState(..), VolunteerState(..), shiftSpec)
import Data.Array (find)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (Maybe(..), fromJust, maybe, isJust)
import Data.Tuple (Tuple(..), uncurry, fst, snd)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T 
data ShiftListAction = AddShift
                     | ShiftAction Int ShiftAction

type ShiftListProps = { 
                      }

type ShiftListState = { currentVol :: Maybe Volunteer
                      , shifts :: Array Shift
                      , shiftRows :: L.List ShiftState
                      , currentDate :: Date }

_shiftRows :: Lens' ShiftListState (L.List ShiftState)
_shiftRows = lens (\s -> s.shiftRows)
                  (\s a -> s{shiftRows = a})

_ShiftAction :: Prism' ShiftListAction (Tuple Int ShiftAction)
_ShiftAction = prism (uncurry ShiftAction) \ta ->
  case ta of
    ShiftAction i a -> Right (Tuple i a)
    _ -> Left ta 

shiftListSpec :: forall props eff. T.Spec eff ShiftListState props ShiftListAction
shiftListSpec = 
  (T.focus _shiftRows _ShiftAction (T.foreach \_ -> shiftSpec))
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
    performAction (ShiftAction _ (AddOvernightVol shiftDate)) _ _ = void $ T.modifyState \state -> maybe state (\v -> modifyShifts (addVolunteer shiftDate (Overnight v)) state) state.currentVol
    performAction (ShiftAction _ (AddEveningVol shiftDate))   _ _ = void $ T.modifyState \state -> maybe state (\v -> modifyShifts (addVolunteer shiftDate (Evening v)) state) state.currentVol
    performAction _ _ _ = pure unit

shiftListInitialState :: Maybe Volunteer -> Array Shift -> Date -> ShiftListState
shiftListInitialState currentVol shifts currentDate = 
  { currentVol: currentVol
  , shifts: shifts
  , shiftRows: buildShifts currentVol shifts currentDate 7
  , currentDate: currentDate }

buildShifts :: Maybe Volunteer -> Array Shift -> Date -> Int -> L.List ShiftState
buildShifts _ _ _ 0 = L.Nil
buildShifts currentVol shifts date n = L.Cons (buildShift date) $ buildShifts currentVol shifts (tomorrow date) (n - 1)
  where 
  buildShift :: Date -> ShiftState
  buildShift date = case find (hasDate date) shifts of
    (Just s@(Shift shift)) -> { currentVolName : map (\(Vol v) -> v.name) currentVol
                              , date: shift.date 
                              , vols: map buildVol shift.volunteers 
                              , canAddCurrentVol: canAddVolunteer s currentVol
                              } 
    Nothing          -> { currentVolName : map (\(Vol v) -> v.name) currentVol
                        , date: date
                        , vols: []
                        , canAddCurrentVol: isJust currentVol
                        }
 
  buildVol :: VolunteerShift -> VolunteerState
  buildVol (Overnight (Vol v)) = { name: v.name
                                 , isCurrentVol: maybe false (hasId v.id) currentVol
                                 , isOvernight: true } 
  buildVol (Evening (Vol v)) = { name: v.name
                               , isCurrentVol: maybe false (hasId v.id) currentVol
                               , isOvernight: false }

changeCurrentVol :: Maybe Volunteer -> ShiftListState -> ShiftListState
changeCurrentVol currentVol state =
  state { currentVol = currentVol
        , shiftRows = buildShifts currentVol state.shifts state.currentDate 7
        }

modifyShifts :: (Array Shift -> Array Shift) -> ShiftListState -> ShiftListState
modifyShifts modify state =
  let shifts = modify state.shifts
  in state { shifts = shifts
           , shiftRows = buildShifts state.currentVol shifts state.currentDate 7
           }
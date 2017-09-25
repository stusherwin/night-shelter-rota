module App.ShiftList (ShiftListProps, ShiftListAction(..), ShiftListState, shiftListSpec, shiftListInitialState) where

import Prelude

import App.Common (lensOfListWithProps, tomorrow)
import App.Data (Shift(..), Volunteer(..))
import App.Shift (ShiftAction, ShiftProps, ShiftState(..), VolunteerState(..), shiftSpec)
import Data.Array (find)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..), uncurry, fst, snd)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T 
data ShiftListAction = AddShift
                     | ShiftAction Int ShiftAction

type ShiftListProps = { currentVol :: Maybe Volunteer
                      , shifts :: Array Shift
                      }

type ShiftListState = { shiftRows :: L.List ShiftState
                      , currentDate :: Date }

_shifts :: Lens' (Tuple ShiftListProps ShiftListState) (L.List (Tuple ShiftProps ShiftState))
_shifts = lensOfListWithProps (\(Tuple p s) -> buildShifts p s.currentDate 7)
                              (\(Tuple p s) a -> Tuple p s{shiftRows = a})
                              (\(Tuple p _) -> {currentVolName: map (\(V v) -> v.name) p.currentVol})

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
  { shiftRows: buildShifts props currentDate 7
  , currentDate: currentDate }

buildShifts :: ShiftListProps -> Date -> Int -> L.List ShiftState
buildShifts _ _ 0 = L.Nil
buildShifts props date n = L.Cons (buildShift date) $ buildShifts props (tomorrow date) (n - 1)
  where
  buildShift :: Date -> ShiftState
  buildShift date = case find (\(S s) -> s.date == date) props.shifts of
    (Just (S shift)) -> { date: shift.date, vols: map (buildVol props.currentVol) shift.volunteers }
    Nothing          -> { date: date,       vols: [] }
 
  buildVol :: Maybe Volunteer -> Volunteer -> VolunteerState
  buildVol (Just (V cv)) (V v) | cv.id == v.id = CurrentVol v.name
  buildVol _ (V v)                             = OtherVol v.name
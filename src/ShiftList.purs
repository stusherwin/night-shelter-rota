module App.ShiftList (ShiftListProps, ShiftListAction(..), ShiftListState, shiftListSpec, shiftListInitialState, changeCurrentVol) where

import Prelude

import App.Common (lensOfListWithProps, tomorrow) 
import App.Data (Shift(..), Volunteer(..), VolunteerShift(..), RuleResult(..), canAddVolunteer, addVolunteerShift, changeVolunteerShift, removeVolunteerShift, hasId, hasDate, hasVolWithId, validate) as D 
import App.Shift (CurrentVolState, OtherVolState, ShiftAction(..), ShiftProps, ShiftState(..), ShiftStatus(..), ShiftType(..), shiftSpec)
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Array (find, filter, (!!), sortWith, length, take)
import Data.Date (diff)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (Maybe(..), fromJust, maybe, isJust)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), uncurry, fst, snd)
import Math (e)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
data ShiftListAction = AddShift
                     | ShiftAction Int ShiftAction

type ShiftListProps = { 
                      }

type ShiftListState = { currentVol :: Maybe D.Volunteer
                      , shifts :: Array D.Shift
                      , shiftRows :: L.List ShiftState
                      , currentDate :: Date
                      }

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
  (table $ T.focus _shiftRows _ShiftAction $ T.foreach \_ -> shiftSpec)
  <> footerSpec
  where
  table :: T.Spec eff ShiftListState props ShiftListAction -> T.Spec eff ShiftListState props ShiftListAction
  table = over T._render \render d p s c ->
    [ RD.table [ RP.className "ui structured unstackable table" ]
               [ 
                 RD.thead' [ RD.tr' ([ RD.th [ RP.colSpan 1
                                             , RP.className ""
                                             ]
                                             [ RD.text "" ]
                                     , RD.th [ RP.colSpan 2
                                             , RP.className "left-border"
                                             ]
                                             [ RD.text "Shift" ]
                                     ] <> case s.currentVol of
                                            (Just (D.Vol v)) ->           
                                              [ RD.th [ RP.colSpan 2
                                                      , RP.className "left-border collapsing"
                                                      ]
                                                      [ RD.text "Other volunteers" ]
                                              , RD.th' []
                                              , RD.th [ RP.colSpan 2
                                                      , RP.className "left-border right aligned collapsing"
                                                      ]
                                                      [ RD.text $ v.name <> "'s shifts" ]
                                              ]
                                            _ ->
                                              [ RD.th [ RP.colSpan 2
                                                      , RP.className "left-border collapsing"
                                                      ]
                                                      [ RD.text "Volunteers" ]
                                              , RD.th' []
                                              , RD.th' []
                                              ]
                                    )
                           ]
               , 
               RD.tbody' $ render d p s c
               ]
    ]
 
  footerSpec :: T.Spec _ ShiftListState _ ShiftListAction
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render ShiftListState _ ShiftListAction
    render dispatch _ state _ = []
 
    performAction :: T.PerformAction _ ShiftListState _ ShiftListAction
    performAction (ShiftAction _ (AddCurrentVol shiftDate Overnight))             _ { currentVol: Just cv } = void $ T.modifyState \state -> modifyShifts state $ D.addVolunteerShift    shiftDate (D.Overnight cv)
    performAction (ShiftAction _ (AddCurrentVol shiftDate Evening))               _ { currentVol: Just cv } = void $ T.modifyState \state -> modifyShifts state $ D.addVolunteerShift    shiftDate (D.Evening cv)
    performAction (ShiftAction _ (ChangeCurrentVolShiftType shiftDate Overnight)) _ { currentVol: Just cv } = void $ T.modifyState \state -> modifyShifts state $ D.changeVolunteerShift shiftDate (D.Overnight cv)
    performAction (ShiftAction _ (ChangeCurrentVolShiftType shiftDate Evening))   _ { currentVol: Just cv } = void $ T.modifyState \state -> modifyShifts state $ D.changeVolunteerShift shiftDate (D.Evening cv)
    performAction (ShiftAction _ (RemoveCurrentVol shiftDate))                    _ { currentVol: Just cv } = void $ T.modifyState \state -> modifyShifts state $ D.removeVolunteerShift shiftDate cv
    performAction _ _ _ = pure unit

shiftListInitialState :: Maybe D.Volunteer -> Array D.Shift -> Date -> ShiftListState
shiftListInitialState currentVol shifts currentDate = 
  { currentVol: currentVol
  , shifts: shifts
  , shiftRows: buildShifts currentVol shifts currentDate
  , currentDate: currentDate }

buildShifts :: Maybe D.Volunteer -> Array D.Shift -> Date -> L.List ShiftState
buildShifts currentVol shifts startDate = buildShifts' currentVol shifts startDate 28
  where 
  buildShifts' :: Maybe D.Volunteer -> Array D.Shift -> Date -> Int -> L.List ShiftState
  buildShifts' _ _ _ 0 = L.Nil
  buildShifts' currentVol shifts date n = L.Cons (buildShift currentVol date) $ buildShifts' currentVol shifts (tomorrow date) (n - 1)

  buildShift :: Maybe D.Volunteer -> Date -> ShiftState
  buildShift Nothing date = case find (D.hasDate date) shifts of
    Just s@(D.Shift shift) -> 
      let otherVols = sortWith _.name $ map buildVol shift.volunteers
          noOfVols  = length shift.volunteers 
      in { date: shift.date 
         , noOfVols: noOfVols
         , status: status (Right s) startDate
         , currentVol: Nothing
         , otherVol1: otherVols !! 0
         , otherVol2: otherVols !! 1
         } 
    _ ->
      { date: date
      , noOfVols: 0
      , status: status (Left date) startDate
      , currentVol: Nothing
      , otherVol1: Nothing
      , otherVol2: Nothing
      }
  buildShift (Just cv@(D.Vol v)) date = case find (D.hasDate date) shifts of
    Just s@(D.Shift shift) -> 
      let otherVols = sortWith _.name $ map buildVol $ filter (not <<< D.hasVolWithId $ v.id) shift.volunteers
          noOfVols  = length shift.volunteers
      in { date: shift.date 
         , noOfVols: length shift.volunteers
         , status: status (Right s) startDate
         , currentVol: Just { name: v.name 
                            , shiftType: currentVolShiftType cv shift.volunteers
                            , canAddOvernight: D.canAddVolunteer (D.Overnight cv) (Right s)
                            , canAddEvening: D.canAddVolunteer (D.Evening cv) (Right s)
                            }
         , otherVol1: otherVols !! 0
         , otherVol2: otherVols !! 1
         } 
    _ -> 
      { date: date
      , noOfVols: 0
      , status: status (Left date) startDate
      , currentVol: Just { name: v.name
                         , shiftType: Nothing
                         , canAddOvernight: true
                         , canAddEvening: true
                         }
      , otherVol1: Nothing
      , otherVol2: Nothing
      }

status :: Either Date D.Shift -> Date -> ShiftStatus
status s date =
  case take 1 $ D.validate s date of
    [D.Error e]   -> Error e
    [D.Warning w] -> Warning w
    [D.Neutral]   -> OK
    _             -> Good

currentVolShiftType :: D.Volunteer -> Array D.VolunteerShift -> Maybe ShiftType
currentVolShiftType (D.Vol v) vols = 
  find (D.hasVolWithId v.id) vols >>= case _ of
    D.Overnight _ -> Just Overnight
    D.Evening   _ -> Just Evening

buildVol :: D.VolunteerShift -> OtherVolState
buildVol (D.Overnight (D.Vol v)) = { name: v.name
                                   , shiftType: Overnight } 
buildVol (D.Evening (D.Vol v)) = { name: v.name
                                 , shiftType: Evening }

changeCurrentVol :: Maybe D.Volunteer -> ShiftListState -> ShiftListState
changeCurrentVol currentVol state =
  state { currentVol = currentVol
        , shiftRows = buildShifts currentVol state.shifts state.currentDate
        }

modifyShifts :: ShiftListState -> (Array D.Shift -> Array D.Shift) -> ShiftListState
modifyShifts state modify =
  let shifts = modify state.shifts
  in state { shifts = shifts
           , shiftRows = buildShifts state.currentVol shifts state.currentDate
           }



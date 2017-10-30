module Test.Main where

import Prelude

import Data.Array (all, toUnfoldable, catMaybes)
import Data.Date (Date, canonicalDate)
import Data.Enum (toEnum)
import Data.List (find, singleton)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert', ASSERT)
import Control.Monad.Eff (Eff)

import App.Commonimport App.Common
import App.Data
import App.Main as M
import App.CurrentVolSelector as CVS
import App.ShiftList as SL
import App.ShiftRow as SR

mkDate :: Int -> Int -> Int -> Date
mkDate d m y = canonicalDate (unsafePartial $ fromJust $ toEnum y) (unsafePartial $ fromJust $ toEnum m) (unsafePartial $ fromJust $ toEnum d)  

assertAll :: forall a e. String -> a -> Array (a -> Maybe Boolean) -> Eff (assert :: ASSERT | e) Unit
assertAll msg s = assert' msg <<< all (maybe false id <<< (#) s)

findShift :: Date -> M.State -> Maybe Shift
findShift date state = find (\s -> s.date == date) state.shiftList.shifts

findVol :: VolId -> Shift -> Maybe Volunteer
findVol id shift = find (\v -> v.id == id) $ map extractVol shift.volunteers
  where
  extractVol (Overnight v) = v
  extractVol (Evening v) = v

findShiftRow :: Date -> M.State -> Maybe SR.State
findShiftRow date state = extract =<< find predicate state.shiftList.rows
  where
  predicate (SL.ShiftRow s) = s.date == date
  predicate _ = false

  extract (SL.ShiftRow s) = Just s
  extract _ = Nothing

hasName :: forall a. String -> { name :: String | a } -> Boolean
hasName name o = name == o.name

getOtherVolNames :: SR.State -> Array String
getOtherVolNames state = map _.name $ catMaybes [state.otherVol1, state.otherVol2]

getCurrentVol :: SR.State -> Maybe SR.CurrentVolState
getCurrentVol state = state.currentVol

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  updating_current_vol_name_updates_all_shifts
  updating_current_vol_name_updates_all_shift_rows
  changing_current_vol_shows_each_shift_row_with_current_vol_as_volunteer
  changing_current_vol_to_nothing_shows_all_vols_in_each_shift_row

updating_current_vol_name_updates_all_shifts :: forall e. Eff (assert :: ASSERT | e) Unit
updating_current_vol_name_updates_all_shifts = do
  let fred = { id: VolId 1, name: "Fred", gender: Nothing, overnightSharingPrefs: Any }
      vols = singleton fred
      shifts = toUnfoldable [ { date: (mkDate 1 1 2017), volunteers: singleton $ Overnight fred }
                            , { date: (mkDate 2 1 2017), volunteers: singleton $ Overnight fred }
                            ]
      currentVol = Just fred
      oldState = { vols: vols
                 , currentVol: currentVol
                 , shiftList: SL.initialState currentVol shifts (mkDate 2 1 2017) (mkDate 1 1 2017) 28
                 , currentVolSelector: CVS.initialState vols currentVol
                 , volDetailsEditState: Just M.EditingCurrentVol
                 , currentVolDetails: Nothing
                 }

      details = { name: "Fred1"
                , notes: ""
                }

      newState = M.updateCurrentVolDetails details oldState

  assertAll "Updating current vol name updates all shifts" newState
    [ map (hasName "Fred1") <<< (findVol (VolId 1) <=< findShift (mkDate 1 1 2017))
    , map (hasName "Fred1") <<< (findVol (VolId 1) <=< findShift (mkDate 2 1 2017))
    ]

updating_current_vol_name_updates_all_shift_rows :: forall e. Eff (assert :: ASSERT | e) Unit
updating_current_vol_name_updates_all_shift_rows = do
  let fred = { id: VolId 1, name: "Fred", gender: Nothing, overnightSharingPrefs: Any }
      vols = singleton fred
      shifts = toUnfoldable [ { date: (mkDate 1 1 2017), volunteers: singleton $ Overnight fred }
                            , { date: (mkDate 2 1 2017), volunteers: singleton $ Overnight fred }
                            ]
      currentVol = Just fred
      oldState = { vols: vols
                 , currentVol: currentVol
                 , shiftList: SL.initialState currentVol shifts (mkDate 2 1 2017) (mkDate 1 1 2017) 28
                 , currentVolSelector: CVS.initialState vols currentVol
                 , volDetailsEditState: Just M.EditingCurrentVol
                 , currentVolDetails: Nothing
                 }

      details = { name: "Fred1"
                , notes: ""
                }
 
      newState = M.updateCurrentVolDetails details oldState

  assertAll "Updating current vol name updates all shift rows" newState
    [ map (hasName "Fred1") <<< (_.currentVol <=< findShiftRow (mkDate 1 1 2017))
    , map (hasName "Fred1") <<< (_.currentVol <=< findShiftRow (mkDate 2 1 2017))
    ]

changing_current_vol_shows_each_shift_row_with_current_vol_as_volunteer :: forall e. Eff (assert :: ASSERT | e ) Unit
changing_current_vol_shows_each_shift_row_with_current_vol_as_volunteer = do
  let fred = { id: VolId 1, name: "Fred", gender: Nothing, overnightSharingPrefs: Any }
      bob  = { id: VolId 2, name: "Bob", gender: Nothing, overnightSharingPrefs: Any }
      jim  = { id: VolId 3, name: "Jim", gender: Nothing, overnightSharingPrefs: Any }
      vols = toUnfoldable [ fred, bob, jim ]
      shifts = toUnfoldable [ { date: (mkDate 1 1 2017), volunteers: toUnfoldable [ Overnight fred, Evening jim ] }
                            , { date: (mkDate 2 1 2017), volunteers: toUnfoldable [ Overnight jim, Evening bob ] }
                            ]
      currentVol = Just fred
      oldState = { vols: vols
                 , currentVol: currentVol
                 , shiftList: SL.initialState currentVol shifts (mkDate 2 1 2017) (mkDate 1 1 2017) 28
                 , currentVolSelector: CVS.initialState vols currentVol
                 , volDetailsEditState: Nothing
                 , currentVolDetails: Nothing
                 } 

      newState = M.changeCurrentVol (Just jim) oldState

  assertAll "changing_current_vol_shows_each_shift_row_with_current_vol_as_volunteer" newState
    [ (map (\v -> v.name == "Jim" && v.shiftType == (Just SR.Evening))) <<< (getCurrentVol <=< findShiftRow (mkDate 1 1 2017))
    , (map (\v -> v.name == "Jim" && v.shiftType == (Just SR.Overnight))) <<< (getCurrentVol <=< findShiftRow (mkDate 2 1 2017))
    ]

changing_current_vol_to_nothing_shows_all_vols_in_each_shift_row :: forall e. Eff (assert :: ASSERT | e ) Unit
changing_current_vol_to_nothing_shows_all_vols_in_each_shift_row = do
  let fred = { id: VolId 1, name: "Fred", gender: Nothing, overnightSharingPrefs: Any }
      bob  = { id: VolId 2, name: "Bob", gender: Nothing, overnightSharingPrefs: Any }
      jim  = { id: VolId 3, name: "Jim", gender: Nothing, overnightSharingPrefs: Any }
      vols = toUnfoldable [ fred, bob, jim ]
      shifts = toUnfoldable [ { date: (mkDate 1 1 2017), volunteers: toUnfoldable [ Overnight fred, Evening jim ] }
                            , { date: (mkDate 2 1 2017), volunteers: toUnfoldable [ Overnight jim, Evening bob ] }
                            ]
      currentVol = Just fred
      oldState = { vols: vols
                 , currentVol: currentVol
                 , shiftList: SL.initialState currentVol shifts (mkDate 2 1 2017) (mkDate 1 1 2017) 28
                 , currentVolSelector: CVS.initialState vols currentVol
                 , volDetailsEditState: Nothing
                 , currentVolDetails: Nothing
                 } 

      newState = M.changeCurrentVol Nothing oldState

  assertAll "changing current vol to nothing shows all vols in each shift row" newState
    [ map ((==) [ "Fred", "Jim" ] <<< getOtherVolNames) <<< findShiftRow (mkDate 1 1 2017)
    , map ((==) [ "Bob", "Jim" ] <<< getOtherVolNames) <<< findShiftRow (mkDate 2 1 2017)
    ]
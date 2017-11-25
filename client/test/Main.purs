module Test.Main where

import Prelude

import Data.Array (toUnfoldable, fromFoldable)
import Data.Date (Date, canonicalDate)
import Data.Enum (toEnum)
import Data.List (find, singleton)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import App.Data
import App.Common
import App.CurrentVolSelector as CVS
import App.CurrentVolShiftEdit as CVSE
import App.Main as M
import App.Row as R
import App.ShiftList as SL
import App.ShiftRow as SR
import App.VolDetails as VD

findShift :: M.State -> Date -> Maybe Shift
findShift state date = find (\s -> s.date == date) state.shiftList.roster.shifts

findVol :: VolId -> Shift -> Maybe Volunteer
findVol id shift = find (\v -> v.id == id) $ map extractVol shift.volunteers
  where
  extractVol (Overnight v) = v
  extractVol (Evening v) = v

findShiftRow :: M.State -> Date -> Maybe SR.State
findShiftRow state date = extract =<< find predicate state.shiftList.rows
  where
  predicate (R.ShiftRow s) = s.date == date
  predicate _ = false

  extract (R.ShiftRow s) = Just s
  extract _ = Nothing

getVolNames :: SR.State -> Array String
getVolNames state = fromFoldable $ map _.name state.volMarkers

state :: Array Volunteer -> Maybe Volunteer -> Array Shift -> M.State
state vols currentVol shifts = 
  let shifts' = toUnfoldable shifts
      vols' = toUnfoldable vols
      config = { currentDate: mkDate 1 1 2017, maxVolsPerShift: 2, urgentPeriodDays: 14 }
  in { vols: vols'
     , currentVol: currentVol
     , shiftList: SL.initialState currentVol shifts' config
     , currentVolSelector: CVS.initialState vols' currentVol
     , volDetails: Nothing
     , newVolButton: Nothing
     , editVolButton: Nothing
     }

volDetailsWithName :: String -> VD.Details
volDetailsWithName name = { name
                          , notes: ""
                          , pref: Nothing
                          , genderPref: Nothing
                          }

fred :: Volunteer
fred = { id: VolId 1, name: "Fred", overnightPreference: Nothing, overnightGenderPreference: Nothing, notes: "" }      

bob :: Volunteer
bob = { id: VolId 2, name: "Bob", overnightPreference: Nothing, overnightGenderPreference: Nothing, notes: "" }

jim :: Volunteer
jim = { id: VolId 3, name: "Jim", overnightPreference: Nothing, overnightGenderPreference: Nothing, notes: "" }
          
main :: _
main = runTest do
  suite "Update current vol details" do
    test "Updating current vol name updates all shifts" do
      let oldState = state [ fred ]
                           (Just fred)
                           [ { date: mkDate 1 1 2017, volunteers: singleton $ Overnight fred }
                           , { date: mkDate 2 1 2017, volunteers: singleton $ Overnight fred }
                           ]
          newState = M.addOrUpdateVol (volDetailsWithName "Fred1") oldState

      Assert.equal [ Just "Fred1", Just "Fred1" ]
        $ map (map _.name <<< findVol (VolId 1) <=< findShift newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]
      
    test "Updating current vol name updates all shift rows" do
      let oldState = state [ fred ]
                           (Just fred)
                           [ { date: mkDate 1 1 2017, volunteers: singleton $ Overnight fred }
                           , { date: mkDate 2 1 2017, volunteers: singleton $ Overnight fred }
                           ]
          newState = M.addOrUpdateVol (volDetailsWithName "Fred1") oldState

      Assert.equal [ Just "Fred1", Just "Fred1" ]
        $ map (map _.name <<< _.currentVolShiftEdit <=< findShiftRow newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]
  
  suite "Change current vol" do
    test "Changing current vol shows each shift row with current vol as volunteer" do
      let oldState = state [ fred, bob, jim ]
                           (Just fred)
                           [ { date: mkDate 1 1 2017, volunteers: toUnfoldable [ Overnight fred, Evening jim ] }
                           , { date: mkDate 2 1 2017, volunteers: toUnfoldable [ Overnight jim, Evening bob ] }
                           ]
          newState = M.changeCurrentVol (Just jim) oldState

      Assert.equal [ Just (Tuple "Jim" (Just CVSE.Evening)), Just (Tuple "Jim" (Just CVSE.Overnight)) ]
        $ map (map (\v -> Tuple v.name v.shiftType) <<< _.currentVolShiftEdit <=< findShiftRow newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]

    test "Changing current vol to nothing shows all vols in each shift row" do
      let oldState = state [ fred, bob, jim ]
                           (Just fred)
                           [ { date: mkDate 1 1 2017, volunteers: toUnfoldable [ Overnight fred, Evening jim ] }
                           , { date: mkDate 2 1 2017, volunteers: toUnfoldable [ Overnight jim, Evening bob ] }
                           ]
          newState = M.changeCurrentVol Nothing oldState
      
      Assert.equal [ Just ["Fred", "Jim"], Just ["Bob", "Jim"] ]
        $ map (map getVolNames <<< findShiftRow newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]
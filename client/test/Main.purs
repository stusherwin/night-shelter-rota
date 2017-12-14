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
import ServerTypes

findShift :: M.State -> Date -> Maybe Shift
findShift state date = find (hasDate date) state.shiftList.roster.shifts

findVol :: Int -> Shift -> Maybe Volunteer
findVol id (Shift s) = find (\(Volunteer v) -> v.vId == id) $ map extractVol s.volunteers
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
fred = Volunteer { vId: 1, vName: "Fred", vOvernightPreference: Nothing, vOvernightGenderPreference: Nothing, vNotes: "" }      

bob :: Volunteer
bob = Volunteer { vId: 2, vName: "Bob", vOvernightPreference: Nothing, vOvernightGenderPreference: Nothing, vNotes: "" }

jim :: Volunteer
jim = Volunteer { vId: 3, vName: "Jim", vOvernightPreference: Nothing, vOvernightGenderPreference: Nothing, vNotes: "" }
          
main :: _
main = runTest do
  suite "Update current vol details" do
    test "Updating current vol name updates all shifts" do
      let oldState = state [ fred ]
                           (Just fred)
                           [ Shift { date: fromDate $ mkDate 1 1 2017, volunteers: [ Overnight fred ] }
                           , Shift { date: fromDate $ mkDate 2 1 2017, volunteers: [ Overnight fred ] }
                           ]
          newState = M.addOrUpdateVol (volDetailsWithName "Fred1") oldState

      Assert.equal [ Just "Fred1", Just "Fred1" ]
        $ map (map (\(Volunteer v) -> v.vName) <<< findVol 1 <=< findShift newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]
      
    test "Updating current vol name updates all shift rows" do
      let oldState = state [ fred ]
                           (Just fred)
                           [ Shift { date: fromDate $ mkDate 1 1 2017, volunteers: [ Overnight fred ] }
                           , Shift { date: fromDate $ mkDate 2 1 2017, volunteers: [ Overnight fred ] }
                           ]
          newState = M.addOrUpdateVol (volDetailsWithName "Fred1") oldState

      Assert.equal [ Just "Fred1", Just "Fred1" ]
        $ map (map _.name <<< _.currentVolShiftEdit <=< findShiftRow newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]
  
  suite "Change current vol" do
    test "Changing current vol shows each shift row with current vol as volunteer" do
      let oldState = state [ fred, bob, jim ]
                           (Just fred)
                           [ Shift { date: fromDate $ mkDate 1 1 2017, volunteers: [ Overnight fred, Evening jim ] }
                           , Shift { date: fromDate $ mkDate 2 1 2017, volunteers: [ Overnight jim, Evening bob ] }
                           ]
          newState = M.changeCurrentVol (Just jim) oldState

      Assert.equal [ Just (Tuple "Jim" (Just CVSE.Evening)), Just (Tuple "Jim" (Just CVSE.Overnight)) ]
        $ map (map (\v -> Tuple v.name v.shiftType) <<< _.currentVolShiftEdit <=< findShiftRow newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]

    test "Changing current vol to nothing shows all vols in each shift row" do
      let oldState = state [ fred, bob, jim ]
                           (Just fred)
                           [ Shift { date: fromDate $ mkDate 1 1 2017, volunteers: [ Overnight fred, Evening jim ] }
                           , Shift { date: fromDate $ mkDate 2 1 2017, volunteers: [ Overnight jim, Evening bob ] }
                           ]
          newState = M.changeCurrentVol Nothing oldState
      
      Assert.equal [ Just ["Fred", "Jim"], Just ["Bob", "Jim"] ]
        $ map (map getVolNames <<< findShiftRow newState)
        [ mkDate 1 1 2017, mkDate 2 1 2017 ]
module App.ShiftTypeRadio (State, Action(..), ShiftType(..), spec, initialState) where

import Prelude

import App.Common (unsafeEventValue, toDateString, surroundIf, onlyIf, className, toDayString, sortWith, justIf)
import App.Data (OvernightPreference(..), OvernightGenderPreference(..), Volunteer(..), VolunteerShift(..), Shift(..), RuleResult(..), Config, canChangeVolunteerShiftType, hasVolWithId, validate, canAddVolunteer) as D
import Data.Array ((:), concatMap, catMaybes)
import Data.DateTime (Date, Weekday(..), year, month, day, weekday)
import Data.Enum (fromEnum)
import Data.List (List(..), find, filter, head, foldl, length, (!!), toUnfoldable, take, fromFoldable)
import Data.List.Lazy (List(..), repeat, zipWith, fromFoldable, take) as Laz
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper, joinWith, length) as S
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

data ShiftType = Overnight
               | Evening
derive instance shiftTypeEq :: Eq ShiftType

type State = { date :: Date
             , shiftType :: ShiftType
             }

data Action = ChangeCurrentVolShiftType Date ShiftType

spec :: T.Spec _ State _ Action
spec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.input [ RP._type "radio"
               , RP._id $ "shift-type-" <> toDateString state.date <> "-overnight"
               , RP.name $ "shift-type-" <> toDateString state.date
               , RP.checked $ state.shiftType == Overnight
               , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType state.date Evening
               ]
               []
    , RD.label [ RP.htmlFor $ "shift-type-" <> toDateString state.date <> "-overnight" ]
               [ renderIcon Overnight
               , RD.text "Overnight"
               ]
    ]

initialState :: Date -> ShiftType -> State
initialState date shiftType = {date, shiftType}

renderIcon :: ShiftType -> ReactElement
renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []
module App.CurrentVolShiftEdit (State, ShiftType(..), Action(..), spec, initialState) where

import Prelude
 
import App.Common (unsafeEventValue, toDateString, surroundIf, onlyIf, className, toDayString, sortWith, justIf)
import App.Data (OvernightPreference(..), OvernightGenderPreference(..), Volunteer(..), VolunteerShift(..), Shift(..), RuleResult(..), Config, canChangeVolunteerShiftType, hasVolWithId, validate, canAddVolunteer) as D
import App.ShiftTypeRadio (State, Action, ShiftType(..), spec, initialState) as STR
import Data.Array ((:), concatMap, catMaybes)
import Data.DateTime (Date, Weekday(..), year, month, day, weekday)
import Data.Enum (fromEnum)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over, _Just)
import Data.List (List(..), find, filter, head, foldl, length, (!!), toUnfoldable, take, fromFoldable)
import Data.List.Lazy (List(..), repeat, zipWith, fromFoldable, take) as Laz
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper, joinWith, length) as S
import Data.Tuple (Tuple(..), uncurry)
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

data ShiftType = Overnight
               | Evening
derive instance shiftTypeEq :: Eq ShiftType

type State = { name :: String
             , date :: Date
             , loading :: Boolean
             , shiftType :: Maybe ShiftType
             , canAddOvernight :: Boolean
             , canAddEvening :: Boolean
             , canChangeShiftType :: Boolean
             }

data Action = AddCurrentVol Date ShiftType
            | RemoveCurrentVol Date
            | ChangeCurrentVolShiftType Date

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
  render :: T.Render State _ Action
  render d _ s _ = renderShiftType d s <> renderSelected d s

  performAction :: T.PerformAction _ State _ Action
  performAction (AddCurrentVol _ _)           _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (RemoveCurrentVol _)          _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (ChangeCurrentVolShiftType _) _ _ = void $ T.modifyState \state -> state { loading = true }

  renderShiftType :: _ -> State -> Array ReactElement
  renderShiftType dispatch s@{ shiftType: Just st } | s.canChangeShiftType =
    [ RD.span [ RP.className "shift-type" ]
              [ RD.input [ RP._type "radio"
                         , RP._id $ "shift-type-" <> toDateString s.date <> "-overnight"
                         , RP.name $ "shift-type-" <> toDateString s.date
                         , RP.checked $ st == Overnight
                         , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType s.date
                         ]
                         [ ]
              , RD.label [ RP.htmlFor $ "shift-type-" <> toDateString s.date <> "-overnight" ]
                         [ renderIcon Overnight
                         , RD.text "Overnight" ] 
              , RD.input [ RP._type "radio"
                         , RP._id $ "shift-type-" <> toDateString s.date <> "-evening"
                         , RP.checked $ st == Evening
                         , RP.name $ "shift-type-" <> toDateString s.date
                         , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType s.date
                         ]
                         [ ]
              , RD.label [ RP.htmlFor $ "shift-type-" <> toDateString s.date <> "-evening" ]
                         [ renderIcon Evening
                         , RD.text "Evening only" ] 
               ]
    ]
  renderShiftType _ _ = []

  description :: ShiftType -> String
  description Evening   = "Evening only"
  description Overnight = "Overnight"

  other :: ShiftType -> ShiftType
  other Evening   = Overnight
  other Overnight = Evening

  renderSelected :: _ -> State -> Array ReactElement
  renderSelected dispatch s@{ shiftType: Nothing } | not s.loading =
    [ RD.span [ RP.className "ui fitted checkbox" ]
              [ RD.input [ RP._type "checkbox"
                         , RP.disabled $ s.loading || (not s.canAddOvernight && not s.canAddEvening)
                         , RP.checked false
                         , RP.onChange \_ -> dispatch $ AddCurrentVol s.date $ if s.canAddOvernight then Overnight else Evening
                         ]
                         []
              , RD.label' []
              ]
    ]
  renderSelected dispatch s@{ shiftType: Just st } | not s.loading =
    [ RD.span [ RP.className "ui fitted checkbox" ]
              [ RD.input [ RP._type "checkbox"
                         , RP.disabled $ s.loading
                         , RP.checked true
                         , RP.onChange \_ -> dispatch $ RemoveCurrentVol s.date
                         ]
                         []
              , RD.label' []
              ]
    ]
  renderSelected _ s | s.loading =
    [ RD.i [ RP.className "icon-spin animate-spin loading" ] [] ]
  renderSelected _ _ = []

initialState :: D.Config -> D.Shift -> D.Volunteer -> State
initialState config shift cv = { name: cv.name
                               , date: shift.date
                               , loading: false
                               , shiftType: currentVolShiftType cv shift.volunteers
                               , canAddOvernight: D.canAddVolunteer config (D.Overnight cv) shift
                               , canAddEvening: D.canAddVolunteer config (D.Evening cv) shift
                               , canChangeShiftType: D.canChangeVolunteerShiftType config cv shift
                               }

currentVolShiftType :: D.Volunteer -> List D.VolunteerShift -> Maybe ShiftType
currentVolShiftType v vols = 
  find (D.hasVolWithId v.id) vols >>= case _ of
    D.Overnight _ -> Just Overnight
    D.Evening   _ -> Just Evening

renderIcon :: ShiftType -> ReactElement
renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []
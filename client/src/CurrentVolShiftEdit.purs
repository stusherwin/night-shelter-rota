module App.CurrentVolShiftEdit (State, ShiftType(..), Action(..), spec, initialState) where

import Prelude
  
import Data.DateTime (Date)
import Data.List (find)
import Data.Maybe (Maybe(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Common (toDateString)
import ServerTypes (Volunteer(..), VolunteerShift(..), Shift(..)) as D
import App.Data (Config, canChangeVolunteerShiftType, hasVolWithId, canAddVolunteer, toDate) as D

data ShiftType = Overnight
               | Evening
derive instance shiftTypeEq :: Eq ShiftType
instance shiftTypeShow :: Show ShiftType
  where
  show Overnight = "Overnight"
  show Evening = "Evening"

type State = { name :: String
             , date :: Date
             , loading :: Boolean
             , shiftType :: Maybe ShiftType
             , canAddOvernight :: Boolean
             , canAddEvening :: Boolean
             , canChangeShiftType :: Boolean
             }

type ShiftTypeRadioState = { date :: Date
                           , shiftType :: ShiftType
                           , currentShiftType :: ShiftType
                           }

data Action = AddCurrentVol Date ShiftType
            | RemoveCurrentVol Date
            | ChangeCurrentVolShiftType Date

spec :: T.Spec _ State _ Action
spec = shiftTypeSpec
    <> selectedSpec
    <> handler
  
shiftTypeSpec :: T.Spec _ State _ Action
shiftTypeSpec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Action
  render dispatch _ s@{ shiftType: Just st } _ | s.canChangeShiftType =
    [ RD.span [ RP.className "shift-type" ]
              ( renderShiftTypeRadio dispatch { shiftType: Overnight, date: s.date, currentShiftType: st }
             <> renderShiftTypeRadio dispatch { shiftType: Evening,   date: s.date, currentShiftType: st }
              )
    ]
  render _ _ _ _ = []

selectedSpec :: T.Spec _ State _ Action
selectedSpec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Action
  render dispatch _ s@{ shiftType: Nothing } _ | not s.loading =
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
  render dispatch _ s@{ shiftType: Just st } _ | not s.loading =
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
  render _ _ s _ | s.loading =
    [ RD.i [ RP.className "icon-spin animate-spin loading" ] [] ]
  render _ _ _ _ = []

handler :: T.Spec _ State _ Action
handler = T.simpleSpec performAction T.defaultRender
  where
  performAction :: T.PerformAction _ State _ Action
  performAction (AddCurrentVol _ _)           _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (RemoveCurrentVol _)          _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (ChangeCurrentVolShiftType _) _ _ = void $ T.modifyState \state -> state { loading = true }

renderShiftTypeRadio :: _ -> ShiftTypeRadioState -> Array ReactElement
renderShiftTypeRadio dispatch s = 
  [ RD.input [ RP._type "radio"
             , RP._id $ "shift-type-" <> toDateString s.date <> "-" <> code s.shiftType
             , RP.name $ "shift-type-" <> toDateString s.date
             , RP.checked $ s.currentShiftType == s.shiftType
             , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType s.date
             ]
             [ ]
  , RD.label [ RP.className "action-label"
             , RP.htmlFor $ "shift-type-" <> toDateString s.date <> "-" <> code s.shiftType ]
             [ renderIcon s.shiftType
             , RD.text $ description s.shiftType
             ]
  ]
  where
  description :: ShiftType -> String
  description Evening   = "Evening only"
  description Overnight = "Overnight"

  code :: ShiftType -> String
  code Evening   = "evening"
  code Overnight = "overnight"

  other :: ShiftType -> ShiftType
  other Evening   = Overnight
  other Overnight = Evening

  renderIcon :: ShiftType -> ReactElement
  renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
  renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []


initialState :: D.Config -> D.Shift -> D.Volunteer -> State
initialState config (D.Shift shift) (D.Volunteer cv) = { name: cv.vName
                               , date: D.toDate shift.date
                               , loading: false
                               , shiftType: shiftType
                               , canAddOvernight: D.canAddVolunteer config (D.Overnight (D.Volunteer cv)) (D.Shift shift)
                               , canAddEvening: D.canAddVolunteer config (D.Evening (D.Volunteer cv)) (D.Shift shift)
                               , canChangeShiftType: D.canChangeVolunteerShiftType config (D.Volunteer cv) (D.Shift shift)
                               }
  where
  shiftType = find (D.hasVolWithId cv.vId) shift.volunteers >>= case _ of
    D.Overnight _ -> Just Overnight
    D.Evening   _ -> Just Evening
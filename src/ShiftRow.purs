module App.ShiftRow (State, MonthInfo, ShiftType(..), OtherVolState, CurrentVolState, Action(..), ShiftStatus(..), spec) where
 
import Prelude
 
import App.Common (unsafeEventValue, toDateString, surroundIf, onlyIf, className, toDayString) 
import App.Data (OvernightSharingPrefs(..), Volunteer(..), VolunteerShift(..), canChangeVolunteerShiftType) as D
import Data.DateTime (Date, Weekday(..), year, month, day, weekday)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper, joinWith)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

data ShiftType = Overnight
               | Evening
derive instance shiftTypeEq :: Eq ShiftType

data ShiftStatus = Good
                 | Warning String
                 | Error String
                 | Info String
                 | OK

type OtherVolState = { name :: String
                     , shiftType :: ShiftType
                     , sharingPrefs :: String
                     }

type CurrentVolState = { name :: String
                       , shiftType :: Maybe ShiftType
                       , canAddOvernight :: Boolean
                       , canAddEvening :: Boolean
                       , canChangeShiftType :: Boolean
                       }

type MonthInfo = { name :: String
                 , noOfDays :: Int
                 }

type State = { date :: Date
             , status :: ShiftStatus
             , currentVol :: Maybe CurrentVolState
             , noOfVols :: Int
             , otherVol1 :: Maybe OtherVolState
             , otherVol2 :: Maybe OtherVolState
             , loading :: Boolean
             , month :: Maybe MonthInfo
             }
 
data Action = AddCurrentVol Date ShiftType
            | RemoveCurrentVol Date
            | ChangeCurrentVolShiftType Date ShiftType

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.tr [ className $ [ onlyIf (isWeekend state.date) "weekend"
                          , onlyIf state.loading "loading"
                          ]
            ]
         ( case state.month of
             Just { name, noOfDays } -> [ RD.td [ RP.rowSpan noOfDays
                                                , className [ "collapsing month-name" ]
                                                ]
                                                [ RD.div' [ RD.text name ]
                                                ]
                                        ]
             _ -> []
         <> [ RD.td  [ className [ "shift-status left-border collapsing", statusClass state ] ]
                     (statusIcon state)
            , RD.td  [ className [ "shift-status collapsing", statusClass state ] ]
                     [ RD.text $ "" <> show state.noOfVols <> "/2" ]
            , RD.td  [ RP.className "shift-date left-border collapsing" ]
                     [ RD.text $ toUpper $ take 3 $ show $ weekday state.date ]
            , RD.td  [ RP.className "shift-date collapsing" ]
                     [ RD.text $ toDayString state.date ]
            ]
         <> [ RD.td  [ RP.className "left-border collapsing" ]
                     $ renderOtherVol state.otherVol1
            , RD.td  [ RP.className "collapsing" ]
                     $ renderOtherVol state.otherVol2
            , RD.td' []
            ]
         <> renderCurrentVol dispatch state
         )
    ]

  isWeekend :: Date -> Boolean
  isWeekend date = case weekday date of
    Saturday -> true
    Sunday   -> true
    _ -> false

  statusClass :: State -> String
  statusClass state = case state.loading, state.status of
    false, Good        -> "positive"
    false, (Error _)   -> "negative"
    false, (Warning _) -> "warning"
    _, _               -> ""

  statusIcon :: State -> Array ReactElement
  statusIcon state = case state.loading, state.status of
    true, _           -> [ RD.i [ RP.className "icon-spin animate-spin" ] [] ]
    _,    Good        -> [ RD.i [ RP.className "icon-ok" ] [] ]
    _,    (Error e)   -> [ RD.i [ RP.className "icon-warning", RP.title e ] [] ]
    _,    (Warning w) -> [ RD.i [ RP.className "icon-info", RP.title w ] [] ]
    _,    (Info i)    -> [ RD.i [ RP.className "icon-info", RP.title i ] [] ]
    _,    _           -> []
 
  renderCurrentVol :: _ -> State -> Array ReactElement
  renderCurrentVol dispatch state@{ currentVol: Just _ } =
    [ RD.td  [ RP.className "collapsing left-border right aligned shift-selected" ]
             $ renderShiftType dispatch state
            <> renderSelected dispatch state
    ]
      where
      renderShiftType :: _ -> State -> Array ReactElement
      renderShiftType dispatch state@{ date, loading, currentVol: (Just { shiftType: Just st, canChangeShiftType }) } | canChangeShiftType =
        [ RD.span [ RP.className "shift-type" ]
                  [ renderIcon st
                   , RD.text $ description st <> " / "
                   , RD.a [ RP.onClick \_ -> dispatch $ ChangeCurrentVolShiftType date $ other st
                          , RP.className "action"
                          , RP.disabled loading
                          ]
                          [ RD.text $ "[" <> (description $ other st) <> "]" ]
                   ]
        ]
      renderShiftType dispatch state@{ date, loading, currentVol: (Just { shiftType: Just st }) } =
        [ RD.span [ RP.className "shift-type" ]
                   [ renderIcon st
                   , RD.text $ description st <> " "
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
      renderSelected dispatch state@{ date, loading, currentVol: (Just cv@{ shiftType: Nothing }) } =
        [ RD.span [ RP.className "ui fitted checkbox" ]
                  [ RD.input [ RP._type "checkbox"
                             , RP.disabled $ loading || (not cv.canAddOvernight && not cv.canAddEvening)
                             , RP.checked false
                             , RP.onChange \_ -> dispatch $ AddCurrentVol date $ if cv.canAddOvernight then Overnight else Evening
                             ]
                             []
                  , RD.label' []
                  ]
        ]
      renderSelected dispatch state@{ date, loading, currentVol: (Just { shiftType: Just st }) } =
        [ RD.span [ RP.className "ui fitted checkbox" ]
                  [ RD.input [ RP._type "checkbox"
                             , RP.disabled $ loading
                             , RP.checked true
                             , RP.onChange \_ -> dispatch $ RemoveCurrentVol date
                             ]
                             []
                  , RD.label' []
                  ]
        ]
      renderSelected _ _ = []
  renderCurrentVol _ _ = []
  
  renderOtherVol :: Maybe OtherVolState -> Array ReactElement
  renderOtherVol (Just v) =
    [ RD.span [ RP.className "other-vol" ]
              [ renderIcon v.shiftType
              , RD.text $ v.name <> v.sharingPrefs
              ]
    ]
  renderOtherVol _ = []

  renderIcon :: ShiftType -> ReactElement
  renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
  renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []
  
  performAction :: T.PerformAction _ State _ Action
  performAction (AddCurrentVol _ _)             _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (RemoveCurrentVol _)            _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (ChangeCurrentVolShiftType _ _) _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction _ _ _ = pure unit
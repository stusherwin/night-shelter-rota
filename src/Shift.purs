module App.Shift (ShiftProps, ShiftState, ShiftType(..), OtherVolState, CurrentVolState, ShiftAction(..), ShiftStatus(..), shiftSpec) where

import Prelude

import App.Common (unsafeEventValue, toDateString, surroundIf)
import App.Data (OvernightSharingPrefs(..), Volunteer(..), canChangeVolunteerShiftType)
import Data.DateTime (Date, Weekday(..), year, month, day, weekday)
import Data.Enum (fromEnum) 
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper, joinWith)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

type ShiftProps = {  }

data ShiftType = Overnight
               | Evening

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

type ShiftState = { date :: Date
                  , status :: ShiftStatus
                  , currentVol :: Maybe CurrentVolState
                  , noOfVols :: Int
                  , otherVol1 :: Maybe OtherVolState
                  , otherVol2 :: Maybe OtherVolState
                  , loading :: Boolean
                  }

data ShiftAction = AddCurrentVol Date ShiftType
                 | RemoveCurrentVol Date
                 | ChangeCurrentVolShiftType Date ShiftType

shiftSpec :: T.Spec _ ShiftState _ ShiftAction
shiftSpec = T.simpleSpec performAction render
  where
  render :: T.Render ShiftState _ ShiftAction
  render dispatch _ state _ =
    [ RD.tr [ RP.className $ joinWith " " [ if isWeekend state.date then "weekend" else ""
                                          , if state.loading then "loading" else ""
                                          ]
            ]
          ( [ RD.td  [ RP.className $ "shift-status collapsing " <> statusClass state ]
                     (statusIcon state)
            , RD.td  [ RP.className $ "shift-status collapsing " <> statusClass state ]
                     [ RD.text $ "" <> show state.noOfVols <> "/2" ]
            , RD.td  [ RP.className $ "shift-date left-border collapsing " ]-- <> statusClass state.status ]
                     [ RD.text $ toUpper $ take 3 $ show $ weekday state.date ]
            , RD.td  [ RP.className $ "shift-date collapsing "] -- <> statusClass state.status ]
                     [ RD.text $ toDateString state.date ]
            ] <> case state.currentVol of
                    (Just _) -> 
                       [ RD.td  [ RP.className "left-border collapsing" ]
                                $ renderSelected dispatch state
                       , RD.td  [ RP.className "collapsing shift-type" ]
                                $ renderShiftType dispatch state
                       ]
                    _ -> []
            <> [ RD.td  [ RP.className "left-border collapsing" ]
                     $ renderOtherVol state.otherVol1
            , RD.td  [ RP.className "collapsing" ]
                     $ renderOtherVol state.otherVol2
            , RD.td' []
            ]
          )
    ]

  statusClass :: ShiftState -> String
  statusClass state = case state.loading, state.status of
    false, Good        -> "positive"
    false, (Error _)   -> "negative"
    false, (Warning _) -> "warning"
    _, _               -> ""

  statusIcon :: ShiftState -> Array ReactElement
  statusIcon state = case state.loading, state.status of
    true, _           -> [ RD.i [ RP.className "icon-spin animate-spin" ] [] ]
    _,    Good        -> [ RD.i [ RP.className "icon-ok" ] [] ]
    _,    (Error e)   -> [ RD.i [ RP.className "icon-warning", RP.title e ] [] ]
    _,    (Warning w) -> [ RD.i [ RP.className "icon-info", RP.title w ] [] ]
    _,    (Info i)    -> [ RD.i [ RP.className "icon-info", RP.title i ] [] ]
    _,    _           -> []
  
  renderShiftType :: _ -> ShiftState -> Array ReactElement
  renderShiftType dispatch state@{ date, loading, currentVol: (Just { shiftType: Just Overnight, canChangeShiftType }) } =
    [ RD.span []
            ([ RD.i [ RP.className "icon-bed" ] []
              , RD.text "Overnight " ]
            <> if canChangeShiftType
                 then [ RD.a [ RP.onClick \_ -> dispatch $ ChangeCurrentVolShiftType date $ Evening
                             , RP.className "action"
                             , RP.disabled loading
                             ]
                             [ RD.text "[change to Evening only]" ]
                      , RD.text ""
                      ]
                 else [])
    ]
  renderShiftType dispatch state@{ date, loading, currentVol: (Just { shiftType: Just Evening, canChangeShiftType }) } =
    [ RD.span []
              ([ RD.i [ RP.className "icon-no-bed" ] []
              , RD.text "Evening only " ]
              <> if canChangeShiftType
                 then [ RD.a [ RP.onClick \_ -> dispatch $ ChangeCurrentVolShiftType date $ Overnight
                             , RP.className "action"
                             , RP.disabled loading
                             ]
                             [ RD.text "[change to Overnight]" ]
                      , RD.text ""
                      ]
                 else [])
    ]
  renderShiftType _ _ = []

  renderSelected :: _ -> ShiftState -> Array ReactElement
  renderSelected dispatch state@{ date, loading, currentVol: (Just cv@{ shiftType: Nothing }) } =
    [ RD.div [ RP.className "ui fitted checkbox" ]
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
    [ RD.div [ RP.className "ui fitted checkbox" ]
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

  renderOtherVol :: Maybe OtherVolState -> Array ReactElement
  renderOtherVol (Just v) =
    [ RD.span [ RP.className "other-vol" ]
              [ RD.i [ RP.className $ case v.shiftType of Overnight -> "icon-bed"
                                                          _         -> "icon-no-bed" ]
                     []
              , RD.text $ v.name <> v.sharingPrefs
              ]
    ]
  renderOtherVol _ = []

  performAction :: T.PerformAction _ ShiftState _ ShiftAction
  performAction (AddCurrentVol _ _)             _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (RemoveCurrentVol _)            _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (ChangeCurrentVolShiftType _ _) _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction _ _ _ = pure unit

  toId :: Date -> String
  toId date = (show $ fromEnum $ year date) <> (show $ month date) <> (show $ fromEnum $ day date)

  isWeekend :: Date -> Boolean
  isWeekend date = case weekday date of
    Saturday -> true
    Sunday   -> true
    _ -> false
  
  changeCurrentVol :: ShiftType -> Maybe CurrentVolState -> Maybe CurrentVolState
  changeCurrentVol st (Just cv@{ shiftType: Just _ }) = Just cv { shiftType = Just st }
  changeCurrentVol _ cv = cv


module App.Shift (ShiftProps, ShiftState, ShiftType(..), OtherVolState, CurrentVolState, ShiftAction(..), ShiftStatus(..), shiftSpec) where

import Prelude

import App.Common (unsafeEventValue, toDateString)
import App.Data (Volunteer(..))
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
                 | OK

type OtherVolState = { name :: String
                     , shiftType :: ShiftType
                     }

type CurrentVolState = { name :: String
                       , shiftType :: Maybe ShiftType
                       , canAddOvernight :: Boolean
                       , canAddEvening :: Boolean
                       }

type ShiftState = { date :: Date
                  , status :: ShiftStatus
                  , currentVol :: Maybe CurrentVolState
                  , noOfVols :: Int
                  , otherVol1 :: Maybe OtherVolState
                  , otherVol2 :: Maybe OtherVolState
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
                                          ]
            ]
          ( [ RD.td  [ RP.className $ "shift-status collapsing " <> statusClass state.status ]
                     (statusIcon state.status)
            , RD.td  [ RP.className $ "shift-status collapsing " <> statusClass state.status ]
                     [ RD.text $ "" <> show state.noOfVols <> "/2" ]
            , RD.td  [ RP.className $ "shift-date left-border collapsing " <> statusClass state.status ]
                     [ RD.text $ toUpper $ take 3 $ show $ weekday state.date ]
            , RD.td  [ RP.className $ "shift-date collapsing " <> statusClass state.status ]
                     [ RD.text $ toDateString state.date ]
            , RD.td  [ RP.className "left-border collapsing" ]
                     $ renderOtherVol state.otherVol1
            , RD.td  [ RP.className "collapsing" ]
                     $ renderOtherVol state.otherVol2
            , RD.td' []
            ] <> case state.currentVol of
                    (Just _) -> 
                       [ RD.td  [ RP.className "left-border collapsing shift-type" ]
                                $ renderShiftType dispatch state
                       , RD.td  [ RP.className "right aligned collapsing" ]
                                $ renderSelected dispatch state
                       ]
                    _ -> 
                       [ RD.td' [] ]
          )
    ]

  statusClass :: ShiftStatus -> String
  statusClass Good        = "positive"
  statusClass (Error _)   = "negative"
  statusClass (Warning _) = "warning"
  statusClass _           = ""

  statusIcon :: ShiftStatus -> Array ReactElement
  statusIcon Good        = [ RD.i [ RP.className "icon-ok" ] [] ]
  statusIcon (Error e)   = [ RD.i [ RP.className "icon-warning", RP.title e ] [] ]
  statusIcon (Warning e) = [ RD.i [ RP.className "icon-info", RP.title e ] [] ]
  statusIcon _ = []
  
  renderEndCells :: _ -> ShiftState -> Array ReactElement
  renderEndCells dispatch { otherVol2: v@(Just _) } =
    [ RD.td  [ RP.className "left-border" ]
             $ renderOtherVol v
    , RD.td  [ RP.className "right aligned collapsing" ]
             []
    ]
    
  renderEndCells dispatch state@{ otherVol2: _ } =
    [ RD.td  [ RP.className "left-border shift-type" ]
              $ renderShiftType dispatch state
    , RD.td  [ RP.className "right aligned collapsing" ]
              $ renderSelected dispatch state
    ]
  
  renderShiftType :: _ -> ShiftState -> Array ReactElement
  renderShiftType dispatch state@{ date: d, currentVol: (Just { shiftType: Just Overnight }) } =
    [ RD.span []
              [ RD.i [ RP.className "icon-bed" ] []
              , RD.text "Overnight "
              , RD.a [ RP.onClick \_ -> dispatch $ ChangeCurrentVolShiftType d $ Evening
                     , RP.href "#"
                     ]
                     [ RD.text "[change]" ]
              , RD.text ""
              ]
    ]
  renderShiftType dispatch state@{ date: d, currentVol: (Just { shiftType: Just Evening }) } =
    [ RD.span []
              [ RD.i [ RP.className "icon-no-bed" ] []
              , RD.text "Evening only "
              , RD.a [ RP.onClick \_ -> dispatch $ ChangeCurrentVolShiftType d $ Overnight
                     , RP.href "#"
                     ]
                     [ RD.text "[change]" ]
              , RD.text ""
              ]
    ]
  renderShiftType _ _ = []

  renderSelected :: _ -> ShiftState -> Array ReactElement
  renderSelected dispatch state@{ date: d, currentVol: (Just cv@{ shiftType: Nothing }) } =
    [ RD.div [ RP.className "ui fitted checkbox" ]
             [ RD.input [ RP._type "checkbox"
                        , RP.disabled $ not cv.canAddOvernight && not cv.canAddEvening
                        , RP.checked false
                        , RP.onChange \_ -> dispatch $ AddCurrentVol d $ if cv.canAddOvernight then Overnight else Evening
                        ]
                        []
             , RD.label' []
             ]
    ]
  renderSelected dispatch state@{ date: d, currentVol: (Just { shiftType: Just st }) } =
    [ RD.div [ RP.className "ui fitted checkbox" ]
             [ RD.input [ RP._type "checkbox"
                        , RP.checked true
                        , RP.onChange \_ -> dispatch $ RemoveCurrentVol d
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
              , RD.text v.name
              ]
    ]
  renderOtherVol _ = []

  performAction :: T.PerformAction _ ShiftState _ ShiftAction
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
module App.ShiftRow (State, ShiftType(..), OtherVolState, CurrentVolState, Action(..), ShiftStatus(..), spec, initialState) where
 
import Prelude

import App.Common (unsafeEventValue, toDateString, surroundIf, onlyIf, className, toDayString, sortWith)
import App.Data (OvernightSharingPrefs(..), Volunteer(..), VolunteerShift(..), Shift(..), RuleResult(..), canChangeVolunteerShiftType, hasVolWithId, validate, canAddVolunteer) as D
import Data.DateTime (Date, Weekday(..), year, month, day, weekday)
import Data.Enum (fromEnum)
import Data.List (List(..), find, filter, head, foldl, length, (!!))
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

type State = { date :: Date
             , status :: ShiftStatus
             , currentVol :: Maybe CurrentVolState
             , noOfVols :: Int
             , otherVol1 :: Maybe OtherVolState
             , otherVol2 :: Maybe OtherVolState
             , loading :: Boolean
             , past :: Boolean
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
                          , onlyIf state.past "past"
                          ]
            ]
         (  [ RD.td  [ RP.className "shift-date collapsing" ]
                     [ RD.text $ toUpper $ take 3 $ show $ weekday state.date ]
            , RD.td  [ RP.className "shift-date collapsing" ]
                     [ RD.text $ toDayString state.date ]
            , RD.td  [ className [ "shift-status left-border collapsing", statusClass state ] ]
                     [ RD.text $ "" <> show state.noOfVols <> "/2" ]
            , RD.td  [ className [ "shift-status collapsing", statusClass state ] ]
                     (statusIcon state)
            , RD.td  [ RP.className "left-border collapsing" ]
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
      renderSelected dispatch state@{ date, loading, currentVol: (Just cv@{ shiftType: Nothing }) } | not loading =
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
      renderSelected dispatch state@{ date, loading, currentVol: (Just { shiftType: Just st }) } | not loading =
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
      renderSelected _ state@{ date, loading } | loading =
        [ RD.i [ RP.className "icon-spin animate-spin loading" ] [] ]
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


initialState :: List D.Shift -> Maybe D.Volunteer -> Date -> Date -> State
initialState shifts currentVol currentDate date = 
  { date 
  , noOfVols: length shift.volunteers
  , status: status shift currentDate
  , loading: false
  , currentVol: buildCurrentVol shift
  , otherVol1: otherVols !! 0
  , otherVol2: otherVols !! 1
  , past: date < currentDate
  }
  where
  shift = maybe {date: date, volunteers: Nil} id $ find (\s -> s.date == date) shifts
  otherVols = sortWith _.name $ map buildVol $ case currentVol of
                                                 Just cv -> filter (not <<< D.hasVolWithId $ cv.id) shift.volunteers
                                                 _ -> shift.volunteers

  buildVol :: D.VolunteerShift -> OtherVolState
  buildVol (D.Overnight v) = { name: v.name
                             , shiftType: Overnight
                             , sharingPrefs: sharingPrefs v.overnightSharingPrefs
                             } 
  buildVol (D.Evening v)   = { name: v.name
                             , shiftType: Evening
                             , sharingPrefs: sharingPrefs v.overnightSharingPrefs
                             }

  sharingPrefs :: D.OvernightSharingPrefs -> String
  sharingPrefs prefs = surroundIf " (" ")" $ case prefs of
                                               D.None -> "No sharing"
                                               (D.OnlyGender gender) -> (show gender) <> " only"
                                               (D.Custom text) -> text
                                               _ -> ""

  status :: D.Shift -> Date -> ShiftStatus
  status s currentDate =
    let errors = D.validate s currentDate
        firstErrorStatus = case head errors of
          Just (D.Error e)   -> Error
          Just (D.Warning w) -> Warning
          Just (D.Info i)    -> Info
          Just (D.Neutral)   -> const OK
          _             -> const Good
        
        extractMsg (D.Error e)   = Just e
        extractMsg (D.Warning w) = Just w
        extractMsg (D.Info i)    = Just i
        extractMsg _             = Nothing

        concat ""  (Just m) = "This shift " <> m
        concat msg (Just m) = msg <> ", and also " <> m
        concat msg Nothing  = msg
    in firstErrorStatus $ foldl concat "" $ map extractMsg errors
  
  buildCurrentVol :: D.Shift -> Maybe CurrentVolState
  buildCurrentVol shift = case currentVol of
    (Just cv) -> Just { name: cv.name 
                      , shiftType: currentVolShiftType cv shift.volunteers
                      , canAddOvernight: D.canAddVolunteer (D.Overnight cv) shift
                      , canAddEvening: D.canAddVolunteer (D.Evening cv) shift
                      , canChangeShiftType: D.canChangeVolunteerShiftType cv shift
                      }
    _         -> Nothing

  currentVolShiftType :: D.Volunteer -> List D.VolunteerShift -> Maybe ShiftType
  currentVolShiftType v vols = 
    find (D.hasVolWithId v.id) vols >>= case _ of
      D.Overnight _ -> Just Overnight
      D.Evening   _ -> Just Evening
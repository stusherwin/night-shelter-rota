module App.ShiftRow (State, ShiftType(..), Action(..), ShiftStatus(..), RosterData, spec, initialState) where
 
import Prelude
 
import Data.DateTime (Date, Weekday(..), weekday)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over, _Just)
import Data.Array (length) as A
import Data.List (List(..), find, head, foldl, length, fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper) as S
import Data.Tuple (Tuple(..), uncurry)
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
 
import App.Common (onlyIf, classNames, toDayString, sortWith)
import App.Data (RuleResult(..), Config, validate, toDate, hasDate, fromDate) as D
import ServerTypes (Volunteer(..), Shift(..)) as D
import App.CurrentVolShiftEdit (State, Action(..), spec, initialState) as CVSE
import App.VolMarker (State, spec, initialState) as VM

data ShiftType = Overnight
               | Evening 
derive instance shiftTypeEq :: Eq ShiftType

data ShiftStatus = Good
                 | Warning String
                 | Error String
                 | Info String
                 | Past
                 | OK

type State = { date :: Date
             , status :: ShiftStatus
             , currentVolShiftEdit :: Maybe CVSE.State
             , noOfVols :: Int
             , maxVols :: Int
             , volMarkers :: List VM.State
             , loading :: Boolean
             }

type RosterData d = { currentVol :: Maybe D.Volunteer
                    , shifts :: List D.Shift
                    | d
                    }
 
data Action = CurrentVolShiftEditAction CVSE.Action
            | VolMarkerAction Int Unit

_currentVolShiftEdit :: Lens' State (Maybe CVSE.State)
_currentVolShiftEdit = lens _.currentVolShiftEdit _{currentVolShiftEdit = _}
 
_CurrentVolShiftEditAction :: Prism' Action CVSE.Action
_CurrentVolShiftEditAction = prism CurrentVolShiftEditAction unwrap
  where
  unwrap (CurrentVolShiftEditAction a) = Right a
  unwrap ta = Left ta

_volMarkers :: Lens' State (List VM.State)
_volMarkers = lens _.volMarkers _{volMarkers = _}

_VolMarkerAction :: Prism' Action (Tuple Int Unit)
_VolMarkerAction = prism (uncurry VolMarkerAction) unwrap
  where
  unwrap (VolMarkerAction i a) = Right (Tuple i a)
  unwrap ta = Left ta

spec :: T.Spec _ State _ Action
spec = 
  over T._render row
    $ fold [ over T._render (\render d p s c  ->
               [ RD.td [ classNames [ "vol-markers shift-status collapsing" ] ]
                       $ render d p s c
               ])
               $ T.focus _volMarkers _VolMarkerAction $ T.foreach \_ -> VM.spec
           , T.simpleSpec T.defaultPerformAction \_ _ s _ -> [ RD.td [ classNames [ "shift-status" ] ] [] ]
           , over T._render (\render d p s c ->
               [ RD.td [ classNames [ "shift-selected shift-status collapsing right aligned" ] ] 
                       $ render d p s c
               ])
               $ T.focus _currentVolShiftEdit _CurrentVolShiftEditAction $ T.split _Just CVSE.spec
           ]
  <> handler
  where
  row :: T.Render State _ Action -> T.Render State _ Action
  row render d p s c =
    [ RD.tr [ classNames $ [ onlyIf (isWeekend s.date) "weekend"
                           , onlyIf s.loading "loading"
                           , statusClass s.status
                           , pastClass s.status
                           ]
            ]
         (  [ RD.td  [ classNames [ "shift-status-icon shift-status collapsing" ] ]
                     (statusIcon s.status)
            , RD.td  [ classNames [ "shift-day shift-status collapsing" ] ]
                     [ RD.text $ S.toUpper $ S.take 3 $ show $ weekday s.date ]
            , RD.td  [ classNames [ "shift-date shift-status collapsing" ] ]
                     [ RD.text $ toDayString s.date ]
            , RD.td  [ classNames [ "vol-count shift-status collapsing" ] ]
                     [ RD.text $ "" <> show s.noOfVols <> "/" <> show s.maxVols ]
            ]
         <> render d p s c
         )
    ]

  handler :: T.Spec _ State _ Action
  handler = T.simpleSpec performAction T.defaultRender
    where
    performAction :: T.PerformAction _ State _ Action
    performAction (CurrentVolShiftEditAction (CVSE.AddCurrentVol _ _))           _ _ = void $ T.modifyState \state -> state { loading = true }
    performAction (CurrentVolShiftEditAction (CVSE.RemoveCurrentVol _))          _ _ = void $ T.modifyState \state -> state { loading = true }
    performAction (CurrentVolShiftEditAction (CVSE.ChangeCurrentVolShiftType _)) _ _ = void $ T.modifyState \state -> state { loading = true }
    performAction _ _ _ = pure unit

  isWeekend :: Date -> Boolean
  isWeekend date = case weekday date of
    Saturday -> true
    Sunday   -> true
    _ -> false 

  pastClass :: ShiftStatus -> String
  pastClass Past = "past"
  pastClass _ = "" 

  statusClass :: ShiftStatus -> String
  statusClass Good        = "positive"
  statusClass (Error _)   = "negative"
  statusClass (Warning _) = "warning"
  statusClass _ = ""

  statusIcon :: ShiftStatus -> Array ReactElement
  statusIcon Past        = [ RD.i [ RP.className "icon-clock", RP.title "This shift is in the past" ] [] ]
  statusIcon Good        = [ RD.i [ RP.className "icon-ok" ] [] ]
  statusIcon (Error e)   = [ RD.i [ RP.className "icon-warning", RP.title e ] [] ]
  statusIcon (Warning w) = [ RD.i [ RP.className "icon-info", RP.title w ] [] ]
  statusIcon (Info i)    = [ RD.i [ RP.className "icon-info", RP.title i ] [] ]
  statusIcon _ = []

initialState :: forall d. RosterData d -> D.Config -> Date -> State
initialState roster config date = 
  let shift@(D.Shift sh) = maybe (D.Shift {date: D.fromDate date, volunteers: []}) id
                $ find (D.hasDate date) roster.shifts
  in { date 
     , noOfVols: A.length sh.volunteers
     , maxVols: config.maxVolsPerShift
     , status: status config shift
     , loading: false
     , currentVolShiftEdit: map (CVSE.initialState config shift) roster.currentVol
     , volMarkers: sortWith _.name $ map VM.initialState $ fromFoldable sh.volunteers
     }
  where
  status :: D.Config -> D.Shift -> ShiftStatus
  status config (D.Shift s) | (D.toDate s.date) < config.currentDate = Past
  status config (D.Shift s) =
    let errors = D.validate config (D.Shift s)
        firstErrorStatus = case head errors of
          Just (D.Error e)   -> Error
          Just (D.Warning w) -> Warning
          Just (D.Info i)    -> Info
          Just (D.Neutral)   -> const OK
          _ -> const Good
        
        extractMsg (D.Error e)   = Just e
        extractMsg (D.Warning w) = Just w
        extractMsg (D.Info i)    = Just i
        extractMsg _ = Nothing

        concat ""  (Just m) = "This shift " <> m
        concat msg (Just m) = msg <> ", and also " <> m
        concat msg Nothing  = msg
    in firstErrorStatus $ foldl concat "" $ map extractMsg errors
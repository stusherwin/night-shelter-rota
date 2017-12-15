module App.ShiftRow (State, VolMarkerState, SharingPref, CVSEState, Action(..), ShiftStatus(..), RosterData, spec, initialState) where
 
import Prelude
 
import Data.Array (catMaybes, fromFoldable)
import Data.DateTime (Date, Weekday(..), weekday)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over, _Just)
import Data.List (List(..), find, head, foldl, length)
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper, toLower, length) as S
import Data.Tuple (Tuple(..), uncurry)
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
   
import App.Common (onlyIf, classNames, toDayString, sortWith, justIf, toDateString)
import App.Data (Config, validate, canChangeVolunteerShiftType, canAddVolunteer)
import App.Data (RuleResult(..)) as D
import App.Types (Volunteer, Shift, VolunteerShift, ShiftType(..), OvernightPreference(..), OvernightGenderPreference(..))

data ShiftStatus = Good
                 | Warning String
                 | Error String
                 | Info String
                 | Past
                 | OK

type State = { date :: Date
             , status :: ShiftStatus
             , currentVolShiftEdit :: Maybe CVSEState
             , noOfVols :: Int
             , maxVols :: Int
             , volMarkers :: List VolMarkerState
             , loading :: Boolean
             }

type RosterData d = { currentVol :: Maybe Volunteer
                    , shifts :: List Shift
                    | d
                    }
 
data Action = AddCurrentVol Date ShiftType
            | RemoveCurrentVol Date
            | ChangeCurrentVolShiftType Date ShiftType

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render 
  where
  render :: T.Render State _ Action
  render dispatch _ s _ =
    [ RD.tr [ classNames $ [ onlyIf (isWeekend s.date) "weekend"
                           , onlyIf s.loading "loading"
                           , statusClass s.status
                           , pastClass s.status
                           ]
            ]
            [ RD.td [ classNames [ "shift-day shift-status collapsing" ] ]
                    [ RD.text $ S.toUpper $ S.take 3 $ show $ weekday s.date ]
            , RD.td [ classNames [ "shift-date shift-status collapsing" ] ]
                    [ RD.text $ toDayString s.date ]
            , RD.td [ classNames [ "shift-status-icon shift-status collapsing" ] ]
                    (statusIcon s.status)
            , RD.td [ classNames [ "vol-count shift-status collapsing" ] ]
                    [ RD.text $ "" <> show s.noOfVols <> "/" <> show s.maxVols ]
            , RD.td [ classNames [ "vol-markers shift-status collapsing" ] ]
                  $ fromFoldable $ map renderVolMarker s.volMarkers
            , RD.td [ classNames [ "shift-status" ] ]
                    []
            , RD.td [ classNames [ "shift-selected shift-status collapsing right aligned" ] ] 
                  $ renderCurrentVolSelected dispatch s.currentVolShiftEdit
            ]
    ]

  performAction :: T.PerformAction _ State _ Action
  performAction (AddCurrentVol _ _)           _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (RemoveCurrentVol _)          _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (ChangeCurrentVolShiftType _ _) _ _ = void $ T.modifyState \state -> state { loading = true }

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

data SharingPref = GM
                 | GF
                 | P1
                 | P2
                 | N String

type VolMarkerState = { name :: String
                      , shiftType :: ShiftType
                      , sharingPrefs :: Array SharingPref
                      }

renderVolMarker :: VolMarkerState -> ReactElement
renderVolMarker s =
  RD.span [ RP.className "vol-marker" ]
          $ [ renderIcon s.shiftType
            , RD.text $ s.name
            ]
          <> map renderSharingPref s.sharingPrefs
  
  where
  renderSharingPref :: SharingPref -> ReactElement
  renderSharingPref GM    = RD.div [ RP.className "sharing-pref gender" 
                                   , RP.title "Males only" 
                                   ] 
                                   [ RD.span' [ RD.text "M" ] ]
  renderSharingPref GF    = RD.div [ RP.className "sharing-pref gender" 
                                   , RP.title "Females only" 
                                   ] 
                                   [ RD.span' [ RD.text "F" ] ]
  renderSharingPref P1    = RD.div [ RP.className "sharing-pref alone" 
                                   , RP.title "I prefer to be on my own" 
                                   ] 
                                   [ RD.span' [ RD.text "1" ] ]
  renderSharingPref P2    = RD.div [ RP.className "sharing-pref alone" 
                                   , RP.title "I prefer to work with another volunteer" 
                                   ] 
                                   [ RD.span' [ RD.text "2" ] ]
  renderSharingPref (N n) = RD.div [ RP.className "sharing-pref icon" 
                                   , RP.title n
                                   , RP.dangerouslySetInnerHTML { __html: "<i class=\"icon-info-1\"></i>&nbsp;" }
                                   ] 
                                   []

renderIcon :: ShiftType -> ReactElement
renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []

initialState :: forall d. RosterData d -> Config -> Date -> State
initialState roster config date = 
  let shift = maybe { date: date, volunteers: Nil } id
              $ find (\s -> s.date == date) roster.shifts
  in { date 
     , noOfVols: length shift.volunteers
     , maxVols: config.maxVolsPerShift
     , status: status config shift
     , loading: false
     , currentVolShiftEdit: map (cvseState config shift) roster.currentVol
     , volMarkers: sortWith (S.toLower <<< _.name) $ map volMarkerState shift.volunteers
     }
  where
  status :: Config -> Shift -> ShiftStatus
  status config s | s.date < config.currentDate = Past
  status config s =
    let errors = validate config s
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

volMarkerState :: VolunteerShift -> VolMarkerState
volMarkerState { volunteer: v, shiftType } = { name: v.name
                                             , shiftType: shiftType
                                             , sharingPrefs: sharingPrefs v
                                             }
  where
  sharingPrefs :: Volunteer -> Array SharingPref
  sharingPrefs vol = catMaybes [ map overnight vol.overnightPreference
                               , map gender vol.overnightGenderPreference
                               , justIf (N vol.notes) $ S.length vol.notes > 0
                               ]

  overnight :: OvernightPreference -> SharingPref
  overnight PreferToBeAlone = P1
  overnight PreferAnotherVolunteer = P2

  gender :: OvernightGenderPreference -> SharingPref
  gender Male = GM
  gender Female = GF

type CVSEState = { name :: String
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

renderCurrentVolSelected :: _ -> Maybe CVSEState -> Array ReactElement
renderCurrentVolSelected _ Nothing = []
renderCurrentVolSelected d (Just s) = renderShiftType d s
                                   <> renderSelected d s
  where
  renderShiftType :: _ -> CVSEState -> Array ReactElement
  renderShiftType dispatch s@{ shiftType: Just st } | s.canChangeShiftType =
    [ RD.span [ RP.className "shift-type" ]
              ( renderShiftTypeRadio dispatch { shiftType: Overnight, date: s.date, currentShiftType: st }
             <> renderShiftTypeRadio dispatch { shiftType: Evening,   date: s.date, currentShiftType: st }
              )
    ]
  renderShiftType _ _ = []

  renderSelected :: _ -> CVSEState -> Array ReactElement
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

  renderShiftTypeRadio :: _ -> ShiftTypeRadioState -> Array ReactElement
  renderShiftTypeRadio dispatch s = 
    [ RD.input [ RP._type "radio"
               , RP._id $ "shift-type-" <> toDateString s.date <> "-" <> code s.shiftType
               , RP.name $ "shift-type-" <> toDateString s.date
               , RP.checked $ s.currentShiftType == s.shiftType
               , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType s.date s.shiftType
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

cvseState :: Config -> Shift -> Volunteer -> CVSEState
cvseState config shift cv = { name: cv.name
                               , date: shift.date
                               , loading: false
                               , shiftType: shiftType
                               , canAddOvernight: canAddVolunteer config { shiftType: Overnight, volunteer: cv} shift
                               , canAddEvening: canAddVolunteer config { shiftType: Evening, volunteer: cv} shift
                               , canChangeShiftType: canChangeVolunteerShiftType config cv shift
                               }
  where
  shiftType = find (\v -> v.volunteer.id == cv.id) shift.volunteers <#> _.shiftType
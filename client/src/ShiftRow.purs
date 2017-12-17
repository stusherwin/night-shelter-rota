module App.ShiftRow (spec, initialState) where
 
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
   
import App.Common (onlyIf, classNames, toDayString, sortWith, justIf, toDateString, isWeekday)
import App.ShiftRules (ShiftRuleConfig, validateShift, canChangeVolunteerShiftType, canAddVolunteer)
import App.ShiftRules (RuleResult(..)) as SR
import App.Types (Volunteer, Shift, VolunteerShift, ShiftType(..), OvernightPreference(..), OvernightGenderPreference(..), otherShiftType)
import ShiftListState
 
spec :: T.Spec _ ShiftRowState _ RowAction
spec = T.simpleSpec performAction render 
  where
  performAction :: T.PerformAction _ ShiftRowState _ RowAction
  performAction (AddCurrentVol _ _)           _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (RemoveCurrentVol _)          _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction (ChangeCurrentVolShiftType _ _) _ _ = void $ T.modifyState \state -> state { loading = true }
  performAction _ _ _ = pure unit

  render :: T.Render ShiftRowState _ RowAction
  render dispatch _ state _ =
    [ RD.tr [ classNames $ weekendClass state.date
                        <> loadingClass state.loading
                        <> statusClass state.status
                        <> pastClass state.status
            ]
            [ RD.td [ classNames [ "shift-day shift-status collapsing" ] ]
                    [ RD.text $ S.toUpper $ S.take 3 $ show $ weekday state.date ]
            , RD.td [ classNames [ "shift-date shift-status collapsing" ] ]
                    [ RD.text $ toDayString state.date ]
            , RD.td [ classNames [ "shift-status-icon shift-status collapsing" ] ]
                    (statusIcon state.status)
            , RD.td [ classNames [ "vol-count shift-status collapsing" ] ]
                    [ RD.text $ "" <> show state.noOfVols <> "/" <> show state.maxVols ]
            , RD.td [ classNames [ "vol-markers shift-status collapsing" ] ]
                  $ fromFoldable $ map renderVolMarker state.volMarkers
            , RD.td [ classNames [ "shift-status" ] ]
                    []
            , RD.td [ classNames [ "shift-selected shift-status collapsing right aligned" ] ] 
                  $ renderCurrentVol state.currentVol
            ]
    ]
    where
    weekendClass :: Date -> Array String
    weekendClass date | not $ isWeekday date = [ "weekend" ]
    weekendClass _ = []

    loadingClass true = [ "loading" ]
    loadingClass _ = []

    pastClass :: ShiftStatus -> Array String
    pastClass Past = [ "past" ]
    pastClass _ = [] 

    statusClass :: ShiftStatus -> Array String
    statusClass Good        = [ "positive" ]
    statusClass (Error _)   = [ "negative" ]
    statusClass (Warning _) = [ "warning" ]
    statusClass _ = []

    statusIcon :: ShiftStatus -> Array ReactElement
    statusIcon Past        = [ RD.i [ RP.className "icon-clock", RP.title "This shift is in the past" ] [] ]
    statusIcon Good        = [ RD.i [ RP.className "icon-ok" ] [] ]
    statusIcon (Error e)   = [ RD.i [ RP.className "icon-warning", RP.title e ] [] ]
    statusIcon (Warning w) = [ RD.i [ RP.className "icon-info", RP.title w ] [] ]
    statusIcon (Info i)    = [ RD.i [ RP.className "icon-info", RP.title i ] [] ]
    statusIcon _ = []

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

    renderCurrentVol :: Maybe CurrentVolState -> Array ReactElement
    renderCurrentVol Nothing = []
    renderCurrentVol (Just currentVol) = renderShiftType currentVol
                                      <> renderSelected currentVol
      where
      renderShiftType :: CurrentVolState -> Array ReactElement
      renderShiftType s@{ shiftType: Just st } | s.canChangeShiftType =
        [ RD.span [ RP.className "shift-type" ]
                  ( renderShiftTypeRadio Overnight st
                 <> renderShiftTypeRadio Evening st
                  )
        ]
        where
        renderShiftTypeRadio :: ShiftType -> ShiftType -> Array ReactElement
        renderShiftTypeRadio shiftType currentShiftType = 
          [ RD.input [ RP._type "radio"
                     , RP._id $ "shift-type-" <> toDateString state.date <> "-" <> code shiftType
                     , RP.name $ "shift-type-" <> toDateString state.date
                     , RP.checked $ currentShiftType == shiftType
                     , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType state.date shiftType
                     ]
                     [ ]
          , RD.label [ RP.className "action-label"
                     , RP.htmlFor $ "shift-type-" <> toDateString state.date <> "-" <> code shiftType ]
                     [ renderIcon shiftType
                     , RD.text $ description shiftType
                     ]
          ]
          where
          description :: ShiftType -> String
          description Evening   = "Evening only"
          description Overnight = "Overnight"

          code :: ShiftType -> String
          code Evening   = "evening"
          code Overnight = "overnight"
      renderShiftType _ = []

      renderSelected :: CurrentVolState -> Array ReactElement
      renderSelected s@{ shiftType: Nothing } | not state.loading =
        [ RD.span [ RP.className "ui fitted checkbox" ]
                  [ RD.input [ RP._type "checkbox"
                             , RP.disabled $ state.loading || (not s.canAddOvernight && not s.canAddEvening)
                             , RP.checked false
                             , RP.onChange \_ -> dispatch $ AddCurrentVol state.date $ if s.canAddOvernight then Overnight else Evening
                             ]
                             []
                  , RD.label' []
                  ]
        ]
      renderSelected s@{ shiftType: Just st } | not state.loading =
        [ RD.span [ RP.className "ui fitted checkbox" ]
                  [ RD.input [ RP._type "checkbox"
                             , RP.disabled $ state.loading
                             , RP.checked true
                             , RP.onChange \_ -> dispatch $ RemoveCurrentVol state.date
                             ]
                             []
                  , RD.label' []
                  ]
        ]
      renderSelected _ | state.loading =
        [ RD.i [ RP.className "icon-spin animate-spin loading" ] [] ]
      renderSelected _ = []
    
    renderIcon :: ShiftType -> ReactElement
    renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
    renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []

initialState :: RosterState -> ShiftRuleConfig -> Date -> ShiftRowState
initialState roster config date = 
  { date 
  , noOfVols: length shift.volunteers
  , maxVols: config.maxVolsPerShift
  , status: status
  , loading: false
  , currentVol: map currentVolState roster.currentVol
  , volMarkers: sortWith (S.toLower <<< _.name) $ map volMarkerState shift.volunteers
  }
  where
  shift = maybe { date: date, volunteers: Nil } id
              $ find (\s -> s.date == date) roster.shifts
  status :: ShiftStatus
  status | shift.date < config.currentDate = Past
  status =
    let errors = validateShift config shift
        firstErrorStatus = case head errors of
          Just (SR.Error e)   -> Error
          Just (SR.Warning w) -> Warning
          Just (SR.Info i)    -> Info
          Just (SR.Neutral)   -> const OK
          _ -> const Good
        
        extractMsg (SR.Error e)   = Just e
        extractMsg (SR.Warning w) = Just w
        extractMsg (SR.Info i)    = Just i
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

  currentVolState :: Volunteer -> CurrentVolState
  currentVolState cv = { name: cv.name
                       , shiftType: find (\v -> v.volunteer.id == cv.id) shift.volunteers <#> _.shiftType
                       , canAddOvernight: canAddVolunteer config { shiftType: Overnight, volunteer: cv} shift
                       , canAddEvening: canAddVolunteer config { shiftType: Evening, volunteer: cv} shift
                       , canChangeShiftType: canChangeVolunteerShiftType config cv shift
                       }
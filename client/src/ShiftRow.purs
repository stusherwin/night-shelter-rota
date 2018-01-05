module App.ShiftRow (spec, initialState) where
 
import Prelude
 
import Data.Array (catMaybes, fromFoldable, reverse)
import Data.DateTime (Date, Weekday(..), weekday, month)
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
   
import App.Common (onlyIf, classNames, dayString1, dayPostfix, sortWith, justIf, toDateString, isWeekday)
import App.ShiftRules (ShiftRuleConfig, validateShift, canChangeVolunteerShiftType, canAddVolunteer)
import App.ShiftRules (RuleResult(..)) as SR
import App.Types (Volunteer, Shift, VolunteerShift, ShiftType(..), OvernightPreference(..), OvernightGenderPreference(..), otherShiftType, overnightPrefMarker, overnightPrefDescription, overnightGenderPrefMarker, overnightGenderPrefDescription)
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
    [ RD.div [ classNames $ [ "row shift-row" ]
                            <> weekendClass state
                            <> loadingClass state
                            <> statusClass state
                            <> pastClass state
                            <> todayClass state
             , RP._id $ "shift-row-" <> toDateString state.date
             ]
             [ RD.div [ classNames [ "row-item shift-date" ] ]
                      [ RD.div [ classNames [ "shift-date-part shift-date-day collapsing" ] ]
                               [ RD.text $ S.toUpper $ S.take 3 $ show $ weekday state.date ]
                      , RD.div [ classNames [ "shift-date-part shift-date-month collapsing" ] ]
                               [ RD.text $ S.toUpper $ S.take 3 $ show $ month state.date ]
                      , RD.div [ classNames [ "shift-date-part shift-date-date collapsing" ] ]
                               [ RD.text $ dayString1 state.date
                               , RD.span [ RP.className "shift-date-postfix" ]
                                         [ RD.text $ dayPostfix state.date ] 
                               ]
                      ]
             , RD.div [ classNames [ "row-item shift-status" ] ]
                      [ RD.div [ classNames [ "shift-status-part shift-status-vol-count collapsing" ] ]
                               [ RD.text $ "" <> show state.noOfVols <> "/" <> show state.maxVols ]
                      , RD.div [ classNames [ "shift-status-part shift-status-icon collapsing" ] ]
                             $ statusIcon state
                      ]
             , RD.div [ classNames [ "row-item current-vol collapsing right aligned" ] ] 
                    $ renderCurrentVol state.currentVol
             , RD.div [ classNames [ "row-item vol-markers collapsing" ] ]
                    $ fromFoldable $ map (renderVolMarker dispatch) state.volMarkers
            --  , RD.div [ classNames [ "row-item" ] ]
            --           []
             ]
    ]
    where
    weekendClass :: ShiftRowState -> Array String
    weekendClass { date } | not $ isWeekday date = [ "weekend" ]
    weekendClass _ = []

    loadingClass :: ShiftRowState -> Array String
    loadingClass { loading } | loading = [ "loading" ]
    loadingClass _ = []

    pastClass :: ShiftRowState -> Array String
    pastClass { date, currentDate } | date < currentDate = [ "past" ]
    pastClass _ = [] 

    todayClass :: ShiftRowState -> Array String
    todayClass { date, currentDate } | date == currentDate = [ "today" ]
    todayClass _ = [] 

    statusClass :: ShiftRowState -> Array String
    -- statusClass Good        = [ "positive1" ]
    statusClass { status: Error _ }   = [ "negative1" ]
    statusClass { status: Warning _ } = [ "warning1" ]
    statusClass _ = []

    statusIcon :: ShiftRowState -> Array ReactElement
    statusIcon { date, currentDate } | date < currentDate = [ RD.i [ RP.className "icon-clock", RP.title "This shift is in the past" ] [] ]
    -- statusIcon Good        = [ RD.i [ RP.className "icon-ok" ] [] ]
    statusIcon { status: Error e }   = [ RD.i [ RP.className "icon-warning", RP.title e ] [] ]
    statusIcon { status: Warning w } = [ RD.i [ RP.className "icon-info", RP.title w ] [] ]
    statusIcon { status: Info i }    = [ RD.i [ RP.className "icon-info", RP.title i ] [] ]
    statusIcon _ = []

    renderVolMarker :: _ -> VolunteerShift -> ReactElement
    renderVolMarker d s =
      RD.span [ RP.className "vol-marker" ]
              [ RD.span [ RP.className "vol-name"
                        , RP.onClick \_ -> d $ ShowVolInfo s.volunteer
                        ]
                        $ [ renderIcon s.shiftType
                          , RD.text $ s.volunteer.name
                          ]
                          <> renderSharingPrefs s.volunteer
              ]
      where
      renderSharingPrefs :: Volunteer -> Array ReactElement
      renderSharingPrefs vol = catMaybes [ map renderOvernight vol.overnightPreference
                                         , map renderGender vol.overnightGenderPreference
                                         , map renderNotes $ justIf vol.notes $ S.length vol.notes > 0
                                         ]

      renderOvernight :: OvernightPreference -> ReactElement
      renderOvernight p = RD.span [ RP.className "sharing-pref alone" 
                                  ] 
                                  [ RD.span' [ RD.text $ overnightPrefMarker p ] ]

      renderGender :: OvernightGenderPreference -> ReactElement
      renderGender p = RD.span [ RP.className "sharing-pref gender" 
                               ] 
                               [ RD.span' [ RD.text $ overnightGenderPrefMarker p ] ]

      renderNotes :: String -> ReactElement
      renderNotes n = RD.span [ RP.className "sharing-pref icon" 
                              , RP.dangerouslySetInnerHTML { __html: "<i class=\"icon-info\"></i>&nbsp;" }
                              ] 
                              []

    renderCurrentVol :: Maybe CurrentVolState -> Array ReactElement
    renderCurrentVol Nothing = []
    renderCurrentVol (Just currentVol) = renderSelected currentVol
                                      <> renderShiftType currentVol
      where
      renderShiftType :: CurrentVolState -> Array ReactElement
      renderShiftType s@{ shiftType: Just st } | s.canChangeShiftType =
        [ RD.span [ RP.className "current-vol-shift-type radio media-large-screen media-larger-screen" ]
                  ( renderShiftTypeRadio Overnight st
                 <> renderShiftTypeRadio Evening st
                  )
        , RD.span [ RP.className "current-vol-shift-type toggle media-medium-screen media-small-screen" ]
                  [ RD.div [ RP.className "ui toggle checkbox" ]
                           [ RD.input [ RP.tabIndex 0
                                      , RP.className "hidden"
                                      , RP._type "checkbox"
                                      , RP._id $ "shift-type-" <> toDateString state.date
                                      , RP.checked $ st == Overnight
                                      , RP.onChange \_ -> dispatch $ ChangeCurrentVolShiftType state.date $ otherShiftType st
                                      ]
                                      []
                           , RD.label [ RP.htmlFor $ "shift-type-" <> toDateString state.date ]
                                      []
                           , RD.span [ RP.className "current-vol-shift-type-toggle-description" ]
                                     [ RD.text $ description st ]
                           ]
                  ]
        ]
        where
        renderShiftTypeRadio :: ShiftType -> ShiftType -> Array ReactElement
        renderShiftTypeRadio shiftType currentShiftType = 
          [ RD.span [ RP.className "current-vol-shift-type-option" ]
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
          ]
        description :: ShiftType -> String
        description Evening   = "Evening"
        description Overnight = "Overnight"

        code :: ShiftType -> String
        code Evening   = "evening"
        code Overnight = "overnight"
      renderShiftType _ = []

      renderSelected :: CurrentVolState -> Array ReactElement
      renderSelected s@{ shiftType: Nothing } | not state.loading =
        [ RD.span [ RP.className "current-vol-selected ui fitted checkbox" ]
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
        [ RD.span [ RP.className "current-vol-selected ui fitted checkbox" ]
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
  , currentDate: config.currentDate
  , noOfVols: length shift.volunteers
  , maxVols: config.maxVolsPerShift
  , status: status
  , loading: false
  , currentVol: map currentVolState roster.currentVol
  , volMarkers: sortWith (S.toLower <<< _.volunteer.name) $ shift.volunteers
  }
  where
  shift = maybe { date: date, volunteers: Nil } id
              $ find (\s -> s.date == date) roster.shifts
  status :: ShiftStatus
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

  currentVolState :: Volunteer -> CurrentVolState
  currentVolState cv = { name: cv.name
                       , shiftType: find (\v -> v.volunteer.id == cv.id) shift.volunteers <#> _.shiftType
                       , canAddOvernight: canAddVolunteer config { shiftType: Overnight, volunteer: cv} shift
                       , canAddEvening: canAddVolunteer config { shiftType: Evening, volunteer: cv} shift
                       , canChangeShiftType: canChangeVolunteerShiftType config cv shift
                       } 
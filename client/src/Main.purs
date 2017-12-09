module App.Main where 
     
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Data.Argonaut.Generic.Aeson
import Data.Generic
import Data.Tuple
import Prelude
import Servant.PureScript.Settings
import ServerAPI
import Servant.PureScript.Affjax 
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Network.HTTP.Affjax (AJAX, get)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Now (nowDate, NOW)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import Data.Array (toUnfoldable)
import Data.DateTime (Date, Weekday(..))
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over, _Just)
import Data.List (List(..), snoc, last, fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import React as R
import React.DOM (s)
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import ServerTypes (OvernightPreference(..), OvernightGenderPreference(..), Volunteer(..), VolunteerShift(..), Shift(..), VolunteerDetails(..))
import Thermite as T

import App.Common (updateWhere, sortWith, nextWeekday)
import App.CurrentVolSelector (State, Action(..), spec, initialState, changeVols) as CVS
import App.Data (fromDate, Config, hasVId, vId)
import App.EditVolButton (State, Action, spec, initialState) as EVB
import App.NewVolButton (State, Action, spec, initialState) as NVB
import App.ShiftList (State, Action, spec, initialState, changeCurrentVol) as SL
import App.VolDetails (State, Action(..), Details, spec, initialState, disable) as VD

data Action = ShiftListAction SL.Action
            | CurrentVolSelectorAction CVS.Action
            | VolDetailsAction VD.Action
            | NewVolButtonAction NVB.Action
            | EditVolButtonAction EVB.Action
            | ReportError AjaxError
 
type State = { vols :: List Volunteer 
             , shiftList :: SL.State
             , currentVol :: Maybe Volunteer
             , currentVolSelector :: CVS.State
             , volDetails :: Maybe VD.State 
             , newVolButton :: Maybe NVB.State
             , editVolButton :: Maybe EVB.State
             , loading :: Boolean
             , config :: Config
             , errorMessage :: Maybe String
             }
 
_shiftList :: Lens' State SL.State
_shiftList = lens _.shiftList _{shiftList = _}

_ShiftListAction :: Prism' Action SL.Action
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a

_currentVolSelector :: Lens' State CVS.State
_currentVolSelector = lens _.currentVolSelector _{currentVolSelector = _}

_CurrentVolSelectorAction :: Prism' Action CVS.Action
_CurrentVolSelectorAction = prism CurrentVolSelectorAction unwrap
  where 
  unwrap (CurrentVolSelectorAction a) = Right a
  unwrap a = Left a 
 
_volDetails :: Lens' State (Maybe VD.State)
_volDetails = lens _.volDetails _{volDetails = _}

_VolDetailsAction :: Prism' Action VD.Action
_VolDetailsAction = prism VolDetailsAction unwrap
  where 
  unwrap (VolDetailsAction a) = Right a
  unwrap a = Left a 
 
_newVolButton :: Lens' State (Maybe NVB.State)
_newVolButton = lens _.newVolButton _{newVolButton = _}

_NewVolButtonAction :: Prism' Action NVB.Action
_NewVolButtonAction = prism NewVolButtonAction unwrap
  where 
  unwrap (NewVolButtonAction a) = Right a
  unwrap a = Left a 

_editVolButton :: Lens' State (Maybe EVB.State)
_editVolButton = lens _.editVolButton _{editVolButton = _}

_EditVolButtonAction :: Prism' Action EVB.Action
_EditVolButtonAction = prism EditVolButtonAction unwrap
  where 
  unwrap (EditVolButtonAction a) = Right a
  unwrap a = Left a 
  
spec :: T.Spec _ State _ Action
spec =
  over T._render container
    $ fold [ T.focus _newVolButton _NewVolButtonAction
               $ T.split _Just NVB.spec
           , T.focus _editVolButton _EditVolButtonAction
               $ T.split _Just EVB.spec
           , over T._render header
               $ T.focus _currentVolSelector _CurrentVolSelectorAction CVS.spec 
           , T.focus _volDetails _VolDetailsAction
               $ T.split _Just VD.spec
           , T.focus _shiftList _ShiftListAction SL.spec
           , handler
           ] 
  where 
  container :: T.Render State _ Action -> T.Render State _ Action
  container render d p s c =
    [ RD.div [ RP.className "container" ]
             $ render d p s c
    ]

  header :: T.Render State _ Action -> T.Render State _ Action
  header render d p s c =
    [ RD.h2' $ [ RD.span [ RP.dangerouslySetInnerHTML { __html: "Night Shelter Rota for &nbsp;" } ]
                         []
               ]
               <> render d p s c
               <> if s.loading then [ RD.i [ RP.className "icon-spin animate-spin loading" ] [] ]
                               else []
               <> case s.errorMessage of
                    Just msg -> [ RD.text msg ]
                    _ -> []
    ]
    
  handler :: T.Spec _ State _ Action
  handler = T.simpleSpec performAction T.defaultRender
    where 
    performAction :: T.PerformAction _ State _ Action
    performAction (CurrentVolSelectorAction (CVS.ChangeCurrentVol v)) _ _ = void $ T.modifyState $ changeCurrentVol v
    performAction (NewVolButtonAction _) _ _ = void $ T.modifyState startEditingNewVol
    performAction (EditVolButtonAction _) _ _ = void $ T.modifyState startEditingCurrentVol
    performAction (VolDetailsAction (VD.Save d)) _ s@{ currentVol: Just _ } = updateCurrentVol d s
    performAction (VolDetailsAction (VD.Save d)) _ s = addNewVol d s
    performAction (VolDetailsAction VD.Cancel) _ _ = void $ T.modifyState cancelEditing
    performAction _ _ _ = pure unit

changeCurrentVol :: Maybe Volunteer -> State -> State
changeCurrentVol currentVol' s = s{ currentVol = currentVol'
                                  , shiftList = SL.changeCurrentVol currentVol' s.shiftList
                                  , volDetails = Nothing
                                  , newVolButton = Just NVB.initialState
                                  , editVolButton = map (EVB.initialState <<< (\(Volunteer v) -> v.vName)) currentVol'
                                  }

startEditingNewVol :: State -> State
startEditingNewVol s = s{ currentVol = Nothing
                        , shiftList = SL.changeCurrentVol Nothing s.shiftList
                        , currentVolSelector = CVS.changeVols s.vols Nothing s.currentVolSelector
                        , volDetails = Just $ VD.initialState Nothing
                        , newVolButton = Nothing
                        , editVolButton = Nothing
                        }

startEditingCurrentVol :: State -> State
startEditingCurrentVol s@{ currentVol: Just _ } =
  s{ volDetails = Just $ VD.initialState s.currentVol
   , newVolButton = Nothing
   , editVolButton = Nothing
   }
startEditingCurrentVol s = s

updateCurrentVol :: VD.Details -> State -> _
updateCurrentVol d { currentVol: currentVol@Just (Volunteer v), vols } = do
  _ <- T.modifyState \s -> s { loading = true
                             , volDetails = VD.disable <$> s.volDetails
                             }
  resp <- lift $ apiReq $ flip postApiVolsById v.vId $ VolunteerDetails { vdName: d.name
                                                                        , vdNotes: d.notes
                                                                        , vdPref: d.pref
                                                                        , vdGenderPref: d.genderPref
                                                                        }
  case resp of
    Right vol -> void $ T.modifyState $ updateVols { currentVol: Just vol
                                                   , vols: updateWhere (hasVId v.vId) vol vols
                                                   }
    _ -> pure unit
updateCurrentVol _ _ = pure unit

addNewVol :: VD.Details -> State -> _
addNewVol d { vols } = do
  _ <- T.modifyState \s -> s { loading = true
                             , volDetails = VD.disable <$> s.volDetails
                             }
  resp <- lift $ apiReq $ putApiVols $ VolunteerDetails { vdName: d.name
                                                        , vdNotes: d.notes
                                                        , vdPref: d.pref
                                                        , vdGenderPref: d.genderPref
                                                        }
  case resp of
    Right vol -> void $ T.modifyState $ updateVols { currentVol: Just vol
                                                   , vols: snoc vols vol
                                                   }
    _ -> pure unit

updateVols :: { currentVol :: Maybe Volunteer, vols :: List Volunteer } -> State -> State
updateVols { currentVol, vols } s = s { currentVol = currentVol
                                      , vols = vols
                                      , shiftList = SL.changeCurrentVol currentVol s.shiftList
                                      , currentVolSelector = CVS.changeVols vols currentVol s.currentVolSelector
                                      , volDetails = Nothing
                                      , newVolButton = Just NVB.initialState
                                      , editVolButton = map (EVB.initialState <<< (\(Volunteer v) -> v.vName)) currentVol
                                      , loading = false
                                      }
 
cancelEditing :: State -> State
cancelEditing s = s{ volDetails = Nothing
                   , newVolButton = Just NVB.initialState
                   , editVolButton = map (EVB.initialState <<< (\(Volunteer v) -> v.vName)) s.currentVol
                   }

initialState :: Date -> State
initialState currentDate =
  let config = { maxVolsPerShift: 2
               , urgentPeriodDays: 14
               , currentDate
               }
  in  { vols: Nil
      , shiftList: SL.initialState Nothing Nil config
      , currentVol: Nothing
      , currentVolSelector: CVS.initialState Nil Nothing
      , volDetails: Nothing
      , newVolButton: Just NVB.initialState
      , editVolButton: Nothing
      , loading: true
      , config
      , errorMessage: Nothing
      }

initialDataLoaded :: Array Volunteer -> Array Shift -> State -> State
initialDataLoaded vols shifts state =
  state { vols = toUnfoldable vols
        , shiftList = SL.initialState state.currentVol (toUnfoldable shifts) state.config
        , currentVolSelector = CVS.initialState (toUnfoldable vols) state.currentVol
        , loading = false
        }

initialDataLoadError :: String -> State -> State
initialDataLoadError msg state =
  state { errorMessage = Just msg
        , loading = false
        }

type MySettings = SPSettings_ SPParams_

settings :: MySettings
settings = defaultSettings $ SPParams_ { baseURL: "http://localhost:8081/" }

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff (ajax :: AJAX, err :: EXCEPTION | eff)))

apiReq :: forall a. Generic a => APIEffect _ a -> Aff _ (Either String a)
apiReq m = do
  delay (Milliseconds 5000.0)
  response <- runExceptT $ runReaderT m settings
  case response of
    Left err -> do
      liftEff $ log $ errorToString err
      pure $ Left $ errorToString err
    Right r -> do
      liftEff $ log $ gShow r
      pure $ Right r

main :: Unit
main = unsafePerformEff $ void $ launchAff $ do 
  (LocalValue _ currentDate) <- liftEff nowDate
  let { spec } = T.createReactSpec spec $ initialState currentDate
  let component = R.createClass spec { componentDidMount = \ctx -> void $ launchAff $ do 
                                          liftEff $ log "ComponentDidMount..."
                                          state <- liftEff $ R.readState ctx
                                          (Tuple volsResp shiftsResp) <- sequential $ Tuple 
                                            <$> (parallel $ apiReq getApiVols)
                                            <*> (parallel $ apiReq getApiShifts)
                                          case volsResp, shiftsResp of
                                            Right vols, Right shifts -> do
                                              let state' = initialDataLoaded vols shifts state
                                              liftEff $ R.writeState ctx state'
                                            Left e, _ -> err e ctx state
                                            _, Left e -> err e ctx state
                                     }

  let appEl = R.createFactory component {}
  
  if isServerSide
     then void $ liftEff $ (log (RDOM.renderToString appEl)) 
     else void $ liftEff $ (getElementById "app" >>= RDOM.render appEl)
  liftEff $ hot
  where
    err msg ctx state = do
      let state' = initialDataLoadError msg state
      liftEff $ R.writeState ctx state'

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit 

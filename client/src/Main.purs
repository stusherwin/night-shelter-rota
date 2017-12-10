module App.Main where 
     
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Data.Argonaut.Generic.Aeson
import Data.Generic
import Data.Tuple
import Prelude
import Servant.PureScript.Settings
import ServerAPI
import Servant.PureScript.Affjax (AjaxError, errorToString)
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
import App.Header (State, Action(..), spec, initialState, volDetailsUpdated, editCancelled, reqStarted, reqSucceeded, reqFailed, initialDataLoaded) as H
import App.Data (fromDate, Config, hasVId, vId)
import App.ShiftList (State, Action, spec, initialState, changeCurrentVol) as SL
import App.VolDetails (State, Action(..), Details, spec, initialState, disable, enable) as VD

data Action = ShiftListAction SL.Action
            | HeaderAction H.Action
            | VolDetailsAction VD.Action
            | ReportError AjaxError

type State = { vols :: List Volunteer 
             , shiftList :: SL.State
             , currentVol :: Maybe Volunteer
             , header :: H.State
             , volDetails :: Maybe VD.State 
             , config :: Config
             }
 
_shiftList :: Lens' State SL.State
_shiftList = lens _.shiftList _{shiftList = _}

_ShiftListAction :: Prism' Action SL.Action
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a

_header :: Lens' State H.State
_header = lens _.header _{header = _}

_HeaderAction :: Prism' Action H.Action
_HeaderAction = prism HeaderAction unwrap
  where 
  unwrap (HeaderAction a) = Right a
  unwrap a = Left a 
 
_volDetails :: Lens' State (Maybe VD.State)
_volDetails = lens _.volDetails _{volDetails = _}

_VolDetailsAction :: Prism' Action VD.Action
_VolDetailsAction = prism VolDetailsAction unwrap
  where 
  unwrap (VolDetailsAction a) = Right a
  unwrap a = Left a 
 
spec :: T.Spec _ State _ Action
spec = T.focus _header _HeaderAction H.spec 
    <> (over T._render container $ fold [ T.focus _volDetails _VolDetailsAction
                                            $ T.split _Just VD.spec
                                        , T.focus _shiftList _ShiftListAction SL.spec
                                        , handler
                                        ])
  where 
  container :: T.Render State _ Action -> T.Render State _ Action
  container render d p s c =
    [ RD.div [ RP.className "container" ]
             $ render d p s c
    ]
    
  handler :: T.Spec _ State _ Action
  handler = T.simpleSpec performAction T.defaultRender
    where 
    performAction :: T.PerformAction _ State _ Action
    performAction (HeaderAction (H.ChangeCurrentVol v)) _ _ = void $ T.modifyState $ changeCurrentVol v
    performAction (HeaderAction H.EditNewVol) _ _ = void $ T.modifyState editNewVol
    performAction (HeaderAction H.EditCurrentVol) _ _ = void $ T.modifyState editCurrentVol
    performAction (VolDetailsAction (VD.Save d)) _ s@{ currentVol: Just _ } = updateCurrentVol d s
    performAction (VolDetailsAction (VD.Save d)) _ s = addNewVol d s
    performAction (VolDetailsAction VD.Cancel) _ _ = void $ T.modifyState cancelEdit
    performAction _ _ _ = pure unit

changeCurrentVol :: Maybe Volunteer -> State -> State
changeCurrentVol currentVol' s = s{ currentVol = currentVol'
                                  , shiftList = SL.changeCurrentVol currentVol' s.shiftList
                                  , volDetails = Nothing
                                  }

editNewVol :: State -> State
editNewVol s = s{ currentVol = Nothing
                , shiftList = SL.changeCurrentVol Nothing s.shiftList
                , volDetails = Just $ VD.initialState Nothing
                }

editCurrentVol :: State -> State
editCurrentVol s@{ currentVol: Just _ } =
  s { volDetails = Just $ VD.initialState s.currentVol
    }
editCurrentVol s = s

updateCurrentVol :: VD.Details -> State -> _
updateCurrentVol d { currentVol: currentVol@Just (Volunteer v), vols } = do
  _ <- T.modifyState \s -> s { header = H.reqStarted s.header
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
    Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                           , volDetails = VD.enable <$> s.volDetails
                                           }
updateCurrentVol _ _ = pure unit

addNewVol :: VD.Details -> State -> _
addNewVol d { vols } = do
  _ <- T.modifyState \s -> s { header = H.reqStarted s.header
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
    Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                           , volDetails = VD.enable <$> s.volDetails
                                           }

updateVols :: { currentVol :: Maybe Volunteer, vols :: List Volunteer } -> State -> State
updateVols { currentVol, vols } s = s { currentVol = currentVol
                                      , vols = vols
                                      , shiftList = SL.changeCurrentVol currentVol s.shiftList
                                      , header = H.volDetailsUpdated vols currentVol s.header
                                      , volDetails = Nothing
                                      }
 
cancelEdit :: State -> State
cancelEdit s = s{ header = H.editCancelled s.header
                , volDetails = Nothing
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
      , header: H.initialState
      , volDetails: Nothing
      , config
      }

initialDataLoaded :: Array Volunteer -> Array Shift -> State -> State
initialDataLoaded vols shifts state =
  state { vols = toUnfoldable vols
        , shiftList = SL.initialState state.currentVol (toUnfoldable shifts) state.config
        , header = H.initialDataLoaded (toUnfoldable vols) state.header
        }

type MySettings = SPSettings_ SPParams_

settings :: MySettings
settings = defaultSettings $ SPParams_ { baseURL: "http://localhost:8081/" }

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff (ajax :: AJAX, err :: EXCEPTION | eff)))

apiReq :: forall a. Generic a => APIEffect _ a -> Aff _ (Either AjaxError a)
apiReq m = do
  delay (Milliseconds 5000.0)
  response <- runExceptT $ runReaderT m settings
  case response of
    Left err -> do
      liftEff $ log $ errorToString err
      pure $ Left err
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
    err e ctx state = do
      let state' = state { header = H.reqFailed e state.header }
      liftEff $ R.writeState ctx state'

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
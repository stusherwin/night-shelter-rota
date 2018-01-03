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
import Data.DateTime (Date, Weekday(..))
import Data.DateTime.Locale (LocalValue(..))
import Data.Date (year, month, day)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over, _Just)
import Data.List (List(..), snoc, last, fromFoldable, findIndex)
import Data.List (List(..), findIndex, find, modifyAt, snoc, deleteAt, length, all, nubBy, filter, catMaybes, any, singleton, length, filter, nubBy, any, findIndex, deleteAt, modifyAt, snoc)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import React as R
import React.DOM (s)
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
 
import App.Common (updateWhere, sortWith, nextWeekday)
import App.Header (State, Action(..), spec, initialState, volDetailsUpdated, editCancelled, reqStarted, reqSucceeded, reqFailed, initialDataLoaded) as H
import App.ShiftRules (ShiftRuleConfig)
import App.ShiftList (State, Action(..), RowAction(..), spec, initialState, changeCurrentVol, shiftUpdated) as SL
import App.VolDetails (State, Action(..), spec, initialState, disable, enable) as VD
import App.Types (OvernightPreference(..), OvernightGenderPreference(..), Volunteer, VolunteerShift, Shift, VolunteerDetails, ShiftType(..))
import App.ServerTypeConversion
import ServerTypes (OvernightPreference(..), OvernightGenderPreference(..), Volunteer(..), VolunteerShift(..), Shift(..), VolunteerDetails(..), ShiftType(..), ShiftDate(..)) as API

data Action = ShiftListAction SL.Action
            | HeaderAction H.Action
            | VolDetailsAction VD.Action
            | ReportError AjaxError

type State = { vols :: List Volunteer 
             , shifts :: List Shift
             , shiftList :: Maybe SL.State
             , currentVol :: Maybe Volunteer
             , header :: H.State
             , volDetails :: Maybe VD.State 
             , config :: ShiftRuleConfig
             }
 
_shiftList :: Lens' State (Maybe SL.State)
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
                                        , T.focus _shiftList _ShiftListAction
                                            $ T.split _Just SL.spec
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
    performAction (HeaderAction (H.ChangeCurrentVol v)) _ s =
      changeCurrentVol v s
    performAction (HeaderAction H.EditNewVol) _ _ =
      editNewVol
    performAction (HeaderAction H.EditCurrentVol) _ s =
      editCurrentVol s
    performAction (VolDetailsAction VD.Cancel) _ _ =
      cancelEdit
    performAction (VolDetailsAction (VD.Save d)) _ s =
      addOrUpdateCurrentVol d s
    performAction (ShiftListAction (SL.RowAction _ (SL.AddCurrentVol shiftDate shiftType))) _ s =
      addVolunteerShift shiftDate shiftType s
    performAction (ShiftListAction (SL.RowAction _ (SL.ChangeCurrentVolShiftType shiftDate shiftType))) _ s =
      updateVolunteerShift shiftDate shiftType s
    performAction (ShiftListAction (SL.RowAction _ (SL.RemoveCurrentVol shiftDate))) _ s =
      removeVolunteerShift shiftDate s
    performAction _ _ _ = pure unit

changeCurrentVol :: Maybe Volunteer -> State -> _
changeCurrentVol currentVol' { shiftList: Nothing } = do
  void $ T.modifyState \s -> s { currentVol = currentVol'
                               , shiftList = Just $ SL.initialState currentVol' s.shifts s.config
                               , volDetails = Nothing
                               }
changeCurrentVol currentVol' _ = do
  void $ T.modifyState \s -> s { currentVol = currentVol'
                               , shiftList = SL.changeCurrentVol currentVol' <$> s.shiftList
                               , volDetails = Nothing
                               }

editNewVol :: _
editNewVol = do
  void $ T.modifyState _ { currentVol = Nothing
                         , shiftList = Nothing
                         , volDetails = Just $ VD.initialState Nothing
                         }

editCurrentVol :: State -> _
editCurrentVol { currentVol: Just _ } = do
  void $ T.modifyState \s -> s { volDetails = Just $ VD.initialState s.currentVol
                               , shiftList = Nothing
                               }
editCurrentVol _ = pure unit

cancelEdit :: _
cancelEdit = do
  void $ T.modifyState \s -> s{ header = H.editCancelled s.header
                              , volDetails = Nothing
                              , shiftList = Just $ SL.initialState s.currentVol s.shifts s.config
                              }

addOrUpdateCurrentVol :: VolunteerDetails -> State -> _
addOrUpdateCurrentVol d s = case s.currentVol of
  Just v -> updateCurrentVol d v.id s
  _ -> addNewVol d s
  where
    updateCurrentVol :: VolunteerDetails -> Int -> State -> _
    updateCurrentVol d vId { vols, shifts } = do
      _ <- T.modifyState \s -> s { header = H.reqStarted s.header
                                 , volDetails = VD.disable <$> s.volDetails
                                 }
      resp <- lift $ apiReq $ flip postApiVolsById vId $ toAPIVolDetails d
      case resp of
        Right apiVol -> let vol = fromAPIVol apiVol
                            shifts' = updateVolunteer vol shifts
                        in void $ T.modifyState $ updateVols { currentVol: Just vol
                                                             , vols: updateWhere (\v -> v.id == vId) vol vols
                                                             , shifts: shifts'
                                                             }
        Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                               , volDetails = VD.enable <$> s.volDetails
                                               }
    addNewVol :: VolunteerDetails -> State -> _
    addNewVol d { vols, shifts } = do
      _ <- T.modifyState \s -> s { header = H.reqStarted s.header
                                 , volDetails = VD.disable <$> s.volDetails
                                 }
      resp <- lift $ apiReq $ putApiVols $ toAPIVolDetails d
      case resp of
        Right apiVol -> let vol = fromAPIVol apiVol
                        in  void $ T.modifyState $ updateVols { currentVol: Just vol
                                                              , vols: snoc vols vol
                                                              , shifts
                                                              }
        Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                               , volDetails = VD.enable <$> s.volDetails
                                               }
     
    updateVols :: { currentVol :: Maybe Volunteer, vols :: List Volunteer, shifts :: List Shift } -> State -> State
    updateVols { currentVol, vols, shifts } s = s { currentVol = currentVol
                                                  , vols = vols
                                                  , shifts = shifts
                                                  , shiftList = Just $ SL.initialState currentVol shifts s.config
                                                  , header = H.volDetailsUpdated vols currentVol s.header
                                                  , volDetails = Nothing
                                                  }

addVolunteerShift :: Date -> ShiftType -> State -> _
addVolunteerShift date shiftType { currentVol: Just v } = do
  _ <- T.modifyState \s -> s { header = H.reqStarted s.header
                             , volDetails = VD.disable <$> s.volDetails
                             }
  let (API.ShiftDate { year, month, day}) = toAPIShiftDate date
  resp <- lift $ apiReq $ putApiShiftsByYearByMonthByDayByVolId (toAPIShiftType shiftType) year month day v.id
  case resp of
    Right apiVolShifts -> let volShifts = fromAPIVolShifts apiVolShifts
                          in  void $ T.modifyState $ modifyShifts date $ updateShift date volShifts
    Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                           , volDetails = VD.enable <$> s.volDetails
                                           }
addVolunteerShift _ _ _ = pure unit

updateVolunteerShift :: Date -> ShiftType -> State -> _
updateVolunteerShift date shiftType { currentVol: Just v } = do
  _ <- T.modifyState \s -> s { header = H.reqStarted s.header
                             , volDetails = VD.disable <$> s.volDetails
                             }
  let (API.ShiftDate { year, month, day}) = toAPIShiftDate date
  resp <- lift $ apiReq $ postApiShiftsByYearByMonthByDayByVolId (toAPIShiftType shiftType) year month day v.id
  case resp of
    Right apiVolShifts -> let volShifts = fromAPIVolShifts apiVolShifts
                          in  void $ T.modifyState $ modifyShifts date $ updateShift date volShifts
    Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                           , volDetails = VD.enable <$> s.volDetails
                                           }
updateVolunteerShift _ _ _ = pure unit

removeVolunteerShift :: Date -> State -> _
removeVolunteerShift date { currentVol: Just v } = do
  _ <- T.modifyState \s -> s { header = H.reqStarted s.header
                             , volDetails = VD.disable <$> s.volDetails
                             }
  let (API.ShiftDate { year, month, day}) = toAPIShiftDate date
  resp <- lift $ apiReq $ deleteApiShiftsByYearByMonthByDayByVolId year month day v.id
  case resp of
    Right apiVolShifts -> let volShifts = fromAPIVolShifts apiVolShifts
                          in  void $ T.modifyState $ modifyShifts date $ updateShift date volShifts
    Left e -> void $ T.modifyState \s -> s { header = H.reqFailed e s.header
                                           , volDetails = VD.enable <$> s.volDetails
                                           }
removeVolunteerShift _ _ = pure unit

initialState :: Date -> State
initialState currentDate =
  let config = { maxVolsPerShift: 2
               , urgentPeriodDays: 14
               , currentDate
               }
  in  { vols: Nil
      , shifts: Nil
      , shiftList: Nothing
      , currentVol: Nothing
      , header: H.initialState
      , volDetails: Nothing
      , config
      }

initialDataLoaded :: List Volunteer -> List Shift -> State -> State
initialDataLoaded vols shifts s =
  s { vols = vols
    , shifts = shifts
    , shiftList = Just $ SL.initialState s.currentVol shifts s.config
    , header = H.initialDataLoaded vols s.header
    }

modifyShifts :: Date -> (List Shift -> List Shift) -> State -> State
modifyShifts date modify s =
  let shifts' = modify s.shifts
  in s { shifts = shifts'
       , shiftList = SL.shiftUpdated shifts' date <$> s.shiftList
       , header = H.reqSucceeded s.header
       }

updateShift :: Date -> List VolunteerShift -> List Shift -> List Shift
updateShift date volShifts shifts =
  case findIndex (\s -> s.date == date) shifts of
    Just i -> fromMaybe shifts $ modifyAt i (_ { volunteers = volShifts }) shifts
    _      -> shifts `snoc` { date, volunteers: volShifts }

updateVolunteer :: Volunteer -> List Shift -> List Shift
updateVolunteer v' = map updateShift
  where
  updateShift s = s { volunteers = map updateVol s.volunteers }

  updateVol vs | vs.volunteer.id == v'.id = vs { volunteer = v' }
  updateVol vs = vs

type MySettings = SPSettings_ SPParams_

settings :: MySettings
settings = defaultSettings $ SPParams_ { baseURL: "http://localhost:8081/" }

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff (ajax :: AJAX, err :: EXCEPTION | eff)))

apiReq :: forall a. Generic a => APIEffect _ a -> Aff _ (Either AjaxError a)
apiReq m = do
  delay $ Milliseconds 50.0
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
                                            Right apiVols, Right apiShifts -> do
                                              let vols = fromAPIVols apiVols
                                              let shifts = fromAPIShifts apiShifts
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
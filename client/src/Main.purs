module App.Main where 
     
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Data.Argonaut.Generic.Aeson
import Data.Generic
import Prelude
import Servant.PureScript.Settings
import ServerAPI
import Servant.PureScript.Affjax
import Control.Monad.Aff
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Network.HTTP.Affjax (AJAX, get)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)

import App.Common (updateWhere, sortWith, nextWeekday)
import App.CurrentVolSelector (State, Action(..), spec, initialState, changeVols) as CVS
import App.Data (fromDate)
import App.EditVolButton (State, Action, spec, initialState) as EVB
import App.NewVolButton (State, Action, spec, initialState) as NVB
import App.ShiftList (State, Action, spec, initialState, changeCurrentVol) as SL
import App.VolDetails (State, Action(..), Details, spec, initialState) as VD
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
import Data.List (List, snoc, last)
import Data.Maybe (Maybe(..), maybe)
import React as R
import React.DOM (s)
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import ServerTypes (OvernightPreference(..), OvernightGenderPreference(..), Volunteer(..), VolunteerShift(..), Shift(..))
import Thermite as T
 
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
  
spec :: forall eff. T.Spec (console :: CONSOLE | eff) State _ Action
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
    ]
    
  handler :: T.Spec _ State _ Action
  handler = T.simpleSpec performAction T.defaultRender
    where 
    performAction :: forall e. T.PerformAction (console :: CONSOLE | e) State _ Action
    performAction (CurrentVolSelectorAction (CVS.ChangeCurrentVol v)) _ _ = void $ T.modifyState $ changeCurrentVol v
    performAction (NewVolButtonAction _) _ _ = void $ T.modifyState startEditingNewVol
    performAction (EditVolButtonAction _) _ _ = void $ T.modifyState startEditingCurrentVol
    performAction (VolDetailsAction (VD.Save d)) _ _ = void $ T.modifyState $ addOrUpdateVol d
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
 
addOrUpdateVol :: VD.Details -> State -> State
addOrUpdateVol d s =
  let { currentVol', vols' } = addOrUpdate s
  in  s{ currentVol = currentVol'
       , vols = vols'
       , shiftList = SL.changeCurrentVol currentVol' s.shiftList
       , currentVolSelector = CVS.changeVols vols' currentVol' s.currentVolSelector
       , volDetails = Nothing
       , newVolButton = Just NVB.initialState
       , editVolButton = map (EVB.initialState <<< (\(Volunteer v) -> v.vName)) currentVol'
       }
  where
  addOrUpdate { currentVol: Just cv@(Volunteer v), vols } =
    let cv'  = Volunteer v{ vName = d.name
                 , vOvernightPreference = d.pref
                 , vOvernightGenderPreference = d.genderPref
                 , vNotes = d.notes
                 }
    in { currentVol': Just cv'
       , vols': updateWhere (\(Volunteer v') -> v'.vId == v.vId) cv' vols
       }
  addOrUpdate { vols } =
    let maxId  = maybe 0 (\(Volunteer v) -> v.vId) $ last $ sortWith (\(Volunteer v) -> v.vId) vols
        newVol = Volunteer { vId: maxId + 1
                 , vName: d.name
                 , vOvernightPreference: d.pref
                 , vOvernightGenderPreference: d.genderPref
                 , vNotes: d.notes
                 }
    in { currentVol': Just newVol
       , vols': snoc s.vols newVol
       }
 
cancelEditing :: State -> State
cancelEditing s = s{ volDetails = Nothing
                   , newVolButton = Just NVB.initialState
                   , editVolButton = map (EVB.initialState <<< (\(Volunteer v) -> v.vName)) s.currentVol
                   }

initialState :: Date -> Array Volunteer -> Array Shift -> State
initialState currentDate vols shifts =
  let currentVol = Nothing
      config = { maxVolsPerShift: 2
               , urgentPeriodDays: 14
               , currentDate
               }
  in  { vols: toUnfoldable vols
      , shiftList: SL.initialState currentVol (toUnfoldable shifts) config
      , currentVol: currentVol
      , currentVolSelector: CVS.initialState (toUnfoldable vols) currentVol
      , volDetails: Nothing
      , newVolButton: Just NVB.initialState
      , editVolButton: Nothing
      }

type MySettings = SPSettings_ SPParams_

settings :: MySettings
settings = defaultSettings $ SPParams_ { baseURL: "http://localhost:8081/" }

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE, dom :: DOM, now :: NOW  | eff)))

runEffect :: forall a. MySettings -> APIEffect () a -> Aff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE, dom :: DOM, now :: NOW) (Either AjaxError a)
runEffect settings m = runExceptT $ runReaderT m settings

main :: Unit
main = unsafePerformEff $ do 
  (LocalValue _ currentDate) <- nowDate
  log "stu"
  flip runAff_ (runEffect settings getApiVols) $ \rVols -> do
    case rVols of
      Left e -> log $ show e
      Right v ->
        case v of
          Left e -> log $ errorToString e
          Right vols -> do
            flip runAff_ (runEffect settings getApiShifts) $ \rShifts -> do
              case rShifts of
                Left e -> log $ show e
                Right s ->
                  case s of
                    Left e -> log $ errorToString e
                    Right shifts -> do
                      let component = T.createClass spec $ initialState currentDate vols shifts
                      let appEl = R.createFactory component {}
                      
                      if isServerSide
                         then void (log (RDOM.renderToString appEl)) 
                         else void (getElementById "app" >>= RDOM.render appEl)
                      hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit 

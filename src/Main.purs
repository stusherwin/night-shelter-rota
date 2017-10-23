module App.Main where 
   
import Prelude
 
import App.Common (lensWithProps, modifyWhere, updateWhere, addDays)
import App.CurrentVolDetails (State, Action(..), spec, initialState) as CVD
import App.CurrentVolSelector (State, Action(..), spec, initialState, changeVols) as CVS
import App.Data (OvernightSharingPrefs(..), Shift(..), Volunteer(..), VolId(..), Gender(..), nextVolId)
import App.ShiftList (State, Action(..), spec, initialState, changeCurrentVol) as SL
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Gen (frequency)
import Control.Monad.Trans.Class (lift)
import DOM.HTML.History (state)
import DOM.Node.Types (Element)
import Data.Array (find, cons, (!!), length, sortWith, snoc, last)
import Data.DateTime (Date)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over, _Just)
import Data.Maybe (Maybe(..), fromJust, maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM (render)
import ReactDOM as RDOM
import Thermite as T

data Action = ShiftListAction SL.Action
            | CurrentVolSelectorAction CVS.Action
            | CurrentVolDetailsAction CVD.Action
            | NewVol
            | EditCurrentVol

data VolDetailsEditState = EditingNewVol
                         | EditingCurrentVol

type State = { vols :: Array Volunteer
             , shiftList :: SL.State
             , currentVol :: Maybe Volunteer
             , currentVolSelector :: CVS.State
             , currentVolDetails :: Maybe CVD.State
             , volDetailsEditState :: Maybe VolDetailsEditState
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
 
_currentVolDetails :: Lens' State (Maybe CVD.State)
_currentVolDetails = lens _.currentVolDetails _{currentVolDetails = _}

_CurrentVolDetailsAction :: Prism' Action CVD.Action
_CurrentVolDetailsAction = prism CurrentVolDetailsAction unwrap
  where 
  unwrap (CurrentVolDetailsAction a) = Right a
  unwrap a = Left a 

spec :: forall props eff. T.Spec (console :: CONSOLE | eff) State props Action
spec = container (RD.div [ RP.className "container" ]) $ fold
  [ editCurrentVol
  , newVol
  , container (\c -> RD.h2' ([ RD.text "Night Shelter Rota for " ] <> c)) $
      T.focus _currentVolSelector _CurrentVolSelectorAction CVS.spec
  , T.focus _currentVolDetails _CurrentVolDetailsAction $
      T.split _Just CVD.spec
  , T.focus _shiftList _ShiftListAction SL.spec
  , handler
  ] 
  where 
  container :: forall state action. (Array R.ReactElement -> R.ReactElement) -> T.Spec (console :: CONSOLE | eff) state props action -> T.Spec (console :: CONSOLE | eff) state props action
  container elem = over T._render \render d p s c -> [ elem $ render d p s c ]

  newVol :: T.Spec _ State _ Action
  newVol = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ { volDetailsEditState: Nothing } _ =
      [ RD.button [ RP.className "ui button"
                  , RP.style { float: "right" }
                  , RP.onClick \e -> do
                      _ <- R.preventDefault e
                      dispatch NewVol
                  ]
                  [ RD.i [ RP.className "icon icon-add" ] []
                  , RD.text "New volunteer"
                  ]
      ]
    render dispatch _ _ _ = []

    performAction :: forall e. T.PerformAction (console :: CONSOLE | e) State _ Action
    performAction NewVol _ _ =
      void $ T.modifyState \state -> state{ currentVol = Nothing
                                          , shiftList = SL.changeCurrentVol Nothing state.shiftList
                                          , currentVolSelector = CVS.changeVols state.vols Nothing state.currentVolSelector
                                          , volDetailsEditState = Just EditingNewVol
                                          , currentVolDetails = Just $ CVD.initialState Nothing
                                          }
    performAction _ _ _ = pure unit
    
  editCurrentVol :: T.Spec _ State _ Action
  editCurrentVol = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ { volDetailsEditState: Nothing, currentVol: Just cv } _ =
      [ RD.button [ RP.className "ui primary button"
                  , RP.style { float: "right" }
                  , RP.onClick \e -> do
                      _ <- R.preventDefault e
                      dispatch EditCurrentVol
                  ]
                  [ RD.i [ RP.className "icon icon-edit" ] []
                  , RD.text $ "Edit " <> cv.name <> "'s preferences"
                  ]
      ]
    render dispatch _ _ _ = []

    performAction :: forall e. T.PerformAction (console :: CONSOLE | e) State _ Action
    performAction EditCurrentVol _ { currentVol: currentVol@(Just _) } =
      void $ T.modifyState \state -> state{ volDetailsEditState = Just EditingCurrentVol
                                          , currentVolDetails = Just $ CVD.initialState currentVol
                                          }
    performAction _ _ _ = pure unit
    

  handler :: T.Spec _ State _ Action
  handler = T.simpleSpec performAction T.defaultRender
    where 
    performAction :: forall e. T.PerformAction (console :: CONSOLE | e) State _ Action
    performAction (CurrentVolSelectorAction (CVS.ChangeCurrentVol vol)) _ _ =
      void $ T.modifyState \state -> state{ currentVol = vol
                                          , shiftList = SL.changeCurrentVol vol state.shiftList
                                          , currentVolDetails = Nothing
                                          , volDetailsEditState = Nothing
                                          }
    performAction (CurrentVolDetailsAction (CVD.Save details)) _ { volDetailsEditState: Just EditingCurrentVol } =
      void $ T.modifyState \state -> let vol  = _{ name = details.name, overnightSharingPrefs = Custom details.notes } <$> state.currentVol
                                         vols = maybe state.vols  (\cv -> updateWhere (\v -> v.id == cv.id) cv state.vols) vol
                                     in state{ currentVol = vol
                                             , vols = vols
                                             , shiftList = SL.changeCurrentVol vol state.shiftList
                                             , currentVolSelector = CVS.changeVols vols state.currentVol state.currentVolSelector
                                             , volDetailsEditState = Nothing
                                             , currentVolDetails = Nothing
                                             }
    performAction (CurrentVolDetailsAction (CVD.Save details)) _ { volDetailsEditState: Just EditingNewVol } =
      void $ T.modifyState \state -> let maxId  = maybe (VolId 0) (_.id) $ last $ sortWith (\v -> v.id) state.vols
                                         newVol = { id: nextVolId maxId, name: details.name, gender: Nothing, overnightSharingPrefs: Custom details.notes }
                                         vols   = snoc state.vols newVol
                                     in state{ currentVol = Just newVol
                                             , vols = vols
                                             , shiftList = SL.changeCurrentVol (Just newVol) state.shiftList
                                             , currentVolSelector = CVS.changeVols vols (Just newVol) state.currentVolSelector
                                             , volDetailsEditState = Nothing
                                             , currentVolDetails = Nothing
                                             }
    performAction (CurrentVolDetailsAction CVD.Cancel) _ { volDetailsEditState: Just _ } =
      void $ T.modifyState \state -> state{ volDetailsEditState = Nothing
                                          , currentVolDetails = Nothing
                                          }
    performAction _ _ _ = pure unit 
 

main :: Unit
main = unsafePerformEff $ do 
  (LocalValue _ currentDate) <- nowDate 
  let vols = [ { id: VolId 1, name: "Fred",    gender: Just Male,           overnightSharingPrefs: Any }
             , { id: VolId 2, name: "Alice",   gender: Just Female,         overnightSharingPrefs: (OnlyGender Female) }
             , { id: VolId 3, name: "Jim",     gender: Nothing,             overnightSharingPrefs: None }
             , { id: VolId 4, name: "Mary",    gender: Just Female,         overnightSharingPrefs: (Custom "Only nice people") }
             , { id: VolId 5, name: "Smoo 1",  gender: Just (Other "Smoo"), overnightSharingPrefs: (OnlyGender (Other "Smoo")) }
             , { id: VolId 6, name: "Smoo 2",  gender: Just (Other "Smoo"), overnightSharingPrefs: (OnlyGender (Other "Smoo")) }
             ]
  let shifts = []
  let currentVol = Nothing
  let component = T.createClass spec $ { vols
                                       , shiftList: SL.initialState currentVol shifts currentDate (addDays (-1) currentDate) 28
                                       , currentVol: currentVol
                                       , currentVolSelector: CVS.initialState vols currentVol
                                       , volDetailsEditState: Nothing
                                       , currentVolDetails: Nothing
                                       }
  let appEl = R.createFactory component {}
  
  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit 
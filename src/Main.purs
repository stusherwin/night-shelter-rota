module App.Main where 
    
import Prelude
 
import Data.Array (toUnfoldable) 
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
import Data.List (List(..), find, (:), (!!), length, snoc, last)
import Data.DateTime (Date, Weekday(..))
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

import App.Common (lensWithProps, modifyWhere, updateWhere, addDays, sortWith, nextWeekday)
import App.VolDetails (State, Action(..), Details, spec, initialState) as VD
import App.CurrentVolSelector (State, Action(..), spec, initialState, changeVols) as CVS
import App.Data (OvernightPreference(..), OvernightGenderPreference(..), Shift(..), Volunteer(..), VolunteerShift(..), VolId(..), Config, nextVolId)
import App.ShiftList (State, Action(..), spec, initialState, changeCurrentVol) as SL
import App.NewVolButton (State, Action, spec, initialState) as NVB
import App.EditVolButton (State, Action, spec, initialState) as EVB

data Action = ShiftListAction SL.Action
            | CurrentVolSelectorAction CVS.Action
            | VolDetailsAction VD.Action
            | NewVolButtonAction NVB.Action
            | EditVolButtonAction EVB.Action 
 
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
                                  , editVolButton = map (EVB.initialState <<< _.name) currentVol'
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
  let { currentVol', vols' } = addOrUpdate d s
  in  s{ currentVol = currentVol'
       , vols = vols'
       , shiftList = SL.changeCurrentVol currentVol' s.shiftList
       , currentVolSelector = CVS.changeVols vols' currentVol' s.currentVolSelector
       , volDetails = Nothing
       , newVolButton = Just NVB.initialState
       , editVolButton = map (EVB.initialState <<< _.name) currentVol'
       }
  where
  addOrUpdate d { currentVol: Just cv, vols } =
    let cv'  = cv{ name = d.name
                 , overnightPreference = d.pref
                 , overnightGenderPreference = d.genderPref
                 , notes = d.notes
                 }
    in { currentVol': Just cv'
       , vols': updateWhere (\v -> v.id == cv.id) cv' vols
       }
  addOrUpdate d { vols } =
    let maxId  = maybe (VolId 0) _.id $ last $ sortWith _.id vols
        newVol = { id: nextVolId maxId
                 , name: d.name
                 , overnightPreference: d.pref
                 , overnightGenderPreference: d.genderPref
                 , notes: d.notes
                 }
    in { currentVol': Just newVol
       , vols': snoc s.vols newVol
       }
  
cancelEditing :: State -> State
cancelEditing s = s{ volDetails = Nothing
                   , newVolButton = Just NVB.initialState
                   , editVolButton = map (EVB.initialState <<< _.name) s.currentVol
                   }

initialState :: Date -> State
initialState currentDate =
  let fred  = { id: VolId 1
              , name: "Fred"
              , overnightPreference: Just PreferAnotherVolunteer
              , overnightGenderPreference: Nothing
              , notes: ""
              }
      alice = { id: VolId 2
              , name: "Alice"
              , overnightPreference: Nothing
              , overnightGenderPreference: Just Female
              , notes: ""
              }
      jim   = { id: VolId 3
              , name: "Jim"
              , overnightPreference: Just PreferToBeAlone
              , overnightGenderPreference: Just Male
              , notes: ""
              }
      mary  = { id: VolId 4
              , name: "Mary"
              , overnightPreference: Nothing
              , overnightGenderPreference: Nothing
              , notes: "Only nice people"
              }
      vols = toUnfoldable [ fred, alice, jim, mary ]
      shifts = toUnfoldable [ { date: currentDate
                              , volunteers: toUnfoldable [ Overnight fred
                                                         , Evening alice
                                                         , Overnight jim
                                                         , Evening mary
                                                         ]
                              }
                            , { date: nextWeekday Sunday currentDate
                              , volunteers: toUnfoldable [ Overnight fred
                                                         , Overnight jim
                                                         , Evening mary
                                                         ]
                              }
                            ] 
      currentVol = Nothing --Just fred
      config = { maxVolsPerShift: 2
               , urgentPeriodDays: 14
               , currentDate
               }
  in  { vols
      , shiftList: SL.initialState currentVol shifts config
      , currentVol: currentVol
      , currentVolSelector: CVS.initialState vols currentVol
      , volDetails: Nothing
      , newVolButton: Just NVB.initialState
      , editVolButton: Nothing
      }

main :: Unit
main = unsafePerformEff $ do 
  (LocalValue _ currentDate) <- nowDate
  let component = T.createClass spec $ initialState currentDate
  let appEl = R.createFactory component {}
  
  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit 
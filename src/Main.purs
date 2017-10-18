module App.Main where 
 
import Prelude 

import App.Common (lensWithProps, modifyWhere, updateWhere)
import App.CurrentVolSelector (CurrentVolSelectorState, CurrentVolSelectorAction(..), currentVolSelectorSpec, currentVolSelectorInitialState, changeVols)
import App.CurrentVolDetails (CurrentVolDetailsState, CurrentVolDetailsAction(..), currentVolDetailsSpec, currentVolDetailsInitialState, changeCurrentVol')
import App.Data (OvernightSharingPrefs(..), Shift(..), Volunteer(..), VolId(..), Gender(..), hasId)
import App.Shift (ShiftAction(..))
import App.ShiftList (ShiftListProps, ShiftListState, ShiftListAction(..), shiftListSpec, shiftListInitialState, changeCurrentVol)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import DOM.Node.Types (Element)
import Data.Array (find, cons, (!!), length)
import Data.DateTime (Date)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (fold) 
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data Action = ShiftListAction ShiftListAction
            | CurrentVolSelectorAction CurrentVolSelectorAction
            | CurrentVolDetailsAction CurrentVolDetailsAction
 
type State = { vols :: Array Volunteer
             , shiftList :: ShiftListState
             , currentVol :: Maybe Volunteer
             , currentVolSelector :: CurrentVolSelectorState
             , currentVolDetails :: CurrentVolDetailsState
             }
 
_shiftList :: Lens' State ShiftListState
_shiftList = lens _.shiftList _{shiftList = _}

_ShiftListAction :: Prism' Action ShiftListAction
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a

_currentVolSelector :: Lens' State CurrentVolSelectorState
_currentVolSelector = lens _.currentVolSelector _{currentVolSelector = _}

_CurrentVolSelectorAction :: Prism' Action CurrentVolSelectorAction
_CurrentVolSelectorAction = prism CurrentVolSelectorAction unwrap
  where 
  unwrap (CurrentVolSelectorAction a) = Right a
  unwrap a = Left a 
 
_currentVolDetails :: Lens' State CurrentVolDetailsState
_currentVolDetails = lens _.currentVolDetails _{currentVolDetails = _}

_CurrentVolDetailsAction :: Prism' Action CurrentVolDetailsAction
_CurrentVolDetailsAction = prism CurrentVolDetailsAction unwrap
  where 
  unwrap (CurrentVolDetailsAction a) = Right a
  unwrap a = Left a 

spec :: forall props eff. T.Spec (console :: CONSOLE | eff) State props Action
spec = container (RD.div [ RP.className "container" ]) $ fold
  [ headerSpec
  , container (RD.div [ RP.className "ui form" ]) $ fold
      [ T.focus _currentVolSelector _CurrentVolSelectorAction currentVolSelectorSpec
      , T.focus _currentVolDetails _CurrentVolDetailsAction currentVolDetailsSpec
      ]
  , T.focus _shiftList _ShiftListAction shiftListSpec
  ] 
  where 
  container :: forall state action. (Array R.ReactElement -> R.ReactElement) -> T.Spec (console :: CONSOLE | eff) state props action -> T.Spec (console :: CONSOLE | eff) state props action
  container elem = over T._render \render d p s c -> [ elem $ render d p s c ]

  headerSpec :: T.Spec _ State _ Action
  headerSpec = T.simpleSpec performAction render
    where 
    render :: T.Render State _ Action
    render dispatch _ state _ =
      [ RD.h2' [ RD.text "Night Shelter Rota" 
               ]
      ]
     
    performAction :: forall e. T.PerformAction (console :: CONSOLE | e) State _ Action
    performAction (CurrentVolSelectorAction (ChangeCurrentVol volId)) _ _ =
      void $ T.modifyState \state -> let vol = volId >>= \id -> find (hasId id) state.vols
                                     in state{ currentVol = vol
                                             , shiftList = changeCurrentVol vol state.shiftList
                                             , currentVolDetails = changeCurrentVol' vol state.currentVolDetails
                                             }
    performAction (CurrentVolDetailsAction (ChangeCurrentVolName name)) _ _ =
      void $ T.modifyState \state -> let vol = state.currentVol >>= \(Vol v) -> Just (Vol v{name = name})
                                         vols = maybe state.vols (\cv@(Vol v) -> updateWhere (hasId v.id) cv state.vols) vol
                                     in state{ currentVol = vol
                                             , shiftList = changeCurrentVol vol state.shiftList
                                             , currentVolSelector = changeVols vols state.currentVolSelector
                                             }
    performAction _ _ _ = pure unit 

 
main :: Unit
main = unsafePerformEff $ do 
  (LocalValue _ currentDate) <- nowDate 
  let vols = [ Vol { id: VolId 1, name: "Fred",    gender: Just Male,           overnightSharingPrefs: Any }
             , Vol { id: VolId 2, name: "Alice",   gender: Just Female,         overnightSharingPrefs: (OnlyGender Female) }
             , Vol { id: VolId 3, name: "Jim",     gender: Nothing,             overnightSharingPrefs: None }
             , Vol { id: VolId 4, name: "Mary",    gender: Just Female,         overnightSharingPrefs: (Custom "Only nice people") }
             , Vol { id: VolId 5, name: "Smoo 1",  gender: Just (Other "Smoo"), overnightSharingPrefs: (OnlyGender (Other "Smoo")) }
             , Vol { id: VolId 6, name: "Smoo 2",  gender: Just (Other "Smoo"), overnightSharingPrefs: (OnlyGender (Other "Smoo")) }
             ]
  let shifts = []
  let currentVol = Nothing
  let component = T.createClass spec $ { vols
                                       , shiftList: shiftListInitialState currentVol shifts currentDate
                                       , currentVol: currentVol
                                       , currentVolSelector: currentVolSelectorInitialState vols currentVol
                                       , currentVolDetails: currentVolDetailsInitialState currentVol
                                       }
  let appEl = R.createFactory component {}
 
  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit 
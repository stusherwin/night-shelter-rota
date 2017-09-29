module App.Main where 
  
import Prelude
 
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Class (liftAff)
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
 
import App.Common (lensWithProps, modifyWhere)
import App.CurrentVolunteer (CurrentVolProps, CurrentVolState, CurrentVolAction(..), currentVolSpec, currentVolInitialState)
import App.Data (Shift(..), Volunteer(..))
import App.Shift (ShiftAction(..))
import App.ShiftList (ShiftListProps, ShiftListState, ShiftListAction(..), shiftListSpec, shiftListInitialState, changeCurrentVol)

data Action = ShiftListAction ShiftListAction
            | CurrentVolAction CurrentVolAction
 
type State = { vols :: Array Volunteer
             , shiftList :: ShiftListState
             , currentVol :: CurrentVolState }
 
_ShiftListAction :: Prism' Action ShiftListAction
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a
 
_shiftListState :: Lens' State ShiftListState
_shiftListState = lens _.shiftList _{shiftList = _}

_currentVolState :: Lens' State CurrentVolState
_currentVolState = lens _.currentVol _{currentVol = _}

_CurrentVolAction :: Prism' Action CurrentVolAction
_CurrentVolAction = prism CurrentVolAction unwrap
  where 
  unwrap (CurrentVolAction a) = Right a
  unwrap a = Left a 
 
spec :: forall props eff. T.Spec (console :: CONSOLE | eff) State props Action
spec = container $ fold
  [ T.focus _currentVolState _CurrentVolAction currentVolSpec
  , headerSpec
  , T.focus _shiftListState _ShiftListAction shiftListSpec
  ] 
  where 
  container :: forall state action. T.Spec (console :: CONSOLE | eff) state props action -> T.Spec (console :: CONSOLE | eff) state props action
  container = over T._render \render d p s c ->
    [ RD.div [ RP.className "container" ] (render d p s c) ]
 
  headerSpec :: T.Spec _ State _ Action
  headerSpec = T.simpleSpec performAction render
    where 
    render :: T.Render State _ Action
    render dispatch _ state _ =
      [ RD.h2' [ RD.text "Night Shelter Rota" 
               , RD.text $ maybe "" (\(Vol v) -> " for " <> v.name) state.currentVol.currentVol ] ]
     
    performAction :: forall e. T.PerformAction (console :: CONSOLE | e) State _ Action
    performAction (CurrentVolAction (ChangeCurrentVol v)) _ _ = void $ T.modifyState \state -> state{ shiftList = changeCurrentVol v state.shiftList }
    performAction _ _ _ = pure unit

 
main :: Unit
main = unsafePerformEff $ do 
  (LocalValue _ currentDate) <- nowDate 
  let vols = [ Vol { id: 1, name: "Stu" }
             , Vol { id: 2, name: "Bob" } ]
  let shifts = []
  let currentVol = Nothing 
  let component = T.createClass spec $ { vols: vols 
                                       , shiftList: shiftListInitialState currentVol shifts currentDate
                                       , currentVol: currentVolInitialState vols currentVol }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit 
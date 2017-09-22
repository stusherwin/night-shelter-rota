module App.Main where

import Prelude 
 
import App.CurrentVolunteer (CurrentVolunteerState, CurrentVolunteerAction(..), currentVolunteerSpec, buildCurrentVolunteerState, updateCurrentVolunteerState)
import App.ShiftList (ShiftListState, ShiftListAction, shiftListSpec, buildShiftListState, updateShiftListState)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import Data.DateTime (Date)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Array (find)
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
  
data Action = ShiftListAction ShiftListAction
            | CurrentVolunteerAction CurrentVolunteerAction

type Shift = { shift :: Date }

type Volunteer = { id :: Int
                 , name :: String }

type State = { currentDate :: Date
             , shifts :: Array Shift
             , volunteers :: Array Volunteer
             , currentVolunteer :: Maybe Volunteer
             , shiftListState :: ShiftListState
             , currentVolunteerState :: CurrentVolunteerState }

_ShiftListAction :: Prism' Action ShiftListAction
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a

_shiftListState :: Lens' State ShiftListState
_shiftListState = lens _.shiftListState (_ { shiftListState = _ })

_currentVolunteerState :: Lens' State CurrentVolunteerState
_currentVolunteerState = lens _.currentVolunteerState (_ { currentVolunteerState = _ })
 
_CurrentVolunteerAction :: Prism' Action CurrentVolunteerAction
_CurrentVolunteerAction = prism CurrentVolunteerAction unwrap
  where 
  unwrap (CurrentVolunteerAction a) = Right a
  unwrap a = Left a

spec :: forall props eff. T.Spec eff State props Action
spec = container $ fold
  [ T.focus _currentVolunteerState _CurrentVolunteerAction currentVolunteerSpec
  , headerSpec
  , T.focus _shiftListState _ShiftListAction shiftListSpec
  ]
  where
  container :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
  container = over T._render \render d p s c ->
    [ RD.div [ RP.className "container-fluid mt-3" ] (render d p s c) ]

  headerSpec :: T.Spec _ State _ Action
  headerSpec = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ state _ =
      [ RD.h2' [ RD.text "Night Shelter Rota"
               , RD.text $ maybe "" (\v -> " for " <> v.name) state.currentVolunteer ] ]
  
    performAction :: T.PerformAction _ State _ Action
    performAction (CurrentVolunteerAction (ChangeCurrentVolunteer id)) _ _ = void $ T.modifyState \state ->
        let volunteer = (maybe Nothing (\id -> find (\v -> v.id == id) state.volunteers) id)
        in state { currentVolunteer = volunteer 
                 , shiftListState = (updateShiftListState (volunteer) state.shiftListState)
                 , currentVolunteerState = (updateCurrentVolunteerState volunteer state.currentVolunteerState)
                 }
    performAction _ _ _ = pure unit 

main :: Unit
main = unsafePerformEff $ do
  (LocalValue _ currentDate) <- nowDate
  let volunteers = [ { id: 1, name: "Stu" }
                   , { id: 2, name: "Bob" } ]
  let shifts = []
  let currentVolunteer = Nothing
  let component = T.createClass spec $ { currentDate: currentDate
                                       , shifts: shifts
                                       , currentVolunteer: currentVolunteer
                                       , volunteers: volunteers
                                       , shiftListState: buildShiftListState shifts currentDate currentVolunteer
                                       , currentVolunteerState: buildCurrentVolunteerState volunteers currentVolunteer }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
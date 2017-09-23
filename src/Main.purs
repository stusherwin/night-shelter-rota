module App.Main where

import Prelude 
 
import App.CurrentVolunteer (CurrentVolunteerProps, CurrentVolunteerState, CurrentVolunteerAction(..), currentVolunteerSpec, currentVolunteerInitialState)
import App.ShiftList (ShiftListProps, ShiftListState, ShiftListAction, shiftListSpec, shiftListInitialState)
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
  
import App.Data (Shift, Volunteer)
import App.Common (lensWithProps) 

data Action = ShiftListAction ShiftListAction
            | CurrentVolunteerAction CurrentVolunteerAction

type State = { shifts :: Array Shift
             , volunteers :: Array Volunteer
             , shiftListState :: ShiftListState
             , currentVolunteerState :: CurrentVolunteerState }

_ShiftListAction :: Prism' Action ShiftListAction
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a

_shiftListState :: Lens' State (Tuple ShiftListProps ShiftListState)
_shiftListState = lensWithProps _.shiftListState _{shiftListState = _}
  \s -> { currentVolunteer: s.currentVolunteerState.currentVolunteer
        , shifts: s.shifts }

_currentVolunteerState :: Lens' State (Tuple CurrentVolunteerProps CurrentVolunteerState)
_currentVolunteerState = lensWithProps _.currentVolunteerState _{currentVolunteerState = _}
  \s -> { volunteers: s.volunteers }

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
               , RD.text $ maybe "" (\v -> " for " <> v.name) state.currentVolunteerState.currentVolunteer ] ]
  
    performAction :: T.PerformAction _ State _ Action
    performAction _ _ _ = pure unit 

main :: Unit
main = unsafePerformEff $ do
  (LocalValue _ currentDate) <- nowDate
  let volunteers = [ { id: 1, name: "Stu" }
                   , { id: 2, name: "Bob" } ]
  let shifts = []
  let currentVolunteer = Nothing
  let component = T.createClass spec $ { shifts: shifts
                                       , volunteers: volunteers
                                       , shiftListState: shiftListInitialState { currentVolunteer: currentVolunteer 
                                                                               , shifts: shifts } currentDate
                                       , currentVolunteerState: currentVolunteerInitialState { volunteers: volunteers } currentVolunteer }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
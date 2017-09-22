module App.Main where

import Prelude

import App.Common (Volunteer)
import App.CurrentVolunteer (CurrentVolunteerState, CurrentVolunteerAction, currentVolunteerSpec)
import App.ShiftList (ShiftListState, ShiftListAction, shiftListSpec)
import App.Shift (ShiftState, buildShifts)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List, snoc, last) as L
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
 
type State = { currentDate :: Date
             , shifts :: L.List ShiftState
             , volunteers :: Array Volunteer
             , currentVolunteer :: Maybe Volunteer }

_ShiftListAction :: Prism' Action ShiftListAction
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a

_shiftListState :: Lens' State ShiftListState
_shiftListState = lens getter setter
  where
  getter s = { currentDate: s.currentDate
             , currentVolunteer: s.currentVolunteer
             , shifts: s.shifts }
  setter s _ = s

_currentVolunteerState :: Lens' State CurrentVolunteerState
_currentVolunteerState = lens getter setter
  where
  getter s = { volunteers: s.volunteers, currentVolunteer: s.currentVolunteer }
  setter s a = (s { currentVolunteer = a.currentVolunteer })
 
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
  headerSpec = T.simpleSpec T.defaultPerformAction render
    where
    render :: T.Render State _ Action
    render dispatch _ state _ =
      [ RD.h2' [ RD.text "Night Shelter Rota"
               , RD.text $ maybe "" (\v -> " for " <> v.name) state.currentVolunteer ] ]

main :: Unit
main = unsafePerformEff $ do
  (LocalValue _ now) <- nowDate
  let component = T.createClass spec $ { currentDate: now
                                       , shifts: buildShifts now 7
                                       , currentVolunteer: Nothing
                                       , volunteers: [ { id: 1, name: "Stu"}
                                                     , { id: 2, name: "Bob"} ] }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl))
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
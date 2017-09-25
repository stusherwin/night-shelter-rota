module App.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import Data.Array (find, cons, (!!))
import Data.DateTime (Date)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
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

import App.Common (lensWithProps, modifyWhere)
import App.CurrentVolunteer (CurrentVolProps, CurrentVolState, CurrentVolAction(..), currentVolSpec, currentVolInitialState)
import App.Data (Shift(..), Volunteer(..), addVolunteer)
import App.Shift (ShiftAction(..))
import App.ShiftList (ShiftListProps, ShiftListState, ShiftListAction(..), shiftListSpec, shiftListInitialState)

data Action = ShiftListAction ShiftListAction
            | CurrentVolAction CurrentVolAction
 
type State = { shifts :: Array Shift
             , vols :: Array Volunteer
             , shiftList :: ShiftListState
             , currentVol :: CurrentVolState }
 
_ShiftListAction :: Prism' Action ShiftListAction
_ShiftListAction = prism ShiftListAction unwrap
  where
  unwrap (ShiftListAction a) = Right a
  unwrap a = Left a
 
_shiftListState :: Lens' State (Tuple ShiftListProps ShiftListState)
_shiftListState = lensWithProps _.shiftList _{shiftList = _}
  \s -> { currentVol: s.currentVol.currentVol
        , shifts: s.shifts }

_currentVolState :: Lens' State (Tuple CurrentVolProps CurrentVolState)
_currentVolState = lensWithProps _.currentVol _{currentVol = _}
  \s -> { vols: s.vols }

_CurrentVolAction :: Prism' Action CurrentVolAction
_CurrentVolAction = prism CurrentVolAction unwrap
  where 
  unwrap (CurrentVolAction a) = Right a
  unwrap a = Left a 
 
spec :: forall props eff. T.Spec eff State props Action
spec = container $ fold
  [ T.focus _currentVolState _CurrentVolAction currentVolSpec
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
               , RD.text $ maybe "" (\(V v) -> " for " <> v.name) state.currentVol.currentVol ] ]
    
    performAction :: T.PerformAction _ State _ Action
    performAction (ShiftListAction (ShiftAction _ (AddCurrentVol shiftDate))) _ _ = void $ T.modifyState \state -> state{ shifts = addVolunteer shiftDate state.currentVol.currentVol state.shifts }
    performAction _ _ _ = pure unit 

main :: Unit
main = unsafePerformEff $ do
  (LocalValue _ currentDate) <- nowDate
  let vols = [ V { id: 1, name: "Stu" }
             , V { id: 2, name: "Bob" } ]
  let shifts = []
  let currentVol = Nothing
  let component = T.createClass spec $ { shifts: shifts
                                       , vols: vols
                                       , shiftList: shiftListInitialState { currentVol: currentVol 
                                                                          , shifts: shifts } currentDate
                                       , currentVol: currentVolInitialState { vols: vols } currentVol }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl)) 
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean 

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
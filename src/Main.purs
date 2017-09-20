module Main where

import Prelude

import Common (unsafeEventValue)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import Data.Array (deleteAt, snoc, last, head)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..), fromRight, either)
import Data.Enum (toEnum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Foldable (fold)
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (fromJust, maybe)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Shift (ShiftState, ShiftAction, shiftSpec, buildShifts, tomorrow)

data Action = AddShift
            | ShiftAction Int ShiftAction

type State = { shifts :: L.List ShiftState }

_shifts :: Lens' State (L.List ShiftState)
_shifts = lens _.shifts (_ { shifts = _})

_ShiftAction :: Prism' Action (Tuple Int ShiftAction)
_ShiftAction = prism (uncurry ShiftAction) \ta ->
  case ta of 
    ShiftAction i a -> Right (Tuple i a)
    _ -> Left ta

headerSpec :: T.Spec _ State _ Action
headerSpec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.h2' [ RD.text "Night Shelter Rota" ] ]

footerSpec :: T.Spec _ State _ Action
footerSpec = T.simpleSpec performAction render
  where
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.a [ RP.onClick \_ -> dispatch AddShift
          , RP.href "#"
          , RP.role "button"
          , RP.className "btn btn-primary"
          ]
          [ RD.text "Add shift" ]
    ]

  performAction :: T.PerformAction _ State _ Action
  performAction _ _ _ = void $ T.modifyState \state -> state { shifts = L.snoc state.shifts {shift:(tomorrow $ _.shift $ unsafePartial $ fromJust $ L.last state.shifts)} }

spec :: forall props eff. T.Spec eff State props Action
spec = container $ fold
  [ headerSpec
  , T.focus _shifts _ShiftAction (T.foreach \_ -> shiftSpec)
  , footerSpec
  ]
  where
  container :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
  container = over T._render \render d p s c ->
    [ RD.div [ RP.className "container-fluid mt-3" ] (render d p s c) ]

main :: Unit
main = unsafePerformEff $ do
  (LocalValue _ now) <- nowDate
  let component = T.createClass spec $ { shifts : buildShifts now 3 }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl))
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
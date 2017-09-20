module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Now (nowDate)
import DOM.Node.Types (Element)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..), fromRight, either)
import Data.Enum (toEnum)
import Data.Lens (Lens', lens, Prism', prism)
import Data.Array (deleteAt, snoc, last, head)
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
import Data.Formatter.DateTime (formatDateTime)

data Action = AddShift

type State = { shifts :: Array Date }

renderShift :: Date -> ReactElement
renderShift shift = RD.div [ RP.className "alert alert-primary mt-3" ]
                           [ RD.text $ unsafePartial $ fromRight $ formatDateTime "D MMMM YYYY" (DateTime shift midnight) ] 

render :: T.Render State _ Action
render send _ state _ =
  [ RD.div [ RP.className "container-fluid mt-3" ]
         $ [ RD.h2' [ RD.text "Night Shelter Rota" ] ]
        <> map renderShift state.shifts
        <> [ RD.a [ RP.onClick \_ -> send AddShift
                  , RP.href "#"
                  , RP.role "button"
                  , RP.className "btn btn-primary"
                  ]
                  [ RD.text "Add shift" ]
            ]
  ]

startDate :: Date
startDate = unsafePartial fromJust $ canonicalDate <$> toEnum 2017 <*> pure bottom <*> pure bottom

midnight :: Time
midnight = unsafePartial fromJust $ Time <$> pure bottom <*> pure bottom <*> pure bottom <*> pure bottom

tomorrow :: Date -> Date
tomorrow dt = maybe dt date $ adjust (Days 1.0) (DateTime dt bottom)

performAction :: T.PerformAction _ State _ Action
performAction AddShift _ _ = void $ T.modifyState \state -> state { shifts = snoc state.shifts (maybe startDate tomorrow (last state.shifts)) }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

main :: Unit
main = unsafePerformEff $ do
  (LocalValue _ now) <- nowDate
  let shifts = [now, tomorrow now, tomorrow (tomorrow now)]
  let component = T.createClass spec $ { shifts : shifts }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl))
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
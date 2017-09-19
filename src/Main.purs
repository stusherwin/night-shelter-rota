module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data Action = Increment | Decrement

type State = { counter :: Int }

render :: T.Render State _ Action
render send _ state _ =
  [ RD.div [ RP.className "container-fluid mt-3" ]
           [ RD.h2' [ RD.text "Thermite test app" ]
           , RD.div [ RP.className "alert alert-primary mt-3" ]
                    [ RD.text (show state.counter) ]
           , RD.a [ RP.onClick \_ -> send Increment
                  , RP.href "#"
                  , RP.role "button"
                  , RP.className "btn btn-primary"
                  ]
                  [ RD.text "Increment" ]
           , RD.a [ RP.onClick \_ -> send Decrement
                  , RP.href "#"
                  , RP.role "button"
                  , RP.className "btn btn-primary ml-2"
                  ]
                  [ RD.text "Decrement" ]
           ]   
  ]

performAction :: T.PerformAction _ State _ Action
performAction Increment _ _ = void $ T.modifyState \state -> state { counter = min 5 (state.counter + 1) }
performAction Decrement _ _ = void $ T.modifyState \state -> state { counter = max 0 (state.counter - 1) }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main :: Unit
main = unsafePerformEff $ do
  let component = T.createClass spec $ { counter : 2 }
  let appEl = R.createFactory component {}

  if isServerSide
     then void (log (RDOM.renderToString appEl))
     else void (getElementById "app" >>= RDOM.render appEl)

  hot

foreign import isServerSide :: Boolean

foreign import getElementById :: forall eff. String -> Eff eff Element

foreign import hot :: forall eff. Eff eff Unit
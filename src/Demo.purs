module Demo (Action) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types (Element)
import Data.List (List(..), deleteAt)
import Data.Maybe (fromMaybe)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)
import Data.Lens (Lens', lens, Prism', prism)
import Data.Tuple (Tuple (..), uncurry)
import Data.Either (Either (..))

data Action = Increment | Decrement

type State = { counter :: Int }

render' :: T.Render State _ Action
render' send _ state _ =
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

performAction' :: T.PerformAction _ State _ Action
performAction' Increment _ _ = void $ T.modifyState \state -> state { counter = min 5 (state.counter + 1) }
performAction' Decrement _ _ = void $ T.modifyState \state -> state { counter = max 0 (state.counter - 1) }

spec' :: T.Spec _ State _ Action
spec' = T.simpleSpec performAction' render'







data TaskAction
  = EditText String
  | RemoveTask

type TaskState = { text :: String }

task :: T.Spec _ TaskState _ TaskAction
task = T.simpleSpec performAction render
  where
  performAction :: T.PerformAction _ TaskState _ TaskAction
  performAction (EditText text) _ _ = void $ T.modifyState \state -> state { text = text }
  performAction _ _ _ = pure unit

  render :: T.Render TaskState _ TaskAction
  render send _ state _ =
    [ RD.p' [ RD.input [ RP.value state.text 
                      , RP.onChange \e -> send (EditText (unsafeEventValue e))
                      ] []
            , RD.button [ RP.onClick \_ -> send RemoveTask ] [ RD.text "X" ]
            ]
    ]

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

data TaskListAction
  = NewTask
  | TaskAction Int TaskAction

type TaskListState = { tasks :: List TaskState }

header :: T.Spec _ TaskListState _ TaskListAction
header = T.simpleSpec performAction render
  where
  render :: T.Render TaskListState _ TaskListAction
  render send _ state _ =
    [ RD.p' [ RD.button [ RP.onClick \_ -> send NewTask ]
                        [ RD.text "New task" ] 
            ]
    ]
  
  performAction :: T.PerformAction _ TaskListState _ TaskListAction
  performAction NewTask _ _ = void $ T.modifyState \state -> state { tasks = Cons { text: "" } state.tasks }
  performAction (TaskAction i RemoveTask) _ _ = void $ T.modifyState \state -> state { tasks = fromMaybe state.tasks (deleteAt i state.tasks) }
  performAction _ _ _ = pure unit

_tasks :: Lens' TaskListState (List TaskState)
_tasks = lens _.tasks (_ { tasks = _})

_TaskAction :: Prism' TaskListAction (Tuple Int TaskAction)
_TaskAction = prism (uncurry TaskAction) \ta ->
  case ta of 
    TaskAction i a -> Right (Tuple i a)
    _ -> Left ta

taskList :: T.Spec _ TaskListState _ TaskListAction
taskList = header <> T.focus _tasks _TaskAction (T.foreach \_ -> task)
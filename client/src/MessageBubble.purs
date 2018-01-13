module App.MessageBubble where

import Prelude 
import Data.Maybe(Maybe(..))
import Thermite as T
import React (ReactElement, preventDefault) as R
import React.DOM as RD
import React.DOM.Props as RP

type Message = { header :: String
               , body :: String
               }

data MessageBubbleType = Transitory | Fixed
derive instance eqMessageBubbleType :: Eq MessageBubbleType

data MessageBubble = Hidden (Maybe Message)
                   | Visible MessageBubbleType Message

data MessageBubbleAction = Show MessageBubbleType
                         | Hide MessageBubbleType
                         | Toggle MessageBubbleType

renderMessageBubble :: (MessageBubbleAction -> T.EventHandler) -> MessageBubble -> Array R.ReactElement
renderMessageBubble _ (Hidden _) = []
renderMessageBubble dispatch (Visible t msg) = [ RD.div [ RP.className "header-status-message" ] 
                                                        $
                                                        [ RD.h3' [ RD.text msg.header ]
                                                        , RD.p' [ RD.text msg.body ]
                                                        ]
                                                        <> close t
                                               ]
  where
  close :: MessageBubbleType -> Array R.ReactElement
  close Transitory = []
  close Fixed = [ RD.a [ RP.href "#"
                       , RP.onClick $ R.preventDefault >=> (const $ dispatch $ Hide Fixed)
                       ]
                       [ RD.i [ RP.className "icon-cancel"] []
                       ]
                ]

handleMessageBubbleAction :: MessageBubbleAction -> MessageBubble -> MessageBubble
handleMessageBubbleAction (Show Transitory) (Hidden (Just msg))         = Visible Transitory msg
handleMessageBubbleAction (Show Fixed)      (Hidden (Just msg))         = Visible Fixed msg
handleMessageBubbleAction (Show Fixed)      (Visible Transitory msg)    = Visible Fixed msg
handleMessageBubbleAction (Toggle Fixed)    (Hidden (Just msg))         = Visible Fixed msg
handleMessageBubbleAction (Toggle Fixed)    (Visible Fixed msg)         = Hidden (Just msg)
handleMessageBubbleAction (Toggle Fixed)    (Visible Transitory msg)    = Visible Fixed msg
handleMessageBubbleAction (Hide t1)         (Visible t2 msg) | t1 == t2 = Hidden (Just msg)
handleMessageBubbleAction _ b = b

messageBubbleSpec :: T.Spec _ MessageBubble _ MessageBubbleAction
messageBubbleSpec = T.simpleSpec performAction render
  where
  render :: T.Render MessageBubble _ MessageBubbleAction
  render dispatch _ state _ =
    renderMessageBubble dispatch state

  performAction :: T.PerformAction _ MessageBubble _ MessageBubbleAction
  performAction action _ _ = void $ do
    T.modifyState \s -> handleMessageBubbleAction action s
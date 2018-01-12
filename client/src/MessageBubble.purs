module App.MessageBubble where

import Prelude 
import Thermite as T
import React (ReactElement, preventDefault) as R
import React.DOM as RD
import React.DOM.Props as RP

type Message = { header :: String
               , body :: String
               }

data MessageBubbleType = Transitory | Fixed
derive instance eqMessageBubbleType :: Eq MessageBubbleType

data MessageBubble = Hidden
                   | Visible MessageBubbleType Message

data MessageBubbleAction = Show MessageBubbleType
                         | Hide MessageBubbleType

renderMessageBubble :: (MessageBubbleAction -> T.EventHandler) -> MessageBubble -> Array R.ReactElement
renderMessageBubble _ Hidden = []
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
                       , RP.onClick \e -> do
                          _ <- R.preventDefault e
                          dispatch $ Hide Fixed
                       ]
                       [ RD.i [ RP.className "icon-cancel"] []
                       ]
                ]

handleMessageBubbleAction :: MessageBubbleAction -> Message -> MessageBubble -> MessageBubble
handleMessageBubbleAction (Show Transitory) msg Hidden = Visible Transitory msg
handleMessageBubbleAction (Show Fixed) msg _ = Visible Fixed msg
handleMessageBubbleAction (Hide t1) _ (Visible t2 _) | t1 == t2 = Hidden
handleMessageBubbleAction _ _ b = b
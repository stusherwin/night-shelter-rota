module App.MessageBubble where

import Prelude 
import Data.Maybe(Maybe(..))
import Thermite as T
import React (ReactElement, preventDefault) as R
import React.DOM as RD
import React.DOM.Props as RP
import App.Common (classNames, onlyIf)

type Message = { header :: Maybe String
               , body :: String
               }

data MessageBubbleType = Transitory | Fixed
data MessageBubblePosition = Under | Over
derive instance eqMessageBubbleType :: Eq MessageBubbleType

data MessageBubble = Hidden (Maybe Message) MessageBubblePosition
                   | Visible MessageBubbleType Message MessageBubblePosition

data MessageBubbleAction = Show MessageBubbleType
                         | Hide MessageBubbleType
                         | Toggle MessageBubbleType

renderMessageBubble :: (MessageBubbleAction -> T.EventHandler) -> MessageBubble -> Array R.ReactElement
renderMessageBubble _ (Hidden _ _) = []
renderMessageBubble dispatch (Visible t msg p) = [ RD.div [ classNames [ "message-bubble"
                                                                       , case p of
                                                                           Over -> "inverted"
                                                                           _ -> ""
                                                                       ]
                                                          ] 
                                                          $
                                                          case msg.header of
                                                            Just h -> [ RD.h3' [ RD.text h ] ]
                                                            _ -> []
                                                          <>
                                                          [ RD.p' [ RD.text msg.body ] ]
                                                          <>
                                                          close t
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
handleMessageBubbleAction (Show Transitory) (Hidden (Just msg) p)         = Visible Transitory msg p
handleMessageBubbleAction (Show Fixed)      (Hidden (Just msg) p)         = Visible Fixed msg p
handleMessageBubbleAction (Show Fixed)      (Visible Transitory msg p)    = Visible Fixed msg p
handleMessageBubbleAction (Toggle Fixed)    (Hidden (Just msg) p)         = Visible Fixed msg p
handleMessageBubbleAction (Toggle Fixed)    (Visible Fixed msg p)         = Hidden (Just msg) p
handleMessageBubbleAction (Toggle Fixed)    (Visible Transitory msg p)    = Visible Fixed msg p
handleMessageBubbleAction (Hide t1)         (Visible t2 msg p) | t1 == t2 = Hidden (Just msg) p
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
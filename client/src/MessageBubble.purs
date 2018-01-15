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
               , icon :: Maybe String
               , position :: MessageBubblePosition
               }

data MessageBubblePosition = Under | Over

data MessageBubble = Hidden (Maybe Message)
                   | Transitory Message
                   | Fixed Message

data MessageBubbleAction = ShowTransitory
                         | HideTransitory
                         | ToggleFixed

renderMessageBubble :: (MessageBubbleAction -> T.EventHandler) -> MessageBubble -> Array R.ReactElement
renderMessageBubble dispatch mb =
  case mb of
    Hidden _ -> []
    Transitory msg -> render msg false
    Fixed msg      -> render msg true
  where
  render msg close = [ RD.div [ classNames [ "message-bubble"
                                           , case msg.position of
                                               Over -> "inverted"
                                               _ -> ""
                                           ]
                              ] 
                              $
                              (if close
                                then [ RD.a [ RP.href "#"
                                            , RP.onClick $ R.preventDefault >=> (const $ dispatch $ ToggleFixed)
                                            ]
                                            [ RD.i [ RP.className "icon-cancel"] []
                                            ]
                                     ]
                                else [])
                              <>
                              case msg.header of
                                Just h -> [ RD.h3' $
                                                   case msg.icon of
                                                     Just i -> [ RD.i [ RP.className $ "icon-" <> i ] [] ]
                                                     _ -> []
                                                   <>
                                                   [ RD.text h ]
                                          ]
                                _ -> []
                              <>
                              [ RD.p' $
                                      case msg.header, msg.icon of
                                        Nothing, Just i -> [ RD.i [ RP.className $ "icon-" <> i ] [] ]
                                        _, _ -> []
                                      <>
                                      [ RD.text msg.body ]
                              ]
                     ]

handleMessageBubbleAction :: MessageBubbleAction -> MessageBubble -> MessageBubble
handleMessageBubbleAction ShowTransitory (Hidden (Just msg)) = Transitory msg
handleMessageBubbleAction HideTransitory (Transitory msg)    = Hidden (Just msg)
handleMessageBubbleAction ToggleFixed    (Hidden (Just msg)) = Fixed msg
handleMessageBubbleAction ToggleFixed    (Transitory msg)    = Fixed msg
handleMessageBubbleAction ToggleFixed    (Fixed msg)         = Hidden (Just msg)
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

hideMessageBubble :: MessageBubble -> MessageBubble
hideMessageBubble (Transitory msg)    = Hidden (Just msg)
hideMessageBubble (Fixed msg)         = Hidden (Just msg)
hideMessageBubble b = b
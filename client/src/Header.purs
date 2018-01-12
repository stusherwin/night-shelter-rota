module App.Header (State, VolDetailsState(..), Action(..), MessageBubble, MessageBubbleType, Message, spec, initialState, volDetailsUpdated, editCancelled, reqStarted, reqSucceeded, reqFailed, initialDataLoaded) where

import Prelude 

import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Class (liftEff)
import Data.List (List(..), (!!), find, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import React (ReactElement, preventDefault) as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
import Servant.PureScript.Affjax (AjaxError, ErrorDescription(..), errorToString, runAjaxError)
import Control.Monad.Eff.Console (log, CONSOLE)

import App.Common (unsafeEventSelectedIndex, isJustWith, sortWith, classNames)
import App.Types (Vol)

data VolDetailsState = NotEditing
                     | EditingNewVol
                     | EditingCurrentVol

data MessageBubbleType = Transitory | Fixed
derive instance eqMessageBubbleType :: Eq MessageBubbleType
  
type Message = { header :: String
               , body :: String
               }

data MessageBubble = Hidden
                   | Visible MessageBubbleType Message

type State = { vols :: List Vol
             , currentVol :: Maybe Vol
             , reqInProgress :: Boolean
             , volDetailsState :: VolDetailsState
             , initialDataLoaded :: Boolean
             , errorMessage :: Maybe Message
             , errorMessageBubble :: MessageBubble
             }

data Action = ChangeCurrentVol (Maybe Vol)
            | EditCurrentVol
            | EditNewVol
            | ShowMessageBubble MessageBubbleType
            | HideMessageBubble MessageBubbleType

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state@{ initialDataLoaded: false } _ =
    [ RD.div [ RP.className "header initial-data-loading" ]
           $ statusIcon dispatch state
          <> [ RD.h2' [ RD.text "Night Shelter Rota" ]
             ]
    ]
  render dispatch _ state _ =
    [ RD.div [ RP.className "header" ]
             $ [ RD.div [ RP.className "header-buttons" ]
                       $ case state.volDetailsState of
                           NotEditing -> newVolButton
                           _ -> []
                      <> case state.currentVol, state.volDetailsState of
                           Just v, NotEditing -> editVolButton v.name
                           _, _ -> []
               ]
             <> statusIcon dispatch state
             <> [ RD.h2' [ RD.text "Night Shelter Rota for " ]
                , RD.select [ RP.className "vol-select"
                            , RP.onChange $ dispatch <<< respond state <<< unsafeEventSelectedIndex
                            ]
                         (  [ RD.option [ RP.value "" ]
                                        [ RD.text $ "All volunteers" ]
                            ]
                         <> map (option dispatch state.currentVol) (toUnfoldable state.vols)
                         )
                ]
    ]
    where
      editVolButton :: String -> Array R.ReactElement
      editVolButton volName = [ RD.button [ RP.className "ui primary button header-button header-button-edit media-large-screen"
                                          , RP.onClick \e -> do
                                              _ <- R.preventDefault e
                                              dispatch EditCurrentVol
                                          ]
                                          [ RD.i [ RP.className "icon icon-edit" ] []
                                          , RD.text $ "Edit volunteer details"
                                          ]
                              , RD.button [ RP.className "ui primary button header-button header-button-edit media-larger-screen media-medium-screen"
                                          , RP.onClick \e -> do
                                              _ <- R.preventDefault e
                                              dispatch EditCurrentVol
                                          ]
                                          [ RD.i [ RP.className "icon icon-edit" ] []
                                          , RD.text $ "Edit"
                                          ]
                              , RD.button [ RP.className "ui primary mini icon button header-button header-button-edit media-small-screen"
                                          , RP.onClick \e -> do
                                              _ <- R.preventDefault e
                                              dispatch EditCurrentVol
                                          ]
                                          [ RD.i [ RP.className "icon icon-edit" ] []
                                          ]
                              ]
      newVolButton :: Array R.ReactElement
      newVolButton = [ RD.button [ RP.className "ui button header-button header-button-new media-large-screen"
                                 , RP.onClick \e -> do
                                     _ <- R.preventDefault e
                                     dispatch EditNewVol
                                 ]
                                 [ RD.i [ RP.className "icon icon-add" ] []
                                 , RD.text "New volunteer"
                                 ]
                     , RD.button [ RP.className "ui button header-button header-button-new media-larger-screen media-medium-screen"
                                 , RP.onClick \e -> do
                                     _ <- R.preventDefault e
                                     dispatch EditNewVol
                                 ]
                                 [ RD.i [ RP.className "icon icon-add" ] []
                                 , RD.text "New"
                                 ]
                     , RD.button [ RP.className "ui mini icon button header-button header-button-new media-small-screen"
                                 , RP.onClick \e -> do
                                     _ <- R.preventDefault e
                                     dispatch EditNewVol
                                 ]
                                 [ RD.i [ RP.className "icon icon-add" ] []
                                 ]
                     ]

  statusIcon :: _ -> State -> Array R.ReactElement
  statusIcon dispatch s = [ RD.div [ RP.className "header-status" ]
                                   $ [ RD.i [ RP.className iconType
                                            , RP.onClick \e -> do
                                                _ <- R.preventDefault e
                                                dispatch $ ShowMessageBubble Fixed
                                            , RP.onMouseOver \_ -> dispatch $ ShowMessageBubble Transitory
                                            , RP.onMouseLeave \_ -> dispatch $ HideMessageBubble Transitory
                                            ] 
                                            []
                                     ]
                                     <> messageBubble s.errorMessageBubble
                          ]
    where
      messageBubble :: MessageBubble -> Array R.ReactElement
      messageBubble Hidden = []
      messageBubble (Visible t msg) = [ RD.div [ RP.className "header-status-message" ] 
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
                                dispatch $ HideMessageBubble Fixed
                             ]
                             [ RD.i [ RP.className "icon-cancel"] []
                             ]
                      ]
      iconType = case s.reqInProgress, s.errorMessage of
                   true,  _       -> "icon-spin animate-spin"
                   false, Nothing -> "logo" --"icon-ok"
                   _,     _       -> "icon-warning"

  respond :: State -> Int -> Action
  respond state i | i > 0 = ChangeCurrentVol $ state.vols !! (i - 1)
  respond _ _ = ChangeCurrentVol Nothing

  findVol :: List Vol -> Maybe Int -> Maybe Vol
  findVol vols = (=<<) \id -> find (\v -> v.id == id) vols

  option :: _ -> Maybe Vol -> Vol -> R.ReactElement
  option dispatch currentVolId v = RD.option [ RP.selected $ isJustWith (\cv -> cv.id == v.id) currentVolId
                                             , RP.value $ show v.id
                                             ]
                                             [ RD.text v.name ]

  performAction :: T.PerformAction _ State _ Action
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState _ { currentVol = v
                                                                  , volDetailsState = NotEditing
                                                                  , errorMessage = Nothing
                                                                  , errorMessageBubble = Hidden
                                                                  }
  performAction EditCurrentVol       _ _ = void $ T.modifyState _ { volDetailsState = EditingCurrentVol
                                                                  , errorMessage = Nothing
                                                                  , errorMessageBubble = Hidden
                                                                  }
  performAction EditNewVol           _ _ = void $ T.modifyState _ { currentVol = Nothing
                                                                  , volDetailsState = EditingNewVol
                                                                  , errorMessage = Nothing
                                                                  , errorMessageBubble = Hidden
                                                                  }
  performAction (ShowMessageBubble Transitory) _ { errorMessage: Just msg, errorMessageBubble: Hidden } = void $ do
    lift $ liftEff $ log "show message bubble - trans"
    T.modifyState _{ errorMessageBubble = Visible Transitory msg }
  performAction (ShowMessageBubble Fixed) _ { errorMessage: Just msg } = void $ do
    lift $ liftEff $ log "show message bubble - fixed"
    T.modifyState _{ errorMessageBubble = Visible Fixed msg }
  performAction (HideMessageBubble t1) _ { errorMessageBubble: Visible t2 _ } | t1 == t2 = void $ do
    lift $ liftEff $ log "hide message bubble"
    T.modifyState _{ errorMessageBubble = Hidden }
  performAction _ _ _ = pure unit

initialState :: State
initialState = { vols: Nil
               , currentVol: Nothing
               , volDetailsState: NotEditing
               , reqInProgress: true
               , initialDataLoaded: false
               , errorMessage: Nothing
               , errorMessageBubble: Hidden
               }

initialDataLoaded :: List Vol -> State -> State
initialDataLoaded vols = _ { vols = sortWith (toLower <<< _.name) vols
                           , reqInProgress = false
                           , initialDataLoaded = true
                           }

volDetailsUpdated :: List Vol -> Maybe Vol -> State -> State
volDetailsUpdated vols currentVol = _ { vols = sortWith (toLower <<< _.name) vols
                                      , currentVol = currentVol
                                      , volDetailsState = NotEditing
                                      , reqInProgress = false
                                      }

editCancelled :: State -> State
editCancelled = _ { volDetailsState = NotEditing
                  , errorMessage = Nothing
                  , errorMessageBubble = Hidden
                  }

reqStarted :: State -> State
reqStarted = _ { reqInProgress = true
               , errorMessage = Nothing
               , errorMessageBubble = Hidden
               }

reqSucceeded :: State -> State
reqSucceeded = _ { reqInProgress = false
                 , errorMessage = Nothing
                 , errorMessageBubble = Hidden
                 }

reqFailed :: AjaxError -> State -> State
reqFailed err = _ { reqInProgress = false
                  , errorMessage = Just { header: header error
                                        , body: body error
                                        }
                  }
  where
  error :: ErrorDescription
  error = (runAjaxError err).description

  header :: ErrorDescription -> String
  header (ConnectionError _) = "Can't connect to the server"
  header _ = "Error from the server"

  body :: ErrorDescription -> String
  body (UnexpectedHTTPStatus r) = "Received an unexpected response from the server: " <> show r.status <> ": " <> r.response
  body (ParsingError e) = "There was a problem with the response from the server: " <> e
  body (DecodingError e) = "There was a problem with the response from the server: " <> e
  body (ConnectionError _) = "The server seems to be down or busy, please wait a while and try again."
module App.Header (State, VolDetailsState(..), Action(..), spec, initialState, volDetailsUpdated, editCancelled, reqStarted, reqSucceeded, reqFailed, initialDataLoaded) where

import Prelude 

import Data.List (List(..), (!!), find, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import React (ReactElement, preventDefault) as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
import Servant.PureScript.Affjax (AjaxError, ErrorDescription(..), errorToString, runAjaxError)

import App.Common (unsafeEventSelectedIndex, isJustWith, sortWith, classNames)
import App.Types (Vol)

data VolDetailsState = NotEditing
                     | EditingNewVol
                     | EditingCurrentVol

type State = { vols :: List Vol
             , currentVol :: Maybe Vol
             , reqInProgress :: Boolean
             , errorMessage :: Maybe String
             , volDetailsState :: VolDetailsState
             , initialDataLoaded :: Boolean
             , showErrorMessage :: Boolean
             } 
 
data Action = ChangeCurrentVol (Maybe Vol)
            | EditCurrentVol
            | EditNewVol
            | ToggleErrorMessage

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
  statusIcon d s = [ RD.div [ RP.className "header-status" ]
                            $ [ RD.i ([ RP.className iconType
                                      , RP.onClick \e -> do
                                          _ <- R.preventDefault e
                                          d ToggleErrorMessage
                                      ] <> title)
                                      []
                              ]
                            <> errorMessage s
                   ]
    where
      errorMessage :: State -> Array R.ReactElement
      errorMessage { errorMessage: Nothing } = [] 
      errorMessage { showErrorMessage: false } = [] 
      errorMessage { errorMessage: Just msg } = [ RD.div [ RP.className "header-status-message" ] 
                                                         [ RD.text msg ]
                                                ]
      iconType = case s.reqInProgress, s.errorMessage of
                   true,  _       -> "icon-spin animate-spin"
                   false, Nothing -> "logo" --"icon-ok"
                   _,     _       -> "icon-warning"
      title = case s.errorMessage of
                Just msg -> [ RP.title msg ]
                _        -> []

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
                                                                  }
  performAction EditCurrentVol       _ _ = void $ T.modifyState _ { volDetailsState = EditingCurrentVol
                                                                  , errorMessage = Nothing
                                                                  }
  performAction EditNewVol           _ _ = void $ T.modifyState _ { currentVol = Nothing
                                                                  , volDetailsState = EditingNewVol
                                                                  , errorMessage = Nothing
                                                                  }
  performAction ToggleErrorMessage _ _ = void $ T.modifyState \s -> s { showErrorMessage = not s.showErrorMessage }

initialState :: State
initialState = { vols: Nil
               , currentVol: Nothing
               , volDetailsState: NotEditing
               , reqInProgress: true
               , errorMessage: Nothing
               , initialDataLoaded: false
               , showErrorMessage: false
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
                  }

reqStarted :: State -> State
reqStarted = _ { reqInProgress = true
               , errorMessage = Nothing
               }

reqSucceeded :: State -> State
reqSucceeded = _ { reqInProgress = false
                 , errorMessage = Nothing
                 }

reqFailed :: AjaxError -> State -> State
reqFailed err = _ { reqInProgress = false
                  , errorMessage = Just $ message $ (runAjaxError err).description
                  }
  where
  message :: ErrorDescription -> String
  message (UnexpectedHTTPStatus r) = "Unexpected HTTP status: " <> show r.status
  message (ParsingError e) = "Parsing error: " <> e
  message (DecodingError e) = "Decoding error: " <> e
  message (ConnectionError e) = "Connection error: " <> e
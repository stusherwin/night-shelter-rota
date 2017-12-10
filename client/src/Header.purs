module App.Header (State, VolDetailsState(..), Action(..), spec, initialState, updateVols, editCurrentVol, editNewVol, cancelEdit) where

import Prelude 

import Data.List (List, (!!), find, toUnfoldable)
import Data.Maybe (Maybe(..))
import React (ReactElement, preventDefault) as R
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Common (unsafeEventSelectedIndex, isJustWith, sortWith, classNames)
import ServerTypes (Volunteer(..))

data VolDetailsState = NotEditing
                     | EditingNewVol
                     | EditingCurrentVol

type State = { vols :: List Volunteer
             , currentVol :: Maybe Volunteer
             , loading :: Boolean
             , errorMessage :: Maybe String
             , volDetailsState :: VolDetailsState
             } 
 
data Action = ChangeCurrentVol (Maybe Volunteer)
            | EditCurrentVol
            | EditNewVol

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where 
  render :: T.Render State _ Action
  render dispatch _ state _ =
    [ RD.div [ RP.className "header" ]
           $ statusIcon
          <> case state.volDetailsState of
               NotEditing -> newVolButton
               _ -> []
          <> case state.currentVol, state.volDetailsState of
               Just (Volunteer { vName }), NotEditing -> editVolButton vName
               _, _ -> []
          <> [ RD.h2' $ [ RD.span [ RP.dangerouslySetInnerHTML { __html: "Night Shelter Rota for &nbsp;" } ]
                                  []
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
    ]
    where
      statusIcon :: Array R.ReactElement
      statusIcon = [ RD.i ([ classNames [ "header-status"
                                        , iconType
                                        ]
                           , RP.style { float: "right" }
                           ] <> title)
                          []
                   ]
        where
          iconType = case state.loading, state.errorMessage of
                       true,  _       -> "icon-spin animate-spin"
                       false, Nothing -> "icon-ok"
                       _, _           -> "icon-warning"
          title = case state.errorMessage of
                    Just msg -> [ RP.title msg ]
                    _        -> []
      editVolButton :: String -> Array R.ReactElement
      editVolButton volName = [ RD.button [ RP.className "ui primary button header-edit"
                                          , RP.onClick \e -> do
                                              _ <- R.preventDefault e
                                              dispatch EditCurrentVol
                                          ]
                                          [ RD.i [ RP.className "icon icon-edit" ] []
                                          , RD.text $ "Edit " <> volName <> "'s details"
                                          ]
                              ]
      newVolButton :: Array R.ReactElement
      newVolButton = [ RD.button [ RP.className "ui button header-new"
                                 , RP.onClick \e -> do
                                     _ <- R.preventDefault e
                                     dispatch EditNewVol
                                 ]
                                 [ RD.i [ RP.className "icon icon-add" ] []
                                 , RD.text "New volunteer"
                                 ]
                     ]

  respond :: State -> Int -> Action
  respond state i | i > 0 = ChangeCurrentVol $ state.vols !! (i - 1)
  respond _ _ = ChangeCurrentVol Nothing

  findVol :: List Volunteer -> Maybe Int -> Maybe Volunteer
  findVol vols = (=<<) \id -> find (\(Volunteer v) -> v.vId == id) vols

  option :: _ -> Maybe Volunteer -> Volunteer -> R.ReactElement
  option dispatch currentVolId (Volunteer v) = RD.option [ RP.selected $ isJustWith (\(Volunteer cv) -> cv.vId == v.vId) currentVolId
                                             , RP.value $ show v.vId
                                             ]
                                             [ RD.text v.vName ]

  performAction :: T.PerformAction _ State _ Action
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \state -> state{ currentVol = v }
  performAction EditCurrentVol _ _ = void $ T.modifyState \state -> state{ volDetailsState = EditingCurrentVol }
  performAction EditNewVol _ _ = void $ T.modifyState \state -> state{ volDetailsState = EditingNewVol }

initialState :: List Volunteer -> Maybe Volunteer -> State
initialState vols currentVol = { vols: sortWith (\(Volunteer v) -> v.vName) vols
                               , currentVol: currentVol
                               , loading: true
                               , errorMessage: Nothing
                               , volDetailsState: NotEditing
                               }

updateVols :: List Volunteer -> Maybe Volunteer -> State -> State
updateVols vols currentVol = _ { vols = sortWith (\(Volunteer v) -> v.vName) vols
                               , currentVol = currentVol
                               , volDetailsState = NotEditing
                               , loading = false
                               }

editNewVol :: State -> State
editNewVol = _ { currentVol = Nothing
               , volDetailsState = EditingNewVol
               }

editCurrentVol :: State -> State
editCurrentVol = _ { volDetailsState = EditingCurrentVol
                   }

cancelEdit :: State -> State
cancelEdit = _ { volDetailsState = NotEditing
               }
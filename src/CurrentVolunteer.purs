module App.CurrentVolunteer (CurrentVolProps, CurrentVolState, CurrentVolAction(..), currentVolSpec, currentVolInitialState) where

import Prelude

import App.Common (unsafeEventSelectedIndex)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Data (Volunteer(..))

type CurrentVolProps = { vols :: Array Volunteer }
 
type CurrentVolState = { currentVol :: Maybe Volunteer } 
 
data CurrentVolAction = ChangeCurrentVol (Maybe Volunteer)

currentVolSpec :: T.Spec _ (Tuple CurrentVolProps CurrentVolState) _ CurrentVolAction
currentVolSpec = T.simpleSpec performAction render
  where
  render :: T.Render (Tuple CurrentVolProps CurrentVolState) _ CurrentVolAction
  render dispatch _ (Tuple props state) _ =
    [ RD.span [ RP.className "float-right" ]
                [ RD.select [ RP.onChange \e -> dispatch (ChangeCurrentVol $ props.vols !! ((unsafeEventSelectedIndex e) - 1)) ]
                           ([ RD.option [ RP.value "" ]
                                        [ RD.text "Select a volunteer" ] ]
                           <> map (option dispatch state.currentVol) props.vols)
              ]
    ]
  
  option :: _ -> Maybe Volunteer -> Volunteer -> ReactElement
  option dispatch currentVol (V v) = RD.option [ RP.selected (maybe false (\(V cv) -> cv.id == v.id) currentVol) 
                                                 , RP.value $ show v.id
                                                 ]
                                                 [ RD.text v.name ]

  performAction :: T.PerformAction _ (Tuple CurrentVolProps CurrentVolState) _ CurrentVolAction
  performAction (ChangeCurrentVol v) _ _ = void $ T.modifyState \(Tuple props state) -> Tuple props state{ currentVol = v }
  performAction _ _ _ = pure unit

currentVolInitialState :: CurrentVolProps -> Maybe Volunteer -> CurrentVolState
currentVolInitialState volunteers currentVol = { currentVol: currentVol }
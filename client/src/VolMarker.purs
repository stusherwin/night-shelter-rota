module App.VolMarker (State, SharingPref(..), spec, initialState) where

import Prelude

import Data.Array (catMaybes)
import Data.String (length) as S
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Common (justIf)
import App.Types (OvernightPreference(..), OvernightGenderPreference(..), Volunteer, VolunteerShift, ShiftType(..))

data SharingPref = GM
                 | GF
                 | P1
                 | P2
                 | N String

type State = { name :: String
             , shiftType :: ShiftType
             , sharingPrefs :: Array SharingPref
             }

spec :: T.Spec _ State _ Unit
spec = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render State _ Unit
  render dispatch _ s _ =
    [ RD.span [ RP.className "vol-marker" ]
              $ [ renderIcon s.shiftType
                , RD.text $ s.name
                ]
              <> map renderSharingPref s.sharingPrefs
    ]
  
  renderSharingPref :: SharingPref -> ReactElement
  renderSharingPref GM    = RD.div [ RP.className "sharing-pref gender" 
                                   , RP.title "Males only" 
                                   ] 
                                   [ RD.span' [ RD.text "M" ] ]
  renderSharingPref GF    = RD.div [ RP.className "sharing-pref gender" 
                                   , RP.title "Females only" 
                                   ] 
                                   [ RD.span' [ RD.text "F" ] ]
  renderSharingPref P1    = RD.div [ RP.className "sharing-pref alone" 
                                   , RP.title "I prefer to be on my own" 
                                   ] 
                                   [ RD.span' [ RD.text "1" ] ]
  renderSharingPref P2    = RD.div [ RP.className "sharing-pref alone" 
                                   , RP.title "I prefer to work with another volunteer" 
                                   ] 
                                   [ RD.span' [ RD.text "2" ] ]
  renderSharingPref (N n) = RD.div [ RP.className "sharing-pref icon" 
                                   , RP.title n
                                   , RP.dangerouslySetInnerHTML { __html: "<i class=\"icon-info-1\"></i>&nbsp;" }
                                   ] 
                                   []

renderIcon :: ShiftType -> ReactElement
renderIcon Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
renderIcon Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []

initialState :: VolunteerShift -> State
initialState { volunteer: v, shiftType } = { name: v.name
                                           , shiftType: shiftType
                                           , sharingPrefs: sharingPrefs v
                                           } 

sharingPrefs :: Volunteer -> Array SharingPref
sharingPrefs vol = catMaybes [ map overnight vol.overnightPreference
                             , map gender vol.overnightGenderPreference
                             , justIf (N vol.notes) $ S.length vol.notes > 0
                             ]

overnight :: OvernightPreference -> SharingPref
overnight PreferToBeAlone = P1
overnight PreferAnotherVolunteer = P2

gender :: OvernightGenderPreference -> SharingPref
gender Male = GM
gender Female = GF
module App.VolMarker (State, SharingPref(..), spec, initialState) where

import Prelude

import Data.Array (catMaybes)
import Data.String (length) as S
import React (ReactElement)
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T

import App.Common (justIf)
import ServerTypes (OvernightPreference(..), OvernightGenderPreference(..), Volunteer(..), VolunteerShift(..), ShiftType(..)) as D

data SharingPref = Male
                 | Female
                 | One
                 | Two
                 | Notes String

type State = { name :: String
             , shiftType :: D.ShiftType
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
  renderSharingPref Male      = RD.div [ RP.className "sharing-pref gender" 
                                       , RP.title "Males only" 
                                       ] 
                                       [ RD.span' [ RD.text "M" ] ]
  renderSharingPref Female    = RD.div [ RP.className "sharing-pref gender" 
                                       , RP.title "Females only" 
                                       ] 
                                       [ RD.span' [ RD.text "F" ] ]
  renderSharingPref One       = RD.div [ RP.className "sharing-pref alone" 
                                       , RP.title "I prefer to be on my own" 
                                       ] 
                                       [ RD.span' [ RD.text "1" ] ]
  renderSharingPref Two       = RD.div [ RP.className "sharing-pref alone" 
                                       , RP.title "I prefer to work with another volunteer" 
                                       ] 
                                       [ RD.span' [ RD.text "2" ] ]
  renderSharingPref (Notes n) = RD.div [ RP.className "sharing-pref icon" 
                                       , RP.title n
                                       , RP.dangerouslySetInnerHTML { __html: "<i class=\"icon-info-1\"></i>&nbsp;" }
                                       ] 
                                       []

renderIcon :: D.ShiftType -> ReactElement
renderIcon D.Evening   = RD.i [ RP.className "vol-icon icon-no-bed" ] []
renderIcon D.Overnight = RD.i [ RP.className "vol-icon icon-bed" ]    []

initialState :: D.VolunteerShift -> State
initialState (D.VolunteerShift { vsVolunteer: (D.Volunteer v), vsShiftType }) = { name: v.vName
                               , shiftType: vsShiftType
                               , sharingPrefs: sharingPrefs v
                               } 

sharingPrefs :: _ -> Array SharingPref
sharingPrefs vol = catMaybes [ map overnight vol.vOvernightPreference
                             , map gender vol.vOvernightGenderPreference
                             , justIf (Notes vol.vNotes) $ S.length vol.vNotes > 0
                             ]

overnight :: D.OvernightPreference -> SharingPref
overnight D.PreferToBeAlone = One
overnight D.PreferAnotherVolunteer = Two

gender :: D.OvernightGenderPreference -> SharingPref
gender D.Male = Male
gender D.Female = Female

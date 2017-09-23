module App.Shift (ShiftProps, ShiftState, ShiftAction, shiftSpec, tomorrow) where

import Prelude

import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..), fromRight, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (joinWith)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)

import React (ReactElement)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM 
import Thermite as T

import App.Common (unsafeEventValue)
import App.Data (Volunteer)

type ShiftProps = { currentVolunteer :: Maybe Volunteer }

type ShiftState = { shift :: Date
                  , volunteers :: Array String }

data ShiftAction = Noop

shiftSpec :: T.Spec _ (Tuple ShiftProps ShiftState) _ ShiftAction
shiftSpec = T.simpleSpec performAction render
  where
  render :: T.Render (Tuple ShiftProps ShiftState) _ ShiftAction
  render dispatch _ (Tuple props state) _ =
    [ RD.div [ RP.className "alert alert-primary mt-3" ]
             [ RD.text $ unsafePartial $ fromRight $ formatDateTime "D MMMM YYYY" (DateTime state.shift midnight)
             , RD.span [ RP.className "float-right" ]
                       (map (renderVolunteer props.currentVolunteer) state.volunteers)
             ] 
    ]
  
  renderVolunteer :: Maybe Volunteer -> String -> ReactElement
  renderVolunteer (Just cv) v | cv.name == v = RD.strong' [ RD.text v ]
  renderVolunteer _         v = RD.text v

  performAction :: T.PerformAction _ (Tuple ShiftProps ShiftState) _ ShiftAction
  performAction _ _ _ = pure unit

midnight :: Time
midnight = unsafePartial fromJust $ Time <$> pure bottom <*> pure bottom <*> pure bottom <*> pure bottom

tomorrow :: Date -> Date
tomorrow dt = maybe dt date $ adjust (Days 1.0) (DateTime dt bottom)
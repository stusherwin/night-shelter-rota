module Shift (ShiftState, ShiftAction, shiftSpec, buildShifts, tomorrow) where

import Prelude

import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..), fromRight, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.List (List(..), snoc, last) as L
import Data.Maybe (fromJust, maybe)
import Data.Time.Duration (Days(..))
import Partial.Unsafe (unsafePartial)

import React (ReactElement)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM 
import Thermite as T

import Common (unsafeEventValue)
   
type ShiftState = { shift :: Date }

data ShiftAction = Noop

shiftSpec :: T.Spec _ ShiftState _ ShiftAction
shiftSpec = T.simpleSpec performAction render
  where
  render :: T.Render ShiftState _ ShiftAction
  render dispatch _ state _ =
    [ RD.div [ RP.className "alert alert-primary mt-3" ]
             [ RD.text $ unsafePartial $ fromRight $ formatDateTime "D MMMM YYYY" (DateTime state.shift midnight) ] 
    ]

  performAction :: T.PerformAction _ ShiftState _ ShiftAction
  performAction _ _ _ = pure unit

midnight :: Time
midnight = unsafePartial fromJust $ Time <$> pure bottom <*> pure bottom <*> pure bottom <*> pure bottom

tomorrow :: Date -> Date
tomorrow dt = maybe dt date $ adjust (Days 1.0) (DateTime dt bottom)

buildShifts :: Date -> Int -> L.List ShiftState
buildShifts date 0 = L.Nil
buildShifts date n = L.Cons { shift: date } $ buildShifts (tomorrow date) (n - 1)
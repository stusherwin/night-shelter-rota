module App.Data (Shift, Volunteer) where

import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)

type Shift = { shift :: Date }

type Volunteer = { id :: Int
                 , name :: String }
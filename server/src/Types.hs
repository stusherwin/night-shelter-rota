{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  
  data OvernightPreference = PreferToBeAlone
                           | PreferAnotherVolunteer 
                           deriving (Eq, Show, Generic)
  instance ToJSON OvernightPreference
    
  data OvernightGenderPreference = Male
                                 | Female
                                 deriving (Eq, Show, Generic)
  instance ToJSON OvernightGenderPreference
          
  data Volunteer = Volunteer { id :: Int
                             , name :: String
                             , overnightPreference :: Maybe OvernightPreference
                             , overnightGenderPreference :: Maybe OvernightGenderPreference
                             , notes :: String
                             } deriving (Eq, Show, Generic)
  instance ToJSON Volunteer

  data VolunteerShift = Overnight Volunteer
                      | Evening Volunteer
                      deriving (Eq, Show, Generic)
  instance ToJSON VolunteerShift

  data Date = Date { year :: Int
                   , month :: Int
                   , day :: Int
                   } deriving (Eq, Show, Generic)
  instance ToJSON Date

  data Shift = Shift { date :: Date
                     , volunteers :: [VolunteerShift]
                     } deriving (Eq, Show, Generic)
  instance ToJSON Shift
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  
  data OvernightPreference = PreferToBeAlone
                           | PreferAnotherVolunteer 
                           deriving (Eq, Show, Generic)
  instance ToJSON OvernightPreference
  instance FromJSON OvernightPreference
  
  data OvernightGenderPreference = Male
                                 | Female
                                 deriving (Eq, Show, Generic)
  instance ToJSON OvernightGenderPreference
  instance FromJSON OvernightGenderPreference
  
  data Volunteer = Volunteer { vId :: Int
                             , vName :: String
                             , vOvernightPreference :: Maybe OvernightPreference
                             , vOvernightGenderPreference :: Maybe OvernightGenderPreference
                             , vNotes :: String
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

  data VolunteerDetails = VolunteerDetails { vdName :: String
                                           , vdNotes :: String
                                           , vdPref :: Maybe OvernightPreference
                                           , vdGenderPref :: Maybe OvernightGenderPreference
                                           } deriving (Eq, Show, Generic)
                            
  instance ToJSON VolunteerDetails
  instance FromJSON VolunteerDetails
  
  newVolunteer :: Int -> VolunteerDetails -> Volunteer
  newVolunteer id details = Volunteer id (vdName details) (vdPref details) (vdGenderPref details) (vdNotes details)
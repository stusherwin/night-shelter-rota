{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar
  
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
                             , vIntro :: String
                             , vOvernightPreference :: Maybe OvernightPreference
                             , vOvernightGenderPreference :: Maybe OvernightGenderPreference
                             , vNotes :: String
                             , vActive :: Bool
                             } deriving (Eq, Show, Generic)
  instance ToJSON Volunteer

  data VolunteerDetails = VolunteerDetails { vdName :: String
                                           , vdIntro :: String
                                           , vdPref :: Maybe OvernightPreference
                                           , vdGenderPref :: Maybe OvernightGenderPreference
                                           , vdNotes :: String
                                           } deriving (Eq, Show, Generic)
                            
  instance ToJSON VolunteerDetails
  instance FromJSON VolunteerDetails

  data ShiftType = Overnight
                 | Evening
                 deriving (Eq, Show, Generic)
  instance ToJSON ShiftType
  instance FromJSON ShiftType
  
  data ShiftDate = ShiftDate { year :: Int
                             , month :: Int
                             , day :: Int
                             } deriving (Eq, Ord, Show, Generic)
  instance ToJSON ShiftDate

  data VolunteerShift = VolunteerShift { vsVolunteer :: Volunteer
                                       , vsShiftType :: ShiftType
                                       } deriving (Eq, Show, Generic)
  instance ToJSON VolunteerShift

  data Shift = Shift { sDate :: ShiftDate
                     , sVolunteers :: [VolunteerShift]
                     } deriving (Eq, Show, Generic)
  instance ToJSON Shift

  newVolunteer :: Int -> VolunteerDetails -> Volunteer
  newVolunteer id details = Volunteer id (vdName details) (vdIntro details) (vdPref details) (vdGenderPref details) (vdNotes details) True

  toShiftDate :: Day -> ShiftDate
  toShiftDate day = let (y, m, d) = toGregorian day
                    in ShiftDate (fromInteger y) m d

  fromShiftDate :: ShiftDate -> Day
  fromShiftDate sd = fromGregorian (toInteger $ year sd) (month sd) (day sd)
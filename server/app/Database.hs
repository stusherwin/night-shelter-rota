{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllVolunteers, getVolunteer, addVolunteer, updateVolunteer, getAllShifts) where
  import Control.Monad (mzero)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  instance ToField OvernightPreference where
    toField PreferToBeAlone = toDatabaseChar '1'
    toField PreferAnotherVolunteer = toDatabaseChar '2'

  instance FromField OvernightPreference where
    fromField f char = do
      c <- fromField f char
      case c of
        Just '1' -> return PreferToBeAlone
        Just '2' -> return PreferAnotherVolunteer
        _ -> mzero
  
  instance ToField OvernightGenderPreference where
    toField Male = toDatabaseChar 'M'
    toField Female = toDatabaseChar 'F'

  instance FromField OvernightGenderPreference where
    fromField f char = do
      c <- fromField f char
      case c of
        Just 'M' -> return Male
        Just 'F' -> return Female
        _ -> mzero

  instance FromRow Volunteer where
    fromRow = Volunteer <$> field <*> field <*> field <*> field <*> field

  instance FromField ShiftDate where
    fromField f date = do
      sqlDate <- fromField f date
      case sqlDate of
        Just (Finite day) -> return $ toShiftDate day
        _ -> mzero
        
  instance FromField ShiftType where
    fromField f char = do
      c <- fromField f char
      case c of
        Just 'O' -> return Overnight
        Just 'E' -> return Evening
        _ -> mzero
    
  connectionString :: ByteString
  connectionString = "postgres://shelter_rota_user:password@localhost:5432/shelter_rota"
                
  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  
  getAllVolunteers :: IO [Volunteer]
  getAllVolunteers = do
    conn <- connectPostgreSQL connectionString
    result <- query_ conn "select id, name, overnight_pref, overnight_gender_pref, notes from volunteer"
    close conn
    return result

  getVolunteer :: Int -> IO (Maybe Volunteer)
  getVolunteer id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn "select id, name, overnight_pref, overnight_gender_pref, notes from volunteer where id = ?" (Only id)
    close conn
    return $ listToMaybe result

  addVolunteer :: VolunteerDetails -> IO Int
  addVolunteer details = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn "insert into volunteer (name, overnight_pref, overnight_gender_pref, notes) values (?, ?, ?, ?) returning id"
                            ( vdName details
                            , vdPref details
                            , vdGenderPref details
                            , vdNotes details
                            )
    close conn
    return id
  
  updateVolunteer :: Int -> VolunteerDetails -> IO (Maybe Volunteer)
  updateVolunteer id details = do
    conn <- connectPostgreSQL connectionString
    result <- query conn "update volunteer set name = ?, overnight_pref = ?, overnight_gender_pref = ?, notes = ? where id = ? returning id, name, overnight_pref, overnight_gender_pref, notes"
                         ( vdName details
                         , vdPref details
                         , vdGenderPref details
                         , vdNotes details
                         , id
                         )
    close conn
    return $ listToMaybe result

  getAllShifts :: IO [Shift]
  getAllShifts = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query?
    rVols <- query_ conn "select id, name, overnight_pref, overnight_gender_pref, notes from volunteer"
    rVolShifts <- query_ conn "select shiftDate, volunteerId, shiftType from volunteer_shift"
    close conn
    let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
    -- TODO: handle vol not existing?
    let volShifts = rVolShifts <&> \(d, id, st) -> (d, [VolunteerShift (fromJust $ IM.lookup id vols) st])
    let shifts = fromListWith (++) volShifts
    let result = (assocs shifts) <&> \(d, vols) -> Shift d vols
    return result
  
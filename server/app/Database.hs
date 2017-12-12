{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllVolunteers, getVolunteer, addVolunteer, updateVolunteer) where
  import Control.Monad (mzero)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe)
  import Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import System.IO ()

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
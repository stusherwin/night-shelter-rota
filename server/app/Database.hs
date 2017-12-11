{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllVolunteers, getVolunteer, addVolunteer) where
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Data.ByteString (ByteString)
  import Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import System.IO ()

  instance ToField Char where
    toField c = Escape $ encodeUtf8 $ T.pack [c]
  
  connectionString :: ByteString
  connectionString = "postgres://shelter_rota_user:password@localhost:5432/shelter_rota"
                
  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  
  getAllVolunteers :: IO [Volunteer]
  getAllVolunteers = do
    conn <- connectPostgreSQL connectionString
    row <- query_ conn "select id, name, overnight_pref, overnight_gender_pref, notes from volunteer"
    let result = row <&> \(id, name, op, ogp, notes) ->
                            Volunteer (id :: Int)
                                      (name :: String)
                                      (case op of
                                        Just '1' -> Just PreferToBeAlone
                                        Just '2' -> Just PreferAnotherVolunteer
                                        _ -> Nothing)
                                      (case ogp of
                                        Just 'M' -> Just Male
                                        Just 'F' -> Just Female
                                        _ -> Nothing)
                                      (notes :: String)
    close conn
    return result

  getVolunteer :: Int -> IO (Maybe Volunteer)
  getVolunteer id = do
    conn <- connectPostgreSQL connectionString
    rows <- query conn "select id, name, overnight_pref, overnight_gender_pref, notes from volunteer where id = ?" (Only id)
    let result = case rows of
                  [] -> Nothing
                  row:rs -> Just $ row & \(id, name, op, ogp, notes) ->
                                        Volunteer (id :: Int)
                                                  (name :: String)
                                                  (case op of
                                                    Just '1' -> Just PreferToBeAlone
                                                    Just '2' -> Just PreferAnotherVolunteer
                                                    _ -> Nothing)
                                                  (case ogp of
                                                    Just 'M' -> Just Male
                                                    Just 'F' -> Just Female
                                                    _ -> Nothing)
                                                  (notes :: String)
    close conn
    return result

  addVolunteer :: VolunteerDetails -> IO Int
  addVolunteer details = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn "insert into volunteer (name, overnight_pref, overnight_gender_pref, notes) values (?, ?, ?, ?) returning id"
                            ( vdName details
                            , vdPref details <&> \case PreferToBeAlone -> '1'
                                                       PreferAnotherVolunteer -> '2' 
                            , vdGenderPref details <&> \case Male -> 'M'
                                                             Female -> 'F'
                            , vdNotes details
                            )
    close conn
    return id
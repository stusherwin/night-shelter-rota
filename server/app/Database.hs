{-# LANGUAGE OverloadedStrings #-}

module Database (getVolunteers, getVolunteer) where
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Data.ByteString (ByteString)
  import Types
  import System.IO ()
  
  connectionString :: ByteString
  connectionString = "postgres://shelter_rota_user:password@localhost:5432/shelter_rota"
                
  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)

  getVolunteers :: IO [Volunteer]
  getVolunteers = do
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  
  import Control.Monad.IO.Class (liftIO)
  import Network.Wai.Handler.Warp (run)
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
  import Data.Time.Clock (getCurrentTime, utctDay)
  import Data.Time.Calendar (toGregorian)
  import Servant
  import Data.ByteString (ByteString)
  import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
  import Network.Wai.Middleware.Servant.Options (provideOptions)
  
  import Types 
  import Api
  import Database

  connectionString :: ByteString
  connectionString = "postgres://shelter_rota_user:password@localhost:5432/shelter_rota"
  
  localHostOrigin :: ByteString
  localHostOrigin = "http://localhost:5022"
  
  main :: IO ()
  main = do
    run 8081 $ app connectionString

  app :: ByteString -> Application
  app conn = cors (const $ Just policy)
           $ provideOptions appAPI
           $ serve fullAPI (server conn)
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ]
                                            , corsMethods = simpleMethods ++ ["DELETE", "PUT"]
                                            , corsOrigins = Just ([localHostOrigin], False) }
           
  server :: ByteString -> Server FullAPI
  server conn = appServer conn
           :<|> serveDirectoryFileServer "client/static"

  appServer :: ByteString -> Server AppAPI
  appServer conn = volsServer conn
              :<|> shiftsServer conn
 
  volsServer :: ByteString -> Server VolsAPI
  volsServer conn = getAll
               :<|> add
               :<|> getOne
               :<|> update
    where
    getAll :: Handler [Volunteer]
    getAll =
      liftIO $ getAllVolunteers conn

    add :: VolunteerDetails -> Handler Volunteer
    add details = do
      newId <- liftIO $ addVolunteer conn details
      return $ newVolunteer newId details

    getOne :: Int -> Handler Volunteer
    getOne id = do
      result <- liftIO $ getVolunteer conn id
      case result of
        Just v -> return v
        _ -> throwError err404

    update :: Int -> VolunteerDetails -> Handler Volunteer
    update id details = do
      result <- liftIO $ updateVolunteer conn id details
      case result of
        Just v -> return v
        _ -> throwError err404
 
  shiftsServer :: ByteString -> Server ShiftsAPI
  shiftsServer conn = getAll
                 :<|> getOne
                 :<|> add
                 :<|> remove
                 :<|> update
    where
    getAll :: Handler [Shift]
    getAll = 
      liftIO $ getAllShifts conn
  
    getOne :: Int -> Int -> Int -> Handler [VolunteerShift]
    getOne y m d =
      liftIO $ getVolunteerShifts conn $ ShiftDate y m d

    add ::  Int -> Int -> Int -> Int -> ShiftType -> Handler [VolunteerShift]
    add y m d volId shiftType =
      liftIO $ addVolunteerShift conn (ShiftDate y m d) volId shiftType

    remove :: Int -> Int -> Int -> Int -> Handler [VolunteerShift]
    remove y m d volId = do
      result <- liftIO $ removeVolunteerShift conn (ShiftDate y m d) volId
      case result of
        Just vs -> return vs
        _ -> throwError err404

    update :: Int -> Int -> Int -> Int -> ShiftType -> Handler [VolunteerShift]
    update y m d volId shiftType = do
      result <- liftIO $ updateVolunteerShift conn (ShiftDate y m d) volId shiftType
      case result of
        Just vs -> return vs
        _ -> throwError err404
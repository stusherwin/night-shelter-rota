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
  import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
  import Data.Time.Calendar (toGregorian, fromGregorian, addGregorianYearsClip)
  import Data.Time.Format (formatTime, defaultTimeLocale)
  import Data.ByteString.Char8 (pack)
  import Data.Text (Text, unpack)
  import qualified Data.Text as T (pack)
  import Servant
  import Control.Concurrent(threadDelay)
  import Data.ByteString (ByteString)
  import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
  import Network.Wai.Middleware.Servant.Options (provideOptions)
  import Network.Wai.Middleware.RequestLogger (logStdoutDev)
  import System.Environment (getEnv, getArgs)
  import Web.Cookie (parseCookiesText)
  import Web.HttpApiData
  
  import Types 
  import Api
  import Database

  main :: IO ()
  main = do
    connectionString <- getEnv "DATABASE_URL"
    putStrLn $ "connection string: " ++ connectionString
    args <- getArgs
    let portStr = head args
    putStrLn $ "port: " ++ portStr
    run (read portStr) $ app $ pack connectionString

  app :: ByteString -> Application
  app conn = logStdoutDev
             $ serve fullAPI (server conn)
           
  server :: ByteString -> Server FullAPI
  server conn = appServer conn
           :<|> serveDirectoryFileServer "client/static"

  appServer :: ByteString -> Server AppAPI
  appServer conn = volsServer conn
              :<|> shiftsServer conn
              :<|> currentVolServer conn
 
  volsServer :: ByteString -> Server VolsAPI
  volsServer conn = getAll
               :<|> add
               :<|> update
    where
    getAll :: Handler [Volunteer]
    getAll =
      liftIO $ getAllVolunteers conn

    add :: VolunteerDetails -> Handler Volunteer
    add details = do
      newId <- liftIO $ addVolunteer conn details
      return $ newVolunteer newId details

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
    add y m d volId shiftType = do
      --liftIO $ threadDelay 1000000
      liftIO $ addVolunteerShift conn (ShiftDate y m d) volId shiftType

    remove :: Int -> Int -> Int -> Int -> Handler [VolunteerShift]
    remove y m d volId = do
      --liftIO $ threadDelay 10000000
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

  currentVolServer :: ByteString -> Server CurrentVolAPI
  currentVolServer conn = get
                     :<|> update
    where
    get :: Maybe Text -> Handler (Maybe Int)
    get Nothing = return Nothing
    get (Just cookie) = do
      let x = parseCookiesText $ pack $ unpack cookie
      let y = lookup "CurrentVolId" x
      case y of
        Nothing -> return Nothing
        Just z -> do 
          case (parseUrlPiece z :: Either Text Int) of
            Left _ -> return Nothing
            Right i -> return $ Just i
    
    update :: Int -> Handler (Headers '[Header "Set-Cookie" Text] ())
    update id = do
      result <- liftIO $ getVolunteer conn id
      liftIO $ putStrLn $ "vol: " ++ show result
      case result of
        Nothing -> throwError err404
        Just _ -> do
          time <- liftIO $ getCurrentTime
          return $ addHeader (T.pack $ "CurrentVolId=" ++ show id ++ "; expires=" ++ (getExpiryDateString time) ++ "; path=/; HttpOnly") ()

  getExpiryDateString :: UTCTime -> String
  getExpiryDateString currentTime =
    let day = utctDay currentTime
        expiryDate = addGregorianYearsClip 10 $ day
        expDateStr = (formatTime defaultTimeLocale "%a, %d %b %Y" expiryDate)
        expDateTim = (formatTime defaultTimeLocale "%H:%M:%S" currentTime)
    in expDateStr ++ " " ++ expDateTim ++ " GMT"

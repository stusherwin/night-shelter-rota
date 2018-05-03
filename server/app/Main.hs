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
  import Data.Text (Text)
  import Text.Read (readMaybe)
  import qualified Data.Text as T (pack, unpack)
  import qualified Data.ByteString.Char8 as B (pack, unpack)
  import Servant
  import Control.Concurrent(threadDelay)
  import Data.ByteString (ByteString)
  import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
  import Network.Wai.Middleware.Servant.Options (provideOptions)
  import Network.Wai.Middleware.RequestLogger (logStdoutDev)
  import System.Environment (getEnv, getArgs)
  import Web.Cookie (parseCookiesText)
  import Web.HttpApiData (parseUrlPiece, toUrlPiece)
  
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
    run (read portStr) $ app $ B.pack connectionString

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
                     :<|> set
                     :<|> clear
    where
    get :: Maybe Text -> Handler (Maybe Int)
    get Nothing = return Nothing
    get (Just cookies) = 
      return $ readMaybe . T.unpack =<< (lookup "CurrentVolId" $ parseCookiesText $ B.pack $ T.unpack cookies)
    
    set :: Int -> Handler (Headers '[Header "Set-Cookie" Text] ())
    set id = do
      liftIO $ putStrLn "set"
      result <- liftIO $ getVolunteer conn id
      case result of
        Nothing -> throwError err404
        Just _ -> do
          time <- liftIO $ getCurrentTime
          return $ addHeader (setCookie "CurrentVolId" id time 10) ()

    clear :: Handler (Headers '[Header "Set-Cookie" Text] ())
    clear = do
      liftIO $ putStrLn "clear"
      time <- liftIO $ getCurrentTime
      return $ addHeader (setCookie "CurrentVolId" "" time 10) ()

  setCookie :: Show a => String -> a -> UTCTime -> Integer -> Text
  setCookie key val currentTime years =
    let expDateStr = (formatTime defaultTimeLocale "%a, %d %b %Y" $ addGregorianYearsClip years $ utctDay currentTime)
        expDateTim = (formatTime defaultTimeLocale "%H:%M:%S" currentTime)
        expDate = expDateStr ++ " " ++ expDateTim ++ " GMT"
    in  T.pack $ key ++ "=" ++ (show val) ++ "; expires=" ++ expDate ++ "; path=/; HttpOnly"

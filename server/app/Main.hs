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
           :<|> serveFilesWithRota
           :<|> serveDirectoryFileServer "client/static"
    where
    serveFilesWithRota :: Text -> Server Raw
    serveFilesWithRota _ = serveDirectoryFileServer "client/static"

  appServer :: ByteString -> Server AppAPI
  appServer conn = verifyServer conn
              :<|> withRotaServer conn

  verifyServer :: ByteString -> Server VerifyAPI
  verifyServer conn rotaKey = do
    rota <- liftIO $ findRota conn rotaKey
    case rota of
      Just _ -> return True
      _ -> return False
  
  withRotaServer :: ByteString -> Server WithRotaAPI
  withRotaServer conn rotaKey = volsServer conn rotaKey
                          :<|> shiftsServer conn rotaKey
                          :<|> currentVolServer conn rotaKey

  volsServer :: ByteString -> Text -> Server VolsAPI
  volsServer conn rotaKey = getAll rotaKey
                      :<|> add rotaKey
                      :<|> update rotaKey
                      :<|> activate rotaKey
                      :<|> deactivate rotaKey
    where
    getAll :: Text -> Handler [Volunteer]
    getAll rotaKey = findRotaOr404 conn rotaKey $ \rotaId ->
      liftIO $ getAllVolunteers conn rotaId

    add :: Text -> VolunteerDetails -> Handler Volunteer
    add rotaKey details = findRotaOr404 conn rotaKey $ \rotaId -> do
      newId <- liftIO $ addVolunteer conn rotaId details
      return $ newVolunteer newId rotaId details

    update :: Text -> Int -> VolunteerDetails -> Handler Volunteer
    update rotaKey id details = findRotaOr404 conn rotaKey $ \rotaId -> do
      result <- liftIO $ updateVolunteer conn rotaId id details
      case result of
        Just v -> return v
        _ -> throwError err404
 
    activate :: Text -> Int -> Handler Volunteer
    activate rotaKey id = findRotaOr404 conn rotaKey $ \rotaId -> do
      result <- liftIO $ activateVolunteer conn rotaId id
      case result of
        Just v -> return v
        _ -> throwError err404

    deactivate :: Text -> Int -> Handler Volunteer
    deactivate rotaKey id = findRotaOr404 conn rotaKey $ \rotaId -> do
      result <- liftIO $ deactivateVolunteer conn rotaId id
      case result of
        Just v -> return v
        _ -> throwError err404

  shiftsServer :: ByteString -> Text -> Server ShiftsAPI
  shiftsServer conn rotaKey = getAll rotaKey
                        :<|> getOne rotaKey
                        :<|> add rotaKey
                        :<|> remove rotaKey
                        :<|> update rotaKey
    where
    getAll :: Text -> Handler [Shift]
    getAll rotaKey = findRotaOr404 conn rotaKey $ \rotaId -> 
      liftIO $ getAllShifts conn rotaId
  
    getOne :: Text -> Int -> Int -> Int -> Handler [VolunteerShift]
    getOne rotaKey y m d = findRotaOr404 conn rotaKey $ \rotaId -> 
      liftIO $ getVolunteerShifts conn rotaId $ ShiftDate y m d

    add ::  Text -> Int -> Int -> Int -> Int -> ShiftType -> Handler [VolunteerShift]
    add rotaKey y m d volId shiftType = findRotaOr404 conn rotaKey $ \rotaId -> do
      --liftIO $ threadDelay 1000000
      result <- liftIO $ addVolunteerShift conn rotaId (ShiftDate y m d) volId shiftType
      case result of
        Just vs -> return vs
        _ -> throwError err404

    remove :: Text -> Int -> Int -> Int -> Int -> Handler [VolunteerShift]
    remove rotaKey y m d volId = findRotaOr404 conn rotaKey $ \rotaId -> do
      --liftIO $ threadDelay 10000000
      result <- liftIO $ removeVolunteerShift conn rotaId (ShiftDate y m d) volId
      case result of
        Just vs -> return vs
        _ -> throwError err404

    update :: Text -> Int -> Int -> Int -> Int -> ShiftType -> Handler [VolunteerShift]
    update rotaKey y m d volId shiftType = findRotaOr404 conn rotaKey $ \rotaId -> do
      result <- liftIO $ updateVolunteerShift conn rotaId (ShiftDate y m d) volId shiftType
      case result of
        Just vs -> return vs
        _ -> throwError err404

  currentVolServer :: ByteString -> Text -> Server CurrentVolAPI
  currentVolServer conn rotaKey = get rotaKey
                            :<|> set rotaKey
                            :<|> clear rotaKey
    where
    get :: Text -> Maybe Text -> Handler (Maybe Int)
    get rotaKey c = findRotaOr404 conn rotaKey $ \rotaId -> get' c
      where
      get' Nothing = return Nothing
      get' (Just cookies) = 
        -- DO THIS:
        --select vol id from database

        return $ readMaybe . T.unpack =<< (lookup "CurrentVolId" $ parseCookiesText $ B.pack $ T.unpack cookies)
    
    set :: Text -> Int -> Handler (Headers '[Header "Set-Cookie" Text] ())
    set rotaKey id = findRotaOr404 conn rotaKey $ \rotaId -> do
      liftIO $ putStrLn "set"
      result <- liftIO $ getVolunteer conn rotaId id
      case result of
        Nothing -> throwError err404
        Just _ -> do
          time <- liftIO $ getCurrentTime
          return $ addHeader (setCookie "CurrentVolId" id time 10) ()

    clear :: Text -> Handler (Headers '[Header "Set-Cookie" Text] ())
    clear rotaKey = findRotaOr404 conn rotaKey $ \rotaId -> do
      liftIO $ putStrLn "clear"
      time <- liftIO $ getCurrentTime
      return $ addHeader (setCookie "CurrentVolId" "" time 10) ()

  setCookie :: Show a => String -> a -> UTCTime -> Integer -> Text
  setCookie key val currentTime years =
    let expDateStr = (formatTime defaultTimeLocale "%a, %d %b %Y" $ addGregorianYearsClip years $ utctDay currentTime)
        expDateTim = (formatTime defaultTimeLocale "%H:%M:%S" currentTime)
        expDate = expDateStr ++ " " ++ expDateTim ++ " GMT"
    in  T.pack $ key ++ "=" ++ (show val) ++ "; expires=" ++ expDate ++ "; path=/; HttpOnly"

  findRota :: ByteString -> Text -> IO (Maybe Int)
  findRota conn rotaKey = do
    rotaId <- getRota conn (T.unpack rotaKey)
    return rotaId

  findRotaOr404 :: ByteString -> Text -> (Int -> Handler a) -> Handler a
  findRotaOr404 conn rotaId handler = do
    rotaId <- liftIO $ findRota conn rotaId
    case rotaId of
      Just id -> handler id
      _ -> throwError err404
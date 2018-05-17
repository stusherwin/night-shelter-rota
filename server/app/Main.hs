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
  verifyServer conn rotaId = do
    rota <- liftIO $ findRota conn rotaId
    case rota of
      Just _ -> return True
      _ -> return False
  
  withRotaServer :: ByteString -> Server WithRotaAPI
  withRotaServer conn rotaId = volsServer conn rotaId
                          :<|> shiftsServer conn rotaId
                          :<|> currentVolServer conn rotaId

  volsServer :: ByteString -> Text -> Server VolsAPI
  volsServer conn rotaId = getAll rotaId
                      :<|> add rotaId
                      :<|> update rotaId
                      :<|> activate rotaId
                      :<|> deactivate rotaId
    where
    getAll :: Text -> Handler [Volunteer]
    getAll rotaId = findRotaOr404 conn rotaId $
      liftIO $ getAllVolunteers conn

    add :: Text -> VolunteerDetails -> Handler Volunteer
    add rotaId details = findRotaOr404 conn rotaId $ do
      newId <- liftIO $ addVolunteer conn details
      return $ newVolunteer newId details

    update :: Text -> Int -> VolunteerDetails -> Handler Volunteer
    update rotaId id details = findRotaOr404 conn rotaId $ do
      result <- liftIO $ updateVolunteer conn id details
      case result of
        Just v -> return v
        _ -> throwError err404
 
    activate :: Text -> Int -> Handler Volunteer
    activate rotaId id = findRotaOr404 conn rotaId $ do
      result <- liftIO $ activateVolunteer conn id
      case result of
        Just v -> return v
        _ -> throwError err404

    deactivate :: Text -> Int -> Handler Volunteer
    deactivate rotaId id = findRotaOr404 conn rotaId $ do
      result <- liftIO $ deactivateVolunteer conn id
      case result of
        Just v -> return v
        _ -> throwError err404

  shiftsServer :: ByteString -> Text -> Server ShiftsAPI
  shiftsServer conn rotaId = getAll rotaId
                        :<|> getOne rotaId
                        :<|> add rotaId
                        :<|> remove rotaId
                        :<|> update rotaId
    where
    getAll :: Text -> Handler [Shift]
    getAll rotaId = findRotaOr404 conn rotaId $ 
      liftIO $ getAllShifts conn
  
    getOne :: Text -> Int -> Int -> Int -> Handler [VolunteerShift]
    getOne rotaId y m d = findRotaOr404 conn rotaId $ 
      liftIO $ getVolunteerShifts conn $ ShiftDate y m d

    add ::  Text -> Int -> Int -> Int -> Int -> ShiftType -> Handler [VolunteerShift]
    add rotaId y m d volId shiftType = findRotaOr404 conn rotaId $ do
      --liftIO $ threadDelay 1000000
      liftIO $ addVolunteerShift conn (ShiftDate y m d) volId shiftType

    remove :: Text -> Int -> Int -> Int -> Int -> Handler [VolunteerShift]
    remove rotaId y m d volId = findRotaOr404 conn rotaId $ do
      --liftIO $ threadDelay 10000000
      result <- liftIO $ removeVolunteerShift conn (ShiftDate y m d) volId
      case result of
        Just vs -> return vs
        _ -> throwError err404

    update :: Text -> Int -> Int -> Int -> Int -> ShiftType -> Handler [VolunteerShift]
    update rotaId y m d volId shiftType = findRotaOr404 conn rotaId $ do
      result <- liftIO $ updateVolunteerShift conn (ShiftDate y m d) volId shiftType
      case result of
        Just vs -> return vs
        _ -> throwError err404

  currentVolServer :: ByteString -> Text -> Server CurrentVolAPI
  currentVolServer conn rotaId = get rotaId
                            :<|> set rotaId
                            :<|> clear rotaId
    where
    get :: Text -> Maybe Text -> Handler (Maybe Int)
    get rotaId c = findRotaOr404 conn rotaId $ get' c
      where
      get' Nothing = return Nothing
      get' (Just cookies) = 
        return $ readMaybe . T.unpack =<< (lookup "CurrentVolId" $ parseCookiesText $ B.pack $ T.unpack cookies)
    
    set :: Text -> Int -> Handler (Headers '[Header "Set-Cookie" Text] ())
    set rotaId id = findRotaOr404 conn rotaId $ do
      liftIO $ putStrLn "set"
      result <- liftIO $ getVolunteer conn id
      case result of
        Nothing -> throwError err404
        Just _ -> do
          time <- liftIO $ getCurrentTime
          return $ addHeader (setCookie "CurrentVolId" id time 10) ()

    clear :: Text -> Handler (Headers '[Header "Set-Cookie" Text] ())
    clear rotaId = findRotaOr404 conn rotaId $ do
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
  findRota conn rotaId = do
    case T.unpack rotaId of
      "hi" -> return $ Just 123
      _ -> return Nothing

  findRotaOr404 :: ByteString -> Text -> Handler a -> Handler a
  findRotaOr404 conn rotaId handler = do
    rota <- liftIO $ findRota conn rotaId
    case rota of
      Just _ -> handler
      _ -> throwError err404
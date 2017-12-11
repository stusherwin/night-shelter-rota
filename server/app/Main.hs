{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where
  
  import Control.Monad.IO.Class (liftIO)
  import Network.Wai.Handler.Warp (run)
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
  import Data.Time.Clock (getCurrentTime, utctDay)
  import Data.Time.Calendar (toGregorian)
  import Servant

  import Types 
  import Api
  import Database

  currentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
  currentDate = getCurrentTime >>= return . toGregorian . utctDay
  
  data State = State { vols :: IM.IntMap Volunteer
                     , shifts :: [Shift]
                     }
  
  main :: IO ()
  main = do
    today <- currentDate
    ref <- newIORef $ initialState today
    run 8081 (app ref)
     
  app :: IORef State -> Application
  app ref = serve fullAPI (server ref)
                            
  server :: IORef State -> Server FullAPI
  server ref = appServer ref
          :<|> serveDirectoryFileServer "client/static"

  appServer :: IORef State -> Server AppAPI
  appServer ref = volsServer ref
             :<|> shiftsServer ref
 
  volsServer :: IORef State -> Server VolsAPI
  volsServer ref = getAll
              :<|> add
              :<|> getOne
              :<|> update
    where
    getAll :: Handler [Volunteer]
    getAll = do
      vols <- liftIO getVolunteers
      return vols

    add :: VolunteerDetails -> Handler Volunteer
    add details = do
      liftIO $ atomicModifyIORef' ref $ \state ->
        let  vs = vols state
             newId = IM.size vs + 1
             newVol = newVolunteer newId details
        in (state { vols = IM.insert newId newVol vs }, newVol)

    getOne :: Int -> Handler Volunteer
    getOne id = do
      vol <- liftIO $ getVolunteer id
      case vol of
        Just v -> return v
        _ -> throwError err404

    update :: Int -> VolunteerDetails -> Handler Volunteer
    update id details = do
      v <- liftIO $ atomicModifyIORef' ref $ \state ->
        let vs = vols state
        in case IM.lookup id vs of
             Just v -> let v' = v { vName = vdName details
                                  , vNotes = vdNotes details
                                  , vOvernightPreference = vdPref details
                                  , vOvernightGenderPreference = vdGenderPref details
                                  }
                       in (state { vols = IM.insert id v' vs }, Just v')
             _ -> (state, Nothing)

      case v of
        Just v -> return v
        _ -> throwError err404
 
  shiftsServer :: IORef State -> Handler [Shift]
  shiftsServer ref = do
    state <- liftIO $ readIORef ref
    return $ shifts state

  initialState :: (Integer, Int, Int) -> State
  initialState (y, m, d) =
     let fred  = Volunteer { vId = 1
                           , vName = "Fred"
                           , vOvernightPreference = Just PreferAnotherVolunteer
                           , vOvernightGenderPreference = Nothing
                           , vNotes = ""
                           }
         alice = Volunteer { vId = 2
                           , vName = "Alice"
                           , vOvernightPreference = Nothing
                           , vOvernightGenderPreference = Just Female
                           , vNotes = ""
                           }
         jim   = Volunteer { vId = 3
                           , vName = "Jim"
                           , vOvernightPreference = Just PreferToBeAlone
                           , vOvernightGenderPreference = Just Male
                           , vNotes = ""
                           }
         mary  = Volunteer { vId = 4
                           , vName = "Mary"
                           , vOvernightPreference = Nothing
                           , vOvernightGenderPreference = Nothing
                           , vNotes = "Only nice people"
                           }

    in State { vols = IM.fromList $ map (\v -> (vId v, v)) [ fred, alice, jim, mary ]
             , shifts = [ Shift (Date (fromInteger y) m d) [ Overnight fred
                                                           , Evening alice
                                                           , Overnight jim
                                                           , Evening mary
                                                           ]
                        , Shift (Date (fromInteger y) m (d + 1)) [ Overnight fred
                                                                 , Overnight jim
                                                                 , Evening mary
                                                                 ]
                        ]
             } 
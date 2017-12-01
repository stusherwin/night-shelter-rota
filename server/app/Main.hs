{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where
  
  import Control.Monad.Trans.Except ()
  import Control.Monad.IO.Class (liftIO)
  import Data.Aeson ()
  import GHC.Generics ()
  import Network.Wai ()
  import Network.Wai.Handler.Warp (run)
  import Servant
  import System.IO ()
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
 
  import Types
  import Api

  data State = State { vols :: IM.IntMap Volunteer
                     , shifts :: [Shift]
                     }
  
  main :: IO ()
  main = do
    ref <- newIORef initialState
    run 8081 (app ref)
    where
    fred = Volunteer 1 "Fred" Nothing Nothing ""
    jim = Volunteer 2 "Jim" (Just PreferToBeAlone) (Just Male) "hi"
    initialState = State { vols = IM.fromList [ (1, fred)
                                              , (2, jim) 
                                              ]
                         , shifts = [ Shift (Date 2017 1 1) []
                                    , Shift (Date 2017 1 2) [Overnight fred, Evening jim]
                                    ]
                         }
     
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
      state <- liftIO $ readIORef ref
      return $ IM.elems $ vols state

    add :: VolunteerDetails -> Handler Volunteer
    add details = do
      liftIO $ atomicModifyIORef' ref $ \state ->
        let  vs = vols state
             newId = IM.size vs + 1
             newVol = newVolunteer newId details
        in (state { vols = IM.insert newId newVol vs }, newVol)

    getOne :: Int -> Handler Volunteer
    getOne id = do
      state <- liftIO $ readIORef ref
      case IM.lookup id $ vols state of
        Just v -> return v
        _ -> throwError err404

    update :: Int -> VolunteerDetails -> Handler Volunteer
    update id details = do
      v <- liftIO $ atomicModifyIORef' ref $ \state ->
        let vs = vols state
        in case IM.lookup id vs of
             Just v -> let v' = v { vName = vdName details }
                       in (state { vols = IM.insert id v' vs }, Just v')
             _ -> (state, Nothing)

      case v of
        Just v -> return v
        _ -> throwError err404
 
  shiftsServer :: IORef State -> Handler [Shift]
  shiftsServer ref = do
    state <- liftIO $ readIORef ref
    return $ shifts state
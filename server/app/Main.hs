{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  
  import Control.Monad.Trans.Except
  import Data.Aeson
  import GHC.Generics
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Servant
  import System.IO
  import Types
  import Api
    
  main :: IO ()
  main = run 8081 app
                         
  appServer :: Server AppAPI
  appServer = vols
      :<|> shifts

  server :: Server FullAPI
  server = appServer
      :<|> serveDirectoryFileServer "client/static"
  
  fred :: Volunteer
  fred = Volunteer 1 "Fred" Nothing Nothing ""

  jim :: Volunteer
  jim = Volunteer 2 "Jim" (Just PreferToBeAlone) (Just Male) "hi"

  vols :: Handler [Volunteer]
  vols = return [ fred, jim ]

  shifts :: Handler [Shift]
  shifts = return [ Shift (Date 2017 1 1) []
                  , Shift (Date 2017 1 2) [Overnight fred, Evening jim]
                  ]

  app :: Application
  app = serve fullAPI server
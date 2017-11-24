{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  
  import           Control.Monad.Trans.Except
  import           Data.Aeson
  import           GHC.Generics
  import           Network.Wai
  import           Network.Wai.Handler.Warp
  import           Servant
  import           System.IO
    
  main :: IO ()
  main = run 8081 app
    
  type StuAPI =
         "stu" :> Get '[JSON] Int
    :<|> Raw

  api :: Proxy StuAPI
  api = Proxy

  server :: Server StuAPI
  server = stu :<|> content
  
  stu :: Handler Int
  stu = return 123

  content :: Server Raw
  content = serveDirectoryFileServer "client/static"

  app :: Application
  app = serve api server
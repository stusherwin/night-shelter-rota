{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Api where
  import Servant
  import Types

  type AppAPI =
    "api" :> (
           "vols" :> Get '[JSON] [Volunteer]
      :<|> "shifts" :> Get '[JSON] [Shift]
    )

  type FullAPI =
    AppAPI :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy

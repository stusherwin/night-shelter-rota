{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Api where
  import Servant
  import Types
 
  type AppAPI =
    "api" :> (
           "vols" :> VolsAPI
      :<|> "shifts" :> ShiftsAPI
    )

  type VolsAPI =
         Get '[JSON] [Volunteer] 
    :<|> ReqBody '[JSON] VolunteerDetails :> Put '[JSON] Volunteer
    :<|> Capture "id" Int :> Get '[JSON] Volunteer
    :<|> Capture "id" Int :> ReqBody '[JSON] VolunteerDetails :> Post '[JSON] Volunteer

  type ShiftsAPI =
    Get '[JSON] [Shift]

  type FullAPI =
    AppAPI :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy

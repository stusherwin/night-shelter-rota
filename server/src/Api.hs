{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    :<|> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Get '[JSON] [VolunteerShift]
    :<|> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Capture "volId" Int :> ReqBody '[JSON] ShiftType :> Put '[JSON] [VolunteerShift]
    :<|> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Capture "volId" Int :> Delete '[JSON] [VolunteerShift]
    :<|> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Capture "volId" Int :> ReqBody '[JSON] ShiftType :> Post '[JSON] [VolunteerShift]
    
  type FullAPI =
    AppAPI :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy
{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}

module Main where
  
  import           Control.Applicative
  import           Control.Lens
  import           Data.Aeson
  import           Data.Monoid
  import           Data.Proxy
  import qualified Data.Set                           as Set
  import           Data.Text                          (Text)
  import qualified Data.Text                          as T
  import qualified Data.Text.Encoding                 as T
  import qualified Data.Text.IO                       as T
  import           Language.PureScript.Bridge
  import           Language.PureScript.Bridge.PSTypes
  import           Servant.API
  import           Servant.PureScript
  import           Servant.Subscriber.Subscribable
  import           GHC.Generics                       (Generic)
  
  import Types
  import Api
                
  fixTypesModule :: BridgePart
  fixTypesModule = do
    typeModule ^== "Types"
    t <- view haskType
    TypeInfo (_typePackage t) "ServerTypes" (_typeName t) <$> psTypeParameters
  
  myBridge = defaultBridge <|> fixTypesModule
  
  data MyBridge
  
  myBridgeProxy :: Proxy MyBridge
  myBridgeProxy = Proxy
  
  instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge
  
  myTypes :: [SumType 'Haskell]
  myTypes = [ mkSumType (Proxy :: Proxy Volunteer)
            , mkSumType (Proxy :: Proxy Shift)
            , mkSumType (Proxy :: Proxy OvernightPreference)
            , mkSumType (Proxy :: Proxy OvernightGenderPreference)
            , mkSumType (Proxy :: Proxy Date)
            , mkSumType (Proxy :: Proxy VolunteerShift)
            , mkSumType (Proxy :: Proxy VolunteerDetails)
            ]

  -- mySettings :: Settings
  -- mySettings = defaultSettings {
  --   _generateSubscriberAPI = True
  -- }    
    -- addReaderParam "AuthToken" defaultSettings &  (apiModuleName .~ "Api") {
    -- _generateSubscriberAPI = True
    -- }
  
  main :: IO ()
  main = do
    let frontEndRoot = "client/src"
    writeAPIModule frontEndRoot myBridgeProxy appAPI
    writePSTypes frontEndRoot (buildBridge myBridge) myTypes

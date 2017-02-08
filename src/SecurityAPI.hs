{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module SecurityAPI where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           Data.Bson.Generic
import           APIDataTypes



type SecurityAPI = "login" :> ReqBody '[JSON] SecurityUser :> Post '[JSON] (Maybe ServerDetails)
             :<|> "adduser" :> ReqBody '[JSON] SecurityUser :> Post '[JSON] Bool
             :<|> "removeuser" :> ReqBody '[JSON] SecurityUser :> Post '[JSON] Bool


securityAPI :: Proxy SecurityAPI
securityAPI = Proxy

loginSecurityUser :: SecurityUser -> ClientM (Maybe ServerDetails)
addSecurityUser :: SecurityUser -> ClientM Bool
removeSecurityUsers :: SecurityUser -> ClientM Bool

loginSecurityUser :<|> addSecurityUser :<|> removeSecurityUsers = client securityAPI
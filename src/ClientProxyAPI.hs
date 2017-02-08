{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module ClientProxyAPI where

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

type ClientProxyAPI = "setcredentials" :> ReqBody '[JSON] SecurityUser :> Post '[JSON] Bool
             :<|> "readfile" :> ReqBody '[JSON] FileDetails :> Post '[JSON] FileDetails
             :<|> "writefile" :> ReqBody '[JSON] FileDetails :> Post '[JSON] Bool


clientProxyAPI :: Proxy ClientProxyAPI
clientProxyAPI = Proxy

setCredentials :: SecurityUser -> ClientM Bool
readFile :: FileDetails -> ClientM FileDetails
writeFile :: FileDetails -> ClientM Bool

setCredentials :<|> readFile :<|> writeFile = client clientProxyAPI
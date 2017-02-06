{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module ClientProxyAPI where -- (getSecurityUsers, getSecurityUser, getPackages, Package(..))  where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           SecurityAPI
import           Data.Bson.Generic

-- The purpose of this section is to explain how to perform a REST call on a remote service fro your own Servant
-- service. This code will be called from the Handler doRESTCall in the handler set above.
--
-- We will access the REST serivice hackage.haskell.org, availabel on port 80. This service provides a set of endpoints
-- for haskell documentation. We will implemnt a single endpoint. For a more comprehensive example, see
-- https://haskell-servant.github.io/client-in-5-minutes.html.

-- First up, some data types we need to define the API call we want to maketype SecurityUsername = Text


-- Next, the hackage API definition - this is the remote service
-- This defines the functions we  want to be able to call. That is all there is to it. We can now call these funtions,
-- passing in the apporpriate parameters, and returning the appropriate data from hackage.haskell.org.

data FileDetails = FileDetails
  { fileid :: String
  , filename :: String
  , filecontents :: String
  } deriving (Eq, Show, Generic, ToBSON, FromBSON, ToJSON, FromJSON)




type ClientProxyAPI = "setcredentials" :> ReqBody '[JSON] SecurityUser :> Post '[JSON] Bool
             :<|> "readfile" :> ReqBody '[JSON] FileDetails :> Post '[JSON] FileDetails
             :<|> "writefile" :> ReqBody '[JSON] FileDetails :> Post '[JSON] Bool


clientProxyAPI :: Proxy ClientProxyAPI
clientProxyAPI = Proxy

setCredentials :: SecurityUser -> ClientM Bool
readFile :: FileDetails -> ClientM FileDetails
writeFile :: FileDetails -> ClientM Bool

setCredentials :<|> readFile :<|> writeFile = client clientProxyAPI
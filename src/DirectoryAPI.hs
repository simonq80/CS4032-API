{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module DirectoryAPI where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           APIDataTypes



type DirectoryAPI = "getfilelocation" :> ReqBody '[JSON] (FileDetails, ServerDetails):> Post '[JSON] (Maybe ServerDetails)
             :<|> "addtoken" :> ReqBody '[JSON] (ServerDetails, ServerDetails) :> Post '[JSON] Bool
             :<|> "propogatewrite" :> ReqBody '[JSON] (FileDetails, ServerDetails) :> Post '[JSON] Bool


directoryAPI :: Proxy DirectoryAPI
directoryAPI = Proxy

getFileLocation :: (FileDetails, ServerDetails)-> ClientM (Maybe ServerDetails)
addToken :: (ServerDetails, ServerDetails) -> ClientM Bool
propogateWrite :: (FileDetails, ServerDetails) -> ClientM Bool

getFileLocation :<|> addToken :<|> propogateWrite = client directoryAPI
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module FileAPI where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           APIDataTypes


type FileAPI = "readfile" :> ReqBody '[JSON] (FileDetails, ServerDetails):> Post '[JSON] (Maybe FileDetails)
             :<|> "writefile" :> ReqBody '[JSON] (FileDetails, ServerDetails) :> Post '[JSON] Bool
             :<|> "addtoken" :> ReqBody '[JSON] ServerDetails :> Post '[JSON] Bool


fileAPI :: Proxy FileAPI
fileAPI = Proxy

readFile :: (FileDetails, ServerDetails)-> ClientM (Maybe FileDetails)
writeFile :: (FileDetails, ServerDetails) -> ClientM Bool
addToken :: ServerDetails -> ClientM Bool

readFile :<|> writeFile :<|> addToken = client fileAPI
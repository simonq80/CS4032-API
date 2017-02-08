{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module APIDataTypes where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           Data.Bson.Generic


data SecurityUser = SecurityUser
  { username :: String
  , password  :: String
  } deriving (Eq, Show, Generic, ToBSON, FromBSON, ToJSON, FromJSON)

deriving instance FromBSON String
deriving instance ToBSON String

data ServerDetails = ServerDetails
  { serverip :: String
  , serverport :: String
  , token :: String  
  } deriving (Eq, Show, Generic, ToBSON, FromBSON, ToJSON, FromJSON)

data FileDetails = FileDetails
  { fileid :: String
  , filename :: String
  , filecontents :: String
  } deriving (Eq, Show, Generic, ToBSON, FromBSON, ToJSON, FromJSON)

data FileServerDetails = FileServerDetails
  { filedetails :: FileDetails
  , serverdetails :: ServerDetails
  } deriving (Eq, Show, Generic, ToBSON, FromBSON, ToJSON, FromJSON)
{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.Client where

import Network.HTTP.Simple(httpJSON)
import Network.Oracle.BMC.Transport.Request
import Network.Oracle.BMC.Credentials
import Network.Oracle.BMC.Core.Requests.All
import Network.Oracle.BMC.RequestBase(toRequest, ToRequest)
import Network.Oracle.BMC.Core.Model.Instance

import Data.Aeson(FromJSON)
import Network.HTTP.Client(Response)

request = listInstancesRequest "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

listInstances :: (ToRequest a, FromJSON b) => IO Credentials -> a -> IO (Response b)
listInstances credentialsProvider request = do
  credentials <- credentialsProvider
  transform credentials (toRequest request) >>= httpJSON

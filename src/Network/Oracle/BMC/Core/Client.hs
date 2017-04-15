{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.Client where

import Network.HTTP.Simple(httpJSON, httpLBS)
import Network.Oracle.BMC.Transport.Request
import Network.Oracle.BMC.Credentials
import Network.Oracle.BMC.Core.Requests
import Network.Oracle.BMC.RequestBase(toRequest, ToRequest)
import Network.Oracle.BMC.Core.Model.Instance

import Data.Aeson(FromJSON)
import Network.HTTP.Client(Response)

request = listInstancesRequest "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"
request2 = getInstanceRequest "ocid1.instance.oc1.phx.abyhqljtmiy6zjhzkesrjypco7ehdxhxby2vnqab3yxyf57tsc66ulgsp57a"

---------------------------------------------------------------------
-- Instances

-- getInstance
--getInstance :: (ToRequest a) => IO Credentials -> a -> IO (Response Instance)
getInstance credentialsProvider request = do
  credentials <- credentialsProvider
  transform credentials (toRequest request) >>= httpLBS

-- deleteInstance
listInstances :: (ToRequest a) => IO Credentials -> a -> IO (Response [Instance])
listInstances credentialsProvider request = do
  credentials <- credentialsProvider
  transform credentials (toRequest request) >>= httpJSON

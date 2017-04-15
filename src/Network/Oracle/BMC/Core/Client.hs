{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.Client where

import Network.HTTP.Simple(httpJSON, httpLBS)
import Network.Oracle.BMC.Transport.Request
import Network.Oracle.BMC.Credentials
import Network.Oracle.BMC.Core.Requests
import Network.Oracle.BMC.RequestBase(toRequest, ToRequest)
import Network.Oracle.BMC.Core.Model.Instance
import Network.Oracle.BMC.Core.Model.APIError

import Data.Aeson(FromJSON, decode)
import Network.HTTP.Client(Response, responseStatus, responseBody)
import Network.HTTP.Types.Status(statusCode)


type CredentialsProvider = IO Credentials

request = listInstancesRequest "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"
request2 = getInstanceRequest "ocid1.instance.oc1.phx.abyhqljtmiy6zjhzkesrjypco7ehdxhxby2vnqab3yxyf57tsc66ulgsp57a"
request3 = getInstanceRequest "ocid1.instance.oc1.phx.abyhqljtmiy6zjhzkesrjypco7ehdxhxby2vnqab3yxyf57tsc66ulgsp57"

runRequest :: (ToRequest a1, FromJSON a) => CredentialsProvider -> a1 -> IO (Response a)
runRequest credentialsProvider request = do
  credentials <- credentialsProvider
  transform credentials (toRequest request) >>= httpJSON

-- | WIP attempt to unify all the request and response transforms
--
runRequestModified :: (ToRequest a, FromJSON b) => IO Credentials -> a -> IO (Either (Maybe APIError) (Maybe b))
runRequestModified credentialsProvider request = do
  credentials <- credentialsProvider
  response <- transform credentials (toRequest request) >>= httpLBS
  let responseCode = statusCode $ responseStatus response
  let outcome =
        if responseCode >= 400
        then Left <$> decode $ responseBody response
        else Right <$> decode $ responseBody response
  return outcome

errExample :: IO (Either (Maybe APIError) (Maybe (Instance)))
errExample = runRequestModified defaultCredentialsProvider request3
---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

getInstance :: CredentialsProvider -> GetInstanceRequest -> IO (Response Instance)
getInstance = runRequest

-- | List instances
--
-- @
--   listInstances (listInstancesRequest compartmentOCID)
-- @
listInstances :: CredentialsProvider -> ListInstancesRequest -> IO (Response [Instance])
listInstances = runRequest

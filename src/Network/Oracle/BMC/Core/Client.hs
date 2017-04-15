{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Client where

import Network.HTTP.Simple (httpJSON, httpLBS)
import Network.Oracle.BMC.Core.Model.APIError
import Network.Oracle.BMC.Core.Model.Instance
import Network.Oracle.BMC.Core.Requests
import Network.Oracle.BMC.Credentials
import Network.Oracle.BMC.Exception (BMCException(..))
import Network.Oracle.BMC.RequestBase (toRequest, ToRequest)
import Network.Oracle.BMC.Transport.Request

import Data.Aeson (FromJSON, decode)
import Network.HTTP.Client (Response, responseStatus, responseBody)
import Network.HTTP.Types.Status (statusCode)

import qualified Data.Traversable as T

import Control.Exception (throwIO)

type BMCAPIResponse a = IO (Either APIError a)

request =
  listInstancesRequest
    "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

request2 =
  getInstanceRequest
    "ocid1.instance.oc1.phx.abyhqljtmiy6zjhzkesrjypco7ehdxhxby2vnqab3yxyf57tsc66ulgsp57a"

request3 =
  getInstanceRequest
    "ocid1.instance.oc1.phx.abyhqljtmiy6zjhzkesrjypco7ehdxhxby2vnqab3yxyf57tsc66ulgsp57"

----------------------------------------------------------------------
throwEitherMaybe :: IO (Either (Maybe a) (Maybe b)) -> IO (Either a b)
throwEitherMaybe ioEither = do
  result <- ioEither
  case result of
    Left me -> throwMaybe Left me
    Right ma -> throwMaybe Right ma
  where
    throwMaybe f ma =
      case ma of
        Just x -> return (f x)
        Nothing -> throwIO (JSONParseException)

-- | WIP attempt to unify all the request and response transforms
--
runRequestMaybe
  :: (ToRequest a, FromJSON b)
  => IO Credentials -> a -> IO (Either (Maybe APIError) (Maybe b))
runRequestMaybe credentialsProvider request = do
  credentials <- credentialsProvider
  response <- transform credentials (toRequest request) >>= httpLBS
  let responseCode = statusCode $ responseStatus response
  let outcome =
        if responseCode >= 400
          then Left <$> decode $ responseBody response
          else Right <$> decode $ responseBody response
  return outcome

runRequest
  :: (ToRequest a, FromJSON b)
  => IO Credentials -> a -> IO (Either APIError b)
runRequest credentialsProvider request =
  throwEitherMaybe (runRequestMaybe credentialsProvider request)

errExample :: IO (Either APIError Instance)
errExample = runRequest defaultCredentialsProvider request2

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------
getInstance :: CredentialsProvider
            -> GetInstanceRequest
            -> BMCAPIResponse Instance
getInstance = runRequest

-- | List instances
--
-- @
--   listInstances (listInstancesRequest compartmentOCID)
-- @
listInstances :: CredentialsProvider
              -> ListInstancesRequest
              -> BMCAPIResponse [Instance]
listInstances = runRequest

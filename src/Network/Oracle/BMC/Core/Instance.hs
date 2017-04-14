{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Instance where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)
import Network.Oracle.BMC.Core.Types.Instance
import Network.Oracle.BMC.Credentials
       (Credentials, defaultCredentialsProvider)
import qualified Network.Oracle.BMC.Transport.Request as Request

import qualified
       Network.Oracle.BMC.Core.Requests.ListInstancesRequest as LIR

transform credentialsProvider request = do
  credentials <- credentialsProvider
  Request.transform credentials request

request =
  LIR.listInstancesRequest
    "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

dispatch credentials request =
  transform credentials (LIR.toRequest request) >>= httpJSON

main :: IO [Instance]
main = do
  response <- dispatch defaultCredentialsProvider request
  let body = Client.responseBody response
  return $ body

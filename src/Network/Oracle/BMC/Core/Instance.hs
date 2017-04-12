{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Instance where

import Network.HTTP.Simple
import qualified Network.Oracle.BMC.Transport.Request as Request

import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)

import qualified Data.ByteString as BS

import Network.Oracle.BMC.Credentials(Credentials, defaultCredentialsProvider)

versionedPath
  :: (Semigroup a, IsString a)
  => a -> a
versionedPath path = "/20160918" <> path

----------------------------------------------------------------------
compartment =
  "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

mkBaseRequest :: BS.ByteString -> Request
mkBaseRequest path =
  setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
  setRequestSecure True $
  setRequestPort 443 $
  setRequestPath (versionedPath path) $
  defaultRequest

listInstances :: BS.ByteString -> Request
listInstances compartmentId =
  setRequestQueryString [("compartmentId", Just compartmentId)] $
  mkBaseRequest "/instances"


listInstancesRequest :: IO Credentials -> IO Request
listInstancesRequest credentialsProvider = do
  credentials <- credentialsProvider
  req <-
    Request.transform
      credentials
      (listInstances
         "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq")
  return req

main = listInstancesRequest defaultCredentialsProvider >>= httpLbs

{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Instance where

import Network.HTTP.Simple
import qualified Network.Oracle.BMC.Transport.Request as Request

import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)

import qualified Data.ByteString as BS

import qualified Network.Oracle.BMC.Credentials as Credentials

base :: String
base = "https://iaas.us-phoenix-1.oraclecloud.com"

versionedPath
  :: (Semigroup a, IsString a)
  => a -> a
versionedPath path = "20160918" <> path

----------------------------------------------------------------------
compartment =
  "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

listInstances :: BS.ByteString -> Request
listInstances compartmentId =
  setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
  setRequestSecure True $
  setRequestPort 443 $
  setRequestPath "/20160918/instances" $
  setRequestQueryString [("compartmentId", Just compartmentId)] $ defaultRequest

binRequest compartmentId =
  setRequestHost "requestb.in" $
  setRequestPath "/10r0cri1" $
  setRequestQueryString [("compartmentId", Just compartmentId)] $ defaultRequest

testing = do
  credentials <- Credentials.defaultCredentialsProvider
  req <- Request.transform credentials (binRequest compartment)
  return req

testRequest = do
  credentials <- Credentials.defaultCredentialsProvider
  req <-
    Request.transform
      credentials
      (listInstances
         "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq")
  return req

main = testRequest >>= httpLbs

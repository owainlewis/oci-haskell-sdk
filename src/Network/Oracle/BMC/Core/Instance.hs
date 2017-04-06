{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.Instance where

import           Network.HTTP.Simple
import           Network.Oracle.BMC.Transport.Request

import Data.ByteString.Char8

import Data.Semigroup((<>), Semigroup)
import Data.String(IsString)

base :: String
base = "https://iaas.us-phoenix-1.oraclecloud.com"

versionedPath :: (Semigroup a, IsString a) => a -> a
versionedPath path = "20160918" <> path

listInstances compartmentId =
    setRequestHost "https://iaas.us-phoenix-1.oraclecloud.com" $
    setRequestPath "/instances" $
    setRequestQueryString [("compartmentId", Just compartmentId)]
    defaultRequest


{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.Requests.ListInstancesRequest where

import Network.HTTP.Simple
import qualified Data.ByteString as BS

----------------------------------------------------------------------

data Path a = Path a deriving ( Eq, Show )

data Query a = Query a deriving ( Eq, Show )

unQuery :: Query t -> t
unQuery (Query x) = x

unPath :: Path t -> t
unPath (Path x) = x

data ListInstancesRequest = ListInstancesRequest {
    compartmentId :: Query BS.ByteString
  , availabilityDomain :: Query (Maybe BS.ByteString)
  , displayName :: Query (Maybe BS.ByteString)
  , limit :: Query (Maybe Int)
  , page :: Query (Maybe BS.ByteString)
} deriving ( Eq, Show )

req = ListInstancesRequest {
    compartmentId = Query "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"
  , availabilityDomain = Query Nothing
  , displayName = Query Nothing
  , limit = Query Nothing
  , page = Query Nothing
}

class ToRequest a where
    toRequest :: a -> Request

instance ToRequest ListInstancesRequest where
  toRequest (ListInstancesRequest c a d l p) =
    setRequestQueryString [("compartmentId", Just $ unQuery c)] $
    mkBaseRequest "/20160918/instances"

mkBaseRequest :: BS.ByteString -> Request
mkBaseRequest path =
  setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
  setRequestSecure True $
  setRequestPort 443 $
  setRequestPath path $
  defaultRequest

listInstances :: BS.ByteString -> Request
listInstances compartmentId =
  setRequestQueryString [("compartmentId", Just compartmentId)] $
  mkBaseRequest "/instances"

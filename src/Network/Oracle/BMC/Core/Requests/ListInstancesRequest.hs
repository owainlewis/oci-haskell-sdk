{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListInstancesRequest where

import qualified Data.ByteString as BS
import Network.HTTP.Simple
import Network.Oracle.BMC.Core.Requests.Base (mkBaseRequest)
import Network.Oracle.BMC.Internal.Request

----------------------------------------------------------------------
data ListInstancesRequest = ListInstancesRequest
  { compartmentId :: Query BS.ByteString
  , availabilityDomain :: Maybe (Query BS.ByteString)
  , displayName :: Maybe (Query BS.ByteString)
  , limit :: Maybe (Query Int)
  , page :: Maybe (Query BS.ByteString)
  } deriving (Eq, Show)

-- | Smart constructor for a list instances request
--
-- @
--   let request = listInstancesRequest compartment in
--     request { availabilityDomain = "PHX" }
-- @
listInstancesRequest :: BS.ByteString -> ListInstancesRequest
listInstancesRequest compartmentId =
  ListInstancesRequest
  { compartmentId = Query compartmentId
  , availabilityDomain = Nothing
  , displayName = Nothing
  , limit = Nothing
  , page = Nothing
  }

instance ToRequest ListInstancesRequest where
  toRequest (ListInstancesRequest c a d l p) =
    setRequestQueryString [("compartmentId", Just $ unQuery c)] $
    mkBaseRequest "/20160918/instances"

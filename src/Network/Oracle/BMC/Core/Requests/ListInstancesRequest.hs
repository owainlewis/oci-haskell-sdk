{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListInstancesRequest where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Simple
import Network.Oracle.BMC.Core.Requests.Base (mkBaseRequest)
import Network.Oracle.BMC.Internal.Request

import Data.Maybe (catMaybes)

----------------------------------------------------------------------
data ListInstancesRequest = ListInstancesRequest
  { compartmentId :: Query BS.ByteString
  , availabilityDomain :: Maybe (Query BS.ByteString)
  , displayName :: Maybe (Query BS.ByteString)
  , limit :: Maybe (Query Int)
  , page :: Maybe (Query Int)
  } deriving (Eq, Show)

-- | Smart constructor for a list instances request
--
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
  toRequest request@(ListInstancesRequest c a d l p) =
    setRequestQueryString (extractQuery request) $
    mkBaseRequest "/20160918/instances"
  extractQuery request =
    flattenQuery
      [ ("compartmentId", (pure . compartmentId $ request))
      , ("availabilityDomain", (availabilityDomain request))
      , ("displayName", (displayName request))
      ]

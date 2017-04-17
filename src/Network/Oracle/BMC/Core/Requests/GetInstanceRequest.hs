{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetInstanceRequest where

import qualified Data.ByteString as BS
import Data.Semigroup ((<>))
import Network.HTTP.Simple
import Network.Oracle.BMC.Core.Requests.Base (mkBaseRequest)
import Network.Oracle.BMC.Internal.Request

----------------------------------------------------------------------
data GetInstanceRequest = GetInstanceRequest
  { instanceId :: Path BS.ByteString
  } deriving (Eq, Show)

getInstanceRequest instanceId =
  GetInstanceRequest {instanceId = Path instanceId}

instance ToRequest GetInstanceRequest where
  toRequest (GetInstanceRequest instanceId) =
    mkBaseRequest $ "/20160918/instances/" <> (unPath instanceId)
  extractQuery _ = []

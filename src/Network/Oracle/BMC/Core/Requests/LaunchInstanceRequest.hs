{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.LaunchInstanceRequest where

import qualified Data.ByteString as BS
import Network.HTTP.Simple
import Network.Oracle.BMC.Core.Requests.Base (mkBaseRequest)
import Network.Oracle.BMC.Internal.Query
import Network.Oracle.BMC.Internal.Request

----------------------------------------------------------------------
data LaunchInstanceRequest = LaunchInstanceRequest
  { compartmentId :: Query BS.ByteString
  , availabilityDomain :: Query BS.ByteString
  , imageId :: Query BS.ByteString
  , shape :: Query BS.ByteString
  , subnetId :: Query BS.ByteString
  } deriving (Eq, Show)

launchInstanceRequest
  :: BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> LaunchInstanceRequest
launchInstanceRequest compartmentId availabilityDomain imageId shape subnetId =
  LaunchInstanceRequest
  { compartmentId = Query compartmentId
  , availabilityDomain = Query availabilityDomain
  , imageId = Query imageId
  , shape = Query shape
  , subnetId = Query subnetId
  }

instance ToRequest LaunchInstanceRequest where
  toRequest request@(LaunchInstanceRequest compartmentId availabilityDomain imageId shape subnetId) =
    setRequestMethod "POST" $ mkBaseRequest "/20160918/instances"
  extractQuery request =
    flattenQuery
      [ ("compartmentId", (pure . compartmentId $ request))
      , ("availabilityDomain", (pure . availabilityDomain $ request))
      , ("imageId", (pure . imageId $ request))
      , ("shape", (pure . shape $ request))
      , ("subnetId", (pure . subnetId $ request))
      ]

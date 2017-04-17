{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Compute
  ( getInstance
  , listInstances
  ) where

import Network.Oracle.BMC.Core.Model.Instance
import Network.Oracle.BMC.Core.Requests.GetInstanceRequest
import Network.Oracle.BMC.Core.Requests.ListInstancesRequest
import Network.Oracle.BMC.Credentials
import Network.Oracle.BMC.Internal.Dispatcher

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

eg = listInstances defaultCredentialsProvider (listInstancesRequest "")

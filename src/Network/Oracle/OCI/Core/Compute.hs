{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.OCI.Core.Compute
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- A high level client for the Oracle OCIS compute services
--
-----------------------------------------------------------------------------
module Network.Oracle.OCI.Core.Compute
  ( getInstance
  , listInstances
  ) where

import           Network.Oracle.OCI.Core.Model.Instance
import           Network.Oracle.OCI.Core.Requests.GetInstanceRequest
import           Network.Oracle.OCI.Core.Requests.LaunchInstanceRequest
import           Network.Oracle.OCI.Core.Requests.ListInstancesRequest
import           Network.Oracle.OCI.Credentials
import           Network.Oracle.OCI.Internal.Dispatcher

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------
getInstance :: CredentialsProvider
            -> GetInstanceRequest
            -> OCIAPIResponse Instance
getInstance = runRequest

-- | List instances
--
-- @
--   listInstances (listInstancesRequest compartmentOCID)
-- @
listInstances :: CredentialsProvider
              -> ListInstancesRequest
              -> OCIAPIResponse [Instance]
listInstances = runRequest

launchExample =
  runRequestRaw
    defaultCredentialsProvider
    (launchInstanceRequest ad compartmentId "" "" "")
  where
    ad = ""
    compartmentId = ""

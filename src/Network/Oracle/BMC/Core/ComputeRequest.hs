{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.ComputeRequest
  ( mkBaseRequest
  ) where

import qualified Data.ByteString as BS
import Network.HTTP.Simple

-- | Defines the base for all Oracle Bare Metal compute requests
--
mkBaseRequest :: BS.ByteString -> Request
mkBaseRequest path =
  setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
  setRequestSecure True $
  setRequestPort 443 $ setRequestPath path $ defaultRequest

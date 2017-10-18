{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetIPSecConnectionDeviceStatusRequest where

data GetIPSecConnectionDeviceStatusRequest = GetIPSecConnectionDeviceStatusRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetIPSecConnectionDeviceStatusRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

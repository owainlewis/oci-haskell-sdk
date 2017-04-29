
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetIPSecConnectionDeviceConfigRequest where

data GetIPSecConnectionDeviceConfigRequest = GetIPSecConnectionDeviceConfigRequest {

} deriving ( Eq, Show )

instance ToRequest GetIPSecConnectionDeviceConfigRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


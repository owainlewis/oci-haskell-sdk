
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateVolumeRequest where

data CreateVolumeRequest = CreateVolumeRequest {

} deriving ( Eq, Show )

instance ToRequest CreateVolumeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


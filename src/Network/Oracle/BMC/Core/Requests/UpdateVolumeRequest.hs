
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateVolumeRequest where

data UpdateVolumeRequest = UpdateVolumeRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateVolumeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


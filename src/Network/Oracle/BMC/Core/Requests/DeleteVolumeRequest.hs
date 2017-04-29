
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteVolumeRequest where

data DeleteVolumeRequest = DeleteVolumeRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteVolumeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


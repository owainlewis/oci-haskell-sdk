{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.AttachVolumeRequest where

data AttachVolumeRequest = AttachVolumeRequest {

} deriving ( Eq, Show )

instance ToRequest AttachVolumeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []



{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListVolumesRequest where

data ListVolumesRequest = ListVolumesRequest {

} deriving ( Eq, Show )

instance ToRequest ListVolumesRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


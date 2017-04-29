
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListVolumeAttachmentsRequest where

data ListVolumeAttachmentsRequest = ListVolumeAttachmentsRequest {

} deriving ( Eq, Show )

instance ToRequest ListVolumeAttachmentsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


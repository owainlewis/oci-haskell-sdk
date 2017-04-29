
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetVolumeAttachmentRequest where

data GetVolumeAttachmentRequest = GetVolumeAttachmentRequest {

} deriving ( Eq, Show )

instance ToRequest GetVolumeAttachmentRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


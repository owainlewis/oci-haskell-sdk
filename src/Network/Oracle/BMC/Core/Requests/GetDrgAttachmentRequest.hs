
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetDrgAttachmentRequest where

data GetDrgAttachmentRequest = GetDrgAttachmentRequest {

} deriving ( Eq, Show )

instance ToRequest GetDrgAttachmentRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


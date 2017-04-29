
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateDrgAttachmentRequest where

data CreateDrgAttachmentRequest = CreateDrgAttachmentRequest {

} deriving ( Eq, Show )

instance ToRequest CreateDrgAttachmentRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


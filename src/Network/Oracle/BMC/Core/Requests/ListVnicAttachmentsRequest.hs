
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListVnicAttachmentsRequest where

data ListVnicAttachmentsRequest = ListVnicAttachmentsRequest {

} deriving ( Eq, Show )

instance ToRequest ListVnicAttachmentsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


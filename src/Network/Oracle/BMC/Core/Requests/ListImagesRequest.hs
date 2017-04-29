
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListImagesRequest where

data ListImagesRequest = ListImagesRequest {

} deriving ( Eq, Show )

instance ToRequest ListImagesRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


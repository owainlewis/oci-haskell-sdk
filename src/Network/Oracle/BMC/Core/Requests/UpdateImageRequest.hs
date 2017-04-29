
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateImageRequest where

data UpdateImageRequest = UpdateImageRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateImageRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


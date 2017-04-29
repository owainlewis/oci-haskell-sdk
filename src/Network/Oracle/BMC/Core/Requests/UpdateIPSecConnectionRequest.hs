
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateIPSecConnectionRequest where

data UpdateIPSecConnectionRequest = UpdateIPSecConnectionRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateIPSecConnectionRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


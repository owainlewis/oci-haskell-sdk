
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateSecurityListRequest where

data UpdateSecurityListRequest = UpdateSecurityListRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateSecurityListRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


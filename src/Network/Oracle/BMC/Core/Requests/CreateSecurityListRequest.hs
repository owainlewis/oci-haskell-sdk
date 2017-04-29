
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateSecurityListRequest where

data CreateSecurityListRequest = CreateSecurityListRequest {

} deriving ( Eq, Show )

instance ToRequest CreateSecurityListRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []



{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetSubnetRequest where

data GetSubnetRequest = GetSubnetRequest {

} deriving ( Eq, Show )

instance ToRequest GetSubnetRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []



{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetRouteTableRequest where

data GetRouteTableRequest = GetRouteTableRequest {

} deriving ( Eq, Show )

instance ToRequest GetRouteTableRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


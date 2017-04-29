
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListRouteTablesRequest where

data ListRouteTablesRequest = ListRouteTablesRequest {

} deriving ( Eq, Show )

instance ToRequest ListRouteTablesRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


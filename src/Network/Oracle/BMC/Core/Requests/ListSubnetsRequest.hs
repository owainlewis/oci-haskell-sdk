
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListSubnetsRequest where

data ListSubnetsRequest = ListSubnetsRequest {

} deriving ( Eq, Show )

instance ToRequest ListSubnetsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


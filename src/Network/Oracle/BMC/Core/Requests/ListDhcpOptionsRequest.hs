
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListDhcpOptionsRequest where

data ListDhcpOptionsRequest = ListDhcpOptionsRequest {

} deriving ( Eq, Show )

instance ToRequest ListDhcpOptionsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


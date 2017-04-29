
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateDhcpOptionsRequest where

data CreateDhcpOptionsRequest = CreateDhcpOptionsRequest {

} deriving ( Eq, Show )

instance ToRequest CreateDhcpOptionsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


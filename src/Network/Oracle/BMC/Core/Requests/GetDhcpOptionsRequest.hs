
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetDhcpOptionsRequest where

data GetDhcpOptionsRequest = GetDhcpOptionsRequest {

} deriving ( Eq, Show )

instance ToRequest GetDhcpOptionsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


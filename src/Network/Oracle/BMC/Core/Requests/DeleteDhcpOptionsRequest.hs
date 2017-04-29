
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteDhcpOptionsRequest where

data DeleteDhcpOptionsRequest = DeleteDhcpOptionsRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteDhcpOptionsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


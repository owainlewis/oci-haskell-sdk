
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListVcnsRequest where

data ListVcnsRequest = ListVcnsRequest {

} deriving ( Eq, Show )

instance ToRequest ListVcnsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


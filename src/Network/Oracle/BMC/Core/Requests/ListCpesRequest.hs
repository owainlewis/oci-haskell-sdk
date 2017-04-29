
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListCpesRequest where

data ListCpesRequest = ListCpesRequest {

} deriving ( Eq, Show )

instance ToRequest ListCpesRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []



{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetDrgRequest where

data GetDrgRequest = GetDrgRequest {

} deriving ( Eq, Show )

instance ToRequest GetDrgRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


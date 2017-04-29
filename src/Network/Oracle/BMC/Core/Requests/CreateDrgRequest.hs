
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateDrgRequest where

data CreateDrgRequest = CreateDrgRequest {

} deriving ( Eq, Show )

instance ToRequest CreateDrgRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


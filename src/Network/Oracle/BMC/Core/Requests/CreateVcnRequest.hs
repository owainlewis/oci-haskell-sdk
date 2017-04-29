
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateVcnRequest where

data CreateVcnRequest = CreateVcnRequest {

} deriving ( Eq, Show )

instance ToRequest CreateVcnRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


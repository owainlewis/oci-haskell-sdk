
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateIPSecConnectionRequest where

data CreateIPSecConnectionRequest = CreateIPSecConnectionRequest {

} deriving ( Eq, Show )

instance ToRequest CreateIPSecConnectionRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []



{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateSubnetRequest where

data CreateSubnetRequest = CreateSubnetRequest {

} deriving ( Eq, Show )

instance ToRequest CreateSubnetRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


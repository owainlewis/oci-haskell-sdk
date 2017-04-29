
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateInstanceRequest where

data UpdateInstanceRequest = UpdateInstanceRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateInstanceRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


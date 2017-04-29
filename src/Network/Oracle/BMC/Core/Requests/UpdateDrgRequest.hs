
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateDrgRequest where

data UpdateDrgRequest = UpdateDrgRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateDrgRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


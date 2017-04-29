
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteDrgRequest where

data DeleteDrgRequest = DeleteDrgRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteDrgRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


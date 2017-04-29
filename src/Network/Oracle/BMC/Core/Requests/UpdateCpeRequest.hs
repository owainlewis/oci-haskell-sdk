
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateCpeRequest where

data UpdateCpeRequest = UpdateCpeRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateCpeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []



{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetVnicRequest where

data GetVnicRequest = GetVnicRequest {

} deriving ( Eq, Show )

instance ToRequest GetVnicRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


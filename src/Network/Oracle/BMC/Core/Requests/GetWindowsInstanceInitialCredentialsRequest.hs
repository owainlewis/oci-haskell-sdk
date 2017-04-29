
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetWindowsInstanceInitialCredentialsRequest where

data GetWindowsInstanceInitialCredentialsRequest = GetWindowsInstanceInitialCredentialsRequest {

} deriving ( Eq, Show )

instance ToRequest GetWindowsInstanceInitialCredentialsRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


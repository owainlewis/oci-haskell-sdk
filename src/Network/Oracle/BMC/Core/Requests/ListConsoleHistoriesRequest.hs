
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListConsoleHistoriesRequest where

data ListConsoleHistoriesRequest = ListConsoleHistoriesRequest {

} deriving ( Eq, Show )

instance ToRequest ListConsoleHistoriesRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


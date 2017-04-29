
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetConsoleHistoryRequest where

data GetConsoleHistoryRequest = GetConsoleHistoryRequest {

} deriving ( Eq, Show )

instance ToRequest GetConsoleHistoryRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


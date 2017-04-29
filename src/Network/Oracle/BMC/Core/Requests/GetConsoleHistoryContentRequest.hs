
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetConsoleHistoryContentRequest where

data GetConsoleHistoryContentRequest = GetConsoleHistoryContentRequest {

} deriving ( Eq, Show )

instance ToRequest GetConsoleHistoryContentRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


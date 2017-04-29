
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteConsoleHistoryRequest where

data DeleteConsoleHistoryRequest = DeleteConsoleHistoryRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteConsoleHistoryRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


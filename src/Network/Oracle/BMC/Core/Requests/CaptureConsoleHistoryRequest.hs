{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CaptureConsoleHistoryRequest where

data CaptureConsoleHistoryRequest = CaptureConsoleHistoryRequest {

} deriving ( Eq, Show )

instance ToRequest CaptureConsoleHistoryRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


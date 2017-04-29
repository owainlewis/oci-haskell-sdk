{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateCpeRequest where

data CreateCpeRequest = CreateCpeRequest {

} deriving ( Eq, Show )

instance ToRequest CreateCpeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


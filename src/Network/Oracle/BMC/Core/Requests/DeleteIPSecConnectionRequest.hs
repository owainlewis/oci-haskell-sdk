
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteIPSecConnectionRequest where

data DeleteIPSecConnectionRequest = DeleteIPSecConnectionRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteIPSecConnectionRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


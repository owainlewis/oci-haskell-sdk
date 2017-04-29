
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteCpeRequest where

data DeleteCpeRequest = DeleteCpeRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteCpeRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


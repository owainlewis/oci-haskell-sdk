
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteVcnRequest where

data DeleteVcnRequest = DeleteVcnRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteVcnRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


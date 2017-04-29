
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteSubnetRequest where

data DeleteSubnetRequest = DeleteSubnetRequest {

} deriving ( Eq, Show )

instance ToRequest DeleteSubnetRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


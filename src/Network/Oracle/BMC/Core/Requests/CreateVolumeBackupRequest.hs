
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateVolumeBackupRequest where

data CreateVolumeBackupRequest = CreateVolumeBackupRequest {

} deriving ( Eq, Show )

instance ToRequest CreateVolumeBackupRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


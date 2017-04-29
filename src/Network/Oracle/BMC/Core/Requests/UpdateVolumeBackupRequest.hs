
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateVolumeBackupRequest where

data UpdateVolumeBackupRequest = UpdateVolumeBackupRequest {

} deriving ( Eq, Show )

instance ToRequest UpdateVolumeBackupRequest where
    toRequest request = error "Not defined"
    extractQuery _ = []


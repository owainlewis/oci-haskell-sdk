{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteVolumeBackupRequest where

data DeleteVolumeBackupRequest = DeleteVolumeBackupRequest
  {
  } deriving (Eq, Show)

instance ToRequest DeleteVolumeBackupRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

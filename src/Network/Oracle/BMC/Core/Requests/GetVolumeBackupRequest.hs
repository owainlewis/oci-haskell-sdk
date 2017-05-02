{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetVolumeBackupRequest where

data GetVolumeBackupRequest = GetVolumeBackupRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetVolumeBackupRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListVolumeBackupsRequest where

data ListVolumeBackupsRequest = ListVolumeBackupsRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListVolumeBackupsRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

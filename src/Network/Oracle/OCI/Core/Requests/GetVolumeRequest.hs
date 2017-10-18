{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetVolumeRequest where

data GetVolumeRequest = GetVolumeRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetVolumeRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

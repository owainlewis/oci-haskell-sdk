{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DetachVolumeRequest where

data DetachVolumeRequest = DetachVolumeRequest
  {
  } deriving (Eq, Show)

instance ToRequest DetachVolumeRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

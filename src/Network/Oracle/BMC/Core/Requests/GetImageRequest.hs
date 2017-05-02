{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetImageRequest where

data GetImageRequest = GetImageRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetImageRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

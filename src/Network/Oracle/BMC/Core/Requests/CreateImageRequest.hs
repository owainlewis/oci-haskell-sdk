{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateImageRequest where

data CreateImageRequest = CreateImageRequest
  {
  } deriving (Eq, Show)

instance ToRequest CreateImageRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

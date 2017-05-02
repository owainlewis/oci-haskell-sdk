{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteImageRequest where

data DeleteImageRequest = DeleteImageRequest
  {
  } deriving (Eq, Show)

instance ToRequest DeleteImageRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

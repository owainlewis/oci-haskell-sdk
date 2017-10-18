{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListIPSecConnectionsRequest where

data ListIPSecConnectionsRequest = ListIPSecConnectionsRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListIPSecConnectionsRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

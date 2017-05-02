{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetIPSecConnectionRequest where

data GetIPSecConnectionRequest = GetIPSecConnectionRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetIPSecConnectionRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

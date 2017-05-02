{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetSecurityListRequest where

data GetSecurityListRequest = GetSecurityListRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetSecurityListRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

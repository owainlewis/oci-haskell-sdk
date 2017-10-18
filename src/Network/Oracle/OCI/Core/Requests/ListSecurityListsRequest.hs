{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListSecurityListsRequest where

data ListSecurityListsRequest = ListSecurityListsRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListSecurityListsRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteSecurityListRequest where

data DeleteSecurityListRequest = DeleteSecurityListRequest
  {
  } deriving (Eq, Show)

instance ToRequest DeleteSecurityListRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []

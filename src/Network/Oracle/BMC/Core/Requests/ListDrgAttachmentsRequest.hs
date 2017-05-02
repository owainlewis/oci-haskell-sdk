{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListDrgAttachmentsRequest where

data ListDrgAttachmentsRequest = ListDrgAttachmentsRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListDrgAttachmentsRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
